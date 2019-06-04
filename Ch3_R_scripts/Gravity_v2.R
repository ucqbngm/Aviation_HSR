setwd("D:/Aviation_HSR")
library(dplyr)
library(tidyr)
library(ggplot2)

######################################## Create functions for computing various stage ###############################
# Logit Model Estimation: OLS estimation, add frequency as the ratio of freq to total
logit <- function(Group){
  output <- lm(log(share_ratio) ~ price_diff + time_diff + freq_ratio_diff + AE_drive_diff, data=Group)
  return(output)
}

# compute initial utility and logsum
utility <- function(Group){
  Group$Utility_AIR <- Group$Air_price*logit.model$coefficients[2] + Group$Air_time_hrs*logit.model$coefficients[3]+
                       log(Group$Air_freq_ratio)*logit.model$coefficients[4] + Group$Air_AE_drive*logit.model$coefficients[5]
  
  Group$Utility_HSR <- logit.model$coefficients[1] + Group$HSR_USD*logit.model$coefficients[2] + 
                       Group$HSR_time*logit.model$coefficients[3] + log(Group$HSR_freq_ratio)*logit.model$coefficients[4] + 
                       Group$HSR_AE_drive*logit.model$coefficients[5]
  
  Group$logsum <- log(exp(Group$Utility_AIR) + exp(Group$Utility_HSR))
  
  return(Group)
}

library(minpack.lm)
# estimate the model for: full 608 routes, Short-haul routes, Medium-haul routes, and Long-haul routes, respectively
getPred.int <- function(parS, population, income, special_1, special_2, logsum){
  
    parS$const * population^parS$beta_pop * income^parS$beta_inc * exp(parS$beta_both*special_1) * exp(parS$beta_none*special_2)* 
    exp(parS$beta_logsum*logsum)
}


getPred.int <- function(parS, population, income, special_1, special_2, logsum){
  
  parS$const + log(population)*parS$beta_pop + log(income)*parS$beta_inc + parS$beta_both*special_1 + parS$beta_none*special_2 + 
    parS$beta_logsum*logsum
}

# residual function for the initial LM estimation
residFun.int <- function(p, observed, population, income, special_1, special_2, logsum){
   observed - getPred.int(p,population, income, special_1, special_2, logsum)
}    

LM_int <- function(Init_Par, Resid, Group){
   nls.int <- nls.lm(par = Init_Par, fn = Resid, observed = log(Group$Total_demand), 
                    population = Group$Population, income = Group$Income, special_1 = Group$Special_both, 
                    special_2 = Group$Special_none,logsum = Group$logsum, control = nls.lm.control(nprint=1))
  
}

# generate estimation result from LM for the initialisation
output_int <- function(group){
  output <- LM_int(Init_Par = parS.int, Resid=residFun.int,Group=group)
  return(output)
}

####################################### Run above functions for each distance group ###########################
All <- read.csv('All_sample_v2.csv')
#All <- Gravity

All$HSR_freq_ratio <- 100*All$HSR_freq/(All$Air_freq + All$HSR_freq)
All$Air_freq_ratio <- 100*All$Air_freq/(All$Air_freq + All$HSR_freq)
All$freq_ratio_diff <- log(All$Air_freq_ratio) - log(All$HSR_freq_ratio)

Short <- subset(All, Air_dist < 500)
Med <- subset(All, Air_dist >=500 & Air_dist <=1000)
Long <- subset(All, Air_dist > 1000)

#Long <- subset(Long, Total_demand <= 1000000)#!city.pair=='Beijing-Shanghai') #
logit.model <- logit(Group= All)
summary(logit.model)
ggplot(All, aes(y=log(share_ratio), x=freq_ratio_diff, label=id)) + geom_point() + geom_text(check_overlap =T)

All <- utility(Group=All)
# initial parameters
parS.int <- list(const = 50, beta_pop = 1, beta_inc = 1, beta_both = 1, beta_none = -1, beta_logsum = 1)
result_int <- output_int(group=All)
summary(result_int)

validate <- All
parS <- result_int$par
validate$Pred_int <- getPred.int(parS, population = validate$Population, income = validate$Income, special_1 = validate$Special_both, 
                                 special_2 = validate$Special_none,logsum = validate$logsum)

summary(lm(Total_demand ~ Pred_int,validate))
ggplot(validate,#subset(validate,Total_demand>=1200000),##
       aes(x=Total_demand, y=Pred_int, label=id)) + geom_abline(intercept = 0, slope = 1)+
       #scale_y_continuous(limits=c(0,max(validate$Total_demand)))+
       geom_point() + geom_text(check_overlap =T) 
#ggplot(subset(Short, Air_dist>1000), aes(x=Income, y=Total_demand, label=id)) + geom_point() + geom_text(check_overlap = T)

library(ggrepel)
ggplot(validate, aes(y=HSR_share, x = -time_diff, label=id)) + geom_point(size=2) + 
  #geom_text_repel(data= subset(model_2,city.pair == 'Beijing-Shanghai'), aes(x=-time_diff, y=HSR_share, label= city.pair),col = 'red', size = 3)+
  ylab('HSR market share (%)')+xlab('Time difference in hours (HSR - AIR)') + geom_text(check_overlap=T)

ggplot(validate, aes(x=AE_drive_diff, y=log(share_ratio), label=id)) + geom_point() + geom_text(check_overlap =T)

########################################################### Full Model ################################################
getPred <- function(parS, population, income, special_1, special_2, air_price,hsr_price,air_time, hsr_time, 
                    air_freq, hsr_freq, hsr_AE, air_AE
                     ){
  
    parS$const * population^parS$beta_pop * income^parS$beta_inc * exp(parS$beta_both*special_1) * exp(parS$beta_none*special_2)* 
    exp(parS$beta_logsum*(log(exp(parS$beta_cost*air_price + parS$beta_time*air_time + parS$beta_freq_ratio*air_freq+ parS$beta_AE_time*air_AE)+ 
                              exp(parS$HSR_cons + parS$beta_cost*hsr_price + parS$beta_time*hsr_time + parS$beta_freq_ratio*hsr_freq +parS$beta_AE_time*hsr_AE))))
}

residFun <- function(p, observed, 
                      population, income, special_1, special_2, hsr_price, hsr_time, hsr_AE, air_AE,air_freq, hsr_freq,
                      air_price, air_time){
  
  observed - getPred(p, population, income, special_1, special_2, hsr_price, hsr_time, hsr_AE, air_AE,air_freq, hsr_freq,air_price, air_time)
}    


LM_estimate <- function(Init_Par, Resid, Group){
  nls.all <- nls.lm(par = Init_Par, fn = Resid, observed = Group$Total_demand, 
                    population = Group$Population, income = Group$Income, special_1 = Group$Special_both, special_2 = Group$Special_none,
                    hsr_price = Group$HSR_USD, hsr_time = Group$HSR_time, hsr_freq=log(Group$HSR_freq_ratio), hsr_AE = Group$HSR_AE_drive,
                    air_freq=log(Group$Air_freq_ratio), air_price = Group$Air_price, air_time = Group$Air_time_hrs, air_AE = Group$Air_AE_drive, 
                    control = nls.lm.control(nprint=1))
  return(nls.all)
}

LM_predict <- function(result_par, Group){

   predictions <- getPred(result_par, population = Group$Population, income = Group$Income, special_1 = Group$Special_both, 
                          special_2 = Group$Special_none,hsr_price = Group$HSR_USD, hsr_time = Group$HSR_time, 
                          hsr_AE = Group$HSR_AE_drive,air_AE = Group$Air_AE_drive,hsr_freq=log(Group$HSR_freq_ratio),
                          air_freq=log(Group$Air_freq_ratio), air_price = Group$Air_price, air_time = Group$Air_time_hrs)
  return(predictions)
}

result_summary <- function(result){
  result.logit <- summary(logit(Group=validate))
  result.nls <- summary(result_int)
  
  result$coefficients[1,1:4] <- result.nls$coefficients[1,1:4]
  result$coefficients[2,1:4] <- result.nls$coefficients[2,1:4]
  result$coefficients[3,1:4] <- result.nls$coefficients[3,1:4]
  result$coefficients[4,1:4] <- result.nls$coefficients[4,1:4]
  result$coefficients[5,1:4] <- result.nls$coefficients[5,1:4]
  result$coefficients[6,1:4] <- result.nls$coefficients[6,1:4]
  result$coefficients[7,1:4] <- result.logit$coefficients[1,1:4]
  result$coefficients[8,1:4] <- result.logit$coefficients[2,1:4]
  result$coefficients[9,1:4] <- result.logit$coefficients[3,1:4]
  result$coefficients[10,1:4] <- result.logit$coefficients[4,1:4]
  result$sigma <- result.nls$sigma
  result$niter <- result.nls$niter
  
  return(result)
  
}

##############################################################################################
parS_full <- list(const= 125.83, beta_pop = 0.689, beta_inc = 1.54, beta_both = 0.38, beta_none = -0.36, beta_logsum = 0.38,
                  HSR_cons = -1.084, beta_cost = -0.0042, beta_time = -0.2746, beta_freq_ratio=0.2004,beta_AE_time = -0.0004)

#parS_full <- list(const= 173.09, beta_pop = 0.55, beta_inc = 1.57, beta_both = 0.34, beta_none = -0.31, beta_logsum = 0.07,
#                  HSR_cons = -0.76, beta_cost = -0.0080, beta_time = -0.3261, beta_AE_time = -0.007)


parS_short <- list(const = 7092, beta_pop = 0.28, beta_inc = 0.94, beta_both = 0.70, beta_none = -0.42, beta_logsum = 0.14,
                   HSR_cons = 0.50, beta_cost = -0.0234, beta_time = -0.4018, beta_AE_time = -0.023)


parS_med <- list(const = 14710, beta_pop = 0.55, beta_inc = 0.64, beta_both = 0.16, beta_none = -0.64, beta_logsum = 0.45,
                   HSR_cons = -0.75, beta_cost = -0.0078, beta_time = -0.3362, beta_AE_time = -0.010)


parS_long <- list(const = 559.44, beta_pop = 0.61, beta_inc = 1.28, beta_both = 0.52, beta_none = -0.51, beta_logsum = 0.33,
                  HSR_cons = -0.39, beta_cost = -0.0063, beta_time = -0.2295, beta_AE_time = -0.003)

###############################################################################################
validate$total_demand <- validate$Total_demand
validate$Total_demand <- validate$Pred_int

result <- LM_estimate(Init_Par = parS_full, Resid = residFun, Group=All)
summary(result)






result <- result_summary(result)
result_par <- LM_estimate(Init_Par = parS_full, Resid = residFun, Group=validate)$par
result_par[1:10] <- result$coefficients[1:10]
validate$total_pred <- LM_predict(result_par = result_par, Group=validate)

summary(lm(total_demand ~ total_pred,validate))
ggplot(validate, aes(x=total_demand/1000000, y=total_pred/1000000, label=id)) + geom_point() +
  geom_text(check_overlap =T) + #geom_smooth(method='lm')+
  geom_abline(intercept = 0, slope = 1, color='red')+
  scale_x_continuous(limits=c(0,max(validate$total_demand/1000000))) + 
  scale_y_continuous(limits = c(0,max(validate$total_demand/1000000)))+
  xlab('Total City Pair Demand (millions)') + ylab('Predicted Total Demand (millions)')

ggplot(validate, aes(x=Air_dist, y=(total_demand-Pred_all), label=id)) + geom_point() + geom_text(check_overlap =T)


######################################## market share #####################################
choice_prob <- function(group, model){
  ### use the logit model choice predictions
  group$Utility_AIR <- group$Air_price*model$coefficients[8] + group$Air_time_hrs*model$coefficients[9] +
                       group$Air_AE_drive*model$coefficients[10] 
  
  group$Utility_HSR <- model$coefficients[7] + group$HSR_USD*model$coefficients[8] + group$HSR_time*model$coefficients[9] +
                       group$HSR_AE_drive*model$coefficients[10]
  
  # compuate choice probability based on the utility
  group$Pred_prob_HSR <- exp(group$Utility_HSR)/(exp(group$Utility_AIR) + exp(group$Utility_HSR))
  group$Pred_prob_AIR <- 1- group$Pred_prob_HSR
  
  return(group)
}

validate <- choice_prob(group=validate, model=result)

validate$Pred_Air <- validate$total_pred * validate$Pred_prob_AIR
summary(lm(Air_pax ~ Pred_Air,validate))

validate$Air_Erorr <- abs(validate$Air_pax - validate$Pred_Air)

ggplot(validate, aes(x=Air_pax/1000000, y=Pred_Air/1000000, label=id)) + geom_point() + 
  geom_text(check_overlap =T)+ #geom_smooth(method='lm')+ 
  geom_abline(intercept = 0, slope = 1, color='red')+
  #scale_x_continuous(limits=c(0,max(validate$Air_pax/1000000))) + 
  scale_y_continuous(limits = c(0,max(validate$Air_pax/1000000)))+
  xlab('Total City Pair Air Demand (millions)') + ylab('Predicted Total Air Demand (millions)')

ggplot(validate, aes(x=Air_share, y=Pred_prob_AIR)) + geom_point() + 
  #scale_y_continuous(limits=c(0,max(validate$Air_share)))+ 
  geom_abline(intercept=0,slope=1)







setwd("D:/Aviation_HSR")
All <- merge(All, Gravity[,c('id','Air_freq','HSR_freq')],by='id',all.x=T)

All <- All[,c('city.pair','id','OriginCityID','DestCityID','Population','Income','Special_both','Special_none',
              'Air_dist','HSR_dist','Air_freq','HSR_freq','Air_time_hrs','HSR_time','time_diff',
              'Air_price','HSR_USD','price_diff','Air_AE_drive','HSR_AE_drive','AE_drive_diff','Air_pax','HSR_pax',
              'Total_demand','Air_share','HSR_share','share_ratio')]

write.csv(All, 'All_sample_v2.csv',row.names = F)














