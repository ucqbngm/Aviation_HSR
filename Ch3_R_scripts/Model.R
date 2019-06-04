setwd("D:/Aviation_HSR")
library(dplyr)
library(tidyr)
library(ggplot2)
source("E:/Dropbox/R/ggplot.R")

################################################# Load Sample Dataset ######################################
All <- read.csv('All_sample.csv')

All$share_ratio <- All$Air_share/All$HSR_share 
All$freq_diff <- log(All$Air_freq) - log(All$HSR_freq)
All$HSR_freq_share <- 100*All$HSR_freq/(sum(All$Air_freq)+sum(All$HSR_freq))
All$Air_freq_share <- 100*All$Air_freq/(sum(All$Air_freq) + sum(All$HSR_freq))
All$freq_share_diff <- log(All$Air_freq_share) - log(All$HSR_freq_share)

Short <- subset(All, (Air_dist<500))
Med <- subset(All, (Air_dist>=500 & Air_dist<=1000))
Long <- subset(All, Air_dist>1000)

ggplot(All, aes(x=-time_diff, y=HSR_share,label=city.pair))+geom_point()+geom_text(check_overlap = T)

logit.model <- lm(log(share_ratio) ~ price_diff + time_diff + freq_share_diff + AE_drive_diff, data=All)
summary(logit.model)

All$Utility_AIR <- All$Air_price*logit.model$coefficients[2] + All$Air_time_hrs*logit.model$coefficients[3]+
                   log(All$Air_freq_share)*logit.model$coefficients[4] + All$Air_AE_drive*logit.model$coefficients[5]

All$Utility_HSR <- logit.model$coefficients[1] + All$HSR_USD*logit.model$coefficients[2] + 
                    All$HSR_time*logit.model$coefficients[3] + log(All$HSR_freq_share)*logit.model$coefficients[4] + 
                    All$HSR_AE_drive*logit.model$coefficients[5]

All$logsum <- log(exp(All$Utility_AIR) + exp(All$Utility_HSR))

test <- lm(log(Total_demand) ~ log(Population) + log(Income) + Special_both + Special_none + logsum, data=All)
summary(test)

logit.model$coefficients[3]/logit.model$coefficients[2]

################################################## SUR Estimation ################################################
# Hausman test for the endogeneity of frequency share: 
# if the Hausman test rejected the null hypothesis of exogeneity, no endogenous variables
library(lmtest)
library(sandwich)
library(car)
library(AER)

All <- read.csv('All_sample.csv')
#All <- subset(All, (Air_dist<500))
#All <- subset(All, (Air_dist>=500 & Air_dist<=1000))
#All <- subset(All, Air_dist>1000)

All$HSR_freq_share <- 100*All$HSR_freq/(All$Air_freq+All$HSR_freq)
All$Air_freq_share <- 100*All$Air_freq/(All$Air_freq + All$HSR_freq)

ggplot(All, aes(y=Air_pax, x=Air_freq,label=city.pair))+geom_point()#+geom_text(check_overlap = T)
ggplot(All, aes(y=HSR_pax, x=HSR_freq,label=city.pair))+geom_point()+geom_text(check_overlap = T)

TSLS <- ivreg(log(Total_demand) ~ log(Population) + log(Income)+ Special_both + Special_none + log(Air_freq) + log(HSR_freq) |
                log(Population) + log(Income) + Special_both + Special_none + log(Air_freq_share)+log(HSR_freq_share), data = All)
summary(TSLS, vcov = sandwich, diagnostics = TRUE)

# two steps: first SUR get coefficients for logit model and compute the logsum
library(systemfit)

SUR_estimate <- function(group){
  group$HSR_freq_share <- 100*group$HSR_freq/(sum(group$Air_freq)+sum(group$HSR_freq))
  group$Air_freq_share <- 100*group$Air_freq/(sum(group$Air_freq) + sum(group$HSR_freq))
  group$freq_share_diff <- log(group$Air_freq_share) - log(group$HSR_freq_share)
  eq_1 <- log(Total_demand) ~ log(Population) + log(Income) + Special_both + Special_none 
  eq_2 <- log(share_ratio) ~ price_diff + time_diff + freq_share_diff + AE_drive_diff
  system <- list(eq1=eq_1, eq2=eq_2)
  sur <- systemfit(system, method='SUR', data=group)
  logsum <- log(exp(sur$coefficients[7]*group$Air_price + sur$coefficients[8]*group$Air_time_hrs + 
                      sur$coefficients[9]*log(group$Air_freq_share) + 
                      sur$coefficients[10]*group$Air_AE_drive) +
                  exp(sur$coefficients[6] + sur$coefficients[7]*group$HSR_USD + sur$coefficients[8]*group$HSR_time + 
                        sur$coefficients[9]*log(group$HSR_freq_share) + 
                        sur$coefficients[10]*group$HSR_AE_drive))
  
  # second step include the logsum in the gravity equation and re-estimate the SUR
  eq_1 <- log(Total_demand) ~ log(Population) + log(Income) + Special_both + Special_none + logsum
  eq_2 <- log(share_ratio) ~ price_diff + time_diff + freq_share_diff + AE_drive_diff
  summary(lm(eq_1, data=group))
  summary(lm(eq_2, data=group))
  system <- list(eq1=eq_1, eq2=eq_2)
  sur <- systemfit(system, method='SUR', data=group)
  
  return(sur)
}

result_All <- SUR_estimate(group=All)
summary(result_All)

confint(result_All)

choice_prob <- function(Group, model){
  ### use the logit model choice predictions
  Group$Utility_AIR <- Group$Air_price*model$coefficients[8] + Group$Air_time_hrs*model$coefficients[9]+
    log(Group$Air_freq_share)*model$coefficients[10] + Group$Air_AE_drive*model$coefficients[11]
  
  Group$Utility_HSR <- model$coefficients[7] + Group$HSR_USD*model$coefficients[8] + 
    Group$HSR_time*model$coefficients[9] + log(Group$HSR_freq_share)*model$coefficients[10] + 
    Group$HSR_AE_drive*model$coefficients[11]
  
  # compuate choice probability based on the utility
  Group$Pred_prob_HSR <- exp(Group$Utility_HSR)/(exp(Group$Utility_AIR) + exp(Group$Utility_HSR))
  Group$Pred_prob_AIR <- 1- Group$Pred_prob_HSR
  
  return(Group)
}

All <- choice_prob(Group=All, model=result_All)
Short <- choice_prob(Group=Short, model=result_Short)
Med <- choice_prob(Group=Med, model=result_Med)
Long <- choice_prob(Group=Long, model=result_Long)

###################################### Compuate Price and Time Elasticity #######################
load("D:/Aviation_HSR/Final_Model.RData")
elasticity <- function(group, model){
  group$Price_Elas_AIR <- (1-group$Pred_prob_AIR)*model$coefficients[8]*group$Air_price
  group$Price_Elas_HSR <- (1-group$Pred_prob_HSR)*model$coefficients[8]*group$HSR_USD
  # cross elasticity of choosing HSR, given airfare change
  group$Cross_Price_Elas_AIR <- -group$Pred_prob_AIR*model$coefficients[8]*group$Air_price
  group$Cross_Price_Elas_HSR <- -group$Pred_prob_HSR*model$coefficients[8]*group$HSR_USD
  
  group$Time_Elas_AIR <- (1-group$Pred_prob_AIR)*model$coefficients[9]*group$Air_time_hrs
  group$Time_Elas_HSR <- (1-group$Pred_prob_HSR)*model$coefficients[9]*group$HSR_time
  group$Cross_Time_Elas_AIR <- -group$Pred_prob_AIR*model$coefficients[9]*group$Air_time_hrs
  group$Cross_Time_Elas_HSR <- -group$Pred_prob_HSR*model$coefficients[9]*group$HSR_time
  
  return(group)
}

elast.summary <- function(group){
  direct.AIR.price <- sum(group$Price_Elas_AIR * group$Pred_prob_AIR)/sum(group$Pred_prob_AIR)
  # cross elasticity of choosing HSR, given airfare change
  cross.AIR.price <- sum(group$Cross_Price_Elas_AIR * group$Pred_prob_HSR)/sum(group$Pred_prob_HSR)
  direct.HSR.price <- sum(group$Price_Elas_HSR * group$Pred_prob_HSR)/sum(group$Pred_prob_HSR)
  cross.HSR.price <- sum(group$Cross_Price_Elas_HSR * group$Pred_prob_AIR)/sum(group$Pred_prob_AIR)
  
  direct.AIR.time <- sum(group$Time_Elas_AIR * group$Pred_prob_AIR)/sum(group$Pred_prob_AIR)
  cross.AIR.time <- sum(group$Cross_Time_Elas_AIR * group$Pred_prob_HSR)/sum(group$Pred_prob_HSR)
  direct.HSR.time <- sum(group$Time_Elas_HSR * group$Pred_prob_HSR)/sum(group$Pred_prob_HSR)
  cross.HSR.time <- sum(group$Cross_Time_Elas_HSR * group$Pred_prob_AIR)/sum(group$Pred_prob_AIR)
  
  output <- cbind.data.frame(Type = c('direct_AIR.price','cross_AIR.price','direct_HSR.price','cross_HSR.price',
                                      'direct_AIR.time','cross_AIR.time','direct_HSR.time','cross_HSR.time'), 
                             Value =c(direct.AIR.price,cross.AIR.price,direct.HSR.price,cross.HSR.price,
                                      direct.AIR.time,cross.AIR.time,direct.HSR.time,cross.HSR.time))
  return(output)
}


All <- elasticity(group=All, model=result_All)
Short <- elasticity(group=Short, model=result_Short)
Med <- elasticity(group=Med, model=result_Med)
Long <- elasticity(group=Long, model=result_Long)

final <-  merge(elast.summary(group=All),elast.summary(group=Short),by='Type')
names(final) <- c('Type','Full','Short')
final <- merge(final, merge(elast.summary(group=Med),elast.summary(group=Long),by='Type'),by='Type')
names(final) <- c('Type','Full','Short','Med','Long')
final <- final %>% separate(., col='Type', into=c('Type','Elast_type'),sep='_',remove = T)
final$Type <- as.factor(final$Type)

################################################## LM Estimation ##################################################
library(minpack.lm)
getPred <- function(parS, population, income, special_1, special_2, hsr_price, hsr_time, hsr_AE, air_AE,air_freq, hsr_freq,
                    air_price, air_time){
  
  parS$const + log(population)*parS$beta_pop + log(income)*parS$beta_inc + parS$beta_both*special_1 + parS$beta_none*special_2 + 
               parS$beta_logsum*(log(exp(parS$beta_cost*air_price + parS$beta_time*air_time + parS$beta_freq_ratio*air_freq+ parS$beta_AE_time*air_AE)+ 
                                     exp(parS$HSR_cons + parS$beta_cost*hsr_price + parS$beta_time*hsr_time + parS$beta_freq_ratio*hsr_freq +parS$beta_AE_time*hsr_AE)))
}


residFun <- function(p, observed, 
                     population, income, special_1, special_2, hsr_price, hsr_time, hsr_AE, air_AE,air_freq, hsr_freq,
                     air_price, air_time){
  
  observed - getPred(p, population, income, special_1, special_2, hsr_price, hsr_time, hsr_AE, air_AE,air_freq, hsr_freq,air_price, air_time)
}    


LM_estimate <- function(Init_Par, Resid, Group){
  nls.all <- nls.lm(par = Init_Par, fn = Resid, observed = log(Group$Total_demand), 
                    population = Group$Population, income = Group$Income, special_1 = Group$Special_both, special_2 = Group$Special_none,
                    hsr_price = Group$HSR_USD, hsr_time = Group$HSR_time, hsr_freq= log(Group$HSR_freq_share), hsr_AE = Group$HSR_AE_drive,
                    air_freq=log(Group$Air_freq_share), air_price = Group$Air_price, air_time = Group$Air_time_hrs, air_AE = Group$Air_AE_drive, 
                    control = nls.lm.control(nprint=1))
  return(nls.all)
}

# first step estimation
All <- read.csv('all_data.csv')
#All <- update(All)
All$HSR_freq_share <- 100*All$HSR_freq/(sum(All$Air_freq)+sum(All$HSR_freq))
All$Air_freq_share <- 100*All$Air_freq/(sum(All$Air_freq) + sum(All$HSR_freq))
All$freq_diff <- log(All$Air_freq) - log(All$HSR_freq)
All$freq_share_diff <- log(All$Air_freq_share) - log(All$HSR_freq_share)
logit.model <- lm(log(share_ratio) ~ price_diff + time_diff + freq_share_diff + AE_drive_diff, data=All)

All$Utility_AIR <- All$Air_price*logit.model$coefficients[2] + All$Air_time_hrs*logit.model$coefficients[3]+
  log(All$Air_freq_share)*logit.model$coefficients[4] + All$Air_AE_drive*logit.model$coefficients[5]

All$Utility_HSR <- logit.model$coefficients[1] + All$HSR_USD*logit.model$coefficients[2] + 
  All$HSR_time*logit.model$coefficients[3] + log(All$HSR_freq_share)*logit.model$coefficients[4] + 
  All$HSR_AE_drive*logit.model$coefficients[5]

All$logsum <- log(exp(All$Utility_AIR) + exp(All$Utility_HSR))
test <- lm(log(Total_demand) ~ log(Population) + log(Income) + Special_both + Special_none + logsum, data=All)

#### get initial parameters 
parS_full <- list(const= 8.39, beta_pop = 0.60, beta_inc = 0.82, beta_both = 0.39, beta_none = -0.35, beta_logsum = 0.49,
                  HSR_cons = -0.33, beta_cost = -0.0046, beta_time = -0.193, beta_freq_ratio=0.27,beta_AE_time = -0.007)

parS_short <- list(const= 9.69, beta_pop = 0.46, beta_inc = 0.85, beta_both = 0.37, beta_none = -0.52, beta_logsum = 0.93,
                 HSR_cons = -0.47, beta_cost = -0.003, beta_time = -0.201, beta_freq_ratio=0.244,beta_AE_time = -0.008)

parS_med <- list(const= 7.87, beta_pop = 0.64, beta_inc = 0.75, beta_both = 0.32, beta_none = -0.54, beta_logsum = 2.04,
                  HSR_cons = -0.66, beta_cost = -0.004, beta_time = -0.255, beta_freq_ratio=0.265,beta_AE_time = -0.007)

parS_long <- list(const= 8.52, beta_pop = 0.36, beta_inc = 0.90, beta_both = 0.69, beta_none = -0.74, beta_logsum = 0.55,
                 HSR_cons = -0.36, beta_cost = -0.005, beta_time = -0.201, beta_freq_ratio=0.217,beta_AE_time = -0.004)


result_int <- LM_estimate(Init_Par = parS_full,Resid = residFun, Group=All)
result<- summary(result_int)
regress <- summary(test)
logit <- summary(logit.model)
result$coefficients[1:6,1:4] <- regress$coefficients[1:6,1:4]
result$coefficients[7:11,1:4] <- logit$coefficients[1:5,1:4]

LM_predict <- function(result_par, Group){
  
  predictions <- getPred(result_par, population = Group$Population, income = Group$Income, special_1 = Group$Special_both, 
                         special_2 = Group$Special_none,hsr_price = Group$HSR_USD, hsr_time = Group$HSR_time, 
                         hsr_AE = Group$HSR_AE_drive,air_AE = Group$Air_AE_drive,hsr_freq= log(Group$HSR_freq_share),
                         air_freq=log(Group$Air_freq_share), air_price = Group$Air_price, air_time = Group$Air_time_hrs)
  return(predictions)
}

result_par <- LM_estimate(Init_Par = parS_full, Resid = residFun, Group=All)$par
All$total_pred <- exp(LM_predict(result_par = result_par, Group=All))

#result_par2 <- as.list(result$coefficients[,1])
#All$total_pred <- exp(LM_predict(result_par = result_par2, Group=All))

summary(lm(Total_demand ~ total_pred,All))

ggplot(All, aes(x=Total_demand/1000000, y=total_pred/1000000, label=id)) + geom_point() +
  geom_text(check_overlap =T) + #
  #geom_smooth(method='lm')+
  geom_abline(intercept = 0, slope = 1, color='red')+
  scale_x_continuous(limits=c(0,round(max(All$Total_demand/1000000)))) + 
  scale_y_continuous(limits = c(0,round(max(All$Total_demand/1000000))))+
  xlab('Total City Pair Demand (millions)') + ylab('Predicted Total Demand (millions)')

choice_prob <- function(Group, model){
  ### use the logit model choice predictions
  Group$Utility_AIR <- Group$Air_price*logit.model$coefficients[2] + Group$Air_time_hrs*logit.model$coefficients[3]+
    log(Group$Air_freq_share)*logit.model$coefficients[4] + Group$Air_AE_drive*logit.model$coefficients[5]
  
  Group$Utility_HSR <- logit.model$coefficients[1] + Group$HSR_USD*logit.model$coefficients[2] + 
    Group$HSR_time*logit.model$coefficients[3] + log(Group$HSR_freq_share)*logit.model$coefficients[4] + 
    Group$HSR_AE_drive*logit.model$coefficients[5]
  
  # compuate choice probability based on the utility
  Group$Pred_prob_HSR <- exp(Group$Utility_HSR)/(exp(Group$Utility_AIR) + exp(Group$Utility_HSR))
  Group$Pred_prob_AIR <- 1- Group$Pred_prob_HSR
  
  return(Group)
}

All <- choice_prob(Group=All, model=logit.model)

All$Pred_Air <- All$total_pred * All$Pred_prob_AIR
summary(lm(Air_pax ~ Pred_Air,All))

ggplot(All, aes(x=Air_pax/1000000, y=Pred_Air/1000000, label=id)) + geom_point() + 
  geom_text(check_overlap =T)+ 
  geom_smooth(method='lm')+ 
  #geom_abline(intercept = 0, slope = 1, color='red')+
  scale_x_continuous(limits=c(0,round(max(All$Air_pax/1000000)))) + 
  #scale_y_continuous(limits = c(0,round(max(All$Air_pax/1000000))))+
  xlab('Total City Pair Air Demand (millions)') + ylab('Predicted Total Air Demand (millions)')



