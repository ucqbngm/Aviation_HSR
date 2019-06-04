setwd("D:/Aviation_HSR")
library(dplyr)
library(tidyr)
library(ggplot2)
library(systemfit)

# two gravity equations for air and HSR, and estimate using seemingly unrelated regression
All <- read.csv('All_sample.csv')
All <- update(All)

# add drive time 
drive_time <- read.csv('drive_time_CHN.csv')
drive_time$city.pair <- paste(drive_time$Origin.City,drive_time$Dest.City,sep='-')
drive_time$city.pair <- apply(sapply(strsplit(as.character(drive_time$city.pair), "-"), sort), 2, paste, collapse="-")
drive_time <- drive_time %>% group_by(city.pair) %>% summarise(drive_time = round(mean(drive_time2),2))

All <- merge(All, drive_time[,c('city.pair','drive_time')],by='city.pair',all.x = T)

Short <- subset(All, Air_dist < 500)
Med <- subset(All, Air_dist >=500 & Air_dist <=1000)
Long <- subset(All, Air_dist > 1000)

####################################### Seemingly Unrelated Regression #######################################
# two steps: first SUR get coefficients for logit model and compute the logsum
SUR_estimate <- function(group){
  eq_1 <- log(Total_demand) ~ log(Population) + log(Income) + Special_both + Special_none 
  eq_2 <- log(share_ratio) ~ price_diff + time_diff + AE_drive_diff
  system <- list(eq1=eq_1, eq2=eq_2)
  sur <- systemfit(system, method='SUR', data=group)
  logsum <- log(exp(sur$coefficients[7]*group$Air_price + sur$coefficients[8]*group$Air_time_hrs + 
                    sur$coefficients[9]*group$Air_AE_drive) +
                exp(sur$coefficients[6] + sur$coefficients[7]*group$HSR_USD + sur$coefficients[8]*group$HSR_time + 
                    sur$coefficients[9]*group$HSR_AE_drive))
  
  # second step include the logsum in the gravity equation and re-estimate the SUR
  eq_1 <- log(Total_demand) ~ log(Population) + log(Income) + Special_both + Special_none + logsum
  eq_2 <- log(share_ratio) ~ price_diff + time_diff + AE_drive_diff
  summary(lm(eq_1, data=group))
  summary(lm(eq_2, data=group))
  system <- list(eq1=eq_1, eq2=eq_2)
  sur <- systemfit(system, method='SUR', data=group)
  
  return(sur)
}

result_All <- SUR_estimate(group=All)
summary(result_All)

confint(result_All)

###################################### Predicting total OD demand using coefficients ##########################
SUR_predict <- function(result, group){
  group$total_pred <- exp(result$coefficients[1] + result$coefficients[2]*log(group$Population) + result$coefficients[3]*log(group$Income) +
                          result$coefficients[4]*group$Special_both + result$coefficients[5]*group$Special_none + 
                          result$coefficients[6]*(log(exp(result$coefficients[8]*group$Air_price + result$coefficients[9]*group$Air_time_hrs + 
                                                            result$coefficients[10]*group$Air_AE_drive)+
                                                      exp(result$coefficients[7] + result$coefficients[8]*group$HSR_USD + 
                                                           result$coefficients[9]*group$HSR_time + result$coefficients[10]*group$HSR_AE_drive))))
  
  ### use the logit model choice predictions
  group$Utility_AIR <- group$Air_price*result$coefficients[8] + group$Air_time_hrs*result$coefficients[9] +
                       group$Air_AE_drive*result$coefficients[10] 
  
  group$Utility_HSR <- result$coefficients[7] + group$HSR_USD*result$coefficients[8] + group$HSR_time*result$coefficients[9] +
                       group$HSR_AE_drive*result$coefficients[10]
  
  # compuate choice probability based on the utility
  group$Pred_prob_HSR <- exp(group$Utility_HSR)/(exp(group$Utility_AIR) + exp(group$Utility_HSR))
  group$Pred_prob_AIR <- 1- group$Pred_prob_HSR
  
  group$Pred_Air <- group$total_pred * group$Pred_prob_AIR
  
  return(group)
  
}

All <- SUR_predict(result=result_All, group=All)
Short <- SUR_predict(result=result_Short, group=Short)
Med <- SUR_predict(result=result_Med, group=Med)
Long <- SUR_predict(result=result_Long, group=Long)


summary(lm(Total_demand ~ total_pred, All))
ggplot(All, aes(x=Total_demand/1000000, y=total_pred/1000000, label=id))+
  geom_point()+geom_text(check_overlap = T) + geom_abline(intercept = 0, slope = 1, color='red')+
  scale_x_continuous(limits=c(0,max(All$Total_demand/1000000))) + 
  scale_y_continuous(limits = c(0,max(All$Total_demand/1000000)))+
  xlab('Total City Pair Demand (millions)') + ylab('Predicted Total Demand (millions)')

summary(lm(Air_pax ~ Pred_Air,All))

ggplot(All, aes(x=Air_pax/1000000, y=Pred_Air/1000000, label=id)) + geom_point() + 
  geom_text(check_overlap =T)+ #geom_smooth(method='lm')+ 
  geom_abline(intercept = 0, slope = 1, color='red')+
  #scale_x_continuous(limits=c(0,max(validate$Air_pax/1000000))) + 
  scale_y_continuous(limits = c(0,max(All$Air_pax/1000000)))+
  xlab('Total City Pair Air Demand (millions)') + ylab('Predicted Total Air Demand (millions)')

summary(lm(Air_pax ~ Pred_Air,All))
summary(lm(Total_demand ~ total_pred, All))

###################################### Compuate Price and Time Elasticity #######################
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
  
  output <- cbind.data.frame(Type = c('direct.AIR.price','cross.AIR.price','direct.HSR.price','cross.HSR.price',
                                      'direct.AIR.time','cross.AIR.time','direct.HSR.time','cross.HSR.time'), 
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


##################################### Hausman test on the endogeniety of the two equations ################################### 
library(lmtest)
library(sandwich)
library(car)
library(AER)

# if the null hypothesis of endogeniety are both rejected, we can use SUR
# use air and hsr freq share as instruments for actual freq
ggplot(data = All, aes(x=HSR_dist, y=HSR_USD, label=id))+ geom_point() + geom_text(check_overlap = T)


TSLS_logit <- ivreg(log(Total_demand) ~ log(Population) + log(Income) + log(Air_freq) + log(HSR_freq) |
                      log(Population) + log(Income) + log(Air_freq_share) + log(HSR_freq_share), data = All)
summary(TSLS_logit, vcov = sandwich, diagnostics = TRUE)


# or we can do it using ivreg
TSLS_hsr <- ivreg(log(HSR_pax) ~ log(Population) + log(Income) + Special_both + Special_none + log(HSR_USD) + log(HSR_time) + log(HSR_AE_drive) + log(HSR_freq) |
                  log(Population) + log(Income) + Special_both + Special_none + log(HSR_USD)+ log(HSR_time) + log(HSR_AE_drive) + log(HSR_freq_share), data = All)
summary(TSLS_hsr, vcov = sandwich, diagnostics = TRUE)

TSLS_air <- ivreg(log(Air_pax) ~ log(Population) + log(Income) + Special_both + Special_none + log(Air_price) + log(Air_time_hrs) + log(Air_AE_drive) + log(Air_freq) |
                    log(Population) + log(Income) + Special_both + Special_none + log(Air_price)+ log(Air_time_hrs) + log(Air_AE_drive) + log(Air_freq_share), data = All)
summary(TSLS_air, vcov = sandwich, diagnostics = TRUE)

################## Do the same for air ###################
# 1) first stage: regress the endogenous variable over All exogenous variables and the IV
first_stage_air <- lm(log(Air_price) ~ log(Population) + log(Income) + Special_both + Special_none + log(Air_time_hrs) + log(Air_AE_drive) + log(Air_dist), data=All)
summary(first_stage_air)
resid.hat_air <- first_stage_air$residuals

# 2) second stage: replace price by the fitted value of the first stage regression: if resid.hat1 is insignificant, no endogenous variables
second_stage_air <- lm(log(Air_pax) ~ log(Population) + log(Income) + Special_both + Special_none + 
                         log(Air_price) + log(Air_time_hrs) + log(Air_AE_drive) + resid.hat_air , data=All)
summary(second_stage_air)

TSLS_air <- ivreg(log(Air_pax) ~ log(Population) + log(Income) + Special_both + Special_none + log(Air_price) + log(Air_time_hrs) + log(Air_AE_drive) |
                    log(Population) + log(Income) + Special_both + Special_none + log(Air_time_hrs) + log(Air_AE_drive) + log(Air_dist), data = All)
summary(TSLS_air, vcov = sandwich, diagnostics = TRUE)

