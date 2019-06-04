


ggplot(data=validate, aes(x=logsum, y=(Total_demand), label=city.pair))+
  geom_point() + 
  geom_text_repel(data= subset(All,Total_demand>=2000000), 
                  aes(x=logsum, y=(Total_demand), label= city.pair),col = 'red', size = 3)


rm(list=setdiff(ls(), "Gravity"))


Gravity$Income_v1 <- Gravity$Origin_income*Gravity$Dest_income
Gravity$Population_v1 <- Gravity$Origin_Pop*Gravity$Dest_Pop
All_update <- merge(x=All_update,y=Gravity[,c('id','Income_v1','Population_v1')],by='id',all.x=T)


# first just inflation rates: 2018: CPI = 2.9
# base year income = current income /(100 + CPI)
All$Income <- All$Income*6.228505/1.029
# then adjust by the exchange rate of CNY to USD in 2015
All$Income <- All$Income/6.3986
All$HSR_USD <- (All$HSR_USD*6.228505/1.029)/6.3986


/(1.029*6.3986)



# income elasticity is too high, check
ggplot(All, aes(x=logsum, y=Total_demand, label=id)) + geom_point() + geom_text(check_overlap = T)


ggplot(All, aes(x=Income, y=Total_demand, label=id)) + geom_point() + geom_text(check_overlap = T)#+
  geom_text_repel(data= subset(All,Air_dist>1500 & Income <= 20), 
                  aes(x=Income, y=Total_demand, label= city.pair),col = 'red', size = 3)

ggplot(subset(All, Air_dist>1000 & Income <= 35), aes(x=Income, y=Total_demand, label=id)) + geom_point() + geom_text(check_overlap = T)#+
  geom_text_repel(data= subset(All,Air_dist>1000 & Income <= 30), 
                  aes(x=Income, y=Total_demand, label= city.pair),col = 'red', size = 3)

ggplot(All, aes(x=Population, y=Total_demand, label=id)) + geom_point() + geom_text(check_overlap = T)
ggplot(All, aes(x=Special_both, y=Total_demand, label=id)) + geom_point() + geom_text(check_overlap = T)


# logit model plot
ggplot(All, aes(y=HSR_share, x=-time_diff, label=id)) + geom_point(size=2) + geom_text(check_overlap =T)
  geom_text_repel(data= subset(All,city.pair == 'Qingdao-Shanghai'), 
                  aes(x=-time_diff, y=HSR_share, label= city.pair),col = 'red', size = 3)


ggplot(All, aes(y=log(share_ratio), x=price_diff, label=id)) + geom_point()+geom_text(check_overlap =T) #+ 
  geom_text_repel(data= subset(Med,city.pair == 'Qingdao-Shanghai'), 
                  aes(x=price_diff, y=log(share_ratio), label= city.pair),col = 'red', size = 3)
  #


ggplot(All, aes(y=log(share_ratio), x= freq_ratio_diff, label=id)) + geom_point() + geom_text(check_overlap =T)
  #geom_text_repel(data= subset(Med,city.pair == 'Qingdao-Shanghai'), 
  #                aes(x=price_diff, y=log(share_ratio), label= city.pair),col = 'red', size = 3)


library(ggrepel)
# HSR share plot
ggplot(validate, aes(y=Pred_prob_HSR, x = -time_diff, label=id)) + geom_point(size=2) + theme_Publication()+
  geom_text_repel(data= subset(model_2,city.pair == 'Beijing-Shanghai'), aes(x=-time_diff, y=HSR_share, label= city.pair),col = 'red', size = 3)+
  ylab('HSR market share (%)')+xlab('Time difference in hours (HSR - AIR)') #+ geom_text(check_overlap=T)



# final plot: total OD demand
ggplot(validate, aes(x=total_demand/1000000, y=total_pred/1000000, label=id)) + geom_point() +
  geom_text(check_overlap =T) + #geom_smooth(method='lm')+
  geom_abline(intercept = 0, slope = 1, color='red')+
  scale_x_continuous(limits=c(0,max(validate$total_demand/1000000))) + 
  scale_y_continuous(limits = c(0,max(validate$total_demand/1000000)))+
  xlab('Total City Pair Demand (millions)') + ylab('Predicted Total Demand (millions)')


# final plot: total air demand
ggplot(validate, aes(x=Air_pax/1000000, y=Pred_Air/1000000, label=id)) + geom_point() + 
  geom_text(check_overlap =T)+ #geom_smooth(method='lm')+ 
  geom_abline(intercept = 0, slope = 1, color='red')+
  #scale_x_continuous(limits=c(0,max(validate$Air_pax/1000000))) + 
  #scale_y_continuous(limits = c(0,max(validate$Air_pax/1000000)))+
  xlab('Total City Pair Air Demand (millions)') + ylab('Predicted Total Air Demand (millions)')















