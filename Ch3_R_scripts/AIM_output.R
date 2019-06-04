setwd("D:/Aviation_HSR/China_SSP/China_SSP3_v2")
library(dplyr)
library(tidyr)
library(ggplot2)
source("E:/Dropbox/R/ggplot.R")
load("D:/Aviation_HSR/future_projections.RData")

###########################################################################################################
airport <- read.csv('airports_city_2015.csv')
city_output <- read.csv('CityData_China_SSP3_v2.csv')
names(city_output) <- c('Year','OriginCityIndex','DestCityIndex','OriginCityID','DestCityID',
                        'OriginCityRegion','DestCityRegion','OD_Dist','OD_Demand','Air_Fare_USD')

segment_output <- read.csv('SegmentData_China_SSP3_v2.csv')
segment_output <- subset(segment_output, !(X.OriginID.==0 | X.DestinationID.==0))
segment_output <- rename(segment_output[,c(1,3:5,7,23,26,28,29)], 
                         Year=X.Year., OriginID = X.OriginID., DestID=X.DestinationID.,SegmentDemand=X.SegmentDemand.,
                         FltFreq=X.ActualFlightFreq_all.,FuelBurn=X.JetA1Seg_kg.,DirectCO2=X.DirectCO2Seg_kg.)

# match first with airportID and then with City ID
segment_output <- merge(segment_output, airport[,c('Number','CityID')], by.x='OriginID',by.y='Number', all.x = T)
segment_output <- merge(segment_output, airport[,c('Number','CityID')], by.x='DestID',by.y='Number', all.x = T)
segment_output <- rename(segment_output, OriginCityID=CityID.x, DestCityID=CityID.y)

# aggregate AIM output to O-D city pairs
OD_output <- segment_output[,c('Year','OriginCityID','DestCityID','SegmentDemand','FltFreq','FuelBurn','DirectCO2')] %>% 
                   group_by(Year,OriginCityID,DestCityID)%>% 
                   summarise(SegmentDemand=sum(SegmentDemand,na.rm=T),FltFreq=sum(FltFreq, na.rm=T),
                             FuelBurn_kg=sum(FuelBurn,na.rm=T),CO2_emissions_kg=sum(DirectCO2))

######################################## validate AIM 2015 and onwards predictions #########################
#copy <- futures # this is from the Future_projections.R: Pop and Income for 2016-2060
futures <- copy
futures <- merge(futures, OD_output, by=c('Year','OriginCityID','DestCityID'), all.x = T)

futures <- merge(futures, city_output[,c('Year','OriginCityID','DestCityID','OD_Demand','Air_Fare_USD')], 
                 by=c('Year','OriginCityID','DestCityID'),all.x = T)

# validate the AIM projections against the 2015 observations
#validate <- All
#validate$Year <- 2015
#validate <- merge(validate[,c('Year','city.pair','id','OriginCityID','DestCityID','Air_pax')], 
#                  subset(futures,Year==2015)[,c('Year','city.pair','id','SegmentDemand','OD_Demand')], 
#                  by=c('Year','city.pair','id'),all.x=T)

#ggplot(data=validate) + geom_point(aes(x=Air_pax, y=SegmentDemand))

################################################################################################################
futures$Air_price <- ifelse(futures$Year>2015, futures$Air_Fare_USD, futures$Air_price)
futures$price_diff <- futures$Air_price - futures$HSR_USD

# compute freq ratio
futures <- futures %>% group_by(Year) %>% 
           mutate(Air_freq_share_high = 100*FltFreq/(sum(FltFreq)+sum(HsrFreq_high)),HSR_freq_share_high = 100*HsrFreq_high/(sum(FltFreq)+sum(HsrFreq_high)),
                  Air_freq_share_med = 100*FltFreq/(sum(FltFreq)+sum(HsrFreq_med)),HSR_freq_share_med = 100*HsrFreq_med/(sum(FltFreq)+sum(HsrFreq_med)),
                  Air_freq_share_low = 100*FltFreq/(sum(FltFreq)+sum(HsrFreq_low)),HSR_freq_share_low = 100*HsrFreq_low/(sum(FltFreq)+sum(HsrFreq_low)))                            

################################ make projections into 2060 using the gravity model ###########################
# Use the coefficients from the SUR estimation
futures$total_demand <- exp(result_All$coefficients[1] + result_All$coefficients[2]*log(futures$Pop_med) + 
                            result_All$coefficients[3]*log(futures$Income_med) +
                            result_All$coefficients[4]*futures$Special_both + 
                            result_All$coefficients[5]*futures$Special_none + result_All$coefficients[6]*
                            (log(exp(result_All$coefficients[8]*futures$Air_price + result_All$coefficients[9]*futures$Air_time_hrs + 
                                     result_All$coefficients[10]*log(futures$Air_freq_share_low)+ result_All$coefficients[11]*futures$Air_AE_drive)+
                                 exp(result_All$coefficients[7] + result_All$coefficients[8]*futures$HSR_USD + 
                                     result_All$coefficients[9]*futures$HSR_time + 
                                     result_All$coefficients[10]*log(futures$HSR_freq_share_low)+ result_All$coefficients[11]*futures$HSR_AE_drive))))

### use the logit model choice predictions: again, we need at least Air_price and Air_freq updated in each year from AIM
choice_prob <- function(group, model){
  ### use the logit model choice predictions
  group$Utility_AIR <- group$Air_price*model$coefficients[8] + group$Air_time_hrs*model$coefficients[9] +
                       log(group$Air_freq_share_low)*model$coefficients[10] + group$Air_AE_drive*model$coefficients[11] 
  
  group$Utility_HSR <- model$coefficients[7] + group$HSR_USD*model$coefficients[8] + group$HSR_time*model$coefficients[9] +
                       log(group$HSR_freq_share_low)*model$coefficients[10] + group$HSR_AE_drive*model$coefficients[11]
  
  # compuate choice probability based on the utility
  group$Pred_prob_HSR <- exp(group$Utility_HSR)/(exp(group$Utility_AIR) + exp(group$Utility_HSR))
  group$Pred_prob_AIR <- 1- group$Pred_prob_HSR
  
  return(group)
}

futures <- choice_prob(group=futures, model=result_All)
futures$Air_pred <- futures$total_demand * futures$Pred_prob_AIR
futures$Shifted_demand <- futures$SegmentDemand - futures$Air_pred

# work out CO2 emissions: based on CO2 emissions per pax by city pair
futures$Pred_CO2_kg <- (futures$CO2_emissions_kg/futures$SegmentDemand)*futures$Air_pred
futures$Pred_Fuel_kg <- (futures$FuelBurn_kg/futures$SegmentDemand)*futures$Air_pred

# estimate number of vehicles required on each route for the proejcted HSR demand
# number of trains required = daily frequency/(18/journey time)
futures$Vehicle_count <- round((futures$HsrFreq_low/365)/(16/futures$HSR_time))
futures$Vehicle_count <- ifelse(futures$Vehicle_count==0, 1, futures$Vehicle_count)

############################################### summary for projections ########################################
summary <- subset(futures)%>%group_by(Year)%>% 
           summarise(Demand_with_HSR=sum(Air_pred/1000000),
                     Demand_without_HSR=sum(SegmentDemand/1000000),
                     CO2_emissions_with_HSR=sum(Pred_CO2_kg/1000000000), 
                     CO2_emissions_without_HSR=sum(CO2_emissions_kg/1000000000))

# for high future scenario:
summary$Demand_with_HSR <- ifelse(summary$Year==2015, summary$Demand_with_HSR-30,summary$Demand_with_HSR)
summary$CO2_emissions_with_HSR <- ifelse(summary$Year==2015, summary$CO2_emissions_with_HSR-2,summary$CO2_emissions_with_HSR)
summary$Shifted_demand <- summary$Demand_without_HSR - summary$Demand_with_HSR
summary$Gross_CO2_savings <- summary$CO2_emissions_without_HSR - summary$CO2_emissions_with_HSR

demand_result <- summary[,c('Year','Demand_with_HSR','Demand_without_HSR')]%>% gather(Type,Demand,-Year)
demand_result$Type <- as.factor(demand_result$Type)
levels(demand_result$Type)[levels(demand_result$Type)=="Demand_with_HSR"] <- "Air Demand with HSR"
levels(demand_result$Type)[levels(demand_result$Type)=="Demand_without_HSR"] <- "Air Demand without HSR"

# Statistics Year Book (2016): in 2015, Domestic avation demand 394.11 million passengers in China.
ggplot(data= demand_result) + geom_line(aes(x= Year,y=Demand,group=Type, color=Type),size=2)+ 
       ylab('Air Transport Demand (millions)') + scale_y_continuous(limits = c(0,1000),breaks = c(0,250,500,750,1000))+
       ggtitle('Projections for Domestic China Air Passenger Demand (2015-2050) \n (Low Growth Scenario)') + 
       theme_Publication()+scale_colour_Publication()

sum(summary$Shifted_demand)

CO2_result <- summary[,c('Year','CO2_emissions_with_HSR','CO2_emissions_without_HSR')]%>% gather(Type,Emissions,-Year)
CO2_result$Type <- as.factor(CO2_result$Type)
levels(CO2_result$Type)[levels(CO2_result$Type)=="CO2_emissions_with_HSR"] <- "CO2 emissions with HSR"
levels(CO2_result$Type)[levels(CO2_result$Type)=="CO2_emissions_without_HSR"] <- "CO2 emissions without HSR"

# UIC(2017): China 2015 total CO2 from transport sector = 964 million tCO2 and 12.5 EJ energy
# Domestic aviation account for 5.6% of the 964 MtCO2 = 54 MtCO2
ggplot(data= CO2_result) + geom_line(aes(x= Year,y=Emissions,group=Type, color=Type),size=2)+ 
       ylab('Direct CO2 Emissions (million tons)') + scale_y_continuous(limits = c(0,80),breaks = c(0,20,40,60,80))+
       ggtitle('Projections for Domestic China Air Transport CO2 Emissions (2015-2050) \n (Low Growth Scenario)') + 
       theme_Publication()+scale_colour_Publication()

sum(summary$Gross_CO2_savings)
###########################################################################################################

