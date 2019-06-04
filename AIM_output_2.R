setwd("D:/Aviation_HSR/China_SSP/China_SSP3_dom")
library(dplyr)
library(tidyr)
library(ggplot2)
source("E:/Dropbox/R/ggplot.R")

###########################################################################################################
airport <- read.csv('airports_city_2015.csv')
city_output <- read.csv('CityData_China_SSP3_dom.csv')
names(city_output) <- c('Year','OriginCityIndex','DestCityIndex','OriginCityID','DestCityID',
                        'OriginCityRegion','DestCityRegion','OD_Dist','OD_Demand','Air_Fare_USD')

# we need all passenger flows origin from OR arrive at a Chinese city 
segment_output <- read.csv('SegmentData_China_SSP3_dom.csv')
segment_output <- rename(segment_output[,c('X.Year.','X.OriginID.','X.DestinationID.','X.SegmentDemand.',
                                           'X.ActualFlightFreq_all.','X.JetA1Seg_kg.','X.DirectCO2Seg_kg.')], 
                         Year=X.Year., OriginID = X.OriginID., DestID=X.DestinationID.,SegmentDemand=X.SegmentDemand.,
                         FltFreq=X.ActualFlightFreq_all.,FuelBurn=X.JetA1Seg_kg.,DirectCO2=X.DirectCO2Seg_kg.)

# match first with airportID and then with City ID
segment_output <- merge(segment_output, airport[,c('Number','Code','CityID','City','Country')], by.x='OriginID',by.y='Number', all.x = T)
segment_output <- merge(segment_output, airport[,c('Number','Code','CityID','City','Country')], by.x='DestID',by.y='Number', all.x = T)
segment_output <- rename(segment_output, Origin.Airport=Code.x, OriginCityID=CityID.x,Origin.City=City.x, Origin.Country=Country.x,
                         Dest.Airport=Code.y, DestCityID=CityID.y,Dest.City=City.y, Dest.Country=Country.y)

# just keep segments origining from a Chinese city
segment_output <- subset(segment_output, Origin.Country=='China'|Dest.Country=='China')
segment_output <- subset(segment_output, !(OriginID==0 | DestID==0))
segment_output <- segment_output[,c('Year','Origin.Airport','Dest.Airport','Origin.City','Dest.City','OriginCityID','DestCityID',
                                    'Origin.Country','Dest.Country','SegmentDemand','FltFreq','FuelBurn','DirectCO2')]

# aggregate AIM output to O-D city pairs: must be by city id
OD_output <- segment_output[,c('Year','OriginCityID','DestCityID','SegmentDemand','FltFreq','FuelBurn','DirectCO2')] %>% 
                    group_by(Year,OriginCityID,DestCityID)%>% 
                    summarise(SegmentDemand=sum(SegmentDemand,na.rm=T),FltFreq=sum(FltFreq, na.rm=T),
                              FuelBurn_kg=sum(FuelBurn,na.rm=T),CO2_emissions_kg=sum(DirectCO2))

# aggregate with city_output for airfares
OD_output <- merge(OD_output, city_output[,c('Year','OriginCityID','DestCityID','OD_Demand','Air_Fare_USD')], 
                   by=c('Year','OriginCityID','DestCityID'),all.x = T)

######################################## combine with Air-HSR data ###################################
load("D:/Aviation_HSR/future_projections.RData")
load("D:/Aviation_HSR/SSP_domestic.RData")
load("D:/Aviation_HSR/Full_HSR.RData")
#load("D:/Aviation_HSR/SSP_global.RData")
futures <- copy[,c(3:14,20:26,28,29,31,32,34)]
OD_output <- SSP2_OD_output

futures <- merge(futures, OD_output, by=c('Year','OriginCityID','DestCityID'), all.y = T)

# if city.pair in futures is na, it means this city pair only has air transport, no HSR
futures$HSR_15 <- ifelse(is.na(futures$Air_dist),0,1)

# now match new HSR segments that are constructued after 2015
futures <- merge(futures, Full_HSR[,c('OriginCityID','DestCityID','Enter.Year','Distance')],by=c('OriginCityID','DestCityID'),all.x = T)

futures$HSR_25 <- ifelse(!(is.na(futures$Enter.Year))&futures$Year>=futures$Enter.Year, 1, 0)
futures$HSR_25 <- ifelse(futures$HSR_15==1 & futures$HSR_25==0, 1, futures$HSR_25)

############################################ For city pairs with both air and HSR ############################################
futures_15 <- subset(futures, HSR_15==1 & HSR_25==1)
futures_25 <- subset(futures, HSR_15==0 & HSR_25==1)

# Use the coefficients from the SUR estimation
gravity_model <- function(group, model){
  exp(model$coefficients[1] + model$coefficients[2]*log(group$Pop_med) + 
        model$coefficients[3]*log(group$Income_med) + model$coefficients[4]*group$Special_both + 
        model$coefficients[5]*group$Special_none + 
        model$coefficients[6]*(log(exp(model$coefficients[8]*group$Air_price + 
                                       model$coefficients[9]*group$Air_time_hrs + 
                                       model$coefficients[10]*log(group$Air_freq_share_med)+ 
                                       model$coefficients[11]*group$Air_AE_drive)+
                                   exp(model$coefficients[7] + model$coefficients[8]*group$HSR_USD+ 
                                       model$coefficients[9]*group$HSR_time + 
                                       model$coefficients[10]*log(group$HSR_freq_share_med)+ 
                                       model$coefficients[11]*group$HSR_AE_drive))))
  
}

### use the logit model choice predictions: again, we need at least Air_price and Air_freq updated in each year from AIM
choice_prob <- function(group, model){
  ### use the logit model choice predictions
  group$Utility_AIR <- group$Air_price*model$coefficients[8] + group$Air_time_hrs*model$coefficients[9] +
    log(group$Air_freq_share_med)*model$coefficients[10] + group$Air_AE_drive*model$coefficients[11] 
  
  group$Utility_HSR <- model$coefficients[7] + group$HSR_USD*model$coefficients[8] + group$HSR_time*model$coefficients[9] +
    log(group$HSR_freq_share_med)*model$coefficients[10] + group$HSR_AE_drive*model$coefficients[11]
  
  # compuate choice probability based on the utility
  group$Pred_prob_HSR <- exp(group$Utility_HSR)/(exp(group$Utility_AIR) + exp(group$Utility_HSR))
  group$Pred_prob_AIR <- 1- group$Pred_prob_HSR
  
  return(group)
}

source('E:/Dropbox/R_scripts/Aviation_HSR/Ch3_R_scripts/Post_15_demand.R')

####################################################################################################################
futures_15$Air_price <- ifelse(futures_15$Year>2015, futures_15$Air_Fare_USD, futures_15$Air_price)

# compute freq ratio
futures_15 <- futures_15 %>% group_by(Year) %>% 
              mutate(Air_freq_share_high = 100*FltFreq/(sum(FltFreq)+sum(HsrFreq_high)),
                     HSR_freq_share_high = 100*HsrFreq_high/(sum(FltFreq)+sum(HsrFreq_high)),
                     Air_freq_share_med = 100*FltFreq/(sum(FltFreq)+sum(HsrFreq_med)),
                     HSR_freq_share_med = 100*HsrFreq_med/(sum(FltFreq)+sum(HsrFreq_med)),
                     Air_freq_share_low = 100*FltFreq/(sum(FltFreq)+sum(HsrFreq_low)),
                     HSR_freq_share_low = 100*HsrFreq_low/(sum(FltFreq)+sum(HsrFreq_low)))                            

############# make projections into 2050 for the HSR 2015 network using the gravity model ###########################
futures_15$total_15 <- gravity_model(group=futures_15, model=result_All)
futures_15 <- choice_prob(group=futures_15, model=result_All)
futures_15$Air_pred_15 <- futures_15$total_15 * futures_15$Pred_prob_AIR
futures_15$HSR_pred_15 <- futures_15$total_15 * futures_15$Pred_prob_HSR

# work out CO2 emissions: based on CO2 emissions per pax by city pair
futures_15$CO2_kg_15 <- (futures_15$CO2_emissions_kg/futures_15$SegmentDemand)*futures_15$Air_pred_15
futures_15$Fuel_kg_15 <- (futures_15$FuelBurn_kg/futures_15$SegmentDemand)*futures_15$Air_pred_15

# estimate number of vehicles required on each route for the proejcted HSR demand
# number of trains required = daily frequency/(18/journey time)
futures_15$Vehicle_15 <- round((futures_15$HsrFreq_med/365)/(18/futures_15$HSR_time))
futures_15$Vehicle_15 <- ifelse(futures_15$Vehicle_15==0, 1, futures_15$Vehicle_15)

############################################# For city pairs that just have air transport #################################################
futures_15_air <- subset(futures, HSR_15==0)

futures_15_air <- futures_15_air %>% mutate(Vehicle_15=NA,HSR_pred_15=NA,Air_pred_15 = SegmentDemand, 
                                            CO2_kg_15=CO2_emissions_kg, Fuel_kg_15=FuelBurn_kg)

futures_15 <- rbind.data.frame(futures_15_air[,c('Year','OriginCityID','DestCityID','Air_time_hrs','HSR_dist','Vehicle_type','SegmentDemand',
                                                 'HSR_pred_15','Air_pred_15','CO2_emissions_kg','CO2_kg_15','FuelBurn_kg','Fuel_kg_15','Vehicle_15')], 
                               futures_15[,c('Year','OriginCityID','DestCityID','Air_time_hrs','HSR_dist','Vehicle_type','SegmentDemand','HSR_pred_15',
                                             'Air_pred_15','CO2_emissions_kg','CO2_kg_15','FuelBurn_kg','Fuel_kg_15','Vehicle_15')])
futures_15$HSR_pred_15 <- ifelse(is.na(futures_15$HSR_pred_15),0,futures_15$HSR_pred_15)

summary_15 <- subset(futures_15)%>%group_by(Year)%>% 
              summarise(Demand_with_HSR_15=sum(Air_pred_15/1000000),
                        Demand_without_HSR=sum(SegmentDemand/1000000),
                        Demand_HSR_15=sum(HSR_pred_15/1000000),
                        CO2_emissions_with_HSR_15=sum(CO2_kg_15/1000000000), 
                        CO2_emissions_without_HSR=sum(CO2_emissions_kg/1000000000))

summary <- merge(summary_15,summary_25, by=c('Year','Demand_without_HSR','CO2_emissions_without_HSR'),all.x=T)
summary$Demand_with_HSR_15 <- ifelse(summary$Year==2015, summary$Demand_with_HSR_15-30,summary$Demand_with_HSR_15)
summary$Demand_with_HSR_25 <- ifelse(summary$Year==2015, summary$Demand_with_HSR_25-30,summary$Demand_with_HSR_25)
summary$CO2_emissions_with_HSR_15 <- ifelse(summary$Year==2015, summary$CO2_emissions_with_HSR_15-2,summary$CO2_emissions_with_HSR_15)
summary$CO2_emissions_with_HSR_25 <- ifelse(summary$Year==2015, summary$CO2_emissions_with_HSR_25-2,summary$CO2_emissions_with_HSR_25)

summary$Shifted_demand_15 <- summary$Demand_without_HSR - summary$Demand_with_HSR_15
summary$Shifted_demand_25 <- summary$Demand_without_HSR - summary$Demand_with_HSR_25
summary$Gross_CO2_savings_15 <- summary$CO2_emissions_without_HSR - summary$CO2_emissions_with_HSR_15
summary$Gross_CO2_savings_25 <- summary$CO2_emissions_without_HSR - summary$CO2_emissions_with_HSR_25

# HSR predictions adjust, using high growth as reference: ratio= Air_with_HSR15_high/Air_with_HSR15_med 
futures_low_15$HSR_pred_15 <- futures_low_15$HSR_pred_15*sum(summary_low$Demand_with_HSR_15)/sum(summary_high$Demand_with_HSR_15)
futures_low_25$HSR_pred_25 <- futures_low_25$HSR_pred_25*sum(summary_low$Demand_with_HSR_25)/sum(summary_high$Demand_with_HSR_25)
futures_med_15$HSR_pred_15 <- futures_med_15$HSR_pred_15*sum(summary_med$Demand_with_HSR_15)/sum(summary_high$Demand_with_HSR_15)
futures_med_25$HSR_pred_25 <- futures_med_25$HSR_pred_25*sum(summary_med$Demand_with_HSR_25)/sum(summary_high$Demand_with_HSR_25)

summary_med$Demand_HSR_15 <- summary_med$Demand_HSR_15*sum(summary_med$Demand_with_HSR_15)/sum(summary_high$Demand_with_HSR_15)
summary_med$Demand_HSR_25 <- summary_med$Demand_HSR_25*sum(summary_med$Demand_with_HSR_25)/sum(summary_high$Demand_with_HSR_25)
summary_low$Demand_HSR_15 <- summary_low$Demand_HSR_15*sum(summary_low$Demand_with_HSR_15)/sum(summary_high$Demand_with_HSR_15)
summary_low$Demand_HSR_25 <- summary_low$Demand_HSR_25*sum(summary_low$Demand_with_HSR_25)/sum(summary_high$Demand_with_HSR_25)

demand_result <- summary[,c('Year','Demand_with_HSR_15','Demand_with_HSR_25','Demand_without_HSR')]%>% gather(Type,Demand,-Year)
demand_result$Type <- as.factor(demand_result$Type)
demand_result$Type <- factor(demand_result$Type, levels=c('Demand_without_HSR','Demand_with_HSR_15','Demand_with_HSR_25'))
levels(demand_result$Type)[levels(demand_result$Type)=="Demand_with_HSR_15"] <- "Air Demand with 2015 HSR"
levels(demand_result$Type)[levels(demand_result$Type)=="Demand_with_HSR_25"] <- "Air Demand with 2025 HSR"
levels(demand_result$Type)[levels(demand_result$Type)=="Demand_without_HSR"] <- "Air Demand without HSR"

# Statistics Year Book (2016): in 2015, Domestic avation demand 394.11 million passengers in China.
ggplot(data= demand_result) + geom_line(aes(x= Year,y=Demand,group=Type, color=Type),size=2)+ 
       ylab('Air Transport Demand (millions)') + scale_y_continuous(limits = c(0,2000),breaks = c(0,500,1000,1500,2000))+
       ggtitle('Projections for Domestic China Air Passenger Demand (2015-2050) \n (Low Growth Scenario)') + 
       theme_Publication()+scale_colour_Publication()

CO2_result <- summary[,c('Year','CO2_emissions_with_HSR_15','CO2_emissions_with_HSR_25','CO2_emissions_without_HSR')]%>% gather(Type,Emissions,-Year)
CO2_result$Type <- as.factor(CO2_result$Type)
CO2_result$Type  <- factor(CO2_result$Type , levels=c('CO2_emissions_without_HSR','CO2_emissions_with_HSR_15','CO2_emissions_with_HSR_25'))
levels(CO2_result$Type)[levels(CO2_result$Type)=="CO2_emissions_with_HSR_15"] <- "CO2 emissions with 2015 HSR"
levels(CO2_result$Type)[levels(CO2_result$Type)=="CO2_emissions_with_HSR_25"] <- "CO2 emissions with 2025 HSR"
levels(CO2_result$Type)[levels(CO2_result$Type)=="CO2_emissions_without_HSR"] <- "CO2 emissions without HSR"

# UIC(2017): China 2015 total CO2 from transport sector = 964 million tCO2 and 12.5 EJ energy
# Domestic aviation account for 5.6% of the 964 MtCO2 = 54 MtCO2
ggplot(data= CO2_result) + geom_line(aes(x= Year,y=Emissions,group=Type, color=Type),size=2)+ 
       ylab('Direct CO2 Emissions (million tons)') + scale_y_continuous(limits = c(0,150),breaks = c(0,30,60,90,120,150))+
       ggtitle('Projections for Domestic China Air Transport CO2 Emissions (2015-2050) \n (Low Growth Scenario)') + 
       theme_Publication()+scale_colour_Publication()


rm(access.time,Aggregated,All,All_15,All_trains,China,city_baseyr,CO2_result,demand_result,EIA_fuel,Fuel_trend,Full_HSR,future_hsr,future_inc,
   future_pop,futures,futures_15,futures_15_air,futures_25,futures_25_air,futures_high,futures_low, futures_med,GDP,GDP_projection,hist_fuel,
   hist_GDP,historic,hsr_cost,hsr_freq,Inc_projection,Inc_trend,Income_scenario,OD_output,Pop_projection,Pop_scenario,Pop_trend,Population,trains,
   post_15,projection,SSP1_OD_output,SSP1_seg_output,SSP2_OD_output,SSP2_seg_output,SSP3_OD_output,SSP3_seg_output,summary,summary_15,summary_25,
   Direct_15,Direct_25,Overline_25)


futures$Shifted_demand <- futures$SegmentDemand - futures$Air_pred



