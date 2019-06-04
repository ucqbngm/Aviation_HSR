# This is an out-source script to seperately get all new HSR city pairs after 2015 ready for demand forecasting
source("E:/Dropbox/R/ggplot.R")

# where is future population and income for all city pairs? this should be available from AIM
city_baseyr <- read.csv('D:/Aviation_HSR/CityData_AIM_v8.csv')
city_baseyr <- city_baseyr[,c('CityID','BaseYearPopulation','BaseYearIncome','Spec')]
names(city_baseyr) <- c('CityID', 'Population','Income','Special')

# just need Chinese cities for HSR demadn prediction
city_baseyr <- merge(city_baseyr, airport[,c('CityID','Country')], by='CityID',all.x = T)
city_baseyr <- subset(city_baseyr, Country=='China')

# population is 1million as 1 unit, income is $1000
city_baseyr$Population <- city_baseyr$Population/1000000
city_baseyr$Income <- city_baseyr$Income/1000
city_baseyr <- city_baseyr %>% unique()

# get income and population growth rates from projections
load("D:/Aviation_HSR/future_projections.RData")

city_baseyr <- merge(city_baseyr, Pop_projection)
city_baseyr$Type <- as.factor(city_baseyr$Type)
levels(city_baseyr$Type) <- list('High'='Pop_high_growth','Med'='Pop_med_growth','Low'='Pop_low_growth')

Inc_projection$Type <- as.factor(Inc_projection$Type)
levels(Inc_projection$Type) <- list('High'='Income_high','Med'='Income_med','Low'='Income_low')
city_baseyr <- merge(city_baseyr, Inc_projection[,c('Time','Type','Inc_growth')], by=c('Time','Type'),all.x =T) 
# compute income and population for each year based on the growth rates under each scenario
city_baseyr <- city_baseyr %>% group_by(CityID,Type) %>% mutate(Pop = Population*cumprod(1+Pop_growth))
city_baseyr <- city_baseyr %>% group_by(CityID,Type) %>% mutate(Inc = Income*cumprod(1+Inc_growth))

# fill in pop, income, and spec for new HSR segment city pairs
post_15 <- merge(futures_25[,c(1:3)], city_baseyr[,c('Time','Type','CityID','Special','Pop','Inc')], 
                 by.x=c('Year','OriginCityID'),by.y=c('Time','CityID'),all.x = T)
post_15 <- rename(post_15,Spec_Origin = Special, Origin_Pop=Pop, Origin_Inc = Inc)
post_15 <- merge(post_15, city_baseyr[,c('Time','Type','CityID','Special','Pop','Inc')], 
                 by.x=c('Year','Type','DestCityID'),by.y=c('Time','Type','CityID'),all.x = T)
post_15 <- rename(post_15,Spec_Dest = Special, Dest_Pop=Pop, Dest_Inc = Inc)

post_15$Special_both <- ifelse(post_15$Spec_Origin == 1 & post_15$Spec_Dest==1, 1, 0)
post_15$Special_none <- ifelse(post_15$Spec_Origin == 0 & post_15$Spec_Dest==0, 1, 0)
post_15$Pop <- post_15$Origin_Pop*post_15$Dest_Pop
post_15$Inc <- post_15$Origin_Inc*post_15$Dest_Inc

post_15 <- merge(rename(post_15[,c('Year','OriginCityID', 'DestCityID','Type','Pop','Special_both','Special_none')] %>% 
                          group_by(Year,OriginCityID, DestCityID) %>% spread(Type,Pop),
                        Pop_high=High,Pop_med=Med,Pop_low=Low),
                 rename(post_15[,c('Year','OriginCityID', 'DestCityID','Type','Inc')]%>%
                          group_by(Year,OriginCityID, DestCityID)%>% spread(Type,Inc),
                        Income_high=High,Income_med=Med,Income_low=Low), by=c('Year','OriginCityID','DestCityID'),all.x=T)

# drive time, rail station and airport access time
access.time <- read.csv('D:/Aviation_HSR/city_stations.csv')
access.time <- merge(access.time[,c('City','To_station_drive','To_airport_drive')],
                     airport[,c('City','CityID')]%>%unique(),by='City',all.x=T)
access.time <- subset(access.time, !City=='Yichun')
post_15 <- merge(post_15, access.time[,2:4], by.x='OriginCityID', by.y='CityID', all.x = T)
post_15 <- rename(post_15,Origin_to_rail_drive = To_station_drive,Origin_to_air_drive = To_airport_drive)
post_15 <- merge(post_15, access.time[,2:4], by.x='DestCityID', by.y='CityID', all.x = T)
post_15 <- rename(post_15, Dest_to_rail_drive = To_station_drive,Dest_to_air_drive = To_airport_drive)

# NA's are new HSR stations! for these new stations, 
new_stations <- rbind.data.frame(rename(subset(post_15[,c('OriginCityID','Origin_to_air_drive')],is.na(Origin_to_air_drive)),
                                        CityID=OriginCityID,check=Origin_to_air_drive),
                                 rename(subset(post_15[,c('DestCityID','Dest_to_air_drive')],is.na(Dest_to_air_drive)),
                                        CityID=DestCityID, check=Dest_to_air_drive))
new_stations <- new_stations %>% group_by(CityID) %>% summarise(count=n())
# for these new stations where we don't know drive time, take average drive time
post_15$Origin_to_air_drive <- ifelse(is.na(post_15$Origin_to_air_drive), mean(access.time$To_airport_drive), post_15$Origin_to_air_drive)
post_15$Dest_to_air_drive <- ifelse(is.na(post_15$Dest_to_air_drive), mean(access.time$To_airport_drive), post_15$Dest_to_air_drive)
post_15$Origin_to_rail_drive <- ifelse(is.na(post_15$Origin_to_rail_drive), mean(access.time$To_station_drive), post_15$Origin_to_rail_drive)
post_15$Dest_to_rail_drive <- ifelse(is.na(post_15$Dest_to_rail_drive), mean(access.time$To_station_drive), post_15$Dest_to_rail_drive)

post_15$Air_AE_drive <- post_15$Origin_to_air_drive + post_15$Dest_to_air_drive
post_15$HSR_AE_drive <- post_15$Origin_to_rail_drive + post_15$Dest_to_rail_drive


# for new added HSR segments, we need HSR price, time, frequency, AE_diff
All <- read.csv('D:/Aviation_HSR/All_sample.csv')

# derive other variables using distance, pop, and inc
post_15 <- merge(post_15, futures_25[,c('OriginCityID','DestCityID','Distance')]%>%unique(), by=c('OriginCityID','DestCityID'),all.x=T)
post_15 <- rename(post_15, HSR_dist =Distance)

# 1) HSR cost: regress on distance
hsr_cost <- lm(HSR_USD ~ HSR_dist, data=All)

#ggplot(All, aes(x=HSR_dist, y=HSR_USD, label=id)) + geom_point() + geom_text(check_overlap = T)

post_15$HSR_USD <- hsr_cost$coefficients[1] + hsr_cost$coefficients[2]*post_15$HSR_dist

# 2) HSR time: distance divided by average speed
post_15$HSR_time <- post_15$HSR_dist/250

# 3)Air travel time
load("D:/Aviation_HSR/CHN_All.RData")
post_15 <- merge(post_15, subset(China,Nlegs==1) %>% group_by(Origin.CityID,Dest.CityID) %>% 
                   summarise(Air_time_hrs=round(mean(Time)/60,2)),by.x=c('OriginCityID','DestCityID'),by.y=c('Origin.CityID','Dest.CityID'),all.x=T)

# 4) HSR frequency: regress on distance and population income
hsr_freq <- lm(log(HSR_freq) ~ HSR_dist + log(Population) + log(Income), All)

#ggplot(All, aes(x=(HSR_dist), y=log(HSR_freq), label=id)) + geom_point() + geom_text(check_overlap = T)
#ggplot(All, aes(x=log(Population), y=log(HSR_freq), label=id)) + geom_point() + geom_text(check_overlap = T)
#ggplot(All, aes(x=log(Income), y=log(HSR_freq), label=id)) + geom_point() + geom_text(check_overlap = T)


post_15$HsrFreq_high <- exp(hsr_freq$coefficients[1]+hsr_freq$coefficients[2]*log(post_15$Pop_high)+hsr_freq$coefficients[3]*log(post_15$Income_high))
post_15$HsrFreq_med <- exp(hsr_freq$coefficients[1]+hsr_freq$coefficients[2]*log(post_15$Pop_med)+hsr_freq$coefficients[3]*log(post_15$Income_med))
post_15$HsrFreq_low <- exp(hsr_freq$coefficients[1]+hsr_freq$coefficients[2]*log(post_15$Pop_low)+hsr_freq$coefficients[3]*log(post_15$Income_low))

post_15 <- merge(post_15, futures_25[,c('Year','OriginCityID','DestCityID',"Air_freq","Air_price","SegmentDemand","FltFreq",
                                        "FuelBurn_kg","CO2_emissions_kg","OD_Demand","Air_Fare_USD",'Vehicle_type',
                                        "HSR_15","HSR_25")], by=c('Year','OriginCityID','DestCityID'),all.x=T)

post_15$Air_price <- post_15$Air_Fare_USD
post_15$Air_freq <- post_15$FltFreq

post_15 <- rbind.data.frame(futures_15[,c(1:14,16:30)], post_15[,c(2:3,1,6:11,22:24,4:5,18,25,21,20,26,19,16:17,27:33)])

################################ predict post-15 HSR city pair demands #######################################
post_15$Air_price <- ifelse(post_15$Year>2015, post_15$Air_Fare_USD, post_15$Air_price)

# compute freq ratio
post_15 <- post_15 %>% group_by(Year) %>% 
           mutate(Air_freq_share_high = 100*FltFreq/(sum(FltFreq)+sum(HsrFreq_high)),
                  HSR_freq_share_high = 100*HsrFreq_high/(sum(FltFreq)+sum(HsrFreq_high)),
                  Air_freq_share_med = 100*FltFreq/(sum(FltFreq)+sum(HsrFreq_med)),
                  HSR_freq_share_med = 100*HsrFreq_med/(sum(FltFreq)+sum(HsrFreq_med)),
                  Air_freq_share_low = 100*FltFreq/(sum(FltFreq)+sum(HsrFreq_low)),
                  HSR_freq_share_low = 100*HsrFreq_low/(sum(FltFreq)+sum(HsrFreq_low)))                            

################################ make projections into 2050 using the gravity model ###########################
post_15$total_25 <- gravity_model(group=post_15, model=result_All)
post_15 <- choice_prob(group=post_15, model=result_All)
post_15$Air_pred_25 <- post_15$total_25 * post_15$Pred_prob_AIR
# some new HSR city pairs don't have air links in 2015, fill them with SegmentDemand
post_15$Air_pred_25 <- ifelse(is.na(post_15$Air_pred_25), post_15$SegmentDemand, post_15$Air_pred_25)
post_15$HSR_pred_25 <- post_15$total_25 * post_15$Pred_prob_HSR

# work out CO2 emissions: based on CO2 emissions per pax by city pair
post_15$CO2_kg_25 <- (post_15$CO2_emissions_kg/post_15$SegmentDemand)*post_15$Air_pred_25
post_15$Fuel_kg_25 <- (post_15$FuelBurn_kg/post_15$SegmentDemand)*post_15$Air_pred_25
post_15[is.na(post_15)] <- 0

# estimate number of vehicles required on each route for the proejcted HSR demand
# number of trains required = daily frequency/(18/journey time)
post_15$Vehicle_25 <- round((post_15$HsrFreq_med/365)/(18/post_15$HSR_time))
post_15$Vehicle_25 <- ifelse(post_15$Vehicle_25==0, 1, post_15$Vehicle_25)

# we need to sort out vehicle type for post-2015 HSR routes, they now have vehicle type=0
# can only take guesses: more pax per train, longer routes, 16 cars, otherwise 8 cars
post_15$Vehicle_type <- ifelse(post_15$HsrFreq_high/365>=25 & post_15$HSR_dist <= 1000,'G1',
                              ifelse(post_15$HsrFreq_high/365<25 & post_15$HsrFreq_high/365>=15 & post_15$HSR_dist>1000,'G2',
                                     ifelse(post_15$HsrFreq_high/365<15 & post_15$HSR_dist>1000,'D2','D1')))


############################################# For city pairs that just have air transport #################################################
futures_25_air <- subset(futures, HSR_15==0 & HSR_25==0)

futures_25_air <- futures_25_air %>% mutate(Vehicle_25=NA,HSR_pred_25=NA, Air_pred_25 = SegmentDemand, 
                                            CO2_kg_25=CO2_emissions_kg, Fuel_kg_25=FuelBurn_kg)

futures_25 <- rbind.data.frame(futures_25_air[,c('Year','OriginCityID','DestCityID','HSR_dist','Vehicle_type','Air_time_hrs',
                                                 'SegmentDemand','HSR_pred_25','Air_pred_25','CO2_emissions_kg','CO2_kg_25',
                                                 'FuelBurn_kg','Fuel_kg_25','Vehicle_25')], 
                               post_15[,c('Year','OriginCityID','DestCityID','HSR_dist','Vehicle_type','Air_time_hrs',
                                          'SegmentDemand','HSR_pred_25','Air_pred_25','CO2_emissions_kg','CO2_kg_25',
                                          'FuelBurn_kg','Fuel_kg_25','Vehicle_25')])
futures_25$HSR_pred_25 <- ifelse(is.na(futures_25$HSR_pred_25),0,futures_25$HSR_pred_25)

summary_25 <- subset(futures_25)%>%group_by(Year)%>% 
  summarise(Demand_with_HSR_25=sum(Air_pred_25/1000000),
            Demand_without_HSR=sum(SegmentDemand/1000000),
            Demand_HSR_25=sum(HSR_pred_25/1000000),
            CO2_emissions_with_HSR_25=sum(CO2_kg_25/1000000000), 
            CO2_emissions_without_HSR=sum(CO2_emissions_kg/1000000000))












