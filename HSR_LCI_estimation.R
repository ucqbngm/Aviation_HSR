setwd("D:/Aviation_HSR")
library(dplyr)
library(tidyr)
library(ggplot2)
source("E:/Dropbox/R/ggplot.R")

# We do not need shifted demand here, just estimate LCI for HSR_15 and HSR_25
###################################### Operational Emissions from HSR ##########################################
load("D:/Aviation_HSR/AIM_output_dom.RData")
futures_15 <- futures_high_15
futures_25 <- futures_high_25

HSR_LCI <- cbind.data.frame(Scenario='High Growth',Type = c('HSR_15','HSR_25'))

# CRH types and train sizes
HSR_vehicle <- cbind.data.frame(Vehicle=c('CRH380','CRH380L','CRHA','CRHB'),Type=c('G1','G2','D1','D2'),
                                IntroYear = c(2013,2011,2007,2010),Weights = c(388.4,890,420.4,690),
                                kWh_per_seatkm = c(0.037,0.058,0.037,0.058),MaxSpeed = c(380,350,250,250),
                                Carriages = c(8,16,8,16),Seats =c(556,1061,660,1230), 
                                Length = c(215.3,399.3,213.5,426.3),EF_prev_yr = c(726,758,886,772))

# Vehicle operation: propulsion + idling + auxiliaries

# Step 1: instead of using the 271kWh/VMT, which is (271/1200)*1.609344=0.363kWh/seat-km of CAHSR
# we use 0.058kWh/seat-km (ICE3) for all 16-cars CHSR series vehicles, and 0.037kWh/seat-km for all 8-cars

# Step 2: convert from kWh_per_seat to kWh_per_PKT: elect use per seat * total seasts on the train/average pax on the train
HSR_vehicle$kWh_per_PKT <- HSR_vehicle$kWh_per_seatkm*HSR_vehicle$Seats/(HSR_vehicle$Seats*0.7)

# Step 3: propulsion emissions under 2015 energy mix, gCO2/kWh
HSR_vehicle$gCO2_per_PKT <- 657*HSR_vehicle$kWh_per_PKT
  
# Step 4: step 3 just gives gCO2/PKT for propulsion, get idling and auxi by scaling
# 1 mile = 1.609344 km
HSR_vehicle$CO2_idling <- round(2.2/((87*1.61)/HSR_vehicle$gCO2_per_PKT),1)
HSR_vehicle$CO2_auxiliaries <- round(4.7/((87*1.61)/HSR_vehicle$gCO2_per_PKT),1)
HSR_vehicle$HSR_Op_gCO2_PKT <- HSR_vehicle$gCO2_per_PKT + HSR_vehicle$CO2_idling + HSR_vehicle$CO2_auxiliaries

# Now, compute the total operational emissions for total HSR demand under HSR 15 and 25
# cannot merge by HSR dist and Vehicle type, they are different in HSR_15 and HSR_25!
HSR_OD <- merge(futures_15[,c('Year','OriginCityID','DestCityID','HSR_dist','Vehicle_type','HSR_pred_15')],
                futures_25[,c('Year','OriginCityID','DestCityID','HSR_dist','Vehicle_type','HSR_pred_25')],
                by=c('Year','OriginCityID','DestCityID'), all.x=T)

HSR_OD$HSR_dist <- ifelse(is.na(HSR_OD$HSR_dist.x),HSR_OD$HSR_dist.y,HSR_OD$HSR_dist.x)
HSR_OD$Vehicle_type <- ifelse(is.na(HSR_OD$Vehicle_type.x),HSR_OD$Vehicle_type.y,HSR_OD$Vehicle_type.x)
HSR_OD <- HSR_OD[,c('Year','OriginCityID','DestCityID','HSR_dist','Vehicle_type','HSR_pred_15','HSR_pred_25')]
HSR_OD <- subset(HSR_OD,!is.na(Vehicle_type))

HSR_operation <- HSR_OD %>% mutate(HSR_PKT_15 = HSR_pred_15*HSR_dist,HSR_PKT_25=HSR_pred_25*HSR_dist)

# Total operational (propulsion + idling + auxiliaries) emissions under 2015 emissions factor
HSR_operation <- merge(HSR_operation, HSR_vehicle[,c('Type','HSR_Op_gCO2_PKT')], by.x='Vehicle_type',by.y='Type',all.x = T)

# total operational emissions under the 2015 carbon intensity for each city pair
HSR_operation <- HSR_operation %>% mutate(HSR_Op_tCO2_15 = HSR_PKT_15*(HSR_Op_gCO2_PKT/1000000),
                                          HSR_Op_tCO2_25 = HSR_PKT_25*(HSR_Op_gCO2_PKT/1000000))

# total operational emissoins in million tons
HSR_operation <- merge(HSR_operation[,c('Year','HSR_pred_15','HSR_Op_tCO2_15')]%>%group_by(Year)%>%
                          summarise(HSR_demand_15 = sum(HSR_pred_15/1000000,na.rm=T),
                                    HSR_Op_MtCO2_15=sum(HSR_Op_tCO2_15/1000000, na.rm = T)),
                       HSR_operation[,c('Year','HSR_pred_25','HSR_Op_tCO2_25')]%>%group_by(Year)%>%
                          summarise(HSR_demand_25 = sum(HSR_pred_25/1000000,na.rm=T),
                                    HSR_Op_MtCO2_25=sum(HSR_Op_tCO2_25/1000000)), by='Year',all.x=T)

# calculate HSR operational emissions for fixed and declining carbon intensity
HSR_operation <- merge(rename(HSR_operation[,c('Year','HSR_demand_15','HSR_demand_25')],
                              HSR_15=HSR_demand_15,HSR_25=HSR_demand_25)%>% gather(Type,HSR_demand, -Year),
                       rename(HSR_operation[,c('Year','HSR_Op_MtCO2_15','HSR_Op_MtCO2_25')],
                              HSR_15=HSR_Op_MtCO2_15,HSR_25=HSR_Op_MtCO2_25)%>% gather(Type,HSR_Op_MtCO2_fixed, -Year),
                       by=c('Year','Type'), all.x=T)

# emissions factor in China
HSR_operation <- merge(HSR_operation, emission_factor[,c('Year','EF')], by='Year',all.x = T)
HSR_operation$EF_15 <- 657
# compute total electricity consumption and emissions with declining carbon intensity
# operational electricity consumption: MtCO2 -> gCO2 -> gCO2/gCO2/kWh = kWh -> GWh 
HSR_operation <- HSR_operation %>% mutate(HSR_Op_Elect_GWh = (HSR_Op_MtCO2_fixed*1000000/EF_15),
                                          HSR_Op_MtCO2=(HSR_Op_MtCO2_fixed/EF_15)*EF)

# summarise to HSR LCI inventory table
HSR_LCI <- merge(HSR_LCI, HSR_operation %>% group_by(Type) %>% 
                          summarise(HSR_demand=sum(HSR_demand),
                                    HSR_Operation_Elect_GWh=sum(HSR_Op_Elect_GWh),
                                    HSR_Operation_MtCO2_fixed=sum(HSR_Op_MtCO2_fixed),
                                    HSR_Operation_MtCO2=sum(HSR_Op_MtCO2)),by='Type',all.x=T)

########################################## HSR Vehicle Emissions ##################################################
HSR_train <-  merge(HSR_OD, futures_15[,c('Year','OriginCityID','DestCityID','Vehicle_15')],
                    by=c('Year','OriginCityID','DestCityID'), all.x=T)
HSR_train <-  merge(HSR_train, futures_25[,c('Year','OriginCityID','DestCityID','Vehicle_25')],
                    by=c('Year','OriginCityID','DestCityID'), all.x=T)

# compare total vehicles in previous year to the current year, see if increase
HSR_train <- HSR_train %>% arrange(-desc(Year))%>% group_by(OriginCityID,DestCityID,Vehicle_type) %>% 
             mutate(Vehicle_prev_15 = lag(Vehicle_15),Vehicle_prev_25=lag(Vehicle_25))
HSR_train$Vehicle_prev_15 <- ifelse(HSR_train$Year==2015, HSR_train$Vehicle_15,HSR_train$Vehicle_prev_15)
HSR_train$Vehicle_prev_25 <- ifelse(HSR_train$Year==2015, HSR_train$Vehicle_25,HSR_train$Vehicle_prev_25)

HSR_train <- HSR_train %>% mutate(new_vehicles_15=Vehicle_15-Vehicle_prev_15,new_vehicles_25=Vehicle_25-Vehicle_prev_25)

# count total vehicles
Exist_trains <- subset(HSR_train,Year==2015) %>% group_by(Year,Vehicle_type) %>% 
                       summarise(Total_vehicle_15=sum(Vehicle_15),Total_vehicle_25=sum(Vehicle_25)) 

# total number of new vehicles to be manufactured 
New_trains <- merge(subset(HSR_train,Year>2015)%>% group_by(Year,Vehicle_type)%>%
                    summarise(New_vehicles_15=sum(new_vehicles_15,na.rm = T)),
                    subset(HSR_train,Year>2015)%>% group_by(Year,Vehicle_type)%>%
                    summarise(New_vehicles_25=sum(new_vehicles_25,na.rm = T)),by=c('Year','Vehicle_type'),all.x=T)

emission_factor <- emission_factor %>% mutate(prev_EF=lag(EF))

New_trains <- merge(New_trains, emission_factor[,c('Year','EF','prev_EF')], by='Year',all.x = T)
New_trains$EF_15 <- 657


# (1) Vehicle manufacturing emissions, by Chester using LCA software SimaPro
# SimaPro computes per train manufacture emissions based on train weights: CAHSR 730 tonnes
# Table 35/63: emissions of manufacturing a HSR train under the CA Mix: 2127 metric ton CO2 per train
# China's EF are taken from the previous year of introducing the first CHSR type as the manufacturing year
HSR_vehicle$manu_GWh_perTrain <- ((2127/730)/260)*HSR_vehicle$Weights/1000000
HSR_vehicle$manu_tCO2_perTrain_fixed <- HSR_vehicle$Weights*(2127/730)*(657/260)

# vehicle maintenance electricity consumption from regular maintanence and cleaning
HSR_vehicle$maint_GWh_perTrainYr <- ((((1329+8.5)/730)*HSR_vehicle$Weights/30)/260)/1000000
  
New_trains <- merge(New_trains, HSR_vehicle[,c('Type','Weights','manu_GWh_perTrain',
                    'manu_tCO2_perTrain_fixed','maint_GWh_perTrainYr')], by.x ='Vehicle_type', by.y='Type',all.x = T)

New_trains$manu_tCO2_perTrain <- (New_trains$manu_tCO2_perTrain_fixed/657)*(New_trains$prev_EF)

# total manufacture emissions = per train emissions * total new trains required
New_trains <- drop_na(New_trains) %>% mutate(HSR_Manu_GWh_15 = manu_GWh_perTrain*New_vehicles_15,
                                             HSR_Manu_GWh_25 = manu_GWh_perTrain*New_vehicles_25,
                                             HSR_Manu_MtCO2_15_fixed = (manu_tCO2_perTrain_fixed*New_vehicles_15)/1000000,
                                             HSR_Manu_MtCO2_25_fixed = (manu_tCO2_perTrain_fixed*New_vehicles_25)/1000000,
                                             HSR_Manu_MtCO2_15 = (manu_tCO2_perTrain*New_vehicles_15)/1000000,
                                             HSR_Manu_MtCO2_25 = (manu_tCO2_perTrain*New_vehicles_25)/1000000)
# total vehicle manufacture electricity consumption
HSR_LCI <- cbind.data.frame(HSR_LCI,
                            rbind.data.frame(New_trains%>%summarise(HSR_Manu_GWh = sum(HSR_Manu_GWh_15)),
                                             New_trains%>%summarise(HSR_Manu_GWh = sum(HSR_Manu_GWh_25))))
# total vehicle manufacture emissions
HSR_LCI <- cbind.data.frame(HSR_LCI,
                            rbind.data.frame(New_trains%>%summarise(HSR_Manu_MtCO2_fixed = sum(HSR_Manu_MtCO2_15_fixed)),
                                             New_trains%>%summarise(HSR_Manu_MtCO2_fixed = sum(HSR_Manu_MtCO2_25_fixed))))

HSR_LCI <- cbind.data.frame(HSR_LCI,
                            rbind.data.frame(New_trains%>%summarise(HSR_Manu_MtCO2 = sum(HSR_Manu_MtCO2_15)),
                                             New_trains%>%summarise(HSR_Manu_MtCO2 = sum(HSR_Manu_MtCO2_25))))
                            
# (2) Vehicle maintenance emissions: need maintenance per train per year, also based on train weight
# Maintenance should cover both existing vehicles and new vehicles added in each year
# routine maintenance: Table 38/45/63: 1329 mtCO2 per train in 30 years' lifetime
# cleaning: Table 45: 8.5 mtCO2 per train in 30 years' lifetime
# Flooring: Table 45: 140 mtCO2 per tain in 30 years' lifetime
New_trains <- New_trains %>% mutate(HSR_Maint_GWh_15= maint_GWh_perTrainYr*New_vehicles_15,
                                    HSR_Maint_GWh_25= maint_GWh_perTrainYr*New_vehicles_25,
                                    HSR_Maint_tCO2_15_fixed = (Weights/730)*((1329+8.5+140)/30)*(657/260)*New_vehicles_15,
                                    HSR_Maint_tCO2_25_fixed = (Weights/730)*((1329+8.5+140)/30)*(657/260)*New_vehicles_25,
                                    HSR_Maint_tCO2_15 = Weights/730*((1329+8.5+140)/30)*(EF/260)*New_vehicles_15,
                                    HSR_Maint_tCO2_25 = Weights/730*((1329+8.5+140)/30)*(EF/260)*New_vehicles_25)

# Existing trains need maintenance from 2015 to 2050
Exist_trains <- merge(Exist_trains[,2:4], emission_factor[,c('Year','EF')], all.x = T)
Exist_trains <- merge(Exist_trains, HSR_vehicle[,c('Type','Weights','maint_GWh_perTrainYr')], 
                      by.x ='Vehicle_type', by.y='Type',all.x = T)
Exist_trains <- Exist_trains %>% mutate(HSR_Maint_GWh_15 = maint_GWh_perTrainYr*Total_vehicle_15,
                                        HSR_Maint_GWh_25 = maint_GWh_perTrainYr*Total_vehicle_25,
                                        HSR_Maint_tCO2_15_fixed = (Weights/730)*((1329+8.5+140)/30)*(657/260)*Total_vehicle_15,
                                        HSR_Maint_tCO2_25_fixed = (Weights/730)*((1329+8.5+140)/30)*(657/260)*Total_vehicle_25,
                                        HSR_Maint_tCO2_15 = Weights/730*((1329+8.5+140)/30)*(EF/260)*Total_vehicle_15,
                                        HSR_Maint_tCO2_25 = Weights/730*((1329+8.5+140)/30)*(EF/260)*Total_vehicle_25)

# total HSR maintanence electricity consumption and emissions
HSR_Maint <- cbind.data.frame(Type=c('HSR_15','HSR_25'),
                              HSR_Maint_GWh = c(sum(Exist_trains$HSR_Maint_GWh_15)+sum(New_trains$HSR_Maint_GWh_15),
                                                sum(Exist_trains$HSR_Maint_GWh_25)+sum(New_trains$HSR_Maint_GWh_25)),
                              HSR_Maint_MtCO2_fixed=c(sum(Exist_trains$HSR_Maint_tCO2_15_fixed/1000000)+
                                                      sum(New_trains$HSR_Maint_tCO2_15_fixed/1000000),
                                                      sum(Exist_trains$HSR_Maint_tCO2_25_fixed/1000000)+
                                                      sum(New_trains$HSR_Maint_tCO2_25_fixed/1000000)),
                              HSR_Maint_MtCO2 = c(sum(Exist_trains$HSR_Maint_tCO2_15/1000000)+
                                                  sum(New_trains$HSR_Maint_tCO2_15/1000000),
                                                  sum(Exist_trains$HSR_Maint_tCO2_25/1000000)+
                                                  sum(New_trains$HSR_Maint_tCO2_25/1000000)))

HSR_LCI <- merge(HSR_LCI,HSR_Maint, by='Type',all.x=T)

############################################ HSR Infrastructure Emissions ######################################
# This is the key for this round of update: 
# Under HSR 15, to 2050 no network changes thus no construction emissions
# Under HSR 25, 17 new stations are found to be built, track length will double from the 2015 level, emisssions

# 1) Station construction: 17 new HSR stations to be available, take average number of platforms of existing ones
# Exisitng HSR stations in 2015
stations <- read.csv('D:/Aviation_HSR/stations.csv')

# new HSR stations: number of platforms determined by city population in 2025
new_stations <- cbind.data.frame(new_stations[,1], Length=450, Width=15,Height=1.3,
                                 Platforms=round(mean(stations$Platforms)))
names(new_stations) <- c('CityID','Length','Width','Height','Platforms')

# Estimate material requirements per m3, and then estimate the emissions inventory from material production
# 2x (720ft long x 15 feet wide X 2 feet in height) platforms, on the side of 660 ft track 
# 1 cubic foot = 0.028 m3
total_m3_per_station <- 2*(720*15*2)/0.028

# pp.91: 43000ft3 concrete and 22000 ft3 subbase material, and 32000 lbs steel per station
# amount of concrete, subbase, and steel needed for 1 Chinese HSR platform 450x15x1.3
concrete_per_platform_m3 <- ((43000/0.028)/total_m3_per_station)*(450*15*1.3)
subbase_per_platform_m3 <- ((22000/0.028)/total_m3_per_station)*(450*15*1.3)
# 1 lbs = 0.45359237 kg, 32000 lbs = 32000*0.45359237=14514.96kg steel
# 1 m3 steel weights 7900kg
# total m3 of steel needed per station = 580.5982/7900 m3
steel_per_platform_m3 <- (((32000*0.45359237)/7900)/total_m3_per_station)*(450*15*1.3)

# Table 63: 1 yd3 of concrete production generates 609 kgCO2; 
#           1 yd3 subbase material production generates 35 kgCO2, 1 yd3 steel generate 543kgCO2 

# kg CO2 generated from producing required materials for 1 platform construction
# 1 m3 = 1.31 yd3
concrete_tCO2_per_plt <- concrete_per_platform_m3 *(609/1.31)/1000
subbase_tCO2_per_plt <- subbase_per_platform_m3*(35/1.31)/1000
steel_tCO2_per_plt <- steel_per_platform_m3*(543/1.31)/1000

# station construction emissions by platform
new_stations <- new_stations %>% mutate(HSR_stations_tCO2=Platforms*
                                        (concrete_tCO2_per_plt+subbase_tCO2_per_plt+steel_tCO2_per_plt))

HSR_LCI <- cbind.data.frame(HSR_LCI, rbind.data.frame(HSR_Stations_Const_MtCO2 = 0,
                            new_stations%>%summarise(HSR_Stations_Const_MtCO2 = sum(HSR_stations_tCO2/1000000))))

# 2) Track construction: based on CAHSR total track length, Table 55: 700 miles track, 
# concrete: 340*1000000 ft3 for all 700 mile track
# 1 cubic foot = 0.028 m3, 1 mile = 1.61 km
m3_concrete_per_km <- (340*1000000*0.028)/(700*1.61)
m3_subbase_per_km <- (200*1000000*0.028)/(700*1.61)
# steel: 260*1000000 lbs for all 700 mile track
m3_steel_per_km <- ((260*1000000*0.45359237)/7900)/(700*1.61)

# ton CO2 from producing materials for 1km track construction
# 1 m3 = 1.31 yd3
concrete_tCO2_per_km <- m3_concrete_per_km*(609/1.31)/1000
subbase_tCO2_per_km <- m3_subbase_per_km*(35/1.31)/1000
steel_tCO2_per_km <- m3_steel_per_km*(543/1.31)/1000

# power structure emissoins: $48600 per mile, 728 tCO2/$million
power_tCO2_per_km <- 728*(48600/1.61)/1000000

# total track emissions must avoid double counting! 
# Beijing-Shanghai and Beijing-Nanjing shares the track of Beijing-Nanjing!
# we need cumulative HSR distance by each HSR line, and just for post-2015 new added tracks!
load("D:/Aviation_HSR/Full_HSR.RData")
HSR_track_15 <- Direct_15[,c(1,2,3,6)]%>% unique()
HSR_track_25 <- Direct_25[,c(1,2,3,6)]%>% unique()
# counting the order of city pairs on the HSR line
HSR_track_15 <- HSR_track_15 %>% group_by(HSR, Origin) %>% mutate(order=row_number())
HSR_track_15 <- merge(HSR_track_15,HSR_track_15[,c('HSR','Origin')]%>% unique() %>% group_by(HSR) %>% 
                     mutate(city_order=row_number()),by=c('HSR','Origin'),all.x=T)%>%arrange(city_order)

HSR_track_25 <- HSR_track_25 %>% group_by(HSR, Origin) %>% mutate(order=row_number())
HSR_track_25 <- merge(HSR_track_25,HSR_track_25[,c('HSR','Origin')]%>% unique() %>% group_by(HSR) %>% 
                        mutate(city_order=row_number()),by=c('HSR','Origin'),all.x=T)%>%arrange(city_order)

Total_length_15 <- HSR_track_15 %>% group_by(HSR) %>% filter(city_order==1, order==max(order))
Total_length_25 <- HSR_track_25 %>% group_by(HSR) %>% filter(city_order==1, order==max(order))

HSR_LCI <- cbind.data.frame(HSR_LCI, HSR_track_length=c(sum(Total_length_15$Distance),sum(Total_length_25$Distance)))

# now compute the total track construction CO2 without double counting
HSR_LCI<- cbind.data.frame(HSR_LCI, 
                           HSR_Track_Const_MtCO2 = c(0, (sum(Total_length_25$Distance)-sum(Total_length_15$Distance))*
                           (concrete_tCO2_per_km+subbase_tCO2_per_km+steel_tCO2_per_km+power_tCO2_per_km)/1000000)) 

# 3) Station/Track maintanence: HSR_15: existing stations/track maintenance, HSR_25: exisiting + new maintenance
stations <- stations %>% mutate(HSR_stations_tCO2=Platforms*
                                (concrete_tCO2_per_plt+subbase_tCO2_per_plt+steel_tCO2_per_plt))


HSR_LCI <- cbind.data.frame(HSR_LCI, 
                            HSR_Infra_Maint_MtCO2 = c(0.05*(sum(stations$HSR_stations_tCO2/1000000) + 
                                                            sum(Total_length_15$Distance)*
                                                            (concrete_tCO2_per_km+subbase_tCO2_per_km+
                                                             steel_tCO2_per_km+power_tCO2_per_km)/1000000),
                                                      0.05*(sum(stations$HSR_stations_tCO2/1000000)+
                                                            sum(new_stations$HSR_stations_tCO2/1000000)+
                                                            sum(Total_length_25$Distance)*
                                                           (concrete_tCO2_per_km+subbase_tCO2_per_km+
                                                            steel_tCO2_per_km+power_tCO2_per_km)/1000000)))

# 4) Station operation emissions: Lighting + Escalators + Train Control + Miscellaneous
stations$New <- 0
new_stations$New<-1

# lighting: 0.9 million kWh per station per year
stations_2 <- merge(rbind.data.frame(stations[,c('Platforms','New')],new_stations[,c('Platforms','New')]),emission_factor)

# 2015 HSR: only existing 40 stations operating emissions
stations_2$Lighting_GWh_15 <- ifelse(stations_2$New==0,(900000/2)*stations_2$Platforms/260/1000000,0)
stations_2$Lighting_tCO2_fixed_15 <- ifelse(stations_2$New==0,(900000/2)*stations_2$Platforms*((657/260)/1000000),0)
stations_2$Lighting_tCO2_15 <- ifelse(stations_2$New==0,(900000/2)*stations_2$Platforms*((stations_2$EF/260)/1000000),0)

# 2025 HSR: 40 + 17 stations operating emissions
stations_2$Lighting_GWh_25 <- (900000/2)*stations_2$Platforms/260/1000000
stations_2$Lighting_tCO2_fixed_25 <- (900000/2)*stations_2$Platforms*((657/260)/1000000)
stations_2$Lighting_tCO2_25 <- (900000/2)*stations_2$Platforms*((stations_2$EF/260)/1000000)

# escalators: 2 escalators, 4.7kWh/h per platform, 15hrs/day, 365 days
stations_2$Escalators_GWh_15 <- ifelse(stations_2$New==0,4.7*15*365*stations_2$Platforms/260/1000000,0)
stations_2$Escalators_tCO2_fixed_15 <- ifelse(stations_2$New==0,4.7*15*365*stations_2$Platforms*((657/260)/1000000),0)
stations_2$Escalators_tCO2_15 <- ifelse(stations_2$New==0,4.7*15*365*stations_2$Platforms*((stations_2$EF/260)/1000000),0)

stations_2$Escalators_GWh_25 <- 4.7*15*365*stations_2$Platforms/260/1000000
stations_2$Escalators_tCO2_fixed_25 <- 4.7*15*365*stations_2$Platforms*((657/260)/1000000)
stations_2$Escalators_tCO2_25 <- 4.7*15*365*stations_2$Platforms*((stations_2$EF/260)/1000000)

# Miscellaneous per station per year: 160000 kWh/year
stations_2$Miscellaneous_GWh_15 <- ifelse(stations_2$New==0,(160000/2)*stations_2$Platforms/260/1000000,0)
stations_2$Miscellaneous_tCO2_fixed_15 <- ifelse(stations_2$New==0,(160000/2)*stations_2$Platforms*((657/260)/1000000),0)
stations_2$Miscellaneous_tCO2_15 <- ifelse(stations_2$New==0,(160000/2)*stations_2$Platforms*((stations_2$EF/260)/1000000),0)

stations_2$Miscellaneous_GWh_25 <- (160000/2)*stations_2$Platforms/260/1000000
stations_2$Miscellaneous_tCO2_fixed_25 <- (160000/2)*stations_2$Platforms*((657/260)/1000000)
stations_2$Miscellaneous_tCO2_25 <- (160000/2)*stations_2$Platforms*((stations_2$EF/260)/1000000)


stations_2 <- stations_2 %>% mutate(Station_Op_GWh_15=Lighting_GWh_15+Escalators_GWh_15+Miscellaneous_GWh_15,
                                    Station_Op_GWh_25=Lighting_GWh_25+Escalators_GWh_25+Miscellaneous_GWh_25,
                                    Station_Op_MtCO2_fixed_15=(Lighting_tCO2_fixed_15+Escalators_tCO2_fixed_15+Miscellaneous_tCO2_fixed_15)/1000000,
                                    Station_Op_MtCO2_fixed_25=(Lighting_tCO2_fixed_25+Escalators_tCO2_fixed_25+Miscellaneous_tCO2_fixed_25)/1000000,
                                    Station_Op_MtCO2_15=(Lighting_tCO2_15+Escalators_tCO2_15+Miscellaneous_tCO2_15)/1000000,
                                    Station_Op_MtCO2_25=(Lighting_tCO2_25+Escalators_tCO2_25+Miscellaneous_tCO2_25)/1000000)

HSR_LCI<- cbind.data.frame(HSR_LCI, 
                           Station_Op_GWh = c(sum(stations_2$Station_Op_GWh_15),sum(stations_2$Station_Op_GWh_25)),
                           Station_Op_MtCO2_fixed = c(sum(stations_2$Station_Op_MtCO2_fixed_15), sum(stations_2$Station_Op_MtCO2_fixed_25)),
                           Station_Op_MtCO2 = c(sum(stations_2$Station_Op_MtCO2_15), sum(stations_2$Station_Op_MtCO2_25)))

# train control: 47000 kWh/mile-yr
# HSR 2015: Total_track_15 train control eletrcitiy consumption and emissions
Train_Control <- cbind.data.frame(emission_factor[,c('Year','EF')],
                                  Track_15 = sum(Total_length_15$Distance),Track_25=sum(Total_length_25$Distance))
Train_Control$Track_25 <- ifelse(Train_Control$Year<=2025, Train_Control$Track_15,Train_Control$Track_25)

Train_Control$TrainCon_GWh_15 <- ((47000/1.61)/260)*Train_Control$Track_15/1000000
Train_Control$TrainCon_tCO2_fixed_15 <- (47000/1.61)*(657/260)*Train_Control$Track_15/1000000
Train_Control$TrainCon_tCO2_15 <- (47000/1.61)*(Train_Control$EF/260)*Train_Control$Track_15/1000000

Train_Control$TrainCon_GWh_25 <- ((47000/1.61)/260)*Train_Control$Track_25/1000000
Train_Control$TrainCon_tCO2_fixed_25 <- (47000/1.61)*(657/260)*Train_Control$Track_25/1000000
Train_Control$TrainCon_tCO2_25 <- (47000/1.61)*(Train_Control$EF/260)*Train_Control$Track_25/1000000


HSR_LCI<- cbind.data.frame(HSR_LCI, 
                           TrainCon_GWh = c(sum(Train_Control$TrainCon_GWh_15),sum(Train_Control$TrainCon_GWh_25)),
                           TrainCon_MtCO2_fixed = c(sum(Train_Control$TrainCon_tCO2_fixed_15)/1000000, 
                                                    sum(Train_Control$TrainCon_tCO2_fixed_25)/1000000),
                           TrainCon_MtCO2 = c(sum(Train_Control$TrainCon_tCO2_15)/1000000, sum(Train_Control$TrainCon_tCO2_25)/1000000))

############################## Emissions from Eletricity Production ###########################
# 1) delivered electricity: annual total electricity consumption from all components
Elect_prod <- HSR_operation[,c('Year','Type','EF','EF_15','HSR_Op_Elect_GWh')] 

# vehicle manufacturing electricity use
Elect_prod <- merge(Elect_prod, rename(New_trains,HSR_15=HSR_Manu_GWh_15,HSR_25=HSR_Manu_GWh_25) %>% 
                    group_by(Year,prev_EF) %>% summarise(HSR_15=sum(HSR_15),HSR_25=sum(HSR_25)) %>%
                    gather(Type,HSR_Manu_GWh,-Year,-prev_EF), by=c('Year','Type'),all.x=T)

# vehicle maintenance electricity use
Elect_prod <- merge(Elect_prod, rename(New_trains,HSR_15=HSR_Maint_GWh_15,HSR_25=HSR_Maint_GWh_25) %>% 
                      group_by(Year) %>% summarise(HSR_15=sum(HSR_15),HSR_25=sum(HSR_25)) %>%
                      gather(Type,HSR_Maint_GWh,-Year), by=c('Year','Type'),all.x=T)

# station operation electricity use
Elect_prod <- merge(Elect_prod, rename(stations_2,HSR_15=Station_Op_GWh_15,HSR_25=Station_Op_GWh_25) %>% 
                      group_by(Year) %>% summarise(HSR_15=sum(HSR_15),HSR_25=sum(HSR_25)) %>%
                      gather(Type,Station_Op_GWh,-Year), by=c('Year','Type'),all.x=T)

# train constrol electricity use
Elect_prod <- merge(Elect_prod, rename(Train_Control,HSR_15=TrainCon_GWh_15,HSR_25=TrainCon_GWh_25) %>% 
                    group_by(Year) %>% summarise(HSR_15=sum(HSR_15),HSR_25=sum(HSR_25)) %>%
                    gather(Type,TrainCont_GWh,-Year), by=c('Year','Type'),all.x=T)

Elect_prod[is.na(Elect_prod)] <- 0
Elect_prod <- Elect_prod %>% gather(component,Elect_GWh,-Type,-Year,-EF,-prev_EF,-EF_15) %>% 
              group_by(Year,Type,EF,EF_15) %>% summarise(Elect_GWh=sum(Elect_GWh))

# 2) produced electricity: delivered electricity/(1 - % of T&D losses)
# According to IEA, 
# total electricity production is 5678.945TWh(2014), 5859.515TWh(2015), 6217.907TWh(2016)
# total T&D losses: 309.988TWh(2014), 298.786TWh(2015), 306.293TWh(2016)
Elect_prod$TD_losses_15 <- 298.786/5859.515
Elect_prod <- Elect_prod %>% mutate(Prod_Elect_GWh = Elect_GWh/(1-TD_losses_15))
Elect_prod$TD_losses_Elect <- Elect_prod$Prod_Elect_GWh*Elect_prod$TD_losses_15
Elect_prod$TD_losses_MtCO2_fixed <- (Elect_prod$TD_losses_Elect*1000000)*657/1000000000000
Elect_prod$TD_losses_MtCO2 <- (Elect_prod$TD_losses_Elect*1000000)*Elect_prod$EF/1000000000000

HSR_LCI <- merge(HSR_LCI, Elect_prod%>%group_by(Type)%>%summarise(TD_losses_MtCO2_fixed=sum(TD_losses_MtCO2_fixed),
                                                                  TD_losses_MtCO2=sum(TD_losses_MtCO2)),by='Type')

#HSR_LCI_All <- HSR_LCI
HSR_LCI_All <- rbind.data.frame(HSR_LCI_All,HSR_LCI)
HSR_LCI_All$TrainCon_MtCO2_fixed <- 100*HSR_LCI_All$TrainCon_MtCO2_fixed
HSR_LCI_All$TrainCon_MtCO2<- 100*HSR_LCI_All$TrainCon_MtCO2

################################################################################################
# Think how to best show the results in a waterfall chart
# first, a bar chart showing proportion of different LCI components under HSR15 and HSR25
LCI_fixed <- HSR_LCI_All[,c(1:2,5,8,11,13,15:16,18,21,23)] %>% gather(Components, Inventory, -Type, -Scenario)
LCI_fixed$EF <- 'Fixed'
LCI_fixed$Components <- as.factor(LCI_fixed$Components)
levels(LCI_fixed$Components) <- list('V,Operation'='HSR_Operation_MtCO2_fixed','V,Manufacturing'='HSR_Manu_MtCO2_fixed',
                                     'V,Maintenance'='HSR_Maint_MtCO2_fixed','I,Construction(Station)'='HSR_Stations_Const_MtCO2',
                                     'I,Construction(Track)'='HSR_Track_Const_MtCO2','I,Maintenance'='HSR_Infra_Maint_MtCO2',
                                     'I,Operation(Station)'='Station_Op_MtCO2_fixed','I,Operation(Train Control)'='TrainCon_MtCO2_fixed',
                                     'E,Distribution Losses'='TD_losses_MtCO2_fixed')

LCI_fixed$Components <- factor(LCI_fixed$Components, 
                               levels=c('V,Operation','V,Manufacturing','V,Maintenance','I,Construction(Station)','I,Construction(Track)',
                                        'I,Maintenance','I,Operation(Station)','I,Operation(Train Control)','E,Distribution Losses'))

LCI_fixed$Group <- paste(LCI_fixed$Scenario, LCI_fixed$Type, sep = '-')


################ IEA declining EF LCI #################
LCI_IEA <- HSR_LCI_All[,c(1:2,6,9,12,13,15:16,19,22,24)] %>% gather(Components, Inventory, -Type, -Scenario)

LCI_IEA$EF <- 'IEA'
LCI_IEA$Components <- as.factor(LCI_IEA$Components)
levels(LCI_IEA$Components) <- list('V,Operation'='HSR_Operation_MtCO2','V,Manufacturing'='HSR_Manu_MtCO2',
                                     'V,Maintenance'='HSR_Maint_MtCO2','I,Construction(Station)'='HSR_Stations_Const_MtCO2',
                                     'I,Construction(Track)'='HSR_Track_Const_MtCO2','I,Maintenance'='HSR_Infra_Maint_MtCO2',
                                     'I,Operation(Station)'='Station_Op_MtCO2','I,Operation(Train Control)'='TrainCon_MtCO2',
                                     'E,Distribution Losses'='TD_losses_MtCO2')

LCI_IEA$Components <- factor(LCI_IEA$Components, 
                               levels=c('V,Operation','V,Manufacturing','V,Maintenance','I,Construction(Station)','I,Construction(Track)',
                                        'I,Maintenance','I,Operation(Station)','I,Operation(Train Control)','E,Distribution Losses'))

LCI_IEA$Group <- paste(LCI_IEA$Scenario, LCI_IEA$Type, sep = '-')

################## Final plot ######################
Show <- LCI_IEA

Show$Group <- as.factor(Show$Group)
Show$Group <- factor(Show$Group, levels=c('Low Growth-HSR_25','Low Growth-HSR_15','Central Growth-HSR_25',
                                          'Central Growth-HSR_15','High Growth-HSR_25','High Growth-HSR_15'))
levels(Show$Group) <- list('Low Growth\n HSR2015'='Low Growth-HSR_15','Low Growth\n HSR2025'='Low Growth-HSR_25',
                           'Central Growth\n HSR2015'='Central Growth-HSR_15','Central Growth\n HSR2025'='Central Growth-HSR_25',
                           'High Growth\n HSR2015'='High Growth-HSR_15','High Growth\n HSR2025'='High Growth-HSR_25')

ggplot(Show, aes(fill=Components, y=Inventory, x=Group, group=Group)) + 
  #scale_y_continuous(limits = c(0,1000),breaks = c(0,200,400,600,800,1000))+
  ylab('HSR Cumulative Life-cycle CO2 Emissions Inventory (Mt)')+
  ggtitle('Cumulative HSR Lifecycle Emissions Inventory 2015-2050 (with fixed carbon intensity of power generation in 2015)')+
  geom_bar(stat="identity")+coord_flip()+theme_Publication() +scale_fill_Publication()


ggplot(Show, aes(fill=Components, y=Inventory, x=Group, group=Group)) + 
       scale_y_continuous(limits = c(0,1000),breaks = c(0,200,400,600,800,1000))+
       ylab('HSR Cumulative Life-cycle CO2 Emissions Inventory (Mt)')+
       ggtitle('Cumulative HSR Lifecycle Emissions Inventory 2015-2050 (with declining carbon intensity of power generation by IEA)')+
       geom_bar(stat="identity")+coord_flip()+theme_Publication() +scale_fill_Publication()










