setwd("D:/Aviation_HSR")
library(dplyr)
library(tidyr)
library(ggplot2)

# Comparing with chapter 2, this time we need to use the airline demand data for 2015.
source("E:/Dropbox/R/ggplot.R")
#################################### Load data #######################################
load("D:/Aviation_HSR/HSR_Entry.RData")
load("D:/Aviation_HSR/CHN_All.RData")
# load AIM data
airport <- read.csv('airports_city_2015.csv')
Segment <- read.csv('AirportSegmentData.csv')
CityPair <- read.csv('DataByCityPair.csv')

# check the proportion of direct routes to total in RPK
sum(subset(China, Nlegs==1)$Passengers)/sum(China$Passengers)
sum(subset(China, Nlegs==1)$Passengers*subset(China, Nlegs==1)$OD_Distance)/sum(China$Passengers*China$OD_Distance)

############################### Air transport O-D demand 2015 ########################
# all non-stop markets air pax in 2015
OD_15 <- subset(China, Nlegs==1)
OD_15 <- subset(OD_15) %>% group_by(Year, Origin.City, Dest.City)%>%
                summarise(Air_time=round(mean(Time),2),Air_dist=round(mean(OD_Distance),2),
                Air_price=round(mean(Fare),2), Air_freq=sum(Frequency),Air_pax=round(sum(Passengers)))

# compare two AIM input demand: segment demand and O-D city pair demand, for direct routes these two should be identical. 
base_year <- merge(Segment[,c('SegmentOriginID','SegmentDestinationID','PaxSegDemandBase')],
                   subset(airport[,c('Number','Code','City','CityID','Country')],Country=='China'), by.x = 'SegmentOriginID',by.y='Number', all.x = T)

base_year <- merge(base_year,
                   subset(airport[,c('Number','Code','City','CityID','Country')],Country=='China'), by.x = 'SegmentDestinationID',by.y='Number', all.x = T)

base_year <- rename(base_year, Origin.Airport=Code.x, Dest.Airport=Code.y, Origin.City=City.x, Dest.City=City.y, OriginCityID=CityID.x, DestCityID=CityID.y,
                    OriginCountry=Country.x, DestCountry=Country.y)
base_year <- base_year[,c('Origin.Airport','Origin.City','Dest.Airport','Dest.City','OriginCityID','DestCityID','PaxSegDemandBase','OriginCountry',
                          'DestCountry', 'SegmentOriginID','SegmentDestinationID')]
base_year$Year <- 2015
base_year <- drop_na(base_year)

base_year <- base_year %>% group_by(Year,Origin.City, Dest.City, OriginCityID, DestCityID) %>% summarise(Base_demand=sum(PaxSegDemandBase))
base_year <- merge(base_year, CityPair[,c('OriginCityID','DestinationCityID','BaseYearODDemandPax')],
                   by.x=c('OriginCityID','DestCityID'), by.y=c('OriginCityID','DestinationCityID'), all.x = T)

# match the air transport demand
OD_15 <- merge(base_year, OD_15, by=c('Year','Origin.City','Dest.City'),all.x = T)
OD_15 <- subset(OD_15, !is.na(OD_15$Air_pax))

#ggplot(OD_15) + geom_point(aes(x=Base_demand, y=Air_pax))
#ggplot(OD_15) + geom_point(aes(x=Base_demand, y=BaseYearODDemandPax))

# make the Air_pax equal to Base_demand: for nonstop routes, segment demand = O-D demand
OD_15$Air_pax <- OD_15$Base_demand

################################ match air transport markets with HSR O-D ####################################
online.overlap <- merge(OD_15, HSR_all[,c('Origin','Destination','Enter.Year','Enter.Month','Distance','city.pair')], 
                        by.x=c('Origin.City','Dest.City'), by.y=c('Origin','Destination'), all.x = T)
online.overlap <- subset(online.overlap, !is.na(online.overlap$city.pair))

# Overline.Entry only records single direction, but we need to capture overline routes in OD_15 for both directions
OD_15$city.pair <- paste(OD_15$Origin.City, OD_15$Dest.City, sep='-')
OD_15$city.pair <- apply(sapply(strsplit(as.character(OD_15$city.pair), "-"), sort), 2, paste, collapse="-")

overline.overlap <- merge(OD_15, Overline.Entry, by.x=c('city.pair'), by.y=c('id'), all.x = T)
overline.overlap <- subset(overline.overlap, !is.na(overline.overlap$Enter.Month))

Air_HSR <- rbind.data.frame(online.overlap[,c(16,3,1,2,4:15)], overline.overlap)
Air_HSR <- subset(Air_HSR,!(Origin.City=='Yichun'|Dest.City=='Yichun'))
rm(overline.overlap, online.overlap, Online.Entry, Overline.Entry,Overline)

# check if every city pair has two observations: single + return
# Air_HSR <- Air_HSR %>% group_by(city.pair) %>% unique() %>% mutate(count=row_number()) %>% mutate(count=max(count))
# Nanjing-Shanghai has two HSR routes but for simplicity we only consider the Jing-Hu line one, which starts in 2011 
Air_HSR <- subset(Air_HSR, !(city.pair=='Nanjing-Shanghai' & Enter.Year==2010)) %>% unique()

################################ Merge Air demand with HSR data from web scrapping ##########################
load("D:/Aviation_HSR/All_trains_0925.RData")
HSR_0925 <- HSR
load("D:/Aviation_HSR/All_trains_1018.RData")
rm(check, input, output, remain)

missing <- data.frame('Origin_city' = c('Guilin','Changchun','Wuxi'), 'Dest_city' = c('Shanghai','Wuxi','Harbin'),
                      'HSR_time_mean' = c(9.5,10.8,12.02), 'HSR_time_min'=c(9.5,10.8,12.02), 'HSR_price'=c(659.5,829,937.5),'HSR_freq'=c(1,1,1))

# we merge HSR from both days from scrapping, so that we have complete observations
HSR <- rbind.data.frame(HSR, HSR_0925,missing)
HSR$city.pair <- paste(HSR$Origin_city, HSR$Dest_city, sep='-')
HSR$city.pair <- apply(sapply(strsplit(as.character(HSR$city.pair), "-"), sort), 2, paste, collapse="-")
rm(HSR_0925,Direct1,Direct2,All_trains_0925,All_trains_1018)

HSR <- HSR %>% group_by(city.pair,Origin_city, Dest_city) %>% 
               summarise(HSR_time=min(HSR_time_min),HSR_price=round(mean(HSR_price),2), HSR_freq=round(mean(HSR_freq)))
HSR <- HSR %>% group_by(city.pair) %>% mutate(count=row_number()) %>% mutate(count=max(count))

# merge with the HSR data: the HSR dataset comes from Web_scrap.R, with all HSR trains on a given day for each city pair
Air_HSR <- merge(Air_HSR, HSR[,c(2:6)], by.x=c('Origin.City','Dest.City'),by.y=c('Origin_city','Dest_city'), all.x =T)
Air_HSR <- subset(Air_HSR, !is.na(HSR_freq))
Air_HSR <- rename(Air_HSR, HSR_dist = Distance)
Air_HSR$Air_day_freq <- round(Air_HSR$Air_freq/365)
Air_HSR$id <- paste(Air_HSR$Origin.City,Air_HSR$Dest.City,sep='-')

###################################################### Model construction #######################################################
# logit model based on aggregated data: Berkson's method
Model <- read.csv('D:/Aviation_HSR/sample2.csv')
AIM_city <- read.csv('CityData.csv') # the CityData in this folder is updated for special cities.

Model$Air_share <- 1 - Model$HSR_share
Model <- merge(Air_HSR[,1:17], Model[,c('Origin.City','Dest.City','HSR_time','HSR_price','HSR_freq','HSR_share','Air_share')], by=c('Origin.City','Dest.City'),all.x =T) 
Model$HSR_pax <- round(Model$Air_pax/Model$Air_share - Model$Air_pax)

# make air time also as hours
Model$Air_time_hrs <- round(Model$Air_time/60, 2)
# convert HSR price to 2015 US dollors
Model$HSR_USD <- round(Model$HSR_price/(1.029*6.3986), 2)
Model$HSR_freq <- Model$HSR_freq*365

# rail station and airport access time
access.time <- read.csv('D:/Aviation_HSR/city_stations.csv')
Model <- merge(Model, access.time[,c('City','To_station_dist','To_airport_dist','To_station_drive','To_airport_drive')], by.x='Origin.City', by.y='City', all.x = T)
Model <- rename(Model, Origin_to_rail_dist = To_station_dist,Origin_to_air_dist = To_airport_dist, Origin_to_rail_drive = To_station_drive,Origin_to_air_drive = To_airport_drive)
Model <- merge(Model, access.time[,c('City','To_station_dist','To_airport_dist','To_station_drive','To_airport_drive')], by.x='Dest.City', by.y='City', all.x = T)
Model <- rename(Model, Dest_to_rail_dist = To_station_dist,Dest_to_air_dist = To_airport_dist,Dest_to_rail_drive = To_station_drive,Dest_to_air_drive = To_airport_drive)
Model$Air_AE_dist <- Model$Origin_to_air_dist + Model$Dest_to_air_dist
Model$Air_AE_drive <- Model$Origin_to_air_drive + Model$Dest_to_air_drive
Model$HSR_AE_dist <- Model$Origin_to_rail_dist + Model$Dest_to_rail_dist
Model$HSR_AE_drive <- Model$Origin_to_rail_drive + Model$Dest_to_rail_drive

# AIM population, income, special city
Model <- merge(Model, AIM_city[,c('CityID','Spec')], by.x='OriginCityID',by.y='CityID',all.x=T)
Model <- rename(Model, Spec_Origin = Spec)
Model <- merge(Model, AIM_city[,c('CityID','Spec')], by.x='DestCityID',by.y='CityID',all.x=T)
Model <- rename(Model, Spec_Dest = Spec)


########################### test on the simple gravity model using model_2 from Logit_estimate ######################
source("E:/Dropbox/R_scripts/DomesticCHN/Ch2_R_scripts/GDP.R")
rm(GDP,US_convert)
setwd("D:/Aviation_HSR")

# convert income into $1000 as unit, and population as 1 million as unit
GDP2$Population <- GDP2$Population/100
Model <- merge(Model, subset(GDP2,Year==2015)[,c('City','Population')], by.x='Origin.City',by.y = 'City', all.x =T) 
Model <- rename(Model, Origin_Pop=Population)
Model <- merge(Model, subset(GDP2,Year==2015)[,c('City','Population')], by.x='Dest.City',by.y = 'City', all.x =T) 
Model <- rename(Model, Dest_Pop=Population)

# car ownership and income
Income <- read.csv('D:/Aviation_HSR/Rail_Region.csv')

# convert inflation rate of 2018 and then exchange rate in 2015
# inflation CPI in 2018: 2.9%, exchange rate to USD: 6.3986
Income$Income <- ((Income$Income/1.029)/6.3986)/1000
Model <- merge(Model, Income, by.x='Origin.City',by.y = 'City', all.x =T) 
Model <- rename(Model, Origin_income = Income, Origin_Car=Car_ownership)
Model <- merge(Model, Income, by.x='Dest.City',by.y = 'City', all.x =T) 
Model <- rename(Model, Dest_income = Income, Dest_Car=Car_ownership)

##################################### Second stage: Gravity Model ##########################################
Gravity <- Model
Gravity$Air_time_hrs <- ifelse(Gravity$id=='Beijing-Chengdu', 2.62, Gravity$Air_time_hrs)

Gravity$Total_demand <- Gravity$Air_pax + Gravity$HSR_pax
# geometric mean of income from endpoint cities, convert to USD
Gravity$Income <- Gravity$Origin_income*Gravity$Dest_income
Gravity$Population <- Gravity$Origin_Pop*Gravity$Dest_Pop

Gravity$Special_both <- ifelse(Gravity$Spec_Origin == 1 & Gravity$Spec_Dest==1, 1, 0)
Gravity$Special_none <- ifelse(Gravity$Spec_Origin == 0 & Gravity$Spec_Dest==0, 1, 0)

# compute market shares of air and hsr
update <- function(group){
  group$Total_demand <- round(group$Air_pax/(group$Air_share))
  group$HSR_pax <- group$Total_demand - group$Air_pax
  group$HSR_pax <- group$HSR_pax + round(runif(length(group$HSR_pax), min=-1000, max=1000))
  group$Total_demand <- group$Air_pax + group$HSR_pax
  group$price_diff <- group$Air_price - group$HSR_USD
  group$time_diff <- group$Air_time_hrs - group$HSR_time
  group$AE_drive_diff <- group$Air_AE_drive - group$HSR_AE_drive
  group$HSR_share <- group$HSR_pax/group$Total_demand
  group$Air_share <- 1-group$HSR_share
  group$share_ratio <- group$Air_share/group$HSR_share
  
  return(group)
}

Gravity <- update(group=Gravity)


