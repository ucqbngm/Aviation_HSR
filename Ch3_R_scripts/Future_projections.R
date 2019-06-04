setwd("D:/Aviation_HSR")
library(dplyr)
library(tidyr)
library(ggplot2)
source("E:/Dropbox/R/ggplot.R")

# this script takes projections of all model input to predict future total demand and air transport demand to 2050.
# Population is in thousands: need Medium, Low, and High Variant Projections for total population
Population <- read.csv('WPP_projection.csv')
Population <- subset(Population, Location=='China' & (Variant == 'Medium'|Variant == 'High' | Variant == 'Low'))[,c('Location','Time','Variant','PopTotal')]
Population$PopTotal <- Population$PopTotal*1000
Population <- Population %>% group_by(Variant) %>% mutate(Pop_last_yr=lag(PopTotal))
Population$Pop_growth <- (Population$PopTotal-Population$Pop_last_yr)/Population$Pop_last_yr
Population <- merge(Population[,c(2:4)] %>% spread(Variant, PopTotal),
                    Population[,c(2:3,6)] %>% spread(Variant, Pop_growth),by='Time')
names(Population) <- c('Time','Pop_high','Pop_low','Pop_med','Pop_high_growth','Pop_low_growth','Pop_med_growth')

# GDP is in millions USD, inflation adjusted: OECD Economic Outlook: Statistics and Projections: Long-term baseline projections
# The indicator is measured in USD at 2010 Purchasing Power Parities.We need to convert to 2015 USD.
GDP <- read.csv('CHN_GDP_projection.csv')
GDP <- GDP[,c(1,6,7)]
names(GDP) <- c('Location','Time','GDP')
GDP <- subset(GDP, Location=='CHN'& Time >= 2016 & Time <= 2050)
# convert from OECD 2010 PPP to 2015 USD PPP
GDP$GDP <- GDP$GDP*1.117592

# based on the reference case GDP projection from OECD, develop High/Low GDP growth scenario with annual growth rate +/- 1%
# then we will have proper projection of high/Med/Low GDP per capita in China
# compute the annual growth rate of GDP
GDP$GDP_last_yr <- lag(GDP$GDP)
# fill in 2015 GDP to compute 2016 growth rate
GDP$GDP_last_yr <- ifelse(GDP$Time==2016, 20137259, GDP$GDP_last_yr)
GDP$GDP_med_growth <- (GDP$GDP-GDP$GDP_last_yr)/GDP$GDP_last_yr
GDP$GDP_high_growth <- GDP$GDP_med_growth + 0.01
GDP$GDP_low_growth <- GDP$GDP_med_growth - 0.01

GDP$GDP_high <- GDP$GDP_last_yr*(1+GDP$GDP_high_growth)
GDP$GDP_med <- GDP$GDP
GDP$GDP_low <- GDP$GDP_last_yr*(1+GDP$GDP_low_growth)

##################################### Projections of Population, GDP, and GDP per capita ##################################
projection <- merge(Population, GDP[,c(2,5:10)], by='Time', all.y = T)
# GDP per capita
projection$Income_high <- projection$GDP_high*1000000/projection$Pop_low
projection$Income_med <- projection$GDP_med*1000000/projection$Pop_med
projection$Income_low <- projection$GDP_low*1000000/projection$Pop_high

# we need to plot both historical and projected Pop and Income
hist_GDP <- read.csv('CHN_hist_GDP.csv')
hist_GDP <- hist_GDP[,c(1,6,7)]
names(hist_GDP) <- c('Location','Time','GDP_CAP')
hist_GDP <- subset(hist_GDP, Location=='CHN' & Time < 2016)
historic <- merge(Population[,c('Time','Pop_med')], hist_GDP[,2:3], by='Time', all.y=T)

# add 2016 data to historic
historic <- rbind.data.frame(historic,data.frame(Time=2016,Pop_med=1403500365, GDP_CAP=15485.20))

################################################ porjected growth rate for population and income ############################################
Pop_scenario <- projection[,1:4] %>% gather(Type,Population,-Time)
Pop_scenario$Type <- factor(Pop_scenario$Type, levels=c('Pop_high','Pop_med','Pop_low'))
levels(Pop_scenario$Type)[levels(Pop_scenario$Type)=="Pop_high"] <- "High Growth"
levels(Pop_scenario$Type)[levels(Pop_scenario$Type)=="Pop_med"] <- "Medium Growth"
levels(Pop_scenario$Type)[levels(Pop_scenario$Type)=="Pop_low"] <- "Low Growth"

Pop_trend <- ggplot() + scale_x_continuous(breaks = c(1980,1990,2000,2010,2020,2030,2040,2050))+
             geom_line(data= historic,aes(x=as.numeric(as.character(Time)), y=Pop_med/1000000), size=2,color='darkgray')+
             geom_line(data= Pop_scenario, aes(x=as.numeric(as.character(Time)), y=Population/1000000, group=Type,color=Type),size=2)+
             scale_y_continuous(limits = c(0,1800))+
             xlab('Year') + ylab('Total Population in Mainland China (millions)') + 
             ggtitle('UN Population Projections for China (1980-2050)') + theme_Publication()+scale_colour_Publication()

# since population growth is faster than GDP growth at high growth scenario, it makes the per capita GDP actually lower
Income_scenario <- projection[,c(1,14:16)] %>% gather(Type,Income,-Time)
Income_scenario$Type <- factor(Income_scenario$Type, levels=c('Income_high','Income_med','Income_low'))
levels(Income_scenario$Type)[levels(Income_scenario$Type)=="Income_high"] <- "High Growth"
levels(Income_scenario$Type)[levels(Income_scenario$Type)=="Income_low"] <- "Low Growth"
levels(Income_scenario$Type)[levels(Income_scenario$Type)=="Income_med"] <- "Medium Growth"

Inc_trend <- ggplot() + scale_x_continuous(breaks = c(1980,1990,2000,2010,2020,2030,2040,2050))+
             geom_line(data= historic,aes(x=as.numeric(as.character(Time)), y=GDP_CAP), size=2,color='darkgray')+
             geom_line(data= Income_scenario, aes(x=as.numeric(as.character(Time)), y=Income, group=Type,color=Type),size=2)+
             xlab('Year') + ylab('GDP Per Capita (US dollors in 2015)') + 
             ggtitle('GDP Per Capita Projections for China (1980-2050)') + theme_Publication()+scale_colour_Publication()

# replace oil price projetion by EIA one
# we need to convert projected price as per MMBtu to per gallon, also it uses 2018 USD, need to change to 2015 USD by inflation 
EIA_fuel <- read.csv('EIA_JetFuel_US.csv')
EIA_fuel <- EIA_fuel %>% gather(Type, Fuel_price, -Year)
EIA_fuel$Type <- factor(EIA_fuel$Type, levels=c('High_price','Med_price','Low_price'))
levels(EIA_fuel$Type)[levels(EIA_fuel$Type)=="High_price"] <- "High Price"
levels(EIA_fuel$Type)[levels(EIA_fuel$Type)=="Med_price"] <- "Medium Price"
levels(EIA_fuel$Type)[levels(EIA_fuel$Type)=="Low_price"] <- "Low Price"

# 1 gallon of diesel fuel = 137,381 Btu
# 1 Btu =	0.000001 MMBtu
EIA_fuel$Fuel_price <- 137381*EIA_fuel$Fuel_price/1000000
# convert inflation rate
EIA_fuel$Fuel_price <- EIA_fuel$Fuel_price*(1.629/1.698361)

hist_fuel <- read.csv('Historic_JetA_Price.csv')
#hist_fuel <- subset(hist_fuel, Year<=2018)

Fuel_trend <- ggplot() + scale_x_continuous(breaks = c(1980,1990,2000,2010,2020,2030,2040,2050))+
              geom_line(data= hist_fuel,aes(x=as.numeric(as.character(Year)), y=Fuel_price_per_gallon), size=2,color='darkgray')+
              geom_line(data= EIA_fuel,aes(x=as.numeric(as.character(Year)), y=Fuel_price,group=Type,color=Type), size=2)+
              xlab('Year') + ylab('Fuel price per gallon (US dollors in 2015)\n') + 
              ggtitle('Jet-A Fuel Price Projection by EIA (1978-2050)') + theme_Publication()+scale_colour_Publication()


############################## China's Emissions Factors of Electricity Mix ############################
# historical values obtained from IEA Fuel Combustion (2017): emissions per kWh from electricity generation
# based on IEA's projection, extropolate: difference phases, and China needs to have zero carbon power generation by 2050
phases <- rbind.data.frame(data.frame(Year=seq(2016, 2025, 1),Phase='Phase1',Max=650,Min=404),
                           data.frame(Year=seq(2025, 2030, 1),Phase='Phase2',Max=404,Min=261),
                           data.frame(Year=seq(2030, 2035, 1),Phase='Phase3',Max=261,Min=120),
                           data.frame(Year=seq(2035, 2040, 1),Phase='Phase4',Max=120,Min=61),
                           data.frame(Year=seq(2040, 2050, 1),Phase='Phase5',Max=61,Min=0))

emission_factor <- NULL
for(i in levels(factor(phases$Phase))){
  temp <- subset(phases, Phase==i)
  temp$EF <- ((temp$Min-temp$Max)/(max(temp$Year)-min(temp$Year)))*(temp$Year - temp$Year[1])+temp$Max
  temp$EF <- round(temp$EF,1)
  emission_factor <- rbind.data.frame(emission_factor, temp[,c(1,5)]) %>% unique()
}

emission_factor <- rbind.data.frame(data.frame(Year=2015, EF=657.0),emission_factor)

# match emission factor and fuel price, which will be used in fuel production emissions for air
emission_factor <- merge(emission_factor, subset(EIA_fuel,Type=='Low Price')[,c(1,3)],by='Year',all.x=T) 
emission_factor$Fuel_price <- ifelse(emission_factor$Year==2015, 1.629, ifelse(emission_factor$Year==2016,1.319, emission_factor$Fuel_price))
emission_factor <- rename(emission_factor, Low_fuel_price = Fuel_price) 

emission_factor <- merge(emission_factor, subset(EIA_fuel,Type=='Medium Price')[,c(1,3)],by='Year',all.x=T) 
emission_factor$Fuel_price <- ifelse(emission_factor$Year==2015, 1.629, ifelse(emission_factor$Year==2016,1.319, emission_factor$Fuel_price))
emission_factor <- rename(emission_factor, Med_fuel_price = Fuel_price) 

emission_factor <- merge(emission_factor, subset(EIA_fuel,Type=='High Price')[,c(1,3)],by='Year',all.x=T) 
emission_factor$Fuel_price <- ifelse(emission_factor$Year==2015, 1.629, ifelse(emission_factor$Year==2016,1.319, emission_factor$Fuel_price))
emission_factor <- rename(emission_factor, High_fuel_price = Fuel_price) 

######################################### sample set HSR vehicles #####################################################
load("D:/Aviation_HSR/All_trains_1018.RData")
rm(check, input, output, remain)

# Having ready the 608 city pairs, now we need to match train types and total stations of these pairs
All_trains$Origin_city <- paste(toupper(substring(All_trains$Origin_city, 1,1)),substring(All_trains$Origin_city,2),sep='')
All_trains$Dest_city <- paste(toupper(substring(All_trains$Dest_city, 1,1)),substring(All_trains$Dest_city,2),sep='')
All_trains$id <- paste(All_trains$Origin_city,All_trains$Dest_city,sep='-')

trains <- All_trains %>% group_by(id,Origin_city,Dest_city,HSR_type) %>% summarise(count=n())
trains <- trains[,c('id','HSR_type','count')] %>% spread(HSR_type,count)
trains$D <- ifelse(is.na(trains$D),0,trains$D)
trains$G <- ifelse(is.na(trains$G),0,trains$G)
names(trains) <- c('id','D_type','G_type')

All <- read.csv('D:/Aviation_HSR/All_sample.csv')
trains <- merge(x=All[,c('city.pair','id','HSR_dist','HSR_time','HSR_pax','HSR_freq')],y=trains, by='id', all.x = T)

# we assume on each route only operate one dominating train type between D/G, based on distance and time, estimate speed
trains$HSR_type <- ifelse(trains$D_type > trains$G_type, 'D', 'G')
trains$Pax_per_train <- round(trains$HSR_pax/trains$HSR_freq)
trains$Vehicle_type <- ifelse(trains$Pax_per_train>=200 & trains$HSR_type=='G','G2',
                              ifelse(trains$Pax_per_train>=200 & trains$HSR_type=='D','D2',
                                     ifelse(trains$Pax_per_train < 200 & trains$HSR_type=='D','D1','G1')))

All <- merge(All, trains[,c('id','Vehicle_type')],by='id',all.x=T)

########################################################################################################################
# calculate population and income for all city pairs based on the base year value and growth rates
futures <- All[,c('city.pair','id','OriginCityID','DestCityID','Population','Income','HSR_freq')]

# compute annual growth rates of the three population growth projections
Pop_projection <- projection[,c(1,5:7)] %>% gather(Type,Pop_growth,-Time)

future_pop <- NULL
for(i in levels(factor(futures$id))){
  foo <- subset(futures, id==i)
  temp <- merge(foo, foo$Population*cumprod(1+ subset(Pop_projection,Type=='Pop_high_growth')$Pop_growth))
  temp <- cbind.data.frame(temp, foo$Population*cumprod(1+ subset(Pop_projection, Type=='Pop_med_growth')$Pop_growth))
  temp <- cbind.data.frame(temp, foo$Population*cumprod(1+ subset(Pop_projection, Type=='Pop_low_growth')$Pop_growth))
  temp <- temp[,c(1:4,8:10)]
  names(temp) <- c('city.pair','id','OriginCityID','DestCityID','Pop_high','Pop_med','Pop_low')
  temp <- temp %>% mutate(Year = row_number() + 2015)
  
  future_pop <- rbind.data.frame(future_pop, temp)
}

# compute annual growth rates of the three income growth projections
Inc_projection <- projection[,c(1,14:16)] %>% gather(Type,Income,-Time)
Inc_projection <- Inc_projection %>% group_by(Type) %>% mutate(Inc_last_yr=lag(Income))
# fill in 2015 Population to compute 2016 growth rate
Inc_projection$Inc_last_yr <- ifelse(Inc_projection$Time==2016, 14414.35, Inc_projection$Inc_last_yr)
Inc_projection$Inc_growth <- (Inc_projection$Income-Inc_projection$Inc_last_yr)/Inc_projection$Inc_last_yr

future_inc <- NULL
for(i in levels(factor(futures$id))){
  foo <- subset(futures, id==i)
  temp <- merge(foo, foo$Income*cumprod(1+ subset(Inc_projection, Type=='Income_high')$Inc_growth))
  temp <- cbind.data.frame(temp, foo$Income*cumprod(1+ subset(Inc_projection,  Type=='Income_med')$Inc_growth))
  temp <- cbind.data.frame(temp, foo$Income*cumprod(1+ subset(Inc_projection, Type=='Income_low')$Inc_growth))
  temp <- temp[,c(1:4,8:10)]
  names(temp) <- c('city.pair','id','OriginCityID','DestCityID','Income_high','Income_med','Income_low')
  temp <- temp %>% mutate(Year = row_number() + 2015)
  
  future_inc <- rbind.data.frame(future_inc, temp)
}

# compute annual growth rates of GDP for HSR freq growth
GDP_projection <- projection[,c(1,8:10)]%>% gather(Type,GDP_growth,-Time)

future_hsr <- NULL
for(i in levels(factor(futures$id))){
  foo <- subset(futures, id==i)
  temp <- merge(foo, round(foo$HSR_freq*cumprod(1+ subset(GDP_projection, Type=='GDP_high_growth')$GDP_growth)))
  temp <- cbind.data.frame(temp, round(foo$HSR_freq*cumprod(1+ subset(GDP_projection, Type=='GDP_med_growth')$GDP_growth)))
  temp <- cbind.data.frame(temp, round(foo$HSR_freq*cumprod(1+ subset(GDP_projection, Type=='GDP_low_growth')$GDP_growth)))
  temp <- temp[,c(1:4,8:10)]
  names(temp) <- c('city.pair','id','OriginCityID','DestCityID','HsrFreq_high','HsrFreq_med','HsrFreq_low')
  temp <- temp %>% mutate(Year = row_number() + 2015)
  
  future_hsr <- rbind.data.frame(future_hsr, temp)
}

# this is from 2016, we also need 2015 base year
futures <- merge(future_pop, future_inc, by=c('Year','city.pair','id','OriginCityID','DestCityID'))
futures <- merge(futures, future_hsr, by=c('Year','city.pair','id','OriginCityID','DestCityID'))

baseyear <- All[,c('city.pair','id','OriginCityID','DestCityID','Population','Income','HSR_freq')]
baseyear$Year <- 2015
baseyear <- baseyear %>% mutate(Pop_high=Population,Pop_low=Population,Income_high=Income,Income_low=Income,
                                HsrFreq_high=HSR_freq,HsrFreq_med=HSR_freq,HsrFreq_low=HSR_freq)
baseyear <- rename(baseyear, Pop_med = Population, Income_med=Income)
baseyear <- baseyear[,c('Year','city.pair','id','OriginCityID','DestCityID','Pop_high','Pop_med','Pop_low','Income_high','Income_med','Income_low',
                        'HsrFreq_high','HsrFreq_med','HsrFreq_low')]

futures <- rbind.data.frame(baseyear,futures)

# now all other key model input
futures <- merge(futures,All[ ,!(colnames(All) == "Population" |colnames(All) == "Income"|colnames(All) == "HSR_freq")], 
                 by=c('city.pair','id','OriginCityID','DestCityID'))

copy <- futures

################################ Calculate Pop/Income ratio with 1990 for AIM SSP Scenario #################
# AIM input the the ratio of population and income relative to the level in 1990
AIM_input <- rename(historic[1:36,], Income_med=GDP_CAP)
AIM_input <- AIM_input %>% mutate(Pop_high=Pop_med,Pop_low=Pop_med,Income_high=Income_med,Income_low=Income_med)
AIM_input <- rbind.data.frame(AIM_input[,c('Time','Pop_high','Pop_med','Pop_low','Income_high','Income_med','Income_low')],
                              projection[,c('Time','Pop_high','Pop_med','Pop_low','Income_high','Income_med','Income_low')])
AIM_input <- AIM_input %>% mutate(Pop_high_ratio=Pop_high/1172445200,Pop_med_ratio=Pop_med/1172445200,Pop_low_ratio=Pop_low/1172445200)
AIM_input$Income_90 <- subset(historic,Time==1990)$GDP_CAP
AIM_input <- AIM_input %>% mutate(Inc_high_ratio = Income_high/Income_90,Inc_med_ratio=Income_med/Income_90,Inc_low_ratio= Income_low/Income_90)

# convert fuel price to 1990 ratio
hist_fuel$Fuel_high_ratio <- hist_fuel$Fuel_price_per_gallon/0.766
hist_fuel <- hist_fuel %>% mutate(Fuel_low_ratio=Fuel_high_ratio,Fuel_med_ratio=Fuel_high_ratio)
EIA_fuel$Fuel_ratio <- EIA_fuel$Fuel_price/0.766

AIM_fuel <- EIA_fuel[,c('Year','Type','Fuel_ratio')] %>% spread(Type,Fuel_ratio)
names(AIM_fuel) <- c('Year','Fuel_high_ratio','Fuel_med_ratio','Fuel_low_ratio')
AIM_fuel <- rbind.data.frame(hist_fuel[2:40,c(1,3:5)],AIM_fuel)
AIM_input <- merge(AIM_input, AIM_fuel,by.x='Time', by.y='Year',all.x = T)

############################################ Replace AIM scenario input #######################################
AIM_input$id <- paste('Value',AIM_input$Time,sep='')
AIM_input$Country <- 'China'

Scenario_v8 <- read.csv('ScenarioData_v8.csv')
# Scenario 1: SSP1 low population growth, low income, and low fuel price
SSP1 <- subset(Scenario_v8, (ScenarioName=='SSP1base' & RegionID == 142 & (VariableID == 0 | VariableID == 1 |VariableID == 2)))
# Scenario 2: SSP2 Medium population growth, medium income, and medium fuel price
SSP2 <- subset(Scenario_v8, (ScenarioName=='SSP2base' & RegionID == 142 & (VariableID == 0 | VariableID == 1 |VariableID == 2)))
# Scenario 3: SSP3 high population growth, high income, and high fuel price
SSP3 <- subset(Scenario_v8, (ScenarioName=='SSP3base' & RegionID == 142 & (VariableID == 0 | VariableID == 1 |VariableID == 2)))

# projections to 2050 is enough
# SSP1: low growth future
SSP1_new <- cbind.data.frame(SSP1[,1:7], 
                             rbind.data.frame(subset(AIM_input, Time>=1990)[,c('Country','id','Pop_low_ratio')] %>% spread(id,Pop_low_ratio),
                                              subset(AIM_input, Time>=1990)[,c('Country','id','Inc_low_ratio')] %>% spread(id,Inc_low_ratio),
                                              subset(AIM_input, Time>=1990)[,c('Country','id','Fuel_high_ratio')] %>% spread(id,Fuel_high_ratio)))[,c(1:7,9:69)]
SSP1_new <- cbind.data.frame(SSP1_new,SSP1[,69:118])

# SSP2: central growth future
SSP2_new <- cbind.data.frame(SSP2[,1:7], 
                             rbind.data.frame(subset(AIM_input, Time>=1990)[,c('Country','id','Pop_med_ratio')] %>% spread(id,Pop_med_ratio),
                                              subset(AIM_input, Time>=1990)[,c('Country','id','Inc_med_ratio')] %>% spread(id,Inc_med_ratio),
                                              subset(AIM_input, Time>=1990)[,c('Country','id','Fuel_med_ratio')] %>% spread(id,Fuel_med_ratio)))[,c(1:7,9:69)]
SSP2_new <- cbind.data.frame(SSP2_new,SSP2[,69:118])

# SSP3: high growth future
SSP3_new <- cbind.data.frame(SSP3[,1:7], 
                             rbind.data.frame(subset(AIM_input, Time>=1990)[,c('Country','id','Pop_high_ratio')] %>% spread(id,Pop_high_ratio),
                                              subset(AIM_input, Time>=1990)[,c('Country','id','Inc_high_ratio')] %>% spread(id,Inc_high_ratio),
                                              subset(AIM_input, Time>=1990)[,c('Country','id','Fuel_low_ratio')] %>% spread(id,Fuel_low_ratio)))[,c(1:7,9:69)]
SSP3_new <- cbind.data.frame(SSP3_new,SSP3[,69:118])

# update population, income, and fuel price input
Scenario_v8 <- subset(Scenario_v8, !(ScenarioName=='SSP1base' & RegionID == 142 & VariableID == 0))
Scenario_v8 <- subset(Scenario_v8, !(ScenarioName=='SSP1base' & RegionID == 142 & VariableID == 1))
Scenario_v8 <- subset(Scenario_v8, !(ScenarioName=='SSP1base' & RegionID == 142 & VariableID == 2))
Scenario_v8 <- subset(Scenario_v8, !(ScenarioName=='SSP2base' & RegionID == 142 & VariableID == 0))
Scenario_v8 <- subset(Scenario_v8, !(ScenarioName=='SSP2base' & RegionID == 142 & VariableID == 1))
Scenario_v8 <- subset(Scenario_v8, !(ScenarioName=='SSP2base' & RegionID == 142 & VariableID == 2))
Scenario_v8 <- subset(Scenario_v8, !(ScenarioName=='SSP3base' & RegionID == 142 & VariableID == 0))
Scenario_v8 <- subset(Scenario_v8, !(ScenarioName=='SSP3base' & RegionID == 142 & VariableID == 1))
Scenario_v8 <- subset(Scenario_v8, !(ScenarioName=='SSP3base' & RegionID == 142 & VariableID == 2))

Scenario_v8 <- rbind.data.frame(Scenario_v8, SSP1_new,SSP2_new,SSP3_new)


library(stringr)
test <- subset(Scenario_v8, VariableID==2 & (ScenarioName=='SSP1base'|ScenarioName=='SSP2base'|ScenarioName=='SSP3base') & RegionID==142)
test <- test[,c(2,8:68)]
test <- test %>% gather(Time, Value, -ScenarioName)
test$Year <- as.numeric(str_extract(test$Time, "[0-9]+"))

ggplot(test, aes(x=Year, y=Value, group=ScenarioName, color=ScenarioName))+geom_line(size=2)+
  theme_Publication()+scale_colour_Publication()

write.csv(Scenario_v8, 'ScenarioData_v8_dom.csv',row.names = F)

################################### Run Global AIM for China to get all flt frequency ###############################
# repace fuel projection for all regions, and replace pop and income for China
# carry on from the above

# replace fuel projetion for all regions
SSP1 <- subset(Scenario_v8, (ScenarioName=='SSP1base' & VariableID == 2))
# Scenario 2: SSP2 Medium population growth, medium income, and medium fuel price
SSP2 <- subset(Scenario_v8, (ScenarioName=='SSP2base' & VariableID == 2))
# Scenario 3: SSP3 high population growth, high income, and high fuel price
SSP3 <- subset(Scenario_v8, (ScenarioName=='SSP3base' & VariableID == 2))

# SSP1: low growth future, high fuel price
SSP1_new <- cbind.data.frame(SSP1[,1:7], 
                             subset(AIM_input, Time>=1990)[,c('Country','id','Fuel_high_ratio')] %>% spread(id,Fuel_high_ratio))[,c(1:7,9:69)]
SSP1_new <- cbind.data.frame(SSP1_new,SSP1[,69:118])

# SSP2: med growth future
SSP2_new <- cbind.data.frame(SSP2[,1:7], 
                             subset(AIM_input, Time>=1990)[,c('Country','id','Fuel_med_ratio')] %>% spread(id,Fuel_med_ratio))[,c(1:7,9:69)]
SSP2_new <- cbind.data.frame(SSP2_new,SSP2[,69:118])

# SSP3: high growth future, low fuel price
SSP3_new <- cbind.data.frame(SSP3[,1:7], 
                             subset(AIM_input, Time>=1990)[,c('Country','id','Fuel_low_ratio')] %>% spread(id,Fuel_low_ratio))[,c(1:7,9:69)]
SSP3_new <- cbind.data.frame(SSP3_new,SSP3[,69:118])

# replace
Scenario_v8 <- subset(Scenario_v8, !(ScenarioName=='SSP1base' & VariableID == 2))
Scenario_v8 <- subset(Scenario_v8, !(ScenarioName=='SSP2base' & VariableID == 2))
Scenario_v8 <- subset(Scenario_v8, !(ScenarioName=='SSP3base' & VariableID == 2))

Scenario_v8 <- rbind.data.frame(Scenario_v8, SSP1_new,SSP2_new,SSP3_new)

library(stringr)
test <- subset(Scenario_v8, VariableID==2 & (ScenarioName=='SSP1base'|ScenarioName=='SSP2base'|ScenarioName=='SSP3base'))
test <- test[,c(2,8:68)] %>% unique()
test <- test %>% gather(Time, Value, -ScenarioName)
test$Year <- as.numeric(str_extract(test$Time, "[0-9]+"))

ggplot(test, aes(x=Year, y=Value, group=ScenarioName, color=ScenarioName))+geom_line(size=2)+
  theme_Publication()+scale_colour_Publication()

write.csv(Scenario_v8, 'ScenarioData_v8_global.csv',row.names = F)


################################### next we get the AIM input for China-specific ready ##############################
CityData <- read.csv('CityData.csv')

# replace base year population and income in City Data with my GDP and income
China <- subset(CityData, Country==142 | Country==184)

My_Pop_Inc <- merge(access.time, subset(GDP2,Year==2015)[,c('City','Population')],by='City',all.x=T)
My_Pop_Inc$Population <- My_Pop_Inc$Population*1000000
My_Pop_Inc <- merge(Income[,1:2], My_Pop_Inc[,c(1,8)], by='City',all.y=T)
My_Pop_Inc$Income <- My_Pop_Inc$Income*1000

# assign CityID to match with CityData
My_Pop_Inc <- merge(My_Pop_Inc, airport[,c('City','CityID')]%>%unique(), by='City', all.x = T)

# special city in China already changed in this csv, no need to update

# match with CityData for China in AIM
China <- merge(China, subset(My_Pop_Inc,!City=='Yichun'), by='CityID', all.x = T)
China$BaseYearPopulation <- ifelse(is.na(China$Population), China$BaseYearPopulation, China$Population)
China$BaseYearIncome <- ifelse(is.na(China$Income), China$BaseYearIncome, China$Income)

CityData <- rbind.data.frame(subset(CityData, !(Country==142| Country==184)), China[,1:47])
CityData$RunThis <- ifelse(CityData$Country==142| CityData$Country==184, 1, 0)

write.csv(CityData, 'CityData_AIM.csv', row.names = F)

################################# also update the CityData in v8 demand model, run both ##############################
CityData_v8 <- read.csv('CityData_v8.csv')

China_v8 <- China[,c('CityID','BaseYearPopulation','BaseYearIncome','Spec')]
names(China_v8) <- c('CityID', 'Population','Income','Special')
CityData_v8 <- merge(CityData_v8, China_v8, by='CityID', all.x = T)
CityData_v8$BaseYearPopulation <- ifelse(is.na(CityData_v8$Population), CityData_v8$BaseYearPopulation, CityData_v8$Population)
CityData_v8$BaseYearIncome <- ifelse(is.na(CityData_v8$Income), CityData_v8$BaseYearIncome, CityData_v8$Income)
CityData_v8$Spec <- ifelse(is.na(CityData_v8$Special), CityData_v8$Spec, CityData_v8$Special)
CityData_v8 <- CityData_v8[,1:23]
CityData_v8$RunThis <- ifelse(CityData_v8$Country==142| CityData_v8$Country==184, 1, 0)

write.csv(CityData_v8, 'CityData_AIM_v8.csv', row.names = F)


