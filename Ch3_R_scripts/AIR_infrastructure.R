setwd("D:/Aviation_HSR")
library(dplyr)
library(tidyr)
library(ggplot2)
source("E:/Dropbox/R/ggplot.R")

########################### Estimate Annual Total ATMs with/without HSR ##################################
# compute annual flights needed fot the projected dmenad on O-D and Airport level, by aircraft size class
# full sample from sabre and flight global for depart/arrive flights and pax at each airprot
load("D:/Aviation_HSR/FlightGlobal_All.RData")
load("D:/Aviation_HSR/Sabre_Leg_CHN.RData")

airport <- read.csv('airports_city_2015.csv')

# 2015 flight schedule data: fleet type by O-D, keep it fixed for the projection period
All_15$TotalSeats <- All_15$Seats.per.Operation*All_15$Depcount
Fleet <- All_15 %>% group_by(Origin..Airport, Destination.Airport,OriginCityID,DestCityID,Equipment)%>% 
         summarise(Departures=sum(Depcount),TotalSeats=sum(TotalSeats),
                   Flight.Time=sum(Flight.Duration..mins.*Depcount))
Fleet <- rename(Fleet, Origin.Airport=Origin..Airport)

# for all SA aircraft categories
SA_Category <- read.csv('SAFleetLookup_v2.csv')
Fleet <- merge(Fleet, SA_Category[,c('OAGCode','SACategory','MTOW_lb')], 
               by.x = 'Equipment', by.y = 'OAGCode', all.x = T)

############# O-D city pairs' total flts and avg LF by aircraft class in 2015 ############################
OD_flts <- Leg_CHN %>% group_by(OriginCityID, DestCityID) %>% summarise(OD_total_pax=sum(Passengers))
OD_flts <- merge(OD_flts, Fleet %>% group_by(OriginCityID,DestCityID)%>%
                 summarise(OD_Flts=sum(Departures),TotalSeats=sum(TotalSeats)),by=c('OriginCityID','DestCityID'))
OD_flts$OD_LF <- OD_flts$OD_total_pax/OD_flts$TotalSeats

# O-D flights by flight category
OD_flts_cate <- merge(OD_flts, Fleet %>% group_by(OriginCityID,DestCityID,SACategory)%>%
                      summarise(OD_flts_cate=sum(Departures),TotalSeats_cate=sum(TotalSeats),
                                FlightTime_cate=sum(Flight.Time),MTOW_lb_cate=round(mean(MTOW_lb))),
                      by=c('OriginCityID','DestCityID'))

# since we need to compuate LCI for each flight category, need flight cate share
OD_flts_cate <- OD_flts_cate %>% group_by(OriginCityID,DestCityID) %>% 
                mutate(Seats_share_cate=TotalSeats_cate/sum(TotalSeats_cate), 
                       Seats_per_flt_cate=round(TotalSeats_cate/OD_flts_cate))

############# Airports' total ATMs, passengers, and Pax/ATM by aircraft class in 2015 #####################
# calculate the total annual ATMs for all Chinese airports
# first, all departing flights
ATMs <- Fleet %>% group_by(Origin.Airport,OriginCityID,SACategory) %>% 
                  summarise(Flights=sum(Departures), TotalSeats=sum(TotalSeats), 
                            Flight.Time=sum(Flight.Time), MTOW_lb=round(mean(MTOW_lb)))
ATMs <- rename(ATMs, Airport=Origin.Airport, CityID=OriginCityID)

# then, all arrival flights
ATMs <- rbind.data.frame(ATMs,rename(Fleet, Airport=Destination.Airport,CityID=DestCityID) %>% 
                         group_by(Airport,CityID, SACategory)%>% 
                         summarise(Flights=sum(Departures), TotalSeats=sum(TotalSeats), 
                                   Flight.Time=sum(Flight.Time), MTOW_lb=round(mean(MTOW_lb))))

# combine departures and arrivals, and get total ATMs
ATMs <- ATMs %>% group_by(Airport,CityID, SACategory) %>% summarise(Annual_ATMs =sum(Flights), 
                 TotalSeats=sum(TotalSeats),Flight.Time=sum(Flight.Time), MTOW_lb=round(mean(MTOW_lb)))

ATMs <- merge(ATMs,ATMs%>%group_by(Airport)%>%summarise(Total_ATMs=sum(Annual_ATMs)),by='Airport',all.x=T)

# we need to distribute airport total pax to different aircraft sizes by their seats share
ATMs <- ATMs %>% group_by(Airport) %>% mutate(Seats_share = TotalSeats/sum(TotalSeats))

# Now, total terminal pax by airport in 2015
TerminalPax <- rbind.data.frame(rename(Leg_CHN,Airport=Origin.Airport) %>% 
                                  group_by(Airport) %>% summarise(Passengers=sum(Passengers)),
                                rename(Leg_CHN,Airport=Destination.Airport) %>% 
                                  group_by(Airport) %>% summarise(Passengers=sum(Passengers)))

TerminalPax <- TerminalPax %>% group_by(Airport) %>% summarise(Passengers=sum(Passengers))

# match annual total ATMs with total terminal passengers
ATMs <- merge(ATMs, TerminalPax, by='Airport', all.x = T)

# passengers per flight by aircraft size
ATMs$Pax_per_flt <- round(ATMs$Passengers*ATMs$Seats_share/ATMs$Annual_ATMs,1)

# match airport with country code
ATMs <- merge(airport[,c('Code','City','Country.code')], ATMs, by.x='Code',by.y='Airport',all.y = T)
ATMs <- rename(ATMs, Airport=Code)

# since the projected demand is for O-D cities, we need to distribute city-pair demand to city's airport
# need to work out the airport share if this city has more than one airport
ATMs <- merge(ATMs,ATMs[,c('Airport','CityID','Passengers')] %>% unique() %>% group_by(CityID) %>% 
                mutate(Airport_share = Passengers/sum(Passengers)),by=c('Airport','CityID','Passengers'), all.x = T)

# Keep all Chinese airports only, excluding HK, Macau
ATMs <- subset(ATMs, Country.code=='CN')

##################################### Total flights needed for the projected air demand #############################
# keep fixed fleet composition at airport: the pax share carried by each aircraft size remain constant
# Pax per flight over each aircraft size also remain constant
load("D:/Aviation_HSR/AIM_output_global.RData")
# now, we estimate the annual total ATMs required for the projected demand
city <- merge(futures_high_15[,c('Year','OriginCityID','DestCityID','SegmentDemand','Air_pred_15')],
              futures_high_25[,c('Year','OriginCityID','DestCityID','Air_pred_25')],
              by=c('Year','OriginCityID','DestCityID'), all.x=T)

# again it's for both depart and arrival flights/passengers
city <- rbind.data.frame(rename(city, CityID = OriginCityID)%>% group_by(Year,CityID)%>%
                                summarise(Demand_without = sum(SegmentDemand), Demand_with_15=sum(Air_pred_15),
                                          Demand_with_25 = sum(Air_pred_25)),
                         rename(city, CityID = DestCityID)%>% group_by(Year,CityID)%>%
                                summarise(Demand_without = sum(SegmentDemand), Demand_with_15=sum(Air_pred_15),
                                          Demand_with_25 = sum(Air_pred_25)))

city <- city %>% group_by(Year, CityID) %>% 
        summarise(City_demand_without = sum(Demand_without), City_demand_with_15=sum(Demand_with_15),
                  City_demand_with_25 = sum(Demand_with_25))

Airport <- merge(ATMs[,c('Airport','Airport_share','City','CityID','SACategory','Seats_share','Pax_per_flt')],
                 city, by.x='CityID',by.y='CityID', all.x = T)

Airport$Aprt_demand_without <- Airport$City_demand_without*Airport$Airport_share
Airport$Aprt_demand_with_15 <- Airport$City_demand_with_15*Airport$Airport_share
Airport$Aprt_demand_with_25 <- Airport$City_demand_with_25*Airport$Airport_share

Airport$Flts_cate_without <- round(Airport$Aprt_demand_without*Airport$Seats_share/Airport$Pax_per_flt)
Airport$Flts_cate_with_15 <- round(Airport$Aprt_demand_with_15*Airport$Seats_share/Airport$Pax_per_flt)
Airport$Flts_cate_with_25 <- round(Airport$Aprt_demand_with_25*Airport$Seats_share/Airport$Pax_per_flt)

# plot total airport movements (thousands) of China 2015-2050
Airport_ATM <- Airport %>% group_by(Year,Airport) %>% 
               summarise(Flts_without=sum(Flts_cate_without)/1000,Flts_with_15=sum(Flts_cate_with_15)/1000,
                         Flts_with_25 = sum(Flts_cate_with_25)/1000)

##################################### Capacity needed for the total flights projected ################################
# we need to match the airport capacity in 2015 and see when it will be full, and need extra capacity for each airport
# assumpotion on evolution of aircraft size used: justify China will keep med-size from Xi's recent purchase of A320s
airport <- read.csv('airports_city_2015.csv')
# steps:
# 1) declared capacity of all Chinese airports, hourly movements
# 2) total annual capacity = ATM/h * 17h/day * 365
# 3) average pax per ATM
# 4) total demand for the full capacity = average pax/ATM * total annual capacity
# 5) compare this total demand to the projected airport demand, in which year it's full

Airport_ATM <- merge(Airport_ATM, airport[,c('Code','Capacity_hr','Nrunways','LongestRunway_m')],
                     by.x='Airport',by.y='Code',all.x = T)
Airport_ATM$Annual_capacity <- (Airport_ATM$Capacity_hr * 17 * 365)/1000

###################### Compare projected ATMs with Existing Capacity ############################################
# by comparing predicted ATMs with/without, we know from which year each airport's ATMs will surpass its capacity
# for the years after, we assume that extra capacity are required from:

# add new runways at the existing airport
Airport_ATM$capacity_hr_per_runway <- Airport_ATM$Capacity_hr/Airport_ATM$Nrunways

# new runayws needed for each airport without HSR
Extra_without <- NULL

for(i in levels(factor(Airport_ATM$Airport))){
  df <- subset(Airport_ATM,Airport==i)
  df <- df[,c('Year','Airport','Flts_without','capacity_hr_per_runway','Nrunways')]%>% arrange(Year)
  df$Total_capacity <- df$capacity_hr_per_runway*df$Nrunways*17*365/1000
  
  for(j in 1:length(df$Year)){
    df$Nrunways <- ifelse(df$Flts_without>=df$Total_capacity, df$Nrunways[j]+1, df$Nrunways)
    df$Total_capacity <- df$capacity_hr_per_runway*df$Nrunways*17*365/1000
  }
  Extra_without <- rbind.data.frame(Extra_without, df)  
}

Extra_without <- rename(Extra_without, Nrunways_without=Nrunways, Total_capacity_without=Total_capacity)

# new runways needed with 2015 HSR
Extra_with_15 <- NULL

for(i in levels(factor(Airport_ATM$Airport))){
  df <- subset(Airport_ATM,Airport==i)
  df <- df[,c('Year','Airport','Flts_with_15','capacity_hr_per_runway','Nrunways')]%>% arrange(Year)
  df$Total_capacity <- df$capacity_hr_per_runway*df$Nrunways*17*365/1000
  
  for(j in 1:length(df$Year)){
    df$Nrunways <- ifelse(df$Flts_with_15>=df$Total_capacity, df$Nrunways[j]+1, df$Nrunways)
    df$Total_capacity <- df$capacity_hr_per_runway*df$Nrunways*17*365/1000
  }
  Extra_with_15 <- rbind.data.frame(Extra_with_15, df)  
}

Extra_with_15 <- rename(Extra_with_15, Nrunways_with_15=Nrunways, Total_capacity_with_15=Total_capacity)

# new runways needed with 2025 HSR
Extra_with_25 <- NULL

for(i in levels(factor(Airport_ATM$Airport))){
  df <- subset(Airport_ATM,Airport==i)
  df <- df[,c('Year','Airport','Flts_with_25','capacity_hr_per_runway','Nrunways')]%>% arrange(Year)
  df$Total_capacity <- df$capacity_hr_per_runway*df$Nrunways*17*365/1000
  
  for(j in 1:length(df$Year)){
    df$Nrunways <- ifelse(df$Flts_with_25>=df$Total_capacity, df$Nrunways[j]+1, df$Nrunways)
    df$Total_capacity <- df$capacity_hr_per_runway*df$Nrunways*17*365/1000
  }
  Extra_with_25 <- rbind.data.frame(Extra_with_25, df)  
}

Extra_with_25 <- rename(Extra_with_25, Nrunways_with_25=Nrunways, Total_capacity_with_25=Total_capacity)

# some annual flts may drop to below inital capacity, fix it
Extra_with_25 <- Extra_with_25 %>% group_by(Airport) %>% mutate(prev_Nrunway = lag(Nrunways_with_25),prev_cap=lag(Total_capacity_with_25))
Extra_with_25$prev_Nrunway <- ifelse(is.na(Extra_with_25$prev_Nrunway),
                                     Extra_with_25$Nrunways_with_25, Extra_with_25$prev_Nrunway)
Extra_with_25$prev_cap <- ifelse(is.na(Extra_with_25$prev_cap),
                                     Extra_with_25$Total_capacity_with_25, Extra_with_25$prev_cap)
Extra_with_25$Nrunways_with_25 <- ifelse(Extra_with_25$prev_Nrunway > Extra_with_25$Nrunways_with_25, 
                                         Extra_with_25$prev_Nrunway, Extra_with_25$Nrunways_with_25)
Extra_with_25$Total_capacity_with_25 <- ifelse(Extra_with_25$prev_cap > Extra_with_25$Total_capacity_with_25,
                                               Extra_with_25$prev_cap, Extra_with_25$Total_capacity_with_25)


################## combine extra capacities need for each airport with AND without HSR ###############################
Airport_ATM <- merge(Airport_ATM, 
                     Extra_without[,c('Year','Airport','Nrunways_without','Total_capacity_without')],
                     by=c('Year','Airport'), all.x = T)

Airport_ATM <- merge(Airport_ATM, Extra_with_15[,c('Year','Airport','Nrunways_with_15','Total_capacity_with_15')],
                     by=c('Year','Airport'), all.x = T)

Airport_ATM <- merge(Airport_ATM, Extra_with_25[,c('Year','Airport','Nrunways_with_25','Total_capacity_with_25')],
                     by=c('Year','Airport'), all.x = T)

# Plot Janic(2004) figure 3: 
Aprt_capacity <- merge(rename(Airport_ATM[,c('Year','Airport','Flts_without','Flts_with_15','Flts_with_25')],
                              without_HSR=Flts_without,with_HSR_15=Flts_with_15,with_HSR_25=Flts_with_25)%>% 
                              gather(Type, total_ATMs, -Year,-Airport),
                       rename(Airport_ATM[,c('Year','Airport','Total_capacity_without','Total_capacity_with_15',
                                             'Total_capacity_with_25')],
                              without_HSR=Total_capacity_without,with_HSR_15=Total_capacity_with_15,
                              with_HSR_25=Total_capacity_with_25)%>% 
                              gather(Type, total_Cap, -Year,-Airport),by=c('Year','Airport','Type'),all.x = T)

Aprt_capacity <- merge(Aprt_capacity,rename(Airport_ATM[,c('Year','Airport','Nrunways_without','Nrunways_with_15','Nrunways_with_25')],
                       without_HSR=Nrunways_without,with_HSR_15=Nrunways_with_15,with_HSR_25=Nrunways_with_25)%>% 
                       gather(Type, Nrunways, -Year,-Airport),by=c('Year','Airport','Type'),all.x = T)

# add Nrunway labels
Aprt_capacity <- Aprt_capacity %>% group_by(Airport,Type) %>% mutate(label=lag(Nrunways))
Aprt_capacity$label <- ifelse(Aprt_capacity$Nrunways==Aprt_capacity$label,'',paste('Nrunways=',Aprt_capacity$Nrunways,sep=''))
Aprt_capacity$label <- ifelse(Aprt_capacity$Year==2015, paste('Nrunways=',Aprt_capacity$Nrunways,sep=''), Aprt_capacity$label)

ggplot(data=subset(Aprt_capacity,Airport=='PEK' & Type=='with_HSR_25'))+ 
  geom_line(aes(x=Year,y=total_ATMs),size=1.5,color='#2E9FDF')+
  geom_line(aes(x=Year,y=total_Cap),linetype='solid',colour='black',size=1)+
  geom_text(aes(x=Year, y=total_Cap,label=label),vjust=-0.5, color="black",position = position_dodge(0.8), size=4)+
  scale_y_continuous(limits = c(0,2500))+
  labs(y= 'Annual Total ATMs (thousands)')+ ggtitle('Total Capacity Required at the PEK Airport to Meet Projected ATM Growth \n(with 2025 HSR Network)')+
  theme_Publication()+scale_colour_Publication()


###################### Now we can plot the timing that Chinese airports reach to its capacity ##########################
Airport_ATM$CUI_without <- Airport_ATM$Flts_without/Airport_ATM$Annual_capacity
Airport_ATM$CUI_with_15 <- Airport_ATM$Flts_with_15/Airport_ATM$Annual_capacity
Airport_ATM$CUI_with_25 <- Airport_ATM$Flts_with_25/Airport_ATM$Annual_capacity

# find out the year since which the airport reach to its full capacity
Airport_ATM <- rbind.data.frame(subset(Airport_ATM,CUI_without>=1)%>%group_by(Airport)%>%mutate(Full_without=row_number()),
                                    subset(Airport_ATM,CUI_without<1)%>%group_by(Airport)%>%mutate(Full_without=row_number()))

Airport_ATM <- rbind.data.frame(subset(Airport_ATM,CUI_with_15>=1)%>%group_by(Airport)%>%mutate(Full_with_15=row_number()),
                                    subset(Airport_ATM,CUI_with_15<1)%>%group_by(Airport)%>%mutate(Full_with_15=row_number()))

Airport_ATM <- rbind.data.frame(subset(Airport_ATM,CUI_with_25>=1)%>%group_by(Airport)%>%mutate(Full_with_25=row_number()),
                                subset(Airport_ATM,CUI_with_25<1)%>%group_by(Airport)%>%mutate(Full_with_25=row_number()))


library(plotly)

# How many airports in China will reach to its full capacity by 2050 without HSR, and when
plot_ly(subset(Airport_ATM,CUI_without>=1 & Full_without==1), 
        x = ~Year, y = ~Annual_capacity, type="scatter", 
        text = ~Airport, textposition = 'middle right',
        textfont = list(color='#000000',size = 12),mode="text+markers", 
        color = ~CUI_without, size = ~Capacity_hr,colors=c('blue','red')) %>%
  layout(title = '<b>Projected Year of Chinese Airports Reaching to Capacity \n (without HSR)<b>',
         legend = list(orientation = 'h', x = 0.9, y = 0.9),
         xaxis = dict(list(range=c(2010,2050)),tickfont=dict('bold')),
         yaxis = list(title = '<b>Airport Total Annual Capacity (thousands)<b>'))

# year when airport each to its full capacity with HSR
plot_ly(subset(Airport_ATM,CUI_with_25>=1 & Full_with_25==1), 
        x = ~Year, y = ~Annual_capacity, type="scatter", 
        text = ~Airport, textposition = 'middle right',
        textfont = list(color = '#000000',size=12), mode = "text+markers", 
        color = ~CUI_with_15, size = ~Capacity_hr, colors=c('blue','red'))%>%
  layout(title = '<b>Projected Year of Chinese Airports Reaching to Capacity \n (with 2025 HSR)<b>',
         legend = list(orientation = 'h', x = 0.9, y = 0.9, title='Utilisation'),
         xaxis = list(range=c(2010,2050)),
         yaxis = list(title = '<b>Airport Total Annual Capacity (thousands)<b>'))

################################ Total flights by O-D and by aircraft size with / without ##################################
# Unlike airport's capacity which needs domestic + international, O-D only needs domestic 
# flights without HSR are directly obtained from AIM projection
load("D:/Aviation_HSR/AIM_output_dom.RData")

# routes with just air transport have missing flight time
OD_ATM <- merge(futures_high_15[,c('Year','OriginCityID','DestCityID','Air_time_hrs','SegmentDemand','Air_pred_15')],
                futures_high_25[,c('Year','OriginCityID','DestCityID','Air_pred_25')],
                by=c('Year','OriginCityID','DestCityID'), all.x=T)

missing_time <- subset(OD_ATM, is.na(Air_time_hrs))[,c('Year','OriginCityID','DestCityID','SegmentDemand','Air_pred_15','Air_pred_25')]

load("D:/Aviation_HSR/CHN_All.RData")
missing_time <- merge(missing_time, subset(China,Nlegs==1) %>% group_by(Origin.CityID,Dest.CityID) %>% 
                      summarise(Air_time_hrs=round(mean(Time)/60,2)),by.x=c('OriginCityID','DestCityID'),
                      by.y=c('Origin.CityID','Dest.CityID'),all.x=T)

OD_ATM <- rbind.data.frame(subset(OD_ATM, !is.na(Air_time_hrs)),
                           missing_time[,c('Year','OriginCityID','DestCityID','Air_time_hrs','SegmentDemand','Air_pred_15','Air_pred_25')])

OD_ATM <- subset(drop_na(OD_ATM), !OriginCityID==DestCityID)

# fixed O-D fleet composition
OD_ATM <- merge(OD_ATM, OD_flts_cate[,c('OriginCityID','DestCityID','OD_LF','SACategory','MTOW_lb_cate',
                'Seats_share_cate','Seats_per_flt_cate')],by=c('OriginCityID','DestCityID'),all.x=T)

OD_ATM$OD_LF <- ifelse(OD_ATM$OD_LF>1, 1, OD_ATM$OD_LF)
OD_ATM <- drop_na(OD_ATM)

# total seats = demand/Avg LF
OD_ATM$OD_seats_without <- OD_ATM$SegmentDemand/OD_ATM$OD_LF
OD_ATM$OD_seats_with_15 <- OD_ATM$Air_pred_15/OD_ATM$OD_LF
OD_ATM$OD_seats_with_25 <- OD_ATM$Air_pred_25/OD_ATM$OD_LF

# total flts = total seats/seats per flt
OD_ATM$OD_Flts_cate_without <- round(OD_ATM$OD_seats_without*OD_ATM$Seats_share_cate/OD_ATM$Seats_per_flt_cate)
OD_ATM$OD_Flts_cate_with_15 <- round(OD_ATM$OD_seats_with_15*OD_ATM$Seats_share_cate/OD_ATM$Seats_per_flt_cate)
OD_ATM$OD_Flts_cate_with_25 <- round(OD_ATM$OD_seats_with_25*OD_ATM$Seats_share_cate/OD_ATM$Seats_per_flt_cate)

# average aircraft utilisation by aircraft type
Utilisation <- cbind.data.frame(SACategory = c('-1','0','1','2','3','4','5','6','7','8'), 
                                RepAircraft = c('Canadair RJ-700','Canadair RJ-700','Embraer 190','Airbus A319','Airbus A320-100/200',
                                                'Boeing 737-800','B787-800 Dreamliner','Airbus A330-300','Boeing 777-300', 'Boeing 747-400'),
                                Avg.Utilisation = c(0.26498,0.26498,0.28518,0.33089,0.36682,0.36232,0.47918,0.46943,0.50285,0.53568))

OD_ATM <- merge(OD_ATM, Utilisation, by='SACategory',all.x=T)

# total flight hours = route flight time * flight freq
OD_ATM$FHs_cate_without <- OD_ATM$Air_time_hrs * OD_ATM$OD_Flts_cate_without
OD_ATM$FHs_cate_with_15 <- OD_ATM$Air_time_hrs * OD_ATM$OD_Flts_cate_with_15
OD_ATM$FHs_cate_with_25 <- OD_ATM$Air_time_hrs * OD_ATM$OD_Flts_cate_with_25

# total number of aircraft = (FHs/avg utilisation)/(365*24)
OD_ATM$aircraft_without <- (OD_ATM$FHs_cate_without/OD_ATM$Avg.Utilisation)/(365*24)
OD_ATM$aircraft_with_15 <- (OD_ATM$FHs_cate_with_15/OD_ATM$Avg.Utilisation)/(365*24)
OD_ATM$aircraft_with_25 <- (OD_ATM$FHs_cate_with_25/OD_ATM$Avg.Utilisation)/(365*24)

# total number of aircraft by size class for each year
OD_ATM <- OD_ATM %>% group_by(Year, SACategory) %>% 
          summarise(aircraft_without = round(sum(aircraft_without)), aircraft_with_15=round(sum(aircraft_with_15)),
                    aircraft_with_25=round(sum(aircraft_with_25)),FHs_without=sum(FHs_cate_without),
                    FHs_with_15=sum(FHs_cate_with_15), FHs_with_25=sum(FHs_cate_with_25))

# take average weight of aircrfat by class class
OD_ATM <- merge(OD_ATM, SA_Category %>% group_by(SACategory) %>% summarise(MTOW_lb=round(mean(MTOW_lb))),by='SACategory',all.x=T)







