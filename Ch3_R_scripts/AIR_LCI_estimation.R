setwd("D:/Aviation_HSR")
library(dplyr)
library(tidyr)
library(ggplot2)
source("E:/Dropbox/R/ggplot.R")

###################################### Operational Emissions from Air Transport ##########################################
load("D:/Aviation_HSR/AIM_output_dom.RData")
futures <- futures_low_25
summary <- summary_low

Air_LCI <- cbind.data.frame(Scenario='Low Growth',Type = c('Without_HSR','HSR_2015','HSR_2025'),
                            Air_Operation_MtCO2 = c(sum(summary$CO2_emissions_without_HSR),
                                                    sum(summary$CO2_emissions_with_HSR_15),
                                                    sum(summary$CO2_emissions_with_HSR_25)))


###################################### Emissions from Extra Airport Capacity Needed #######################################
# under no HSR, air would need the most capacity expansion
# 1) Runway construction emissions
New_capacity <- Airport_ATM[,c('Year','Airport','Nrunways','LongestRunway_m','Nrunways_without','Nrunways_with_15','Nrunways_with_25')]%>%
                gather(Type, Extra_Nrunways, -Airport,-Year,-Nrunways,-LongestRunway_m)

New_capacity <- New_capacity %>% arrange(Year) %>% group_by(Airport,Type,Nrunways,LongestRunway_m) %>% 
                summarise(New_runways=max(Extra_Nrunways))
New_capacity$New_runways <- New_capacity$New_runways - New_capacity$Nrunways 

# assuming all runways width = 60m
# 1m2 = 10.76391 ft2
# Runway Emissions Factor: 10kgCO2/ft2
New_capacity <- New_capacity %>% mutate(Runway_m2=60*LongestRunway_m*New_runways,
                                        Runway_const_MtCO2 = 10*10.76391*Runway_m2/100000000)

New_capacity$Type <- factor(New_capacity$Type, levels=c('Nrunways_without','Nrunways_with_15','Nrunways_with_25'))
Air_LCI <- cbind.data.frame(Air_LCI, (New_capacity %>% arrange(Type) %>% group_by(Type) %>% 
                                      summarise(Runway_Const_MtCO2=sum(Runway_const_MtCO2)))[,2])

# 2) Airport area construction emissions
# figure out associated expansion on airport space area, parking spaces, and taxiways/tarmacs per m2 of runway
PEK <- cbind.data.frame(IATACode = 'PEK', TotalArea=1410000, TotalRunwayLength=10800, RunwayWidth=60,
                        TarmacArea = 860000, ParkingSpaces=9430, ElectricityUse=19108470)

PEK$Aprt_per_m2_runway <- PEK$TotalArea/(PEK$RunwayWidth*PEK$TotalRunwayLength)
PEK$Tarmac_per_m2_runway <- PEK$TarmacArea/(PEK$RunwayWidth*PEK$TotalRunwayLength)
PEK$ParkingArea <- round(PEK$ParkingSpaces*(300/10.76391)*(1+0.1))
PEK$Parking_per_m2_runway <- PEK$ParkingArea/(PEK$RunwayWidth*PEK$TotalRunwayLength)
PEK$ElecUse_per_m2_runway <- PEK$ElectricityUse/(PEK$RunwayWidth*PEK$TotalRunwayLength)

# Airport: 43kgCO2/ft2
New_capacity <- New_capacity %>% mutate(Airport_cosnt_MtCO2 = 43*10.76391*(PEK$Aprt_per_m2_runway*Runway_m2)/100000000)

Air_LCI <- cbind.data.frame(Air_LCI, (New_capacity %>% arrange(Type) %>% group_by(Type) %>% 
                                      summarise(Airport_Const_MtCO2=sum(Airport_cosnt_MtCO2)))[,2])

# 3) Tarmac area construction emissions: 6.8kgCO2/ft2
New_capacity <- New_capacity %>% mutate(Tarmac_cosnt_MtCO2 = 6.8*10.76391*(PEK$Tarmac_per_m2_runway*Runway_m2)/100000000)

Air_LCI <- cbind.data.frame(Air_LCI, (New_capacity %>% arrange(Type) %>% group_by(Type) %>% 
                                      summarise(Tarmac_Cosnt_MtCO2=sum(Tarmac_cosnt_MtCO2)))[,2])

# 4) Parking space construction emissions: 2.2kgCO2/ft2
New_capacity <- New_capacity %>% mutate(Parking_cosnt_MtCO2 = 2.2*10.76391*(PEK$Parking_per_m2_runway*Runway_m2)/100000000)

Air_LCI <- cbind.data.frame(Air_LCI, (New_capacity %>% arrange(Type) %>% group_by(Type) %>% 
                                      summarise(Parking_Cosnt_MtCO2=sum(Parking_cosnt_MtCO2)))[,2])

###################################### Emissions from Airport Operation #############################################
# airport operation mainly electricity consumption, should include both eixsting and new capacity
# Electriity Use emissions: combine with yearly emissions factor
Airport_Operation <- Airport_ATM[,c('Year','Airport','LongestRunway_m','Nrunways_without','Nrunways_with_15','Nrunways_with_25')]
Airport_Operation$ElecUse_per_m2_runway <- PEK$ElecUse_per_m2_runway
Airport_Operation <- Airport_Operation %>% 
                     mutate(Total_Elect_without=Nrunways_without*LongestRunway_m*60*ElecUse_per_m2_runway,
                            Total_Elect_with_15=Nrunways_with_15*LongestRunway_m*60*ElecUse_per_m2_runway,
                            Total_Elect_with_25=Nrunways_with_25*LongestRunway_m*60*ElecUse_per_m2_runway)
Airport_Operation <- merge(Airport_Operation, emission_factor[,c('Year','EF')], by='Year',all.x=T)
  
Airport_Operation <- Airport_Operation %>% 
                     mutate(Infra_Op_MtCO2_without = Total_Elect_without*(EF/100000)/1000000,
                            Infra_Op_MtCO2_with_15 = Total_Elect_with_15*(EF/100000)/1000000,
                            Infra_Op_MtCO2_with_25 = Total_Elect_with_25*(EF/100000)/1000000)

Airport_Operation <- Airport_Operation %>% 
                     summarise(Without_HSR=sum(Infra_Op_MtCO2_without),
                               HSR_2015=sum(Infra_Op_MtCO2_with_15),
                               HSR_2025=sum(Infra_Op_MtCO2_with_25))%>% gather(Type,Infra_Op_MtCO2)

Air_LCI <- merge(Air_LCI, Airport_Operation, by='Type',all.x=T)
Air_LCI$Type <- factor(Air_LCI$Type, levels=c('Without_HSR','HSR_2015','HSR_2025'))

################################ Emissions from Infrastructure Maintenance ##########################################
# 5% of the total construction emissions: including both new and existing airports
Airport_Maint <- rename(Airport_ATM[,c('Year','Airport','LongestRunway_m','Nrunways_without','Nrunways_with_15','Nrunways_with_25')],
                        Without_HSR=Nrunways_without,HSR_2015=Nrunways_with_15,HSR_2025=Nrunways_with_25)
Airport_Maint <- Airport_Maint %>% gather(Type,Nrunways, -Year,-Airport,-LongestRunway_m)

# estimating the existing airport areas, tarmac and taxiways,and parking area based on m2 of runway
Airport_Maint <- Airport_Maint %>% mutate(Runway_m2=Nrunways*LongestRunway_m*60,Airport_m2=2.2*Runway_m2,
                                          Tarmac_m2=1.3*Runway_m2,Parking_m2=0.4*Runway_m2)

# maintanence emissions
Airport_Maint <- Airport_Maint %>% mutate(Runway_Maint_MtCO2= 0.05*(10*10.76391*Runway_m2)/1000000000,
                                          Airport_Maint_MtCO2= 0.05*(43*10.76391*Runway_m2)/1000000000,
                                          Tarmac_Maint_MtCO2= 0.05*(6.8*10.76391*Runway_m2)/1000000000,
                                          Parking_Maint_MtCO2= 0.05*(2.2*10.76391*Runway_m2)/1000000000)

Airport_Maint <- Airport_Maint %>% group_by(Type) %>% summarise(Runway_Maint_MtCO2=sum(Runway_Maint_MtCO2),
                                                                Airport_Maint_MtCO2=sum(Airport_Maint_MtCO2),
                                                                Tarmac_Maint_MtCO2=sum(Tarmac_Maint_MtCO2),
                                                                Parking_Maint_MtCO2=sum(Parking_Maint_MtCO2))

Air_LCI <- merge(Air_LCI, Airport_Maint, by='Type',all.x=T)
Air_LCI$Type <- factor(Air_LCI$Type, levels=c('Without_HSR','HSR_2015','HSR_2025'))
Air_LCI <- Air_LCI %>% arrange(Type)

######################################## Chester Aircraft Emissions Factor ############################################
#aircraft weight from Chestet is not accurate, use weight from SA_Category for Embraer 145 and Boeing 737-800
# emissoins factor of airframe manu isn't correct, use Table 72-74 per Aircraft-life total emissions for airframe manu and enigne manu
# Small: total GHG for engine manu=1800 tCO2, total GHG for aircraft manu = 5100 tCO2
# Med: total GHG for engine manu=3300 tCO2, total GHG for aircraft manu = 17000 tCO2; 
# Large: total GHG for engine manu=11000 tCO2, total GHG for aircraft manu = 52000 tCO2

Chester <- cbind.data.frame(Class = c('Small','Med','Large'),
                            Wights_lbs = c(38501, 98800, 337900), Engines=c(2,2,4), Total_engine_manu_tCO2=c(1800,3300,11000),
                            Total_airframe_manu_tCO2=c(5100,17000,52000))

# tCO2 per lbs for manufacturing
Chester$Airft_tCO2_lbs <- Chester$Total_airframe_manu_tCO2/Chester$Wights_lbs
Chester$Engine_tCO2_lbs <- Chester$Total_engine_manu_tCO2/Chester$Wights_lbs

########################################### Aircraft emissions #########################################################
# under no HSR, air would need the most new aircraft
New_aircraft <- OD_ATM[,c('Year','SACategory','MTOW_lb','aircraft_without','aircraft_with_15','aircraft_with_25')] %>% 
                arrange(Year) %>% gather(Type, Aircraft,-SACategory,-Year,-MTOW_lb)
New_aircraft <- New_aircraft %>% arrange(Year) %>% group_by(SACategory,MTOW_lb,Type)%>%mutate(prev_aircraft=lag(Aircraft))
New_aircraft$prev_aircraft <- ifelse(New_aircraft$Year==2015, New_aircraft$Aircraft, New_aircraft$prev_aircraft)
New_aircraft$New_fleet <- New_aircraft$Aircraft - New_aircraft$prev_aircraft
New_aircraft <- subset(New_aircraft,New_fleet>0) %>% group_by(SACategory,MTOW_lb,Type)%>%summarise(New_fleet=sum(New_fleet))

# 1) emissions from manufacturing: based on emissions per lbs of aircraft
New_aircraft$Manu_tCO2_plane <- New_aircraft$MTOW_lb*mean(Chester$Airft_tCO2_lbs)
New_aircraft$Manu_tCO2_engine <- New_aircraft$MTOW_lb*mean(Chester$Engine_tCO2_lbs)

New_aircraft$Airframe_Manu_MtCO2 <- (New_aircraft$New_fleet*New_aircraft$Manu_tCO2_plane)/1000000
New_aircraft$Engine_Manu_MtCO2 <- (New_aircraft$New_fleet*New_aircraft$Manu_tCO2_engine)/1000000

# airframe manufacturing emissions
New_aircraft$Type <- factor(New_aircraft$Type, levels=c('aircraft_without','aircraft_with_15','aircraft_with_25'))
Air_LCI <- cbind.data.frame(Air_LCI, (New_aircraft %>% arrange(Type) %>% group_by(Type) %>% 
                                      summarise(Airframe_Manu_MtCO2=sum(Airframe_Manu_MtCO2)))[,2])

# engine manufacturing emissions
Air_LCI <- cbind.data.frame(Air_LCI, (New_aircraft  %>% arrange(Type) %>% group_by(Type) %>% 
                                      summarise(Engine_Manu_MtCO2=sum(Engine_Manu_MtCO2)))[,2])

# 2) emissions from aircraft maintanence 
# Emissoins from Maintenance are based on $material cost per flight hour, Table 69
Chester$Airframe_maint_costs_FH <- c(28,110,220)
Chester$Engine_maint_costs_FH <- c(10,61,640)
Chester$Airft_maint_cost_per_lbs <- Chester$Airframe_maint_costs_FH/Chester$Wights_lbs 
Chester$Engine_maint_cost_per_lbs <- Chester$Engine_maint_costs_FH/Chester$Wights_lbs 

# should be the total maintenance cost for total FHs.
Aircraft_maint <- OD_ATM[,c('Year','SACategory','MTOW_lb','FHs_without','FHs_with_15','FHs_with_25')] %>% 
                  arrange(Year) %>% gather(Type,Total_FHs, -SACategory,-MTOW_lb,-Year)

Aircraft_maint <- Aircraft_maint %>% group_by(SACategory,MTOW_lb,Type) %>% summarise(Total_FHs=sum(Total_FHs))

# average airframe/engine material costs per lbs per FH
Aircraft_maint$Airft_maint_cost_per_lbs <- mean(Chester$Airft_maint_cost_per_lbs)
Aircraft_maint$Engine_maint_cost_per_lbs <- mean(Chester$Engine_maint_cost_per_lbs)

# total material costs (in millions) for each aircraft size class
Aircraft_maint$Airft_maint_Mcost <- Aircraft_maint$MTOW_lb*Aircraft_maint$Airft_maint_cost_per_lbs*Aircraft_maint$Total_FHs/1000000
Aircraft_maint$Engine_maint_Mcost <- Aircraft_maint$MTOW_lb*Aircraft_maint$Engine_maint_cost_per_lbs*Aircraft_maint$Total_FHs/1000000

# Airframe maintenance emissions: 1762tCO2/$million, table 82
Aircraft_maint$Airframe_Maint_MtCO2 <- Aircraft_maint$Airft_maint_Mcost*1762/10000000

# Engine maintenance emissions: 411tCO2/$million
Aircraft_maint$Engine_Maint_MtCO2 <- Aircraft_maint$Engine_maint_Mcost*411/10000000

Aircraft_maint$Type <- factor(Aircraft_maint$Type, levels=c('FHs_without','FHs_with_15','FHs_with_25'))
Air_LCI <- cbind.data.frame(Air_LCI, (Aircraft_maint %>% arrange(Type) %>% group_by(Type) %>% 
                                      summarise(Airframe_Maint_MtCO2=sum(Airframe_Maint_MtCO2)))[,2])

Air_LCI <- cbind.data.frame(Air_LCI, (Aircraft_maint %>% arrange(Type) %>% group_by(Type) %>% 
                                      summarise(Engine_Maint_MtCO2=sum(Engine_Maint_MtCO2)))[,2])


###################################### Emissions of Jet Fuel Production ##########################################
# emissions from Jet fuel production: 
Fuel_production <- merge(futures_low_15[,c('Year','OriginCityID','DestCityID','FuelBurn_kg','Fuel_kg_15')],
                         futures_low_25[,c('Year','OriginCityID','DestCityID','Fuel_kg_25')],
                         by=c('Year','OriginCityID','DestCityID'), all.x=T)

Fuel_production <- Fuel_production %>% summarise(Fuel_without_Mt=sum(FuelBurn_kg/1000000000), 
                                                 Fuel_with_15_Mt=sum(Fuel_kg_15/1000000000),
                                                 Fuel_with_25_Mt=sum(Fuel_kg_25/1000000000))

Fuel_production <- Fuel_production %>% gather(Type, FuelBurn_Mt)

# 1) crude oil recoverage: removing oil sands from underground and treating oil sands to crude oil in the oil fields 
# emissions from crude recovery: efficiency at 98 percent
Fuel_production$recovery_efficiency <- 0.98

# 2) petroleum refining: from crude oil to jet fuel
# refining efficiency: 91.1 percent
Fuel_production$refining_efficiency <- 0.911

Fuel_production$CrudeOil_Mt <- (Fuel_production$FuelBurn_Mt/Fuel_production$refining_efficiency)
# 1 barrel crude oil weighs = 139.908821536 kilograms
Fuel_production$CrudeOil_barrel <- Fuel_production$CrudeOil_Mt/(139.91/1000000000)
# 1 barrel crude oil = 5.5513652248856 MMBtu
Fuel_production$CrudeOil_mmBtu <- 5.55*Fuel_production$CrudeOil_barrel

# From the GREET model: WTP (well to pump) emissions of producing conventional jet fuel from conventional crude oil 
# is 12185gCO2 per mmBtu of crude oil
Fuel_production$WTP_MtCO2 <- Fuel_production$CrudeOil_mmBtu*(12.185/1000000000)

# this WTP emissions are 17% of the operational emissions, checked
Fuel_production$Type <- factor(Fuel_production$Type, levels=c('Fuel_without_Mt','Fuel_with_15_Mt','Fuel_with_25_Mt'))
Air_LCI <- cbind.data.frame(Air_LCI, (Fuel_production %>% arrange(Type) %>% group_by(Type) %>% 
                                      summarise(JetFuel_WTP_MtCO2=sum(WTP_MtCO2)))[,2])

#Air_LCI_all <- Air_LCI
Air_LCI_all <- rbind.data.frame(Air_LCI_all, Air_LCI)

################################################################################################
# Think how to best show the results in a waterfall chart
# first, a bar chart showing proportion of different LCI components under HSR15 and HSR25
LCI_All <- Air_LCI_all %>% gather(Components, Inventory, -Type, -Scenario)
LCI_All$Components <- as.factor(LCI_All$Components)

levels(LCI_All$Components) <- list('V,Operation'='Air_Operation_MtCO2','V,Manufacturing'='Airframe_Manu_MtCO2',
                                   'V,Manufacturing'='Engine_Manu_MtCO2','V,Maintenance'='Airframe_Maint_MtCO2',
                                   'V,Maintenance'='Engine_Maint_MtCO2','I,Construction(Runways)'='Runway_Const_MtCO2',
                                   'I,Construction(Airports)'='Airport_Const_MtCO2','I,Construction(Parking)'='Parking_Cosnt_MtCO2',
                                   'I,Construction(Tarmacs/Taxiways)'='Tarmac_Cosnt_MtCO2',
                                   'I,Maintenance'='Runway_Maint_MtCO2','I,Maintenance'='Airport_Maint_MtCO2',
                                   'I,Maintenance'='Tarmac_Maint_MtCO2','I,Maintenance'='Parking_Maint_MtCO2',
                                   'I,Operation'='Infra_Op_MtCO2','F,Production'='JetFuel_WTP_MtCO2')

LCI_All <- LCI_All %>% group_by(Type,Scenario,Components) %>% summarise(Inventory=sum(Inventory))

# compute the percentage of each component to the total LCI, compare with Chester
LCI_All <- LCI_All %>% group_by(Type, Scenario) %>% mutate(Percent = round(Inventory/sum(Inventory),3))


LCI_All$Components <- factor(LCI_All$Components, 
                             levels=c('V,Operation','V,Manufacturing','V,Maintenance','I,Construction(Runways)','I,Construction(Airports)',
                                      'I,Construction(Tarmacs/Taxiways)','I,Construction(Parking)',
                                      'I,Maintenance','I,Operation','F,Production'))

LCI_All$Group <- paste(LCI_All$Scenario, LCI_All$Type, sep = '-')

######################################################
LCI_All$Group <- as.factor(LCI_All$Group)
LCI_All$Group <- factor(LCI_All$Group, levels=c('Low Growth-HSR_2025','Low Growth-HSR_2015','Low Growth-Without_HSR',
                                                'Central Growth-HSR_2025','Central Growth-HSR_2015','Central Growth-Without_HSR',
                                                'High Growth-HSR_2025','High Growth-HSR_2015','High Growth-Without_HSR'))

levels(LCI_All$Group) <- list('Low Growth\n without HSR'='Low Growth-Without_HSR','Low Growth\n HSR2015'='Low Growth-HSR_2015',
                              'Low Growth\n HSR2025'='Low Growth-HSR_2025','Central Growth\n without HSR'='Central Growth-Without_HSR',
                              'Central Growth\n HSR2015'='Central Growth-HSR_2015','Central Growth\n HSR2025'='Central Growth-HSR_2025',
                              'High Growth\n without HSR'='High Growth-Without_HSR','High Growth\n HSR2015'='High Growth-HSR_2015',
                              'High Growth\n HSR2025'='High Growth-HSR_2025')

ggplot(LCI_All, aes(fill=Components, y=Inventory, x=Group, group=Group)) + 
  #scale_y_continuous(limits = c(0,1000),breaks = c(0,200,400,600,800,1000))+
  ylab('Aviation Cumulative Life-cycle CO2 Emissions Inventory (Mt)')+
  ggtitle('Cumulative Air Transport Lifecycle Emissions Inventory 2015-2050')+
  geom_bar(stat="identity")+coord_flip()+theme_Publication() +scale_fill_Publication()




