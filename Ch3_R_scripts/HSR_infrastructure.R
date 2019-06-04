setwd("D:/Aviation_HSR")
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
source("E:/Dropbox/R/ggplot.R")

########################################### China map background ##################################################
load("D:/Network/map_data/CHNmap.RData")
map.df <- CHN.map
background <- ggplot()+ geom_polygon(data=map.df, aes(x=long, y=lat, group=group),fill='grey85',color = "grey70") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank())


######################################### China's future HSR network ##############################################
# make HSR_2025 the same format to HSR_all in 'D:/DomesticCHN/HSR_construct.R', and re-run that script
library(geosphere)

HSR <- read.csv('D:/Aviation_HSR/HSR_full.csv')

Full_HSR <- NULL

for(i in levels(factor(HSR$HSR_2025))){
  temp <- subset(HSR[,c('HSR_2025','Year','City','lat','lon')],HSR_2025==i)%>% 
               group_by(HSR_2025,City) %>% filter(Year==min(Year)) %>% unique()

  temp <- cbind.data.frame(Origin=temp[1,c('City','Year','lon','lat')], Dest=temp[2:length(temp$City), c('City','Year','lon','lat')])
  # use greater circle distance
  temp$Distance <- round(distHaversine(temp[,c('Origin.lon','Origin.lat')], temp[,c('Dest.lon','Dest.lat')],r=6378.0))
  temp$HSR <- i
  temp$Origin.Month <- 1
  temp$Dest.Month <- 1
  temp <- rename(temp, Destination = Dest.City, Origin=Origin.City)

  temp <- temp[,c('HSR','Origin','Origin.Year','Origin.Month','Destination','Dest.Year','Dest.Month','Distance')]
  
  Full_HSR <- rbind.data.frame(Full_HSR, temp)
}

# now, move to D:/DomesticCHN/HSR_construct.R to get all city pairs

# still need overline, for each origin city, link its destination to next dest that is connected to it
Overline_25 <- NULL

for(i in levels(factor(Full_HSR$Origin))){
  temp <- subset(Full_HSR, Origin==i)
  temp <- rename(temp[,c(2,3,4,6)], Connect_1 = Destination,Dist_1=Distance,Year_1=Enter.Year)
  temp <- merge(temp, Full_HSR[,c('Origin','Destination','Enter.Year','Distance')], by.x='Connect_1',by.y='Origin',all.x=T)

  temp$HSR_dist <- temp$Dist_1 + temp$Distance
  temp$Enter.Year <- ifelse(temp$Enter.Year>= temp$Year_1, temp$Enter.Year, temp$Year_1)
  temp <- temp %>% group_by(Origin, Destination) %>% summarise(Enter.Year=max(Enter.Year),HSR_dist = min(HSR_dist))
  temp <- subset(temp, !Origin==Destination)
  
  Overline_25 <- rbind.data.frame(Overline_25, temp)
}

# We don't need to merge this two, just need Full_HSR segments that are after 2015, 
# all pre-2015 data should remain same, untouched. We just need to add these post-2015 segments into futures
# combine post 15 online and overline routes
names(Overline_25) <- c('Origin','Destination','Enter.Year','Distance')

Full_HSR <- rbind.data.frame(subset(Full_HSR, Enter.Year>2015)[,c('Origin','Destination','Enter.Year','Distance')],
                             subset(Overline_25,Enter.Year>2015)[,c('Origin','Destination','Enter.Year','Distance')])

Full_HSR <-  Full_HSR %>% group_by(Origin,Destination)%>%
             summarise(Enter.Year=min(Enter.Year),Distance=min(Distance))

# match to AIM city id
airport <- read.csv('D:/Aviation_HSR/airports_city_2015.csv')
Full_HSR <- merge(Full_HSR, airport[,c('City','CityID')], by.x='Origin',by.y='City',all.x=T)
Full_HSR <- merge(Full_HSR, airport[,c('City','CityID')], by.x='Destination',by.y='City',all.x=T)
Full_HSR <- rename(Full_HSR, OriginCityID=CityID.x, DestCityID=CityID.y)
Full_HSR <- drop_na(Full_HSR) %>% unique()
# Hong Kong and Shenzhen both belong to CityID 8, remove HK
Full_HSR <- subset(Full_HSR, !(Origin=='Hong Kong'|Destination=='Hong Kong'))
# Xiamen and Quanzhou both belong to CityID 51, remove Quanzhou
Full_HSR <- subset(Full_HSR, !(Origin=='Quanzhou'|Destination=='Quanzhou'))

Full_HSR <- Full_HSR %>% group_by(OriginCityID,DestCityID) %>% mutate(check=row_number(), check=max(check))


############################# check the HSR track on map line by line #######################################
Net <- HSR[,c('HSR_2025','Year','City','lat','lon','Main.City')] %>% group_by(HSR_2025) %>% 
       rename(Dest.City=City, Dest.lat=lat, Dest.lon=lon,Dest.Main=Main.City) %>% 
       mutate(Origin.City=lag(Dest.City),Origin.lat=lag(Dest.lat),Origin.lon=lag(Dest.lon),Origin.Main=lag(Dest.Main))

Net <- drop_na(Net)[,c('HSR_2025','Origin.City','Origin.Main','Dest.City','Dest.Main',
                       'Origin.lat','Origin.lon','Dest.lat','Dest.lon')]
Net <- subset(Net, !Origin.City==Dest.City)

HSR_map <- NULL
for(i in levels(factor(Net$HSR_2025))){
  temp <- subset(Net, HSR_2025==i)
  temp <- rbind.data.frame(temp[,c('HSR_2025','Origin.City','Origin.Main','Origin.lat','Origin.lon')], 
                           rename(temp[length(temp$Dest.City),c('HSR_2025','Dest.City','Dest.Main','Dest.lat','Dest.lon')],
                                  Origin.City=Dest.City, Origin.Main=Dest.Main,Origin.lat=Dest.lat,Origin.lon=Dest.lon))
  HSR_map <- rbind.data.frame(HSR_map, temp)
}

HSR_map <- HSR_map %>% group_by(HSR_2025) %>% mutate(sequence=row_number())
HSR_map$Origin.Main <- ifelse(HSR_map$Origin.City=='Hong Kong',0,HSR_map$Origin.Main)

library(ggrepel)
background + 
  geom_point(data = subset(HSR_map[,c('Origin.City','Origin.lat','Origin.lon','Origin.Main')], Origin.Main==1) %>%unique(), 
             shape = 21,size=2.5,aes(x=Origin.lon, y=Origin.lat),fill='red', alpha=1)+
  geom_point(data = subset(HSR_map[,c('Origin.City','Origin.lat','Origin.lon','Origin.Main')], Origin.Main==0) %>%unique(), 
             shape = 21,size=1,aes(x=Origin.lon, y=Origin.lat),fill='skyblue', alpha=.8)+
  geom_text(data= subset(HSR_map[,c('Origin.City','Origin.lat','Origin.lon','Origin.Main')], Origin.Main==1) %>%unique(), 
            aes(x=Origin.lon, y=Origin.lat, label= Origin.City,fontface = 'bold'),col = 'black', size = 3)+
  geom_path(data= subset(HSR_map, HSR_2025=='Bao-Hai'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Chang-Ning'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Guang-Kun'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Guang-Zhan'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Ha-Da'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='He-Fu'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Hu-Kun'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Hu-Nan'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Huandao'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Jing-Gang'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Jing-Gang_2'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Jing-Ha'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Jing-Hu'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Jing-Kun_1'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Jing-Kun_2'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Jing-Lan'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Lan-Guang'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Lan-Guang_2'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Lu-Qiao'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Nan-Fu'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Ning-Hang'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Qin-Jin'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Qin-Shen'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Qing-Rong'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Qing-Yin'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Shang-He-Hang'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Sui-Man'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Wu-Nan'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Xi-Guang'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Xia-Yu'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='YanHai'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Yanjiang'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)+
  geom_path(data= subset(HSR_map, HSR_2025=='Yin-Wu'), aes(x=Origin.lon, y=Origin.lat),color='blue', size=1, group=1, alpha=.8)


