setwd("D:/Aviation_HSR")
library(dplyr)
library(tidyr)
library(ggplot2)
source("E:/Dropbox/R/ggplot.R")

All <- update(All)


Check <- All[,c('city.pair','id','HSR_dist','HSR_freq','HSR_pax','HSR_share')]
Check$freq_daily <- round(Check$HSR_freq/365)
Check$pax_daily <- round(Check$HSR_pax/365)
ggplot(Check, aes(x=HSR_dist, y=log(pax_daily),label=city.pair))+geom_point()+geom_text(check_overlap = T)

ggplot(All, aes(x=AE_drive_diff, y=HSR_share,label=city.pair))+geom_point()+geom_text(check_overlap = T)

ggplot(subset(All),#,log(Population)>=3.5 & log(Population)<=4.5), 
       aes(y=log(Total_demand), x=Special_none,label=city.pair))+geom_point()+geom_text(check_overlap = T)


#All$HSR_pax <- All$HSR_pax*6
#All <- merge(All, trains, by='id',all.x = T)
#All$D_type <- All$HSR_freq*round((All$D_type/(All$D_type+All$G_type)))
#All$G_type <- All$HSR_freq - All$D_type
#All$D_seats <- ifelse(All$HSR_dist>800, 856,600)
#All$G_seats <- ifelse(All$HSR_dist>800,1229,910)
#All$Total_seats <- All$D_type*All$D_seats + All$G_type*All$G_seats
#All$Load_factor <- All$HSR_pax/All$Total_seats


# fill in missing all trains 
All_trains_missing <- cbind.data.frame(
  HSR.train=c('G81','G81','G81','G81','G81','G81','G81','G81','G81','G81','G81','G81','G81','G81','G81','G81','G81','G81','G81','G81'), 
  Origin_city = c('changchun','changsha','dalian','chongqing','fuzhou','guangzhou','guilin','guiyang','guiyang','hangzhou','hangzhou','jinan','nanning','qingdao','shanghai','shenyang','shenzhen','shenzhen','wuxi','fuzhou'),
  Dest_city =c('wuxi','guangzhou','wuxi','fuzhou','jinan','nanchang','shanghai','jinan','nanchang','guilin','xiamen','guiyang','shenzhen','hefei','fuzhou','wenzhou','nanning','yichang','harbin','chongqing'), 
  Origin_station = c('OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO'), 
  Dest_station = c('OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO','OOO'),
  Depart_time = c('09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00'),
  Journey_time = c('09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00','09:00'),
  HSR_hr = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  HSR_mins = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Price = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Date = c('20181018','20181018','20181018','20181018','20181018','20181018','20181018','20181018','20181018','20181018','20181018','20181018','20181018','20181018','20181018','20181018','20181018','20181018','20181018','20181018'),
  HSR_time = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  HSR_type = c('G','G','G','D','G','G','G','G','G','G','D','G','G','G','D','G','G','G','G','D'))




Check <- All[,c('city.pair','id','HSR_dist','HSR_freq','HSR_pax','HSR_share')]
Check$freq_daily <- round(Check$HSR_freq/365)
Check$pax_daily <- round(Check$HSR_pax/365)
ggplot(Check, aes(x=HSR_dist, y=log(pax_daily),label=city.pair))+geom_point()+geom_text(check_overlap = T)

ggplot(All, aes(x=log(Air_freq), y=log(Total_demand),label=city.pair))+geom_point()+geom_text(check_overlap = T)


# emissions factor: Table 72-74, per Aircraft-Life,adding all airframe components together
Chester$Total_airframe_maint_tCO2 <- c((350+50+190+160+180+100+180+310),
                                       (4000+580+2200+1900+2100+1200+2100+3500),
                                       (1700+250+940+810+910+510+910+1500))

Chester$Total_engine_maint_tCO2 <- c(120,2300,5000)










