setwd("D:/Aviation_HSR")
library(dplyr)
library(tidyr)
library(ggplot2)
source("E:/Dropbox/R/ggplot.R")

load("D:/Aviation_HSR/HSR_LCI_output.RData")
load("D:/Aviation_HSR/AIR_LCI_output.RData")

# Net savings = Air without - Air with + HSR CO2
# we need to plot it using the waterfall figure

# unify the LCI components to simply vehicle operation, manufacturing, maintenance
Air_savings <- LCI_All
levels(Air_savings$Components) <- list('V,Operation'='V,Operation','V,Manufacturing'='V,Manufacturing',
                                       'V,Maintenance'='V,Maintenance','I,Construction'='I,Construction(Runways)',
                                       'I,Construction'='I,Construction(Airports)','I,Construction'='I,Construction(Parking)',
                                       'I,Construction'='I,Construction(Tarmacs/Taxiways)','I,Maintenance'='I,Maintenance',
                                       'I,Operation'='I,Operation','F,Production'='F,Production')

Air_savings <- Air_savings %>% group_by(Type, Scenario, Components) %>% summarise(Inventory=sum(Inventory))

df1 <- merge(rename(subset(Air_savings,Type=='Without_HSR')[,c('Scenario','Components','Inventory')],AIR_without_HSR=Inventory),
                     rename(subset(Air_savings,Type=='HSR_2015')[,c('Scenario','Components','Inventory')],AIR_with_HSR15=Inventory),
                     by=c('Scenario','Components'),all.x=T)

Air_savings <- merge(df1,
                     rename(subset(Air_savings,Type=='HSR_2025')[,c('Scenario','Components','Inventory')],AIR_with_HSR25=Inventory),
                     by=c('Scenario','Components'),all.x=T)


# HSR inventory for fixed carbon intensity
HSR_fixed <- LCI_fixed
levels(HSR_fixed$Components) <- list('V,Operation'='V,Operation','V,Manufacturing'='V,Manufacturing',
                                     'V,Maintenance'='V,Maintenance','I,Construction'='I,Construction(Station)',
                                     'I,Construction'='I,Construction(Track)','I,Maintenance'='I,Maintenance',
                                     'I,Operation'='I,Operation(Station)','I,Operation'='I,Operation(Train Control)',
                                     'F,Production'='E,Distribution Losses')

HSR_fixed <- HSR_fixed %>% group_by(Type, Scenario, Components) %>% summarise(Inventory=sum(Inventory))

Net_savings_fixed <- merge(Air_savings,
                     rename(subset(HSR_fixed,Type=='HSR_15')[,c('Scenario','Components','Inventory')],HSR15_fixed=Inventory),
                     by = c('Scenario','Components'),all.x=T)

Net_savings_fixed <- merge(Net_savings_fixed,
                     rename(subset(HSR_fixed,Type=='HSR_25')[,c('Scenario','Components','Inventory')],HSR25_fixed=Inventory),
                     by = c('Scenario','Components'),all.x=T)


# HSR inventory for declining carbon intensity
HSR_IEA <- LCI_IEA
levels(HSR_IEA$Components) <- list('V,Operation'='V,Operation','V,Manufacturing'='V,Manufacturing',
                                     'V,Maintenance'='V,Maintenance','I,Construction'='I,Construction(Station)',
                                     'I,Construction'='I,Construction(Track)','I,Maintenance'='I,Maintenance',
                                     'I,Operation'='I,Operation(Station)','I,Operation'='I,Operation(Train Control)',
                                     'F,Production'='E,Distribution Losses')

HSR_IEA <- HSR_IEA %>% group_by(Type, Scenario, Components) %>% summarise(Inventory=sum(Inventory))

Net_savings_IEA <- merge(Air_savings,
                         rename(subset(HSR_IEA,Type=='HSR_15')[,c('Scenario','Components','Inventory')],HSR15_IEA=Inventory),
                         by = c('Scenario','Components'),all.x=T)

Net_savings_IEA <- merge(Net_savings_IEA,
                         rename(subset(HSR_IEA,Type=='HSR_25')[,c('Scenario','Components','Inventory')],HSR25_IEA=Inventory),
                         by = c('Scenario','Components'),all.x=T)


# air LCI vehicle maint, vehicle manu, and fuel production is off. check.

###################################### summary waterfall charts ##################################################
# we need four charts: 

# 1) HSR without - with_HSR_15 + HSR15_fixed  vs.  HSR without - with_HSR_15 + HSR_15_IEA
# three types of bars: reduction from aviation, increase from HSR, and net savings
waterfall_fixed <- subset(Net_savings_fixed,Scenario=='Low Growth')[,c('Scenario','Components','AIR_without_HSR','AIR_with_HSR25','HSR25_fixed')]
waterfall_fixed$AIR <- waterfall_fixed$AIR_without_HSR - waterfall_fixed$AIR_with_HSR25
waterfall_fixed$HSR <- -waterfall_fixed$HSR25_fixed

waterfall_IEA <- subset(Net_savings_IEA,Scenario=='Low Growth')[,c('Scenario','Components','AIR_without_HSR','AIR_with_HSR25','HSR25_IEA')]
waterfall_IEA$AIR <- waterfall_IEA$AIR_without_HSR - waterfall_IEA$AIR_with_HSR25
waterfall_IEA$HSR <- -waterfall_IEA$HSR25_IEA

#######################################
test <- waterfall_fixed

test$AIR <- abs(test$AIR)
test <- test[,c('Scenario','Components','HSR','AIR')] %>% gather(Type,Inventory,-Scenario,-Components)

# net savings in the end
test <- rbind.data.frame(test, cbind.data.frame(Scenario='Low Growth',Components='Savings',Type='Net LCI',Inventory=sum(test$Inventory)))
test$Components <- paste(test$Type, test$Components,sep=':')

# rank components to same group
test$Components <- as.factor(test$Components)
test$Components <- factor(test$Components, 
                          levels=c('AIR:V,Operation','HSR:V,Operation','AIR:V,Manufacturing','HSR:V,Manufacturing',
                                   'AIR:V,Maintenance','HSR:V,Maintenance','AIR:I,Construction','HSR:I,Construction',
                                   'AIR:I,Maintenance','HSR:I,Maintenance','AIR:I,Operation','HSR:I,Operation',
                                   'AIR:F,Production','HSR:F,Production','Net LCI:Savings'))

test <- test %>% arrange(Components) %>% mutate(id=row_number())
test$end <- cumsum(test$Inventory)
test$end <- c(head(test$end, -1), 0)
test$start <- c(0, head(test$end, -1))
# add label to the chart
test$label_y <- ifelse(test$Inventory>=0, test$end, test$start)
test$label_y <- ifelse(test$Type=='Net LCI', test$Inventory, test$label_y)

# make water fall plot
ggplot(test, aes(x=Components,fill = Type)) + 
      geom_rect(aes(x = Components, xmin = id - 0.45, xmax = id + 0.45, ymin = end, ymax = start))+
      geom_text(aes(x=Components,y=label_y,label=round(Inventory)),size=4,vjust=-0.5)+
      scale_y_continuous(limits = c(0,1500))+
      ylab('Cumulative net lifecycle CO2 emissions savings (Mt)') + 
      ggtitle('Low Growth Scenario Net Lifecycle Emissions Savings \n (with 2025 HSR network, fixed carbon intensity)')+
      theme_Publication() +scale_fill_Publication()+
      scale_fill_discrete(name="Lifecycle Inventory:",
                          breaks=c("AIR", "HSR", "Net LCI"),
                          labels=c("Gross savings in AIR", "HSR emissions", "Net LCI savings"))


















