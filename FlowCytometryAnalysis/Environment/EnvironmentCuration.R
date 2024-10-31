####Environment####
library(tidyverse)
library(cowplot)
library(readr)
library(ggplot2)
library(lubridate)
library(readxl)
library(dplyr)
library(tibble)

Light <- read_excel("Environmental_Data_GARDEN.xlsx", 
                    sheet = "Light")
head(Light)
Light <- na.omit(Light)
mean(Light$Temp)

Pressure <- read_excel("Environmental_Data_GARDEN.xlsx", 
                       sheet = "Water_Pressure")
#View(Pressure)
ViableLightLoggerTemp <- filter(Light, Plot != "EA")
#View(ViableLightLoggerTemp)
head(ViableLightLoggerTemp)
Temp1 <- select(ViableLightLoggerTemp, -Lux)
Temp2 <- select(Pressure, -Press)

head(Temp1)
head(Temp2)

SourceTemp <- read_excel("Environmental_Data_GARDEN.xlsx", 
                         sheet = "LuminaoTemp")

AllTemp <- rbind(Temp1, Temp2)
ALLTEMP <- rbind(Temp2, SourceTemp)
head(AllTemp)

hobotemplongdate <- tidyr::separate(ALLTEMP, 'Date',
                                    into = c('longdate', 'time'),
                                    sep= ' ') 

temp <- hobotemplongdate
head(temp)

temp[['longdate']] <- as.POSIXct(temp[['longdate']],
                                 format = "%Y-%m-%d")

#temp$longdate <- strptime(as.character(temp$longdate), "%Y/%m/%d")
temp$longdate <- format(temp$longdate, "%d/%m/%Y")
temp <- dplyr::rename(temp, Date = longdate)
head(temp)

##Raw Temp
#ntempgraph <- ggplot(data=temp, 
#                     aes(x=as.Date(Date, format = "%d / %m / %Y"), 
#                         y=Temp, colour=Plot)) +
#  geom_point(size=1, alpha = 1/10)+ theme_bw()+
#  facet_grid(cols = vars(Plot))+
#  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
#  labs(title="Raw temperature data", y="Temperature (?C)", x="Date")
#ntempgraph

#Analysis of daily average temperatures

hobofull <- temp %>%
  tidyr::separate('Date',
                  into = c('Day', 'Month', 'Year'),
                  sep= '/',
                  remove = FALSE)
head(hobofull)

hobofull <- na.omit(hobofull)
head(hobofull)
#hobo21 <- hobofull[hobofull$Year != "2022", ]
#View(hobo21)
hobofullmean <- hobofull %>%
  group_by(Year, Month, Day, Site, Plot, Date)%>%
  dplyr::summarise(meantemp = mean(Temp), mintemp = min(Temp), maxtemp = max(Temp)) %>%
  mutate(range = maxtemp-mintemp)

head(hobofullmean)
#View(hobofullmean)

hobofullmean$Date <- dmy(hobofullmean$Date)

head(hobofullmean)
mean <- ggplot(hobofullmean, aes(x=Date, y=meantemp))+
  #geom_point(aes(colour = Site), alpha = 0.5)+
  #geom_boxplot(aes(group = interaction(year, month, Site), color = Site))+
  geom_smooth(aes(group = Plot, colour = Site), span = 0.5, fullrange = TRUE)+
  theme_bw()+
  labs(title= "Daily temperature means", y="Daily mean temperature (?C) with 95% CI", x="Date")
mean

hobofullmeancorrected <- subset(hobofullmean, mintemp >= c(27.4))
hobofullmeancorrected <- subset(hobofullmeancorrected, maxtemp <= c(34.0))
head(hobofullmeancorrected)
all <- ggplot(hobofullmeancorrected, aes(x=Date))+
  #geom_line(aes(y = mintemp), alpha = 0.2)+
  #geom_line(aes(y = maxtemp), alpha = 0.2)+
  geom_ribbon(aes(ymin = mintemp, ymax = maxtemp, fill = Site), alpha = 0.2)+
  geom_smooth(aes(y = meantemp, colour = Site, group = Plot), span = 0.5, fullrange = TRUE, se = T)+
  #geom_smooth(aes(y = maxtemp, colour = Site), span = 0.5, fullrange = TRUE)+
  scale_fill_manual(values = c("#414181","black", "#f06f46"))+
  scale_color_manual(values = c("#414181","ivory", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  #geom_boxplot(aes(y = mintemp, group = interaction(year, month, Site), color = Site))+
  #geom_boxplot(aes(y = maxtemp, group = interaction(year, month, Site), color = Site))+
  #geom_smooth(aes(colour = Site), span = 0.5, fullrange = TRUE)+
  theme_classic()+
  labs(title= "Daily temperature min, max, mean", y="Daily temperature mean, min, and max (?C) with 95% CI", x="")
all

range <- ggplot(hobofullmeancorrected, aes(x=Date, y=range))+
  geom_line(aes(colour = Site, group = Plot), alpha = 0.5)+
  #geom_boxplot(aes(group = interaction(year, month, Site), color = Site))+
  geom_smooth(aes(colour = Site, group = Plot), span = 0.5, fullrange = TRUE)+
  scale_color_manual(values = c("#414181","black", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic()+
  labs(title= "Daily temperature range", y="Daily temperature range (?C) with 95% CI", x="")
range

which(Temp1$Date == "2022-03-11", arr.ind = TRUE)
Temp1[59408,]
Temp1[76233,]
Temp1[93055,]

library(broom)
#MEAN TEMP
#Test for significance between zones, month, LTER sites
hobomeanmonthaov <- aov(meantemp~Site + as.factor(Plot) + as.factor(Month), data=hobofullmeancorrected)
summary(hobomeanmonthaov)

#                  Df Sum Sq Mean Sq F value   Pr(>F)    
#Site               1   1.29   1.286  22.347 2.68e-06 ***
#  as.factor(Plot)    2   0.05   0.026   0.445    0.641    
#as.factor(Month)   7  29.46   4.208  73.125  < 2e-16 ***
#  Residuals        815  46.90   0.058     
meantempaovresults <- tidy(TukeyHSD(hobomeanmonthaov))
write.table(as.data.frame(meantempaovresults),file="TempAOVOutput.csv", quote=F,sep=",",row.names=F)

#TEMP MIN
#Test for significance between zones, month, LTER sites
hobominmonthaov <- aov(mintemp~Site + as.factor(Plot) + as.factor(Month), data=hobofullmean)
summary(hobominmonthaov)
#                   Df Sum Sq Mean Sq F value  Pr(>F)    
#Site                2   57.8   28.89 399.412 < 2e-16 ***
#  as.factor(Plot)     2    0.9    0.43   5.915 0.00275 ** 
#  as.factor(Month)   11  418.3   38.03 525.779 < 2e-16 ***
#  Residuals        1908  138.0    0.07   

mintempaovresults <- tidy(TukeyHSD(hobominmonthaov))
mintempaovresults
write.table(as.data.frame(mintempaovresults),file="MinTempAOVOutput.csv", quote=F,sep=",",row.names=F)

#TEMP MAX
#Test for significance between zones, month, LTER sites
hobomaxmonthaov <- aov(maxtemp~Site + as.factor(Plot) + as.factor(Month), data=hobofullmean)
summary(hobomaxmonthaov)
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Site                2   40.4   20.19  35.277 9.03e-16 ***
#  as.factor(Plot)     2    0.1    0.03   0.052    0.949    
#as.factor(Month)   11 1146.7  104.25 182.188  < 2e-16 ***
#  Residuals        1908 1091.7    0.57  

maxtempaovresults <- tidy(TukeyHSD(hobomaxmonthaov))
maxtempaovresults
write.table(as.data.frame(maxtempaovresults),file="AcroGarden/EnvFigures/MaxTempAOVOutput.csv", quote=F,sep=",",row.names=F)

#TEMP RANGE
#Test for significance between zones, month, LTER sites
hoborangemonthaov <- aov(range~Site + as.factor(Plot) + as.factor(Month), data=hobofullmean)
summary(hoborangemonthaov)
#Df Sum Sq Mean Sq F value Pr(>F)    
#Site                2  131.0   65.50 115.415 <2e-16 ***
#  as.factor(Plot)     2    1.3    0.67   1.181  0.307    
#as.factor(Month)   11  358.7   32.61  57.460 <2e-16 ***
#  Residuals        1908 1082.8    0.57                   

rangetempaovresults <- tidy(TukeyHSD(hoborangemonthaov))
rangetempaovresults
write.table(as.data.frame(rangetempaovresults),file="RangeTempAOVOutput.csv", quote=F,sep=",",row.names=F)

write.table(as.data.frame(hobofullmeancorrected),file="hobofullmeancorrected.csv", quote=F,sep=",",row.names=F)
