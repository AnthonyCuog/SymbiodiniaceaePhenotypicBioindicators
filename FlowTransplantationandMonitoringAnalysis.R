library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(plot3D)
library(plot3Drgl)
library(tidyverse)
library(readr)
library(ggridges)
library(ggplot2)
library(viridis)
#install.packages("hrbrthemes")
library(hrbrthemes)
library(gridExtra)

preprocessedflowdata <- read_csv("preprocessedflowdata.csv", 
                                 col_types = cols(Date = col_date(format = "%Y-%m-%d")))
df4 <- preprocessedflowdata

df6 <- df4 %>%
  group_by(Site, Plot, Colony, Date, Verified) %>%
  dplyr::summarize(RED = mean(RED.B.HLog),
                   REDsd = sd(RED.B.HLog),
                   GRN = mean(GRN.B.HLog),
                   GRNsd = sd(GRN.B.HLog),
                   FSC = mean(FSC.HLog),
                   FSCsd = sd(FSC.HLog),
                   SSC = mean(SSC.HLog),
                   SSCsd = sd(SSC.HLog),)

# Plot
#ggplot(df4, aes(x = RED.B.HLog, y = interaction(Colony,Plot,  Site, Date), fill = Site)) +
#  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#  scale_fill_manual(values = c("#414181","ivory", "#f06f46"))+
#  labs(title = 'Red Fluorescence of all Colonies') +
#  scale_y_discrete(limits=rev)+
#  theme_ipsum() +
#  theme(
#   legend.position="none",
#    panel.spacing = unit(0.1, "lines"), axis.text.y=element_text(size=4),
#    strip.text.x = element_text(size = 4)
#  )
#ggplot(df4, aes(x = GRN.B.HLog, y = interaction(Colony,Plot,  Site, Date), fill = Site)) +
#  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#  scale_fill_manual(values = c("#414181","ivory",  "#f06f46"))+
#  labs(title = 'Green Fluorescence of all Colonies') +
#  scale_y_discrete(limits=rev)+
#  theme_ipsum() +
#  theme(
#    legend.position="none",
#    panel.spacing = unit(0.1, "lines"), axis.text.y=element_text(size=4),
#    strip.text.x = element_text(size = 4)
#  )
#ggplot(df4, aes(x = FSC.HLog, y = interaction(Colony,Plot,  Site, Date), fill = Site)) +
#  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#  scale_fill_manual(values = c("#414181","ivory",  "#f06f46"))+
#  labs(title = 'Forward Scatter of all Colonies') +
#  scale_y_discrete(limits=rev)+
#  theme_ipsum() +
#  theme(
#    legend.position="none",
#    panel.spacing = unit(0.1, "lines"), axis.text.y=element_text(size=4),
#    strip.text.x = element_text(size = 4)
#  )
#ggplot(df4, aes(x = SSC.HLog, y = interaction(Colony,Plot,  Site, Date), fill = Site)) +
#  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#  scale_fill_manual(values = c("#414181","ivory",  "#f06f46"))+
#  labs(title = 'Side Scatter of all Colonies') +
#  scale_y_discrete(limits=rev)+
#  theme_ipsum() +
#  theme(
#    legend.position="none",
#    panel.spacing = unit(0.1, "lines"), axis.text.y=element_text(size=4),
#    strip.text.x = element_text(size = 4)
#  )

#ggplot()+
#  geom_line(data = df6, aes(x = Date, y = RED, group = interaction(Plot, Site, Colony), color = Site), alpha = 0.5, linewidth = 2)+
#  geom_boxplot(data = df4, aes(x = Date, y = RED.B.HLog, group=interaction(Date, Plot, Site), fill = Site), 
#             width = 15, color = "black", size = 0.5, outlier.shape = NA)+
#  scale_fill_manual(values = c("#414181","white",  "#f06f46"))+
#  scale_color_manual(values = c("#414181","black", "#f06f46"))+
#  theme_classic2()+
#  theme(legend.position = "none", axis.title.x=element_blank())


#Test Panel
RED <- ggplot()+
  #geom_line(data = df6, aes(x = Date, y = RED, group = interaction(Plot, Site, Colony), color = Site), linewidth = 1.2, alpha = 0.1)+
  geom_smooth(data = df6, aes(x = Date, y = RED, color = Site), linewidth=1.5, span =0.5)+
  #geom_smooth(data = df6, aes(x = Date, y = RED, color = Site), span =0.9)+
  geom_jitter(data = df6, aes(x = Date, y = RED, group = interaction(Plot, Site, Colony), color = Site), size = 2, alpha = 0.5) +
  scale_fill_manual(values = c("#414181","black",  "#f06f46"))+
  scale_color_manual(values = c("#414181","black", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())
RED


REDsd <- ggplot()+
  #geom_line(data = df6, aes(x = Date, y = REDsd, group = interaction(Plot, Site, Colony), color = Site), linewidth = 1.2, alpha = 0.1)+
  geom_smooth(data = df6, aes(x = Date, y = REDsd, color = Site), linewidth=1.5, span =0.5)+
  #geom_smooth(data = df6, aes(x = Date, y = REDsd, color = Site))+
  geom_jitter(data = df6, aes(x = Date, y = REDsd, group = interaction(Plot, Site, Colony), color = Site), size = 2, alpha = 0.5) +
  scale_fill_manual(values = c("#414181","black",  "#f06f46"))+
  scale_color_manual(values = c("#414181","black", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())

GRN <- ggplot()+
  #geom_line(data = df6, aes(x = Date, y = GRN, group = interaction(Plot, Site, Colony), color = Site), linewidth = 1.2, alpha = 0.1)+
  geom_smooth(data = df6, aes(x = Date, y = GRN, color = Site), linewidth=1.5, span =0.5, alpha = 0.5)+
  #geom_smooth(data = df6, aes(x = Date, y = RED, color = Site), span =0.9)+
  geom_jitter(data = df6, aes(x = Date, y = GRN, group = interaction(Plot, Site, Colony), color = Site), size = 2, alpha = 0.5) +
  scale_fill_manual(values = c("#414181","black",  "#f06f46"))+
  scale_color_manual(values = c("#414181","black", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())
GRN


GRNsd <- ggplot()+
  #geom_line(data = df6, aes(x = Date, y = GRNsd, group = interaction(Plot, Site, Colony), color = Site), linewidth = 1.2, alpha = 0.1)+
  geom_smooth(data = df6, aes(x = Date, y = GRNsd, color = Site), linewidth=1.5, span =0.5)+
  #geom_smooth(data = df6, aes(x = Date, y = REDsd, color = Site))+
  geom_jitter(data = df6, aes(x = Date, y = GRNsd, group = interaction(Plot, Site, Colony), color = Site), size = 2, alpha = 0.5) +
  scale_fill_manual(values = c("#414181","black",  "#f06f46"))+
  scale_color_manual(values = c("#414181","black", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())

FSC <- ggplot()+
  #geom_line(data = df6, aes(x = Date, y = FSC, group = interaction(Plot, Site, Colony), color = Site), linewidth = 1.2, alpha = 0.1)+
  geom_smooth(data = df6, aes(x = Date, y = FSC, color = Site), linewidth=1.5, span =0.5)+
  #geom_smooth(data = df6, aes(x = Date, y = RED, color = Site), span =0.9)+
  geom_jitter(data = df6, aes(x = Date, y = FSC, group = interaction(Plot, Site, Colony), color = Site), size = 2, alpha = 0.5) +
  scale_fill_manual(values = c("#414181","black",  "#f06f46"))+
  scale_color_manual(values = c("#414181","black", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())
FSC


FSCsd <- ggplot()+
  #geom_line(data = df6, aes(x = Date, y = FSCsd, group = interaction(Plot, Site, Colony), color = Site), linewidth = 1.2, alpha = 0.1)+
  geom_smooth(data = df6, aes(x = Date, y = FSCsd, color = Site), linewidth=1.5, span =0.5)+
  #geom_smooth(data = df6, aes(x = Date, y = REDsd, color = Site))+
  geom_jitter(data = df6, aes(x = Date, y = FSCsd, group = interaction(Plot, Site, Colony), color = Site), size = 2, alpha = 0.5) +
  scale_fill_manual(values = c("#414181","black",  "#f06f46"))+
  scale_color_manual(values = c("#414181","black", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())

SSC <- ggplot()+
  #geom_line(data = df6, aes(x = Date, y = SSC, group = interaction(Plot, Site, Colony), color = Site), linewidth = 1.2, alpha = 0.1)+
  geom_smooth(data = df6, aes(x = Date, y = SSC, color = Site), linewidth=1.5, span =0.5)+
  #geom_smooth(data = df6, aes(x = Date, y = RED, color = Site), span =0.9)+
  geom_jitter(data = df6, aes(x = Date, y = SSC, group = interaction(Plot, Site, Colony), color = Site), size = 2, alpha = 0.5) +
  scale_fill_manual(values = c("#414181","black",  "#f06f46"))+
  scale_color_manual(values = c("#414181","black", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())
SSC

SSCsd <- ggplot()+
  #geom_line(data = df6, aes(x = Date, y = SSCsd, group = interaction(Plot, Site, Colony), color = Site), linewidth = 1.2, alpha = 0.1)+
  geom_smooth(data = df6, aes(x = Date, y = SSCsd, color = Site), linewidth=1.5, span =0.5)+
  #geom_smooth(data = df6, aes(x = Date, y = REDsd, color = Site))+
  geom_jitter(data = df6, aes(x = Date, y = SSCsd, group = interaction(Plot, Site, Colony), color = Site), size = 2, alpha = 0.5) +
  scale_fill_manual(values = c("#414181","black",  "#f06f46"))+
  scale_color_manual(values = c("#414181","black", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())
SSCsd

grid.arrange(RED, REDsd, GRN, GRNsd, FSC, FSCsd, SSC, SSCsd, nrow = 4)

library(readxl)
CD <- read_excel("GardenAcroLong_Condensed.xlsx", 
                 sheet = "AllGarden")
Color <- read_excel("GardenAcroLong_Condensed.xlsx", 
                    sheet = "ColorData")
head(Color)
ColorTrust <- subset(Color, Trust == "Yes")
CD2 <- CD %>%
  group_by(Site, Plot, Colony, Date) %>%
  dplyr::summarize(CD = mean(CellDensity))

CD2 <- as.data.frame(CD2)
CD2 <- subset(CD2, Plot != "ABAB")

CD2$Date <- as.Date(CD2$Date)
CDplot <- ggplot()+
  #geom_line(data = CD2, aes(x = Date, y = CD, group = interaction(Plot, Site, Colony), color = Site), linewidth = 1.2, alpha = 0.1)+
  geom_smooth(data = CD2, aes(x = Date, y = CD, color = Site), linewidth=1.5, span =0.5)+
  geom_jitter(data = CD2, aes(x = Date, y = CD, group = interaction(Plot, Site, Colony), color = Site), size = 2, alpha = 0.5) +
  scale_fill_manual(values = c("#414181","black",  "#f06f46"))+
  scale_color_manual(values = c("#414181","black", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())

Color2 <- ColorTrust %>%
  group_by(Site, Plot, Colony, Date, Rep) %>%
  dplyr::summarize(Tint = mean(Intensity))
head(Color2)
ggplot()+
  geom_line(data = Color2, aes(x = Date, y = Tint, group = interaction(Plot, Site, Colony, Rep), color = Site), linewidth = 1.2, alpha = 0.6)+
  #geom_smooth(data = df6, aes(x = Date, y = RED, color = Site), span =0.9)+
  geom_point(data = Color2, aes(x = Date, y = Tint, group = interaction(Plot, Site, Colony, Rep), color = Site), size = 2) +
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())

Color2$Date <- as.Date(Color2$Date)
Colorplot <- ggplot()+
  #geom_line(data = Color2, aes(x = Date, y = Tint, group = interaction(Plot, Site, Colony, Rep), color = Site), linewidth = 1.2, alpha = 0.1)+
  geom_jitter(data = Color2, aes(x = Date, y = Tint, group = interaction(Plot, Site, Colony, Rep), color = Site), size = 2, alpha = 0.5) +
  geom_smooth(data = Color2, aes(x = Date, y = Tint, color = Site), linewidth = 1.5, span =0.5)+
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())
grid.arrange(Colorplot, CDplot, RED, REDsd, GRN, GRNsd, FSC, FSCsd, SSC, SSCsd, nrow = 5)

####Transplantation stress boxplots
head(df6)
tempstress <- df6[df6$Date < ymd("2021-11-01"), ]
class(tempstress$Date)
tempstress$Date2 <- tempstress$Date
tempstress$Date2<-as.character(tempstress$Date2)
head(tempstress)
unique(tempstress$Date2)
t<-tempstress$Date2=="2021-09-30" 
sum(t)
tempstress$Date2[t]<-'Garden 4 months after transplant'
s<-tempstress$Date2== "2021-06-16"
sum(s)
tempstress$Date2[s] <- 'Garden 1 month after transplant'
v <- tempstress$Date2== "2021-05-18" 
sum(v)
tempstress$Date2[v] <- 'Native before transplant'

q<-tempstress$Date2== "2021-08-04"
sum(q)
tempstress$Date2[q] <- 'Native 3 months after transplant'

unique(tempstress$Date2)

tempstress$Date2 <- factor(tempstress$Date2, levels=c('Native before transplant','Garden 1 month after transplant','Native 3 months after transplant','Garden 4 months after transplant')) 

library(nlme)
library(multcomp)
unique(tempstress$Colony)
tempstress$Repeat <- interaction(tempstress$Site, tempstress$Plot, tempstress$Colony)
unique(tempstress$Repeat)

REDtransplantaov <- aov(RED ~ Date2 * Site + Error(Repeat), data = tempstress)
summary(REDtransplantaov)
head(tempstress)
model = lme(RED ~ Date2, random=~1|Repeat,data=tempstress)
summary(glht(model,linfct=mcp(Date2="Tukey")))

REDTrans <- ggplot(tempstress, aes(Date2, RED, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("#414181","ivory", "#f06f46"))+
  scale_color_manual(values = c("#414181","ivory", "#f06f46"))+
  xlab("")+
  ylab("Red fluor. (mean)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
REDTrans

REDsdtransplantaov <- aov(REDsd ~ Date2 * Site + Error(Repeat), data = tempstress)
summary(REDsdtransplantaov)

model = lme(REDsd ~ Date2, random=~1|Repeat,data=tempstress)
summary(glht(model,linfct=mcp(Date2="Tukey")))

REDsdTrans <- ggplot(tempstress, aes(Date2, REDsd, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("#414181","ivory", "#f06f46"))+
  scale_color_manual(values = c("#414181","ivory", "#f06f46"))+
  xlab("")+
  ylab("Red fluor. (s.d.)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
REDsdTrans

GRNtransplantaov <- aov(GRN ~ Date2 * Site + Error(Repeat), data = tempstress)
summary(GRNtransplantaov)

model = lme(GRN ~ Date2, random=~1|Repeat,data=tempstress)
summary(glht(model,linfct=mcp(Date2="Tukey")))

GRNTrans <- ggplot(tempstress, aes(Date2, GRN, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("#414181","ivory", "#f06f46"))+
  scale_color_manual(values = c("#414181","ivory", "#f06f46"))+
  xlab("")+
  ylab("Green fluor. (mean)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
GRNTrans

GRNsdtransplantaov <- aov(GRNsd ~ Date2 * Site + Error(Repeat), data = tempstress)
summary(GRNsdtransplantaov)

model = lme(GRNsd ~ Date2, random=~1|Repeat,data=tempstress)
summary(glht(model,linfct=mcp(Date2="Tukey")))

GRNsdTrans <- ggplot(tempstress, aes(Date2, GRNsd, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("#414181","ivory", "#f06f46"))+
  scale_color_manual(values = c("#414181","ivory", "#f06f46"))+
  xlab("")+
  ylab("Green fluor. (s.d.)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
GRNsdTrans

FSCtransplantaov <- aov(FSC ~ Date2 * Site + Error(Repeat), data = tempstress)
summary(FSCtransplantaov)

model = lme(FSC ~ Date2, random=~1|Repeat,data=tempstress)
summary(glht(model,linfct=mcp(Date2="Tukey")))

FSCTrans <- ggplot(tempstress, aes(Date2, FSC, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("#414181","ivory", "#f06f46"))+
  scale_color_manual(values = c("#414181","ivory", "#f06f46"))+
  xlab("")+
  ylab("Forward scatter (mean)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
FSCTrans

FSCsdtransplantaov <- aov(FSCsd ~ Date2 * Site + Error(Repeat), data = tempstress)
summary(FSCsdtransplantaov)

model = lme(FSCsd ~ Date2, random=~1|Repeat,data=tempstress)
summary(glht(model,linfct=mcp(Date2="Tukey")))

FSCsdTrans <- ggplot(tempstress, aes(Date2, FSCsd, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("#414181","ivory", "#f06f46"))+
  scale_color_manual(values = c("#414181","ivory", "#f06f46"))+
  xlab("")+
  ylab("Forward scatter (s.d.)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
FSCsdTrans

SSCtransplantaov <- aov(SSC ~ Date2 * Site + Error(Repeat), data = tempstress)
summary(SSCtransplantaov)

model = lme(SSC ~ Date2, random=~1|Repeat,data=tempstress)
summary(glht(model,linfct=mcp(Date2="Tukey")))

SSCTrans <- ggplot(tempstress, aes(Date2, SSC, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("#414181","ivory", "#f06f46"))+
  scale_color_manual(values = c("#414181","ivory", "#f06f46"))+
  xlab("")+
  ylab("Side scatter (mean)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
SSCTrans

SSCsdtransplantaov <- aov(SSCsd ~ Date2 * Site + Error(Repeat), data = tempstress)
summary(SSCsdtransplantaov)

model = lme(SSCsd ~ Date2, random=~1|Repeat,data=tempstress)
summary(glht(model,linfct=mcp(Date2="Tukey")))

SSCsdTrans <- ggplot(tempstress, aes(Date2, SSCsd, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("#414181","ivory", "#f06f46"))+
  scale_color_manual(values = c("#414181","ivory", "#f06f46"))+
  xlab("")+
  ylab("Side scatter (s.d.)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
SSCsdTrans

head(CD2)
tempstressCD <- CD2[CD2$Date < ymd("2021-11-01"), ]
class(tempstressCD$Date)
tempstressCD$Date2 <- tempstressCD$Date
tempstressCD$Date2<-as.character(tempstressCD$Date2)
head(tempstressCD)
unique(tempstressCD$Date2)
t<-tempstressCD$Date2=="2021-09-30" 
sum(t)
tempstressCD$Date2[t]<-'Garden 4 months after transplant'
s<-tempstressCD$Date2== "2021-06-16"
sum(s)
tempstressCD$Date2[s] <- 'Garden 1 month after transplant'
v <- tempstressCD$Date2== "2021-05-18" 
sum(v)
tempstressCD$Date2[v] <- 'Native before transplant'

q<-tempstressCD$Date2== "2021-08-04"
sum(q)
tempstressCD$Date2[q] <- 'Native 3 months after transplant'

unique(tempstressCD$Date2)

tempstressCD$Date2 <- factor(tempstressCD$Date2, levels=c('Native before transplant','Garden 1 month after transplant','Native 3 months after transplant','Garden 4 months after transplant')) 
tempstressCD$Repeat <- interaction(tempstressCD$Site, tempstressCD$Plot, tempstressCD$Colony)

CDtransplantaov <- aov(CD ~ Date2 * Site + Error(Repeat), data = tempstressCD)
summary(CDtransplantaov)

model = lme(CD ~ Date2, random=~1|Repeat,data=tempstressCD)
summary(glht(model,linfct=mcp(Date2="Tukey")))

CDTrans <- ggplot(tempstressCD, aes(Date2, CD, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("#414181","ivory", "#f06f46"))+
  scale_color_manual(values = c("#414181","ivory", "#f06f46"))+
  xlab("")+
  ylab("Cell density (cells/cm2)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
CDTrans

grid.arrange(CDTrans +
               theme(axis.text.x = element_blank()),
             SSCTrans+
               theme(axis.text.x = element_blank()), 
             SSCsdTrans+
               theme(axis.text.x = element_blank()),
             REDTrans+
               theme(axis.text.x = element_blank()), 
             REDsdTrans+
               theme(axis.text.x = element_blank()), 
             GRNTrans+
               theme(axis.text.x = element_blank()), 
             GRNsdTrans+
               theme(axis.text.x = element_blank()), 
             FSCTrans+
               theme(axis.text.x = element_blank()), 
             FSCsdTrans+
               theme(axis.text.x = element_blank()), 
             ncol = 3)

####Long term stress
library(lubridate)
permstress <- df6[df6$Date > ymd("2022-08-01"), ]
class(permstress$Date)
permstress$Date2 <- permstress$Date
permstress$Date2<-as.character(permstress$Date2)
head(permstress)
unique(permstress$Date2)
t<-permstress$Date2=="2022-09-01" 
sum(t)
permstress$Date2[t]<-'15 months after transplant'
s<-permstress$Date2== "2022-08-22"
sum(s)
permstress$Date2[s] <- '15 months after transplant'
permstress$Site <- factor(permstress$Site, levels=c('Luminao','East','West')) 

permstressCD <- CD2[CD2$Date > ymd("2022-08-01"), ]
class(permstressCD$Date)
permstressCD$Date2 <- permstressCD$Date
permstressCD$Date2<-as.character(permstressCD$Date2)
head(permstressCD)
unique(permstressCD$Date2)
t<-permstressCD$Date2=="2022-09-01" 
sum(t)
permstressCD$Date2[t]<-'15 months after transplant'
s<-permstressCD$Date2== "2022-08-22"
sum(s)
permstressCD$Date2[s] <- '15 months after transplant'
permstressCD$Site
permstressCD$Site <- factor(permstressCD$Site, levels=c('Luminao','East','West')) 

CDpermstressaov <- aov(CD ~ Site, data = permstressCD)
summary(CDpermstressaov)
TukeyHSD(CDpermstressaov)

CDTrans2 <- ggplot(permstressCD, aes(Site, CD, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("ivory","#414181", "#f06f46"))+
  scale_color_manual(values = c("ivory", "#414181","#f06f46"))+
  xlab("")+
  ylab("Cell density (cells/cm2)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
CDTrans2

SSCpermstressaov <- aov(SSC ~ Site, data = permstress)
summary(SSCpermstressaov)
#TukeyHSD(SSCpermstressaov)

SSCTrans2 <- ggplot(permstress, aes(Site, SSC, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("ivory","#414181", "#f06f46"))+
  scale_color_manual(values = c("ivory", "#414181","#f06f46"))+
  xlab("")+
  ylab("Side scatter (mean)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
SSCTrans2

SSCsdpermstressaov <- aov(SSCsd ~ Site, data = permstress)
summary(SSCsdpermstressaov)
TukeyHSD(SSCsdpermstressaov)

SSCsdTrans2 <- ggplot(permstress, aes(Site, SSCsd, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("ivory","#414181", "#f06f46"))+
  scale_color_manual(values = c("ivory", "#414181","#f06f46"))+
  xlab("")+
  ylab("Side scatter (s.d.)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
SSCsdTrans2

REDpermstressaov <- aov(RED ~ Site, data = permstress)
summary(REDpermstressaov)
TukeyHSD(REDpermstressaov)

REDTrans2 <- ggplot(permstress, aes(Site, RED, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("ivory","#414181", "#f06f46"))+
  scale_color_manual(values = c("ivory", "#414181","#f06f46"))+
  xlab("")+
  ylab("Red fluor. (mean)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
REDTrans2

REDsdpermstressaov <- aov(REDsd ~ Site, data = permstress)
summary(REDsdpermstressaov)
TukeyHSD(REDsdpermstressaov)

REDsdTrans2 <- ggplot(permstress, aes(Site, REDsd, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("ivory","#414181", "#f06f46"))+
  scale_color_manual(values = c("ivory", "#414181","#f06f46"))+
  xlab("")+
  ylab("Red fluor. (s.d.)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
REDsdTrans2

GRNpermstressaov <- aov(GRN ~ Site, data = permstress)
summary(GRNpermstressaov)

GRNTrans2 <- ggplot(permstress, aes(Site, GRN, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("ivory","#414181", "#f06f46"))+
  scale_color_manual(values = c("ivory", "#414181","#f06f46"))+
  xlab("")+
  ylab("Green fluor. (mean)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
GRNTrans2

GRNsdpermstressaov <- aov(GRNsd ~ Site, data = permstress)
summary(GRNsdpermstressaov)
TukeyHSD(GRNsdpermstressaov)

GRNsdTrans2 <- ggplot(permstress, aes(Site, GRNsd, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("ivory","#414181", "#f06f46"))+
  scale_color_manual(values = c("ivory", "#414181","#f06f46"))+
  xlab("")+
  ylab("Green fluor. (s.d.)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
GRNsdTrans2

FSCpermstressaov <- aov(FSC ~ Site, data = permstress)
summary(FSCpermstressaov)
TukeyHSD(FSCpermstressaov)

FSCTrans2 <- ggplot(permstress, aes(Site, FSC, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("ivory","#414181", "#f06f46"))+
  scale_color_manual(values = c("ivory", "#414181","#f06f46"))+
  xlab("")+
  ylab("Forward scatter (mean)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
FSCTrans2

FSCsdpermstressaov <- aov(FSCsd ~ Site, data = permstress)
summary(FSCsdpermstressaov)

FSCsdTrans2 <- ggplot(permstress, aes(Site, FSCsd, fill = Site, group = interaction(Site, Date2)))+
  geom_boxplot(size = 0.5)+
  scale_fill_manual(values = c("ivory","#414181", "#f06f46"))+
  scale_color_manual(values = c("ivory", "#414181","#f06f46"))+
  xlab("")+
  ylab("Forward scatter (s.d.)")+
  theme_classic2() + theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
FSCsdTrans2

permpanel <- grid.arrange(CDTrans2 +
                            theme(axis.text.x = element_blank()),
                          REDTrans2+
                            theme(axis.text.x = element_blank()), 
                          REDsdTrans2+
                            theme(axis.text.x = element_blank()),
                          GRNTrans2+
                            theme(axis.text.x = element_blank()), 
                          GRNsdTrans2+
                            theme(axis.text.x = element_blank()), 
                          FSCTrans2+
                            theme(axis.text.x = element_blank()), 
                          FSCsdTrans2+
                            theme(axis.text.x = element_blank()), 
                          SSCTrans2+
                            theme(axis.text.x = element_blank()), 
                          SSCsdTrans2+
                            theme(axis.text.x = element_blank()),ncol = 3)
temppanel <- grid.arrange(CDTrans +
                            theme(axis.text.x = element_blank()),
                          REDTrans+
                            theme(axis.text.x = element_blank()), 
                          REDsdTrans+
                            theme(axis.text.x = element_blank()), 
                          GRNTrans+
                            theme(axis.text.x = element_blank()), 
                          GRNsdTrans+
                            theme(axis.text.x = element_blank()),
                          FSCTrans+
                            theme(axis.text.x = element_blank()), 
                          FSCsdTrans+
                            theme(axis.text.x = element_blank()), 
                          SSCTrans+
                            theme(axis.text.x = element_blank()), 
                          SSCsdTrans+
                            theme(axis.text.x = element_blank()),
                          ncol = 3)
grid.arrange(temppanel, permpanel, ncol = 2, widths = c(2,1))

####Multivariate statistics####MultivRepeatariate statistics
zzx <- Color2 %>% group_by(Site, Plot, Colony, Date) %>%
  dplyr::summarize(Tint = mean(Tint))
zzz <- merge(CD2, zzx, by = c("Site", "Plot", "Date", "Colony"))

yyy <- merge(zzz, df6, by = c("Site", "Plot", "Date", "Colony"))

View(yyy)

library(ggbiplot)

yyyNANA <- na.omit(yyy)
head(yyyNANA)
theopca <- prcomp(yyyNANA[, c(5,8:15)], center = TRUE, scale. = TRUE)
summary(theopca)

PCA<-ggbiplot(theopca, groups = interaction(yyyNANA$Date, yyyNANA$Site), ellipse = TRUE, alpha = 0.7)+
  theme_classic2()
PCA

my_data <- yyyNANA[, c(5:6,8:15)]
mds <- my_data %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
mds
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2",
          label = interaction(yyyNANA$Date, yyyNANA$Site, yyyNANA$Plot, yyyNANA$Colony),
          size = 1,
          repel = TRUE)

clust <- kmeans(mds, 3)$cluster %>%
  as.factor()

mds <- mds %>%
  mutate(groups = clust)
head(mds)

# Plot and color by groups
CMD <- ggscatter(mds, x = "Dim.1", y = "Dim.2", 
                 label = interaction(yyyNANA$Date, yyyNANA$Site, yyyNANA$Plot, yyyNANA$Colony),
                 color = "groups",
                 palette = "jco",
                 size = 1, 
                 ellipse = TRUE,
                 ellipse.type = "convex",
                 repel = TRUE)
CMD

#Correlation matrix with multidimensional scaling

#OutlierFunction
detect_outlier <- function(x) {
  Quantile1 <- quantile(x, probs=.25)
  Quantile3 <- quantile(x, probs=.75)
  IQR = Quantile3 - Quantile1
  x > Quantile3 + (IQR * 1.5) | x < Quantile1 - (IQR * 1.5)
}
remove_outlier <- function(dataframe, columns = names(dataframe)) {
  for (col in columns) {
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  print("Remove outliers")
  print(dataframe)
}
head(yyyNANA)
yyyNANAOL <- remove_outlier(yyyNANA, c('CD', 'Tint', 'RED', 'REDsd', 'GRN', 'GRNsd', 'FSC', 'FSCsd', 'SSC', 'SSCsd'))
my_data <- yyyNANAOL[, c(5:6,8:15)]

res.cor <- cor(my_data, method = "pearson")
mds.cor <- (1-res.cor) %>%
  cmdscale() %>%
  as_tibble()
colnames(mds.cor) <- c("Dim.1", "Dim.2")

mdscorrelation <- ggscatter(mds.cor, x = "Dim.1", y = "Dim.2", 
                            size = 1,
                            label = colnames(res.cor),
                            repel = FALSE)
mdscorrelation
library("PerformanceAnalytics")
head(yyyNANA)
chart.Correlation(my_data, histogram=TRUE, pch=19, method = "pearson")

#Biotic Monitoring Data
library(readxl)
MonitoringDatabase_Acropora <- read_excel("D:/AcroGarden/MonitoringDatabase_Acropora.xlsx", 
                                          col_types = c("text", "text", "text", 
                                                        "text", "numeric", "text", "date", 
                                                        "text", "text", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "text", "skip"))
head(MonitoringDatabase_Acropora)
MDA <- MonitoringDatabase_Acropora
class(MDA$Date)
MDA$Date <- as.Date(MDA$Date)
DisPlot <- ggplot()+
  geom_smooth(data = MDA, aes(x = Date, y = Dis_Tot, group = interaction(Plot, Site), color = Site), span = 0.8, linewidth = 1.2, alpha = 0.6, se = F)+
  #geom_smooth(data = df6, aes(x = Date, y = RED, color = Site), span =0.9)+
  geom_jitter(data = MDA, aes(x = Date, y = Dis_Tot, group = interaction(Plot, Site, Colony, Rep), color = Site), size = 2, alpha = 0.5) +
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())
ggplot()+
  geom_line(data = MDA, aes(x = Date, y = Dis_Tot, group = interaction(Plot, Site, Colony, Rep), color = Site), linewidth = 1.2, alpha = 0.1)+
  geom_smooth(data = MDA, aes(x = Date, y = Dis_Tot, group = interaction(Plot, Site), color = Site), span = 0.8, linewidth = 1.2, alpha = 0.6, se = T)+
  #geom_point(data = MDA, aes(x = Date, y = PM, group = interaction(Plot, Site, Colony, Rep), color = Site), size = 2) +
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())

MDA<- subset(MDA, PM != "NA")
PMPlot <- ggplot()+
  geom_smooth(data = MDA, aes(x = Date, y = PM, group = interaction(Plot, Site), color = Site), span = 0.8, linewidth = 1.2, alpha = 0.6, se = F)+
  #geom_smooth(data = df6, aes(x = Date, y = RED, color = Site), span =0.9)+
  geom_jitter(data = MDA, aes(x = Date, y = PM, group = interaction(Plot, Site, Colony, Rep), color = Site), size = 2, alpha = 0.5) +
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())
ggplot()+
  geom_line(data = MDA, aes(x = Date, y = PM, group = interaction(Plot, Site, Colony, Rep), color = Site), linewidth = 1.2, alpha = 0.1)+
  geom_smooth(data = MDA, aes(x = Date, y = PM, group = interaction(Plot, Site), color = Site), span = 0.8, linewidth = 1.2, alpha = 0.6, se = T)+
  #geom_point(data = MDA, aes(x = Date, y = PM, group = interaction(Plot, Site, Colony, Rep), color = Site), size = 2) +
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())
EpiPlot <- ggplot()+
  geom_smooth(data = MDA, aes(x = Date, y = EPI, group = interaction(Plot, Site), color = Site), span = 0.8, linewidth = 1.2, alpha = 0.6, se = F)+
  #geom_smooth(data = df6, aes(x = Date, y = RED, color = Site), span =0.9)+
  geom_jitter(data = MDA, aes(x = Date, y = EPI, group = interaction(Plot, Site, Colony, Rep), color = Site), size = 2, alpha = 0.5) +
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())
ggplot()+
  geom_line(data = MDA, aes(x = Date, y = EPI, group = interaction(Plot, Site, Colony, Rep), color = Site), linewidth = 1.2, alpha = 0.1)+
  geom_smooth(data = MDA, aes(x = Date, y = EPI, group = interaction(Plot, Site), color = Site), span = 0.8, linewidth = 1.2, alpha = 0.6, se = T)+
  #geom_point(data = MDA, aes(x = Date, y = PM, group = interaction(Plot, Site, Colony, Rep), color = Site), size = 2) +
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())
library(readxl)
Predation <- read_excel("D:/AcroGarden/AcroGarden/GardenAcroLong_Condensed.xlsx", 
                        sheet = "Predation")

library(ggpubr)
Predation$Date <- as.Date(Predation$Date)
predplot <- ggplot()+
  geom_smooth(data = Predation, aes(x = Date, y = Predation, group = interaction(Plot, Site), color = Site), span = 0.8, linewidth = 1.2, alpha = 0.6, se = F)+
  #geom_smooth(data = df6, aes(x = Date, y = RED, color = Site), span =0.9)+
  geom_jitter(data = Predation, aes(x = Date, y = Predation, group = interaction(Plot, Site, Colony, Rep), color = Site), size = 2, alpha = 0.5) +
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())
predplot

library(stats)
kruskal.test(Predation ~ Date, data = Predation)
kruskal.test(Predation ~ Site, data = Predation)
friedman.test(y = Predation$Predation, groups = Predation$Date, blocks = Predation$Colony)
Color2$Date <- as.Date(Color2$Date)
ggplot()+
  geom_smooth(data = Color2, aes(x = Date, y = Tint, group = interaction(Plot, Site), color = Site), span = 0.8, linewidth = 1.2, alpha = 0.6, se = F)+
  #geom_smooth(data = df6, aes(x = Date, y = RED, color = Site), span =0.9)+
  geom_jitter(data = Color2, aes(x = Date, y = Tint, group = interaction(Plot, Site, Colony), color = Site), size = 2, alpha = 0.5) +
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())
ggplot()+
  geom_line(data = Color2, aes(x = Date, y = Tint, group = interaction(Plot, Site, Colony, Rep), color = Site), linewidth = 1.2, alpha = 0.1)+
  geom_smooth(data = Color2, aes(x = Date, y = Tint, color = Site), linewidth = 1.5, span =0.5)+
  geom_point(data = Color2, aes(x = Date, y = Tint, group = interaction(Plot, Site, Colony, Rep), color = Site), size = 2) +
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  scale_x_date(date_labels="%m-%Y", date_breaks = "1 month")+
  theme_classic2()+
  theme(legend.position = "none", axis.title.x=element_blank())
kruskal.test(Predation$Predation, Predation$Site)
kruskal.test(Predation$Predation, Predation$Date)

library(gridExtra)
p1 <- grid.arrange(PMPlot, DisPlot, ncol =2)
grid.arrange(p1, predplot, nrow = 2)                     

#Correlation matrix with multidimensional scaling
my_data <- yyyNANA[, c(5:6,8:15)]

PR <- subset(Predation, Rep == "R")
head(PR)
aaa <- merge(yyyNANA, PR, by = c("Site", "Plot", "Date", "Colony"))

#aaa$Predation[aaa$Predation == 'N'] <- '0'
#aaa$Predation[aaa$Predation == 'Y'] <- '1'
#aaa$Predation<-as.numeric(aaa$Predation)
library(rstatix)

ggboxplot(aaa, x = "Predation", y = "RED",
          color = "Predation", size = 1.3,
          add = "jitter",
          facet.by = "Site", short.panel.labs = FALSE)+ 
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)))
  )+
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  theme_classic2()

ggboxplot(aaa, x = "Predation", y = "REDsd",
          color = "Predation", size = 1.3,
          add = "jitter",
          facet.by = "Site", short.panel.labs = FALSE)+ 
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)))
  )+
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  theme_classic2()

ggboxplot(aaa, x = "Predation", y = "GRN",
          color = "Predation", size = 1.3,
          add = "jitter",
          facet.by = "Site", short.panel.labs = FALSE)+ 
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)))
  )+
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  theme_classic2()

ggboxplot(aaa, x = "Predation", y = "GRNsd",
          color = "Predation", size = 1.3,
          add = "jitter",
          facet.by = "Site", short.panel.labs = FALSE)+ 
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)))
  )+
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  theme_classic2()

ggboxplot(aaa, x = "Predation", y = "FSC",
          color = "Predation", size = 1.3,
          add = "jitter",
          facet.by = "Site", short.panel.labs = FALSE)+ 
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)))
  )+
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  theme_classic2()

ggboxplot(aaa, x = "Predation", y = "FSCsd",
          color = "Predation", size = 1.3,
          add = "jitter",
          facet.by = "Site", short.panel.labs = FALSE)+ 
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)))
  )+
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  theme_classic2()

ggboxplot(aaa, x = "Predation", y = "SSC",
          color = "Predation", size = 1.3,
          add = "jitter",
          facet.by = "Site", short.panel.labs = FALSE)+ 
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)))
  )+
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  theme_classic2()

ggboxplot(aaa, x = "Predation", y = "SSCsd",
          color = "Predation", size = 1.3,
          add = "jitter",
          facet.by = "Site", short.panel.labs = FALSE)+ 
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)))
  )+
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  theme_classic2()

ggboxplot(aaa, x = "Predation", y = "Tint",
          color = "Predation", size = 1.3,
          add = "jitter",
          facet.by = "Site", short.panel.labs = FALSE)+ 
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)))
  )+
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  theme_classic2()
head(aaa)
#zzx = color
aac <- merge(CD2, PR, by = c("Site", "Plot", "Date", "Colony"))

allp<-ggboxplot(aac, x = "Predation", y = "CD",
                color = "Predation", size = 1.3,
                add = "jitter",
                facet.by = "Site", short.panel.labs = FALSE)+ 
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)))
  )+
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  theme_classic2()

aab <- subset(aac, Date == "2021-06-16")
btp <- ggboxplot(aab, x = "Predation", y = "CD",
                 color = "Predation", size = 1.3,
                 add = "jitter",
                 facet.by = "Site", short.panel.labs = FALSE)+ 
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)))
  )+
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  theme_classic2()
aad <- subset(aac, Date != "2021-06-16")

atp<-ggboxplot(aad, x = "Predation", y = "CD",
               color = "Predation", size = 1.3,
               add = "jitter",
               facet.by = "Site", short.panel.labs = FALSE)+ 
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)))
  )+
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  theme_classic2()

atpbtp <- grid.arrange(btp, atp)
grid.arrange(allp, atpbtp, ncol =2)
head(zzx)



bbb <- merge(Color2, Predation, by = c("Site", "Plot", "Date", "Colony", "Rep"))
head(bbb)
allpc<-ggboxplot(bbb, x = "Predation", y = "Tint",
                 color = "Predation", size = 1.3,
                 add = "jitter",
                 facet.by = "Site", short.panel.labs = FALSE)+ 
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)))
  )+
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  theme_classic2()
allpc

bba <- subset(bbb, Date == "2021-06-16")
btpc <- ggboxplot(bba, x = "Predation", y = "Tint",
                  color = "Predation", size = 1.3,
                  add = "jitter",
                  facet.by = "Site", short.panel.labs = FALSE)+ 
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)))
  )+
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  theme_classic2()
bbd <- subset(bbb, Date != "2021-06-16")

atpc<-ggboxplot(bbd, x = "Predation", y = "Tint",
                color = "Predation", size = 1.3,
                add = "jitter",
                facet.by = "Site", short.panel.labs = FALSE)+ 
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)))
  )+
  scale_fill_manual(values = c("#414181",  "#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  theme_classic2()

atpbtp <- grid.arrange(btpc, atpc)
grid.arrange(allpc, atpbtp, ncol =2)

