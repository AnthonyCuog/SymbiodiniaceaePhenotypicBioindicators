####Now modeling####
library(readr)
hobofullmeancorrected <- read_csv("hobofullmeancorrected.csv", 
                                  col_types = cols(Date = col_date(format = "%Y-%m-%d")))
View(hobofullmeancorrected)

unique(hobofullmeancorrected$Plot)

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
unique(df6$Plot)
unique(df6$Site)

Luminao <- subset(df6, Site == "Luminao")
Luminao$Plot[Luminao$Plot == 'A'] <- '34'
Luminao$Plot[Luminao$Plot == 'B'] <- '34'
Luminao$Plot[Luminao$Plot == 'C'] <- '34'
Luminao$Plot[Luminao$Plot == 'D'] <- '34'

Garden <- subset(df6, Site != "Luminao")
df6 <- rbind(Garden, Luminao)

hobofullmeancorrected$Plot[hobofullmeancorrected$Plot == 'EA'] <- 'A'
hobofullmeancorrected$Plot[hobofullmeancorrected$Plot == 'WA'] <- 'A'
hobofullmeancorrected$Plot[hobofullmeancorrected$Plot == 'EB'] <- 'B'
hobofullmeancorrected$Plot[hobofullmeancorrected$Plot == 'WB'] <- 'B'

library(readxl)
Color <- read_excel("GardenAcroLong_Condensed.xlsx", 
                    sheet = "ColorData")
head(Color)
ColorTrust <- subset(Color, Trust == "Yes")
Color2 <- ColorTrust %>%
  group_by(Site, Plot, Colony, Date, Rep) %>%
  dplyr::summarize(Tint = mean(Intensity))
head(Color2)

colortemp<-merge(Color2, hobofullmeancorrected, by = c("Site", "Plot", "Date"))

colortemp <- subset(colortemp, Date != "2021-06-16")
EastColor <- subset(colortemp, Site == "East")
WestColor <- subset(colortemp, Site == "West")

CD <- read_excel("GardenAcroLong_Condensed.xlsx", 
                 sheet = "AllGarden")

CD2 <- CD %>%
  group_by(Site, Plot, Colony, Date) %>%
  dplyr::summarize(CD = mean(CellDensity))

CD2 <- as.data.frame(CD2)
View(CD2)
CD2$Plot[CD2$Plot == 'ABAB'] <- '34'
CD2$Plot[CD2$Plot == 'C'] <- '34'
CD2$Plot[CD2$Plot == 'D'] <- '34'
unique(CD2$Plot)

phenotemp<-merge(df6, hobofullmeancorrected, by = c("Site", "Plot", "Date"))
head(phenotemp)

phenotemp3 <- subset(phenotemp, Date != "2021-06-16")

write.csv(colortemp, file = "Bayesian/colortemp.csv")
write.csv(CD2, file = "Bayesian/CD2.csv")
write.csv(phenotemp3, file = "Bayesian/phenotemp3.csv")

#### Bayesian modelling straight####
library (gbm); library (brms); 
library('rstan');library('parallel');library('rstanarm'); 

library(readr)
CD2 <- read_csv("Bayesian/CD2.csv", col_types = cols(Colony = col_character(), 
                                                     Date = col_date(format = "%Y-%m-%d")))
head(CD2)

colortemp <- read_csv("Bayesian/colortemp.csv", 
                      col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                       Colony = col_character()))
head(colortemp)

phenotemp3 <- read_csv("Bayesian/phenotemp3.csv", 
                       col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                        Colony = col_character()))
head(phenotemp3)

####GAM####
library(mgcv)

TempTint <- colortemp %>% group_by(Site, Plot, meantemp, mintemp) %>%
  dplyr::summarise(Tint = mean(Tint))

Tintgam <- gam(Tint ~ s(mintemp, bs = "cs", fx = TRUE, k = 3), data = colortemp)
summary(Tintgam)
Tintgam <- gam(Tint ~ s(meantemp, bs = "cs", fx = TRUE, k = 3), data = colortemp)
summary(Tintgam)
Tintgam <- gam(Tint ~ s(maxtemp, bs = "cs", fx = TRUE, k = 3), data = colortemp)
summary(Tintgam)
Tintgam <- gam(Tint ~ s(range, bs = "cs", fx = TRUE, k = 3), data = colortemp)
summary(Tintgam)
Tintlm <- lm(Tint ~ mintemp, data = colortemp)
summary(Tintlm)
Tintlm <- lm(Tint ~ meantemp, data = colortemp)
summary(Tintlm)
Tintlm <- lm(Tint ~ maxtemp, data = colortemp)
summary(Tintlm)
Tintlm <- lm(Tint ~ range, data = colortemp)
summary(Tintlm)

minmean <- lm(hobofullmeancorrected$meantemp ~ hobofullmeancorrected$mintemp)                  
summary(minmean)

TintGAM <- ggplot()+
  geom_line(data = TempTint, aes(meantemp, Tint, group = interaction(Site,Plot), color = Site))+
  #geom_smooth(data = Gonzalo, aes(mintemp, Tint), method = "lm", color = "black")+
  scale_fill_manual(values = c("#414181","#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  geom_smooth(data = TempTint, aes(meantemp, Tint),method='gam',formula=y ~ s(x, bs = "cs", fx = TRUE, k = 3), color = "black")+
  geom_point()+
  theme_classic2()+
  theme(legend.position = "none")
TintGAM

CDtemp <- merge(CD2, hobofullmeancorrected, by = c("Site", "Plot", "Date"))
CDtemp <- subset(CDtemp, Date != "2021-06-16")
CDtemp <- subset(CDtemp, Site != "Luminao")
TempCD <- CDtemp %>% group_by(Site, Plot, meantemp, mintemp) %>%
  dplyr::summarise(CD = mean(CD))

CDgam <- gam(CD ~ s(mintemp, bs = "cs", fx = TRUE, k = 3), data = TempCD)
summary(CDgam)
CDgam <- gam(CD ~ s(meantemp, bs = "cs", fx = TRUE, k = 3), data = TempCD)
summary(CDgam)
CDlm <- lm(CD ~ mintemp, data = TempCD)
summary(CDlm)
CDlm <- lm(CD ~ meantemp, data = TempCD)

CDGAM <- ggplot()+
  geom_line(data = TempCD, aes(meantemp, CD, group = interaction(Site, Plot), color = Site))+
  #geom_smooth(data = Gonzalo, aes(mintemp, CD), method = "lm", color = "black")+
  scale_fill_manual(values = c("#414181","#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  geom_smooth(data = TempCD, aes(meantemp, CD), method='gam',formula=y ~ s(x, bs = "cs", fx = TRUE, k = 3), color = "black")+
  #geom_point()+
  theme_classic2()+
  theme(legend.position = "none")
CDGAM

phenotemp3 <- subset(phenotemp3, Site != "Luminao")
Means <- phenotemp3 %>% group_by(Site, Plot, meantemp, mintemp) %>%
  dplyr::summarise(RED = mean (RED),
                   REDsd = mean(REDsd),
                   GRN = mean(GRN),
                   GRNsd= mean(GRNsd),
                   FSC = mean(FSC),
                   FSCsd = mean(FSCsd),
                   SSC = mean(SSC),
                   SSCsd = mean(SSCsd))

REDgam <- gam(RED ~ s(mintemp, bs = "cs", fx = TRUE, k = 3), data = phenotemp3)
summary(REDgam)
REDgam <- gam(RED ~ s(meantemp, bs = "cs", fx = TRUE, k = 3), data = phenotemp3)
summary(REDgam)
REDlm <- lm(RED ~ mintemp, data = phenotemp3)
summary(REDlm)
REDlm <- lm(RED ~ meantemp, data = phenotemp3)
summary(REDlm)

REDGAM <- ggplot()+
  geom_line(data = Means, aes(meantemp, RED, group = interaction(Site, Plot), color = Site))+
  scale_fill_manual(values = c("#414181","#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  #geom_smooth(method='gam',formula=y ~ s(x, bs = "cs", fx = TRUE, k = 3))+
  #geom_point()+
  theme_classic2()+
  theme(legend.position = "none")
REDGAM

REDsdgam <- gam(REDsd ~ s(mintemp, bs = "cs", fx = TRUE, k = 3), data = phenotemp3)
summary(REDsdgam)
REDsdgam <- gam(REDsd ~ s(meantemp, bs = "cs", fx = TRUE, k = 3), data = phenotemp3)
summary(REDsdgam)
REDsdlm <- lm(REDsd ~ mintemp, data = phenotemp3)
summary(REDsdlm)
REDsdlm <- lm(REDsd ~ meantemp, data = phenotemp3)
summary(REDsdlm)

REDsdGAM <- ggplot()+
  geom_line(data = Means, aes(meantemp, REDsd, group = interaction(Site, Plot), color = Site))+
  scale_fill_manual(values = c("#414181","#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  geom_smooth(data = phenotemp3, aes(meantemp, REDsd), method='gam',formula=y ~ s(x, bs = "cs", fx = TRUE, k = 3), color = "black")+
  #geom_point()+
  theme_classic2()+
  theme(legend.position = "none")
REDsdGAM

GRNgam <- gam(GRN ~ s(mintemp, bs = "cs", fx = TRUE, k = 3), data = phenotemp3)
summary(GRNgam)
GRNgam <- gam(GRN ~ s(meantemp, bs = "cs", fx = TRUE, k = 3), data = phenotemp3)
summary(GRNgam)
GRNlm <- lm(GRN ~ mintemp, data = phenotemp3)
summary(GRNlm)
GRNlm <- lm(GRN ~ meantemp, data = phenotemp3)
summary(GRNlm)

GRNGAM <- ggplot()+
  geom_line(data = Means, aes(meantemp, GRN, group = interaction(Site, Plot), color = Site))+
  scale_fill_manual(values = c("#414181","#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  geom_smooth(data = phenotemp3, aes(meantemp, GRN), method='gam',formula=y ~ s(x, bs = "cs", fx = TRUE, k = 3), color = "black")+
  #geom_point(data = Gonzalo, aes(mintemp, GRN))+
  theme_classic2()+
  theme(legend.position = "none")
GRNGAM

GRNsdgam <- gam(GRNsd ~ s(meantemp, bs = "cs", fx = TRUE, k = 3), data = phenotemp3)
summary(GRNsdgam)
GRNsdgam <- gam(GRNsd ~ s(mintemp, bs = "cs", fx = TRUE, k = 3), data = phenotemp3)
summary(GRNsdgam)
GRNsdlm <- lm(GRNsd ~ meantemp, data = Gonzalo)
summary(GRNsdlm)
GRNsdlm <- lm(GRNsd ~ mintemp, data = Gonzalo)
summary(GRNsdlm)

GRNsdGAM <- ggplot()+
  geom_line(data = Means, aes(meantemp, GRNsd, group = interaction(Site, Plot), color = Site))+
  scale_fill_manual(values = c("#414181","#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  #geom_smooth(data = phenotemp3, aes(meantemp, GRNsd), method='gam',formula=y ~ s(x, bs = "cs", fx = TRUE, k = 3), color = "black")+
  theme_classic2()+
  theme(legend.position = "none")
GRNsdGAM

FSCgam <- gam(FSC ~ s(meantemp, bs = "cs", fx = TRUE, k = 3), data = phenotemp3)
summary(FSCgam)
FSCgam <- gam(FSC ~ s(mintemp, bs = "cs", fx = TRUE, k = 3), data = phenotemp3)
summary(FSCgam)
FSClm <- lm(FSC ~ meantemp, data = phenotemp3)
summary(FSClm)
FSClm <- lm(FSC ~ mintemp, data = phenotemp3)
summary(FSClm)

FSCGAM <- ggplot()+
  geom_line(data = Means, aes(meantemp, FSC, group = interaction(Site, Plot), color = Site))+
  scale_fill_manual(values = c("#414181","#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  geom_smooth(data = phenotemp3, aes(meantemp, FSC), method='gam',formula=y ~ s(x, bs = "cs", fx = TRUE, k = 3), color = "black")+
  theme_classic2()+
  theme(legend.position = "none")
FSCGAM


FSCsdgam <- gam(FSCsd ~ s(meantemp, bs = "cs", fx = TRUE, k = 3), data = phenotemp3)
summary(FSCsdgam)
FSCsdgam <- gam(FSCsd ~ s(mintemp, bs = "cs", fx = TRUE, k = 3), data = phenotemp3)
summary(FSCsdgam)
FSCsdlm <- lm(FSCsd ~ meantemp, data = phenotemp3)
summary(FSCsdlm)
FSCsdlm <- lm(FSCsd ~ mintemp, data = phenotemp3)
summary(FSCsdlm)

FSCsdGAM <- ggplot()+
  geom_line(data = Means, aes(meantemp, FSCsd, group = interaction(Site, Plot), color = Site))+
  scale_fill_manual(values = c("#414181","#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  geom_smooth(data = phenotemp3, aes(meantemp, FSCsd), method='gam',formula=y ~ s(x, bs = "cs", fx = TRUE, k = 3), color = "black")+
  theme_classic2()+
  theme(legend.position = "none")
FSCsdGAM

SSCgam <- gam(SSC ~ s(meantemp, bs = "cs", fx = TRUE, k = 3), data = phenotemp3)
summary(SSCgam)
SSCgam <- gam(SSC ~ s(mintemp, bs = "cs", fx = TRUE, k = 3), data = phenotemp3)
summary(SSCgam)
SSClm <- lm(SSC ~ meantemp, data = phenotemp3)
summary(SSClm)
SSClm <- lm(SSC ~ mintemp, data = phenotemp3)
summary(SSClm)

SSCGAM <- ggplot()+
  geom_line(data = Means, aes(meantemp, SSC, group = interaction(Site, Plot), color = Site))+
  scale_fill_manual(values = c("#414181","#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  #geom_smooth(data = Gonzalo, aes(mintemp, GRNsd), method='gam',formula=y ~ s(x, bs = "cs", fx = TRUE, k = 3), color = "black")+
  theme_classic2()+
  theme(legend.position = "none")
SSCGAM

SSCsdgam <- gam(SSCsd ~ s(meantemp, bs = "cs", fx = TRUE, k = 3), data = phenotemp3)
summary(SSCsdgam)
SSCsdgam <- gam(SSCsd ~ s(mintemp, bs = "cs", fx = TRUE, k = 3), data = phenotemp3)
summary(SSCsdgam)
SSCsdlm <- lm(SSCsd ~ meantemp, data = phenotemp3)
summary(SSCsdlm)
SSCsdlm <- lm(SSCsd ~ mintemp, data = phenotemp3)
summary(SSCsdlm)

SSCsdGAM <- ggplot()+
  geom_line(data = Means, aes(meantemp, SSCsd, group = interaction(Site, Plot), color = Site))+
  scale_fill_manual(values = c("#414181","#f06f46"))+
  scale_color_manual(values = c("#414181", "#f06f46"))+
  geom_smooth(data = phenotemp3, aes(meantemp, SSCsd), method='gam',formula=y ~ s(x, bs = "cs", fx = TRUE, k = 3), color = "black")+
  theme_classic2()+
  theme(legend.position = "none")
SSCsdGAM

grid.arrange(TintGAM, CDGAM, REDGAM, REDsdGAM, GRNGAM, GRNsdGAM, FSCGAM, FSCsdGAM, SSCGAM, SSCsdGAM, nrow = 2)


#install.packages("MuMIn")
library(MuMIn)
library(lme4)
unique(colortemp$Site)
#Best model
model <- lmer(Tint ~ mintemp + (1|Colony), data = colortemp)
r.squaredGLMM(model)
shapiro.test(residuals(model))
plot(model)
anova(model)
AIC(model)

model <- lmer(Tint ~ meantemp + (1|Colony), data = colortemp)
r.squaredGLMM(model)
shapiro.test(residuals(model))
plot(model)
anova(model)
AIC(model)

model2 <- lmer(Tint ~ meantemp + (1|Colony) + (1|Site), data = colortemp)
r.squaredGLMM(model2)
shapiro.test(residuals(model2))
plot(model2)
anova(model2)
AIC(model2)

model3 <- lmer(Tint ~ meantemp + (1|Site), data = colortemp)
r.squaredGLMM(model3)
shapiro.test(residuals(model3))
plot(model3)
anova(model3)
AIC(model3)

#fails to converge
#model4 <- lmer(Tint ~ meantemp + (1+meantemp|Colony), data = colortemp)
#r.squaredGLMM(model4)
#shapiro.test(residuals(model4))
#plot(model4)
#anova(model4)
#AIC(model4)

#already have better model (lmer above), but want to double check temp variable
modellm <- lm(Tint ~ meantemp, data = colortemp)
r.squaredGLMM(modellm)
shapiro.test(residuals(modellm))
residuals(modellm)
plot(modellm)

#PAIRING DATA FOR BAYESIAN
colortempR <-  subset(colortemp, Rep != "L")

unique(colortempR$Date)
unique(df6$Date)
unique(Gonzalo$Date)
View(phenotemp3)
Gonzalo <- merge(x = df6, y = colortempR, by = c("Site", "Plot", "Date", "Colony"))
Gonzalo <- merge(x = Gonzalo, y = CD2, by = c("Site", "Plot", "Date", "Colony"))
head(Gonzalo)

ggplot(Gonzalo, aes(Date, Tint, color = interaction(Site,Plot)))+
  geom_point()
View(Gonzalo)
#See if models before and after pairing are similar (we lose a lot of data)
modellm <- lm(Tint ~ meantemp  + (1|Colony), data = Gonzalo)
r.squaredGLMM(modellm)

####BAYESIAN MODELS####
####Bayesian models to test contribution to cold-water bleaching####
library(brms)
#BayeslineMEAN <- brm(Tint ~ 1 + meantemp, data = Gonzalo,
#                  control = list(adapt_delta = 0.95, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayeslineMEAN, file="BayesianOutputs/MeanTemperature/BayeslineMEAN.RData")
load("BayesianOutputs/MeanTemperature/BayeslineMEAN.RData")

#BayesCDMEAN <- brm(Tint ~ 1 + meantemp + CD, data = Gonzalo,
#                                   control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesCDMEAN, file="BayesianOutputs/MeanTemperature/BayesCDMEAN.RData")
load("BayesianOutputs/MeanTemperature/BayesCDMEAN.RData")

#BayesREDMEAN <- brm(Tint ~ 1 + meantemp + RED, data = Gonzalo,
#                                      control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesREDMEAN, file="BayesianOutputs/MeanTemperature/BayesREDMEAN.RData")
load("BayesianOutputs/MeanTemperature/BayesREDMEAN.RData")

#BayesREDsdMEAN <- brm(Tint ~ 1 + meantemp + REDsd, data = Gonzalo,
#                    control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesREDsdMEAN, file="BayesianOutputs/MeanTemperature/BayesREDsdMEAN.RData")
load("BayesianOutputs/MeanTemperature/BayesREDsdMEAN.RData")

#BayesGRNMEAN <- brm(Tint ~ 1 + meantemp + GRN, data = Gonzalo,
#                  control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesGRNMEAN, file="BayesianOutputs/MeanTemperature/BayesGRNMEAN.RData")
load("BayesianOutputs/MeanTemperature/BayesGRNMEAN.RData")

#BayesGRNsdMEAN <- brm(Tint ~ 1 + meantemp + GRNsd, data = Gonzalo,
#                    control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesGRNsdMEAN, file="BayesianOutputs/MeanTemperature/BayesGRNsdMEAN.RData")
load("BayesianOutputs/MeanTemperature/BayesGRNsdMEAN.RData")

#BayesFSCMEAN <- brm(Tint ~ 1 + meantemp + FSC, data = Gonzalo,
#                  control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesFSCMEAN, file="BayesianOutputs/MeanTemperature/BayesFSCMEAN.RData")
load("BayesianOutputs/MeanTemperature/BayesFEFSC.RData")

#BayesFSCsdMEAN <- brm(Tint ~ 1 + meantemp + FSCsd, data = Gonzalo,
#                    control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesFSCsdMEAN, file="BayesianOutputs/MeanTemperature/BayesFSCsdMEAN.RData")
load("BayesianOutputs/MeanTemperature/BayesFSCsdMEAN.RData")

#BayesSSCMEAN <- brm(Tint ~ 1 + meantemp + SSC, data = Gonzalo,
#                  control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesSSCMEAN, file="BayesianOutputs/MeanTemperature/BayesSSCMEAN.RData")
load("BayesianOutputs/MeanTemperature/BayesSSCMEAN.RData")

#BayesSSCsdMEAN <- brm(Tint ~ 1 + meantemp + SSCsd, data = Gonzalo,
#                    control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesSSCsdMEAN, file="BayesianOutputs/MeanTemperature/BayesSSCsdMEAN.RData")                
load("BayesianOutputs/MeanTemperature/BayesSSCsdMEAN.RData")


BayeslineMEAN <- add_criterion(BayeslineMEAN, c("loo"))
BayesCDMEAN<- add_criterion(BayesCDMEAN, c("loo"))
BayesREDMEAN <- add_criterion(BayesREDMEAN, c("loo"))
BayesREDsdMEAN <- add_criterion(BayesREDsdMEAN, c("loo"))
BayesGRNMEAN <- add_criterion(BayesGRNMEAN, c("loo"))
BayesGRNsdMEAN <- add_criterion(BayesGRNsdMEAN, c("loo"))
BayesFSCMEAN <- add_criterion(BayesFSCMEAN, c("loo"))
BayesFSCsdMEAN <- add_criterion(BayesFSCsdMEAN, c("loo"))
BayesSSCMEAN <- add_criterion(BayesSSCMEAN, c("loo"))
BayesSSCsdMEAN <- add_criterion(BayesSSCsdMEAN, c("loo"))
##### Add the others


library(loo)
modelcompareFE <- loo_compare(BayeslineMEAN , BayesCDMEAN, BayesREDMEAN, BayesREDsdMEAN,
                              BayesGRNMEAN, BayesGRNsdMEAN, BayesFSCMEAN, BayesFSCsdMEAN, 
                              BayesSSCMEAN, BayesSSCsdMEAN, criterion = "loo") %>% 
  print(simplify = F)

bayes_R2(BayeslineMEAN)
bayes_R2(BayesCDMEAN) # The second model explaining more the TINT variability
bayes_R2(BayesREDMEAN)
bayes_R2(BayesREDsdMEAN)
bayes_R2(BayesGRNMEAN) # The model explaining more the TINT variability
bayes_R2(BayesGRNsdMEAN)
bayes_R2(BayesFSCMEAN)
bayes_R2(BayesFSCsdMEAN)
bayes_R2(BayesSSCMEAN)
bayes_R2(BayesSSCsdMEAN)

# Compare model weights (up to 7 times more weight into fit 5)
(mw <- model_weights(BayeslineMEAN , BayesCDMEAN, BayesREDMEAN, BayesREDsdMEAN,
                     BayesGRNMEAN, BayesGRNsdMEAN, BayesFSCMEAN, BayesFSCsdMEAN, 
                     BayesSSCMEAN, BayesSSCsdMEAN, weights = "waic"))


#Modeled linear relationships
ceCD <- conditional_effects(BayesCDMEAN,categorical = F, prob = c(0.95), method = c("fitted"))
plotCD <- plot(ceCD, plot = FALSE)[[2]] + theme_classic2()
plotCD

ceRED <- conditional_effects(BayesREDMEAN,categorical = F, prob = c(0.95), method = c("fitted"))
plotRED <- plot(ceRED, plot = FALSE)[[2]] + theme_classic2()
plotRED

ceREDsd <- conditional_effects(BayesREDsdMEAN,categorical = F, prob = c(0.95), method = c("fitted"))
plotREDsd <- plot(ceREDsd, plot = FALSE)[[2]] + theme_classic2()
plotREDsd

ceGRN <- conditional_effects(BayesGRNMEAN,categorical = F, prob = c(0.95), method = c("fitted"))
plotGRN <- plot(ceGRN, plot = FALSE)[[2]] + theme_classic2()
plotGRN

ceGRNsd <- conditional_effects(BayesGRNsdMEAN,categorical = F, prob = c(0.95), method = c("fitted"))
plotGRNsd <- plot(ceGRNsd, plot = FALSE)[[2]] + theme_classic2()
plotGRNsd

ceFSC <- conditional_effects(BayesFSCMEAN,categorical = F, prob = c(0.95), method = c("fitted"))
plotFSC <- plot(ceFSC, plot = FALSE)[[2]] + theme_classic2()
plotFSC

ceFSCsd <- conditional_effects(BayesFSCsdMEAN,categorical = F, prob = c(0.95), method = c("fitted"))
plotFSCsd <- plot(ceFSCsd, plot = FALSE)[[2]] + theme_classic2()
plotFSCsd

ceSSC <- conditional_effects(BayesSSCMEAN,categorical = F, prob = c(0.95), method = c("fitted"))
plotSSC <- plot(ceSSC, plot = FALSE)[[2]] + theme_classic2()
plotSSC

ceSSCsd <- conditional_effects(BayesSSCsdMEAN,categorical = F, prob = c(0.95), method = c("fitted"))
plotSSCsd <- plot(ceSSCsd, plot = FALSE)[[2]] + theme_classic2()
plotSSCsd

grid.arrange(plotGRN+scale_y_reverse(),
             plotCD+scale_y_reverse(), plotREDsd+scale_y_reverse(), 
             plotSSC+scale_y_reverse(), plotFSCsd+scale_y_reverse(), 
             plotFSC+scale_y_reverse(), plotRED+scale_y_reverse(),  
             plotSSCsd+scale_y_reverse(), plotGRNsd+scale_y_reverse())

#### BAYES WITH COLONY RANDOM EFFECT (High divergence)###
#Bayesline <- brm(Tint ~ 1 + meantemp + (1|Colony), data = Gonzalo,
#                 control = list(adapt_delta = 0.95, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 4)

#save(Bayesline, file="~/BayeslineCE.RData")
load(file="~/BayeslineCE.RData")

#BayesCDCE <- brm(Tint ~ 1 + meantemp + CD + (1+CD|Colony), data = Gonzalo,
#               control = list(adapt_delta = 0.95, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 4)

save(BayesCD, file="~/BayesCDCE.RData")
load(file="~/BayesCDCE.RData")
#Standard setting weren't ideal, many divergent transitions. Models don't indicate any meaningful relationship
#BayesRED <- brm(Tint ~ 1 + meantemp + RED + (1+RED|Colony), data = Gonzalo,
#                control = list(adapt_delta = 0.99, max_treedepth = 35), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 4)

#save(BayesRED, file="~/BayesREDCE.RData")
load(file="~/BayesREDCE.RData")

#BayesREDsd <- brm(Tint ~ 1 + meantemp + REDsd + (1+REDsd|Colony), data = Gonzalo,
#                  control = list(adapt_delta = 0.975, max_treedepth = 35), family = gaussian(), warmup = 5000, iter =10000, chains = 2, cores = 4)

#save(BayesREDsd, file="~/BayesREDsdCE.RData")
load(file="~/BayesREDsdCE.RData")

#BayesGRN <- brm(Tint ~ 1 + meantemp + GRN + (1+GRN|Colony), data = Gonzalo,
#                control = list(adapt_delta = 0.99, max_treedepth = 35), family = gaussian(), warmup = 5000, iter =10000, chains = 4, cores = 2)

#save(BayesGRN, file="~/BayesGRNCE.RData")
load(file="~/BayesGRNCE.RData")

#BayesGRNsd <- brm(Tint ~ 1 + meantemp + GRNsd + (1+GRNsd|Colony), data = Gonzalo,
#                  control = list(adapt_delta = 0.975, max_treedepth = 35), family = gaussian(), warmup = 5000, iter =10000, chains = 4, cores = 2)

#save(BayesGRNsd, file="~/BayesGRNsdCE.RData")
load(file="~/BayesGRNsdCE.RData")

#BayesFSC <- brm(Tint ~ 1 + meantemp + FSC + (1+FSC|Colony), data = Gonzalo,
#                control = list(adapt_delta = 0.99, max_treedepth = 35), family = gaussian(), warmup = 5000, iter =10000, chains = 4, cores = 2)

#save(BayesFSC, file="~/BayesFSCCE.RData")
load(file="~/BayesFSCCE.RData")

#BayesFSCsd <- brm(Tint ~ 1 + meantemp + FSCsd + (1+FSCsd|Colony), data = Gonzalo,
#                  control = list(adapt_delta = 0.975, max_treedepth = 35), family = gaussian(), warmup = 5000, iter =10000, chains = 4, cores = 2)

#save(BayesFSCsd, file="~/BayesFSCsdCE.RData")
load(file="~/BayesFSCsdCE.RData")

#BayesSSC <- brm(Tint ~ 1 + meantemp + SSC + (1+SSC|Colony), data = Gonzalo,
#                control = list(adapt_delta = 0.975, max_treedepth = 35), family = gaussian(), warmup = 5000, iter =10000, chains = 2, cores = 2)

#save(BayesSSC, file="~/BayesSSCCE.RData")
load(file="~/BayesSSCCE.RData")

#BayesSSCBayesSSCsd <- brm(Tint ~ 1 + meantemp + SSCsd + (1+SSCsd|Colony), data = Gonzalo,
#                          control = list(adapt_delta = 0.975, max_treedepth = 35), family = gaussian(), warmup = 5000, iter =10000, chains = 2, cores = 2)

#save(BayesSSCsd, file="~/BayesSSCsdCE.RData")                
load(file="~/BayesSSCsdCE.RData")

# condiitional effects
#ce3 <- conditional_effects(BayesRED,categorical = F, 
#                           probs = c(0.05, 0.95), method = c("fitted"))

#plotRED <- plot(ce3, plot = FALSE)[[1]] +theme_classic2()
#plotRED
#grid.arrange(plot_general,plotGRN,plotRED) 

#Bayesline <- add_criterion(Bayesline, c("loo", "waic"))
#BayesCD <- add_criterion(BayesCD, c("loo", "waic"))
#BayesGRN <- add_criterion(BayesGRN, c("loo", "waic"))
#BayesGRNsd <- add_criterion(BayesGRNsd, c("loo", "waic"))
#BayesRED <- add_criterion(BayesRED, c("loo", "waic"))
#BayesREDsd <- add_criterion(BayesREDsd, c("loo", "waic"))
#BayesFSC <- add_criterion(BayesFSC, c("loo", "waic"))
#BayesFSCsd <- add_criterion(BayesFSCsd, c("loo", "waic"))
#BayesSSC <- add_criterion(BayesSSC, c("loo", "waic"))
#BayesSSCsd <- add_criterion(BayesSSCsd, c("loo", "waic"))

#modelcompare<-loo_compare(Bayesline, BayesCD, BayesGRN, BayesGRNsd, BayesRED, BayesREDsd, 
#                          BayesFSC, BayesFSCsd,BayesSSC, BayesSSCsd,criterion = "loo") %>% print(simplify = F)
#summary(modelcompare)
#write.csv(modelcompare, file = "Bayesion/modelcomparCE.csv")

# Conditional effects per 
bayes_R2(Bayesline)
bayes_R2(BayesCD)
bayes_R2(BayesGRN)
bayes_R2(BayesFERED)
bayes_R2(BayesFEFSC)
bayes_R2(BayesFESSC)
bayes_R2(BayesFEREDsd)
bayes_R2(BayesFEFSCsd)
bayes_R2(BayesFESSCsd)

#Calculate model weights
mw <- model_weights(Bayesline,  BayesRED, BayesREDsd, BayesGRN, 
                    BayesGRNsd, BayesCD, BayesFSC, BayesFSCsd, BayesSSC, BayesSSCsd, weights = "waic")
mw

#### BAYES WITHOUT COLONY RANDOM EFFECT and MODELED WITH MIN TEMP ####

#Bayesline0 <- brm(Tint ~ 1 + mintemp, data = Gonzalo,
#                  control = list(adapt_delta = 0.95, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(Bayesline0, file="BayesianOutputs/MinimumTemperature/Bayesline0.RData")

load("BayesianOutputs/MinimumTemperature/Bayesline0.RData")
summary(Bayesline0)

#BayesFEBayesFElineBayesFECD <- brm(Tint ~ 1 + mintemp + CD, data = Gonzalo,
#                                   control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesFEBayesFElineBayesFECD, file="BayesianOutputs/MinimumTemperature/BayesFECD.RData")
load("BayesianOutputs/MinimumTemperature/BayesFECD.RData")

summary(BayesFEBayesFElineBayesFECD)
#Standard setting weren't ideal, many divergent transitions. Models don't indicate any meaningful relationship
#BayesFERED <- brm(Tint ~ 1 + mintemp + RED, data = Gonzalo,
#                  control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesFERED, file="BayesianOutputs/MinimumTemperature/BayesFERED.RData")
load("BayesianOutputs/MinimumTemperature/BayesFERED.RData")

#BayesFEREDsd <- brm(Tint ~ 1 + mintemp + REDsd, data = Gonzalo,
#                    control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesFEREDsd, file="BayesianOutputs/MinimumTemperature/BayesFEREDsd.RData")
load("BayesianOutputs/MinimumTemperature/BayesFEREDsd.RData")

#BayesFEGRN <- brm(Tint ~ 1 + mintemp + GRN, data = Gonzalo,
#                  control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesFEGRN, file="BayesianOutputs/MinimumTemperature/BayesFEGRN.RData")
load("BayesianOutputs/MinimumTemperature/BayesFEGRN.RData")

#BayesFEGRNsd <- brm(Tint ~ 1 + mintemp + GRNsd, data = Gonzalo,
#                    control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesFEGRNsd, file="BayesianOutputs/MinimumTemperature/BayesFEGRNsd.RData")
load("BayesianOutputs/MinimumTemperature/BayesFEGRNsd.RData")

#BayesFEFSC <- brm(Tint ~ 1 + mintemp + FSC, data = Gonzalo,
#                  control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesFEFSC, file="BayesianOutputs/MinimumTemperature/BayesFEFSC.RData")
load("BayesianOutputs/MinimumTemperature/BayesFEFSC.RData")

#BayesFEFSCsd <- brm(Tint ~ 1 + mintemp + FSCsd, data = Gonzalo,
#                    control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesFEFSCsd, file="BayesianOutputs/MinimumTemperature/BayesFEFSCsd.RData")
load("BayesianOutputs/MinimumTemperature/BayesFEFSCsd.RData")

#BayesFESSC <- brm(Tint ~ 1 + mintemp + SSC, data = Gonzalo,
#                  control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesFESSC, file="BayesianOutputs/MinimumTemperature/BayesFESSC.RData")
load("BayesianOutputs/MinimumTemperature/BayesFESSC.RData")

#BayesFESSCsd <- brm(Tint ~ 1 + mintemp + SSCsd, data = Gonzalo,
#                    control = list(adapt_delta = 0.975, max_treedepth = 20), family = gaussian(), warmup = 3000, iter =7000, chains = 2, cores = 2)

#save(BayesFESSCsd, file="BayesianOutputs/MinimumTemperature/BayesFESSCsd.RData")                
load("BayesianOutputs/MinimumTemperature/BayesFESSCsd.RData")


Bayesline0 <- add_criterion(Bayesline0, c("loo"))
BayesFEBayesFElineBayesFECD<- add_criterion(BayesFEBayesFElineBayesFECD, c("loo"))
BayesFERED <- add_criterion(BayesFERED, c("loo"))
BayesFEREDsd <- add_criterion(BayesFEREDsd, c("loo"))
BayesFEGRN <- add_criterion(BayesFEGRN, c("loo"))
BayesFEGRNsd <- add_criterion(BayesFEGRNsd, c("loo"))
BayesFEFSC <- add_criterion(BayesFEFSC, c("loo"))
BayesFEFSCsd <- add_criterion(BayesFEFSCsd, c("loo"))
BayesFESSC <- add_criterion(BayesFESSC, c("loo"))
BayesFESSCsd <- add_criterion(BayesFESSCsd, c("loo"))
##### Add the others


library(loo)
modelcompareFE <- loo_compare(Bayesline0, BayesFEBayesFElineBayesFECD,BayesFERED, BayesFEREDsd, BayesFEGRN, 
                              BayesFEGRNsd, BayesFEFSC, BayesFEFSCsd, BayesFESSC, BayesFESSCsd, criterion = "loo") %>% 
  print(simplify = F)

bayes_R2(Bayesline0)
bayes_R2(BayesFEBayesFElineBayesFECD) # The second model explaining more the TINT variability
bayes_R2(BayesFEGRN) # The model explaining more the TINT variability
bayes_R2(BayesFERED)
bayes_R2(BayesFEFSC) # The third model explaining more the TINT variability
bayes_R2(BayesFESSC)
bayes_R2(BayesFEREDsd)
bayes_R2(BayesFEFSCsd)
bayes_R2(BayesFESSCsd)
bayes_R2(BayesFEGRNsd)

# Compare model weights (up to 7 times more weight into fit 5)
(mw <- model_weights(Bayesline0,  BayesFERED, BayesFEREDsd, BayesFEGRN, 
                     BayesFEGRNsd, BayesFEBayesFElineBayesFECD, BayesFEFSC, BayesFEFSCsd, BayesFESSC, BayesFESSCsd, weights = "waic"))
(mw <- model_weights(Bayesline0, BayesFEBayesFElineBayesFECD, BayesFERED, BayesFEREDsd, BayesFEGRN, 
                     BayesFEGRNsd, BayesFEFSC, BayesFEFSCsd, BayesFESSC, BayesFESSCsd, weights = "loo"))
# Within all the phenotypes, BAYESFEGRN model weights 80% of all the models
# In other words, it is the more sensible to TINT


write.csv(modelcompareFE, file = "Bayesion/modelcomparFE.csv")

