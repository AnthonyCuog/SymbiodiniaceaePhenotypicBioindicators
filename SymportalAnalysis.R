library(vegan)
library(readr)
library(dplyr)
library(viridis)
#import "seqs.relative.abund_only"
library(readr)
seqs_relative_abund_only <- read_delim("D:/AcroGarden/ITS2/20240202T042351_anthoco1/post_med_seqs/455_20240207T223903_DBV_20240208T064054.seqs.relative.abund_only.txt", 
                                       delim = "\t", escape_double = FALSE, 
                                       trim_ws = TRUE)
View(seqs_relative_abund_only)

relabunid <- subset(seqs_relative_abund_only, C15 <= 0.5)

View(relabun)
# Calculate distance matrix - make distance matrix data frame - write .csv
relabun<-relabunid[-1]
head(relabun)
dismat <- vegdist(relabun, method = "bray")

dismat <- as.matrix(dismat)

# Running NMDS in vegan (metaMDS)
NMS <-
  metaMDS(dismat,
          distance = "bray")
plot(NMS)

PCoA <- wcmdscale(d = dismat, eig = TRUE)

ggplot(data = data.frame(PCoA$points),
       aes(x = Dim1, y = Dim2)) +
  geom_point() +
  theme_bw()

#Bar Graphs and Relative abundance

library(readr)
TypeProfile <- read_delim("D:/AcroGarden/ITS2/20240202T042351_anthoco1/its2_type_profiles/455_20240207T223903_DBV_20240208T064054.profiles.relative.abund_and_meta.txt", 
                          delim = "\t", escape_double = FALSE, skip = 6,
                          trim_ws = TRUE)
head(TypeProfile)

library(reshape2)
pcm = melt(TypeProfile, id.vars = c("Sample","Metadata"))
head(pcm)

pcm$value<-as.double(pcm$value)

library(dplyr)
library(tidyr)
head(pcm)

#contamination removal (C15 is porites profile, 
#given there is a single occurence, it is most likely sample mix-up)

pcm <- subset(pcm, variable != 'C15-C15by-C15er-C15kh')

pcm <- pcm %>% separate(Metadata, c('Site', 'Colony', 'Time'))
head(pcm)
pcm <- subset(pcm, Site != 'NA')
pcm <- subset(pcm, Colony != 'NA')
pcm <- subset(pcm, Time != 'NA')

Source <- subset(pcm, Site == 'Source')
Garden <- subset(pcm, Site != 'Source')

Sourcegg <- ggplot(Source) + 
  geom_bar(aes(x = Colony, fill = variable, y = value),stat = "identity", position = "fill")+
  scale_fill_viridis(option = "D", discrete = TRUE) +
  facet_grid(cols = vars(Time), scale = 'free')+
  theme(legend.position = "none")+
  theme_classic()
Sourcegg

Gardengg <- ggplot(Garden) + 
  geom_bar(aes(x = Colony, fill = variable, y = value),stat = "identity", position = "fill")+
  scale_fill_viridis(option = "D", discrete = TRUE) +
  facet_grid(cols = vars(Time), rows = vars(Site))+
  theme(legend.position = "none")+
  theme_classic()
Gardengg


library(gridExtra)
grid.arrange(Sourcegg, Gardengg, heights = c(1.5,3))

head(relabun)

# Calculate distance matrix - make distance matrix data frame - write .csv
head(TypeProfile)
TypeProfileAbund <- TypeProfile[-1:-2]
TypeProfileAbund <- TypeProfileAbund[-4]
head(TypeProfileAbund)
TypeProfileAbund$`C40-C3-C115-C40z`<- as.double(TypeProfile$`C40-C3-C115-C40z`)
TypeProfileAbund$`C40-C3-C115-C40h-C40e`<- as.double(TypeProfile$`C40-C3-C115-C40h-C40e`)
TypeProfileAbund$`C40-C3-C115-C40h`<- as.double(TypeProfile$`C40-C3-C115-C40h`)
TypeProfileAbund <- na.omit(TypeProfileAbund)


TypeProfile$`C40-C3-C115-C40z`<- as.double(TypeProfile$`C40-C3-C115-C40z`)
TypeProfile$`C40-C3-C115-C40h-C40e`<- as.double(TypeProfile$`C40-C3-C115-C40h-C40e`)
TypeProfile$`C40-C3-C115-C40h`<- as.double(TypeProfile$`C40-C3-C115-C40h`)
TypeProfile <- na.omit(TypeProfile)
head(TypeProfile)

TypeProfile2 <- TypeProfile %>% separate(Metadata, c('Site', 'Colony', 'Time'))
head(TypeProfile2)
TypeProfile2 <- subset(TypeProfile2, Site != 'NA')
TypeProfile2 <- subset(TypeProfile2, Colony != 'NA')
TypeProfile2 <- subset(TypeProfile2, Time != 'NA')

theopca1 <- prcomp(Source[, c(5,6,7)], center = TRUE, scale. = TRUE)
summary(theopca1)
head(TypeProfile2)
library(ggbiplot)
library(viridis)
unique(TypeProfile2$Time)
ggbiplot(theopca1, alpha = 1, size = 2, ellipse = TRUE, groups = TypeProfile2$Site)+
  #scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  #scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  theme_classic()


head(relabunid)
head(TypeProfile2)
metadata <- TypeProfile2[, c(1,2,3,4)]
head(metadata)
relabunid$Sample <- relabunid$sample_uid
head(relabunid)
ra <- relabunid[-1]
head(ra)
ra <- ra[-6]
head(ra)
ra <- merge(ra, metadata, by = "Sample")
head(ra)

ra2 <- melt(ra, id.vars = c("Sample","Site", "Colony", "Time"))
head(ra2)
View(ra2)

####Minority communities only####
ramin <- subset(ra2, variable != "C40")
ramin <- subset(ramin, variable != "C3")
ramin <- subset(ramin, variable != "C115")
ramin <- subset(ramin, variable != "C40h")
ramin <- subset(ramin, variable != "C40z")
ramin <- subset(ramin, variable != "C3ge")
ramin <- subset(ramin, variable != "C115a")
ramin <- subset(ramin, variable != "C40e")
ramin <- subset(ramin, variable != "C40r")
head(ramin)
ramin$variable <- "other"
head(ramin)
ra3 <- subset(ra2, value >= 0.01)
head(ra3)
ra3 <- rbind(ra3, ramin)
ra3$Colony = factor(ra3$Colony,levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9","10","11","12"))
ra3 <- subset(ra3, Colony != 'NA')
ras <- subset(ra3, Site == 'Source')
rag <- subset(ra3, Site != 'Source')


rasgg <- ggplot(ras) + 
  geom_bar(aes(x = Colony, fill = variable, y = value),stat = "identity", position = "fill")+
  scale_fill_viridis(option = "C", discrete = TRUE, direction = -1, end = 0.65) +
  facet_grid(cols = vars(Time), scale = 'free')+
  theme(legend.position = "none")+
  theme_classic()

raggg <- ggplot(rag) + 
  geom_bar(aes(x = Colony, fill = variable, y = value),stat = "identity", position = "fill")+
  scale_fill_viridis(option = "C", discrete = TRUE, direction = -1, begin = 0, end = 0.65) +
  facet_grid(cols = vars(Time), rows = vars(Site))+
  theme(legend.position = "none")+
  theme_classic()


grid.arrange(rasgg, raggg, heights = c(1, 3))

####Minority communities only####
ramin <- subset(ra2, variable != "C40")
ramin <- subset(ramin, variable != "C3")
ramin <- subset(ramin, variable != "C115")
ramin <- subset(ra2, variable != "C40h")
ramin <- subset(ramin, variable != "C40z")
ramin <- subset(ramin, variable != "C3ge")
ramin <- subset(ra2, variable != "C115a")
ramin <- subset(ramin, variable != "C40e")
ramin <- subset(ramin, variable != "C40r")

ggplot(rasmin) + 
  geom_bar(aes(x = Colony, fill = variable, y = value),stat = "identity", position = "fill")+
  scale_fill_viridis(option = "H", discrete = TRUE) +
  facet_grid(cols = vars(Time), scale = 'free')+
  theme(legend.position = "none")+
  theme_classic()

ggplot(ragmin) + 
  geom_bar(aes(x = Colony, fill = variable, y = value),stat = "identity", position = "fill")+
  scale_fill_viridis(option = "H", discrete = TRUE) +
  facet_grid(cols = vars(Time), rows = vars(Site))+
  theme(legend.position = "none")+
  theme_classic()

####Minority communities only####
C40 <- subset(ra2, variable == "C40")
C3 <- subset(ra2, variable == "C3")
C115 <- subset(ra2, variable == "C115")
mean(C40$value)
var(C40$value)

mean(C3$value)
var(C3$value)

mean(C115$value)
var(C115$value)

C40h <- subset(ra2, variable == "C40h")
mean(C40h$value)
var(C40h$value)
C40z <- subset(ra2, variable == "C40z")
mean(C40z$value)
var(C40z$value)
C40e <- subset(ra2, variable == "C40e")
mean(C40e$value)
var(C40e$value)
