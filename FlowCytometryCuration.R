library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
setwd("D:/AcroGarden/Submission/GitHub")
list_of_files <- list.files(path = "FlowCytometry/",
                            recursive = TRUE,
                            pattern = "\\.CSV$",
                            full.names = TRUE)
df <- list_of_files %>%
  set_names() %>% 
  map_df(read_csv, .id = "file_name")

names(df) <- gsub("-", ".", names(df), fixed=TRUE)

df$RED.B.HLog <- as.numeric(df$RED.B.HLog)
head(df)
ggdensity(df, x = "RED.B.HLog", fill = "file_name", rug = FALSE)+
  scale_x_continuous(limits = c(1.5, 5))+
  theme(legend.position = "none")
head(df)

dfaaa <- df
unique(df$file_name)

dfaaa <- subset(df, file_name != "AcroGarden/Flow/FlowCytometry//WestandEast_06_2021/WABEAB_June16_2021_REDONE_2022-09-14_at_04-55-47pm.B05.CSV")
dfaaa <- subset(dfaaa, file_name != "AcroGarden/Flow/FlowCytometry//WestandEast_06_2021/WABEAB_June16_2021_REDONE_2022-09-14_at_04-55-47pm.B09.CSV")
dfaaa <- subset(dfaaa, file_name != "AcroGarden/Flow/FlowCytometry//WestandEast_06_2021/WABEAB_June16_2021_REDONE_2022-09-14_at_04-55-47pm.B10.CSV")
dfaaa <- subset(dfaaa, file_name != "AcroGarden/Flow/FlowCytometry//WestandEast_06_2021/WABEAB_June16_2021_REDONE_2022-09-14_at_04-55-47pm.B11.CSV")
dfaaa <- subset(dfaaa, file_name != "AcroGarden/Flow/FlowCytometry//WestandEast_06_2021/WABEAB_June16_2021_REDONE_2022-09-14_at_04-55-47pm.B12.CSV")
dfaaa <- subset(dfaaa, file_name != "AcroGarden/Flow/FlowCytometry//WestandEast_06_2021/WABEAB_June16_2021_REDONE_2022-09-14_at_04-55-47pm.C04.CSV")
dfaaa <- subset(dfaaa, file_name != "AcroGarden/Flow/FlowCytometry//WestandEast_06_2021/WABEAB_June16_2021_REDONE_2022-09-14_at_04-55-47pm.C05.CSV")
dfaaa <- subset(dfaaa, file_name != "AcroGarden/Flow/FlowCytometry//WestandEast_06_2021/WABEAB_June16_2021_REDONE_2022-09-14_at_04-55-47pm.C06.CSV")
dfaaa <- subset(dfaaa, file_name != "AcroGarden/Flow/FlowCytometry//WestandEast_06_2021/WABEAB_June16_2021_REDONE_2022-09-14_at_04-55-47pm.C07.CSV")
dfaaa <- subset(dfaaa, file_name != "AcroGarden/Flow/FlowCytometry//WestandEast_06_2021/WABEAB_June16_2021_REDONE_2022-09-14_at_04-55-47pm.C08.CSV")
dfaaa <- subset(dfaaa, file_name != "AcroGarden/Flow/FlowCytometry//WestandEast_06_2021/WABEAB_June16_2021_REDONE_2022-09-14_at_04-55-47pm.C09.CSV")
dfaaa <- subset(dfaaa, file_name != "AcroGarden/Flow/FlowCytometry//WestandEast_06_2021/WABEAB_June16_2021_REDONE_2022-09-14_at_04-55-47pm.C10.CSV")
dfaaa <- subset(dfaaa, file_name != "AcroGarden/Flow/FlowCytometry//WestandEast_06_2021/WABEAB_June16_2021_REDONE_2022-09-14_at_04-55-47pm.C11.CSV")
dfaaa <- subset(dfaaa, file_name != "AcroGarden/Flow/FlowCytometry//WestandEast_06_2021/WABEAB_June16_2021_REDONE_2022-09-14_at_04-55-47pm.C12.CSV")

aaa <- dfaaa %>% group_by(file_name) %>%
  mutate(AboveBelow = ifelse(RED.B.HLog>=(density(RED.B.HLog)$x[which(density(RED.B.HLog)$y == (min(density(RED.B.HLog)$y[density(RED.B.HLog)$x < 4 & density(RED.B.HLog)$x > 2])))]), RED.B.HLog, NA))
View(aaa)
unique(aaa$file_name)
aab <- aaa[complete.cases(aaa$AboveBelow),]

ggdensity(aab, x = "RED.B.HLog", fill = "file_name", rug = FALSE)+
  scale_x_continuous(limits = c(1.5, 5))+
  theme(legend.position = "none")
aab <- subset(aab, SSC.HLog>=1)
head(df2)
df2 <- df %>% group_split(file_name)

library(BiocGenerics)
density(df$RED.B.HLog)$x
class(df)
test <- lapply(df2, subset, subset = RED.B.HLog >= (density(RED.B.HLog)$x[which(density(RED.B.HLog)$y == (min(density(RED.B.HLog)$y[density(RED.B.HLog)$x < 1 & density(RED.B.HLog)$x > 5])))]))

testbind <- test %>% bind_rows()
View(testbind)

test2 <- lapply(df2, subset, subset = RED.B.HLog >= (D[which((density(RED.B.HLog)$y) == (min((density(RED.B.HLog)$y)[(density(RED.B.HLog)$x) < 4 & (density(RED.B.HLog)$x) > 3.5])))]))

DensityY <- density(RED.B.HLog)$y
DensityX <- density(RED.B.HLog)$x

a <- subset(df2[[489]], RED.B.HLog>=(density(RED.B.HLog)$x[which(density(RED.B.HLog)$y == (min(density(RED.B.HLog)$y[density(RED.B.HLog)$x < 3.5 & density(RED.B.HLog)$x > 2])))]))
ggdensity(df2[[489]], x = "RED.B.HLog", fill = "file_name", rug = FALSE)+
  scale_x_continuous(limits = c(1.5, 5))+
  theme(legend.position = "none")

dfsym1 <- subset(df, RED.B.HLog>=(density(RED.B.HLog)$x[which(density(RED.B.HLog)$y == (min(density(RED.B.HLog)$y[density(RED.B.HLog)$x < 4 & density(RED.B.HLog)$x > 2])))]))
dfsym1 <- subset(dfsym1, SSC.HLog>=1)
head(dfsym1)
nrow(dfsym1)

ggdensity(dfsym1, x = "RED.B.HLog", rug = TRUE)+
  scale_y_continuous(limits = c(0,0.3))+
  scale_x_continuous(limits = c(1.5, 5))+
  geom_vline(xintercept = density(df$RED.B.HLog)$x[(min(DensityY[DensityX < 4 & DensityX > 2]))])+# Thu Jun 27 16:47:50 2024 ------------------------------
  theme(legend.position = "none")

ggplot(aab, aes(file_name, RED.B.HLog))+
  geom_boxplot(aes(group = file_name))+
  theme_classic2()

filenamesformetadata <- unique(dfsym1$file_name)

#Export unique names to assign variables to
write.table(as.data.frame(filenamesformetadata),file="AcroGarden/Flow/physiometadata2.csv", quote=F,sep=";",row.names=F)

library(readr)
physiometadata <- read_csv("physiometadata.csv", 
                           col_types = cols(Colony = col_character(), 
                                            TechRep = col_character(), Date = col_date(format = "%m/%d/%Y"), 
                                            ...8 = col_skip(), ...9 = col_skip(), 
                                            ...10 = col_skip(), ...11 = col_skip(), 
                                            ...12 = col_skip(), ...13 = col_skip(), 
                                            ...14 = col_skip(), ...15 = col_skip(), 
                                            ...16 = col_skip(), ...17 = col_skip(), 
                                            ...18 = col_skip(), ...19 = col_skip()))
head(physiometadata)

####SecondSubsetOption####

dfsym2 <- aab
dfsym2$file_name <- gsub('//', '/', dfsym2$file_name)
unique(dfsym2$file_name)
df3 <- merge(x = physiometadata, y = dfsym2, by = c("file_name"))
head(df3)

unique(df3$file_name)

df5 <- df3 %>%
  group_by(Site, Plot, Colony, Date, Verified) %>%
  summarize(RED = mean(RED.B.HLog),
            REDvar = var(RED.B.HLog),
            REDsd = sd(RED.B.HLog))

ggplot()+
  geom_line(data = df5, aes(x = Date, y = RED, group = interaction(Plot, Site, Colony, Verified), color = Site))+
  geom_boxplot(data = df3, aes(x = Date, y = RED.B.HLog, group=interaction(Date, Verified, Plot, Site), fill = Site), 
               alpha = 0.8, width = 10, color = "black", size = 1, outlier.shape = NA)+
  scale_fill_manual(values = c("#414181","black",  "#f06f46"))+
  scale_color_manual(values = c("#414181","black", "#f06f46"))+
  theme_classic2()+
  theme(legend.position = "right")

df4 <- subset(df3, Verified == "Yes")
df4 <- subset(df4, GRN.B.HLog>=0.25)
write.table(as.data.frame(df4),file="AcroGarden/Flow/preprocessedflowdata.csv", quote=F,sep=",",row.names=F)
