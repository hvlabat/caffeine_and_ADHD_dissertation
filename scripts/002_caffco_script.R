library(here)
library(tidyverse)

#Loading caffco pre-dose data for all experiment versions
df_caffco <- read.csv(here("data","raw","gorilla","long","v34","long_v34_caffco.csv"))
df_caffco <- bind_rows(head(df_caffco,-1),read.csv(here("data","raw","gorilla","long","v33","long_v33_caffco.csv")))
df_caffco <- bind_rows(head(df_caffco,-1),read.csv(here("data","raw","gorilla","long","v31","long_v31_caffco.csv")))
df_caffco <- head(df_caffco,-1)

#deleting unnecesary columns 
df_caffco <- df_caffco %>% 
  dplyr::select(-UTC.Timestamp, -UTC.Date, -Local.Timestamp,-Participant.Private.ID, 
         -Local.Timezone, -Local.Date, -Experiment.ID,-Task.Name, 
         -Experiment.Version, -Repeat.Key, -Schedule.ID, 
         -Participant.Starting.Group, -Participant.Status, 
         -Participant.Completion.Code, -Participant.External.Session.ID, 
         -Participant.Device.Type, -Participant.Device, -Participant.OS, 
         -Participant.Browser, -Participant.Monitor.Size, -Participant.Viewport.Size, 
         -Checkpoint, -Task.Version, -Tree.Node.Key, -Randomise.questionnaire.elements.)

#changing three entries because they are not compatible with code
df_caffco[c(2029,14581,18383),4] <- "over 6 per day"

#filtering for the quantity responses
df_quant <- df_caffco[grep("Never|< once a month|1-3 a month|Once a week|2-4 per week|5-6 per week|Once a day|2-3 per day|4-5 per day|Less than once a month|1-3 times a month|2-4 times a week|5-6 times a week|2-3 times a day|4-5 times a day|over 6 per day",
                           df_caffco$Response), ]


#adding a new column with the caffeine quantities 
df_quant$milligrams <- c(31, 57, 3, 4.7, 83, 100, 120, 210, 1.9, 120, 10, 40, 
                         30, 120, 2, 25, 35, 60, 35, 42, 84, 80, 75, 105, 150, 45, 
                         52, 200, 75, 50, 100, 200)
#note there are 32 entries

#multiplier
rep_str <- c("Never" = "0", "< once a month" = "0.0333","Less than once a month" = "0.0333",
             "1-3 a month" = "0.0667", "1-3 times a month" = "0.0667", "Once a week" = "0.143", 
             "2-4 per week" = "0.429", "2-4 times a week" = "0.429", "5-6 times a week" = "0.786", 
             "5-6 per week" = "0.786", "Once a day" = "1", "2-3 times a day" = "2.5", 
             "2-3 per day" = "2.5", "4-5 per day" = "4.5", "4-5 times a day" = "4.5", 
             "over 6 per day" = "6")

df_quant$multiplier <- str_replace_all(df_quant$Response, rep_str)

#converting to numeric
df_quant$milligrams <- as.numeric(df_quant$milligrams)
df_quant$multiplier <- as.numeric(df_quant$multiplier)

#estimating daily caffeine intake 
df_quant$daily_caff <- df_quant$milligrams*df_quant$multiplier

part_num_dailycaff <- sum(df_quant$daily_caff)

#Renaming ppt
df_quant <- df_quant %>% rename(ppt=Participant.Public.ID)

#Removing 'p' in front of ppt
df_quant$ppt <- gsub("p","",df_quant$ppt)
df_quant$ppt <- as.numeric(df_quant$ppt)

#Ordering per ppt
df_quant <- df_quant[order(df_quant$ppt),]


#calculating total caffeine per ppt
df_ppt$daily_caff <- NA

for (i in c(1:nrow(df_ppt))){
  df_ppt$daily_caff[i] <- sum(df_quant[c((1+((i-1)*32)):(i*32)),7])
}

#finding mean and sd of caffeine consumption
mean(df_ppt$daily_caff)
sd(df_ppt$daily_caff)

#Plotting caffeine intake against ASRS
p4 <- ggplot(df_ppt,aes(x=daily_caff,y=asrs))
p4+geom_point()+
  geom_smooth(method=lm)

#T-tests and Chi-square test of 

