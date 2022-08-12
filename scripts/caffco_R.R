library(here)
library(tidyverse)
library(stringr)

#load in the IGT data
df_CaffCo <- read.csv(here("dissertation", "caffco_data.csv"))

#deleting unnecesary columns 
df_CaffCo <- df_CaffCo %>% 
  select(-UTC.Timestamp, -UTC.Date, -Local.Timestamp, 
         -Local.Timezone, -Local.Date, -Experiment.ID, 
         -Experiment.Version, -Repeat.Key, -Schedule.ID, 
         -Participant.Starting.Group, -Participant.Status, 
         -Participant.Completion.Code, -Participant.External.Session.ID, 
         -Participant.Device.Type, -Participant.Device, -Participant.OS, 
         -Participant.Browser, -Participant.Monitor.Size, -Participant.Viewport.Size, 
         -Checkpoint, -Task.Version, -Tree.Node.Key, -Randomise.questionnaire.elements.)

#filtering for the quantity responses
df_quant <- df_CaffCo[grep("Never|< once a month|1-3 a month|Once a week|2-4 per week|
                            5-6 per week|Once a day|2-3 per day|4-5 per day|
                            6+ per day|Less than once a month|1-3 times a month|
                            2-4 times a week|5-6 times a week|2-3 times a day|
                            4-5 times a day|6+ times a day",df_CaffCo$Response), ]


#adding a new column with the caffeine quantities 
df_quant$milligrams <- c(31, 57, 3, 4.7, 83, 100, 120, 210, 1.9, 120, 10, 40, 
                         30, 120, 2, 25, 35, 60, 35, 42, 84, 80, 75, 105, 150, 45, 
                         52, 200, 75, 50, 100, 200)

#multiplier
rep_str <- c("Never" = "0", "< once a month" = "0.0333","less than once a month" = "0.0333",
             "1-3 a month" = "0.0667", "1-3 times a month" = "0.0667", "Once a week" = "0.143", 
             "2-4 per week" = "0.429", "2-4 times a week" = "0.429", "5-6 times a week" = "0.786", 
             "5-6 per week" = "0.786", "Once a day" = "1", "2-3 times a day" = "2.5", 
             "2-3 per day" = "2.5", "4-5 per day" = "4.5", "4-5 times a day" = "4.5", "6+ per day" = "6", 
             "6+ times a day" = "6")

df_quant$multiplier <- str_replace_all(df_quant$Response, rep_str)

#converting to numeric
df_quant$milligrams <- as.numeric(df_quant$milligrams)
df_quant$multiplier <- as.numeric(df_quant$multiplier)

#estimating daily caffeine intake 
df_quant$daily_caff <- df_quant$milligrams*df_quant$multiplier

part_num_dailycaff <- sum(df_quant$daily_caff)
