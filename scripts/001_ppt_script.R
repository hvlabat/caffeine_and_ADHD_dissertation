#Load libraries
library(here)
library(tidyverse)

#Loading participant info (ppt) data
df_ppt <- read.csv(here("data","raw","gorilla","ppt_groups.csv"))

#Loading ASRS data for all experiment versions
df_asrs <- read.csv(here("data","raw","gorilla","short","v34","short_v34_asrs.csv"))
df_asrs <- bind_rows(head(df_asrs,-1),read.csv(here("data","raw","gorilla","short","v33","short_v33_asrs.csv")))
df_asrs <- bind_rows(head(df_asrs,-1),read.csv(here("data","raw","gorilla","short","v31","short_v31_asrs.csv")))
df_asrs <- head(df_asrs,-1)

#Cleaning up df
df_asrs <- df_asrs %>%
  select(Participant.Public.ID,
         response.1.1.quantised,
         response.1.2.quantised,
         response.1.3.quantised,
         response.1.4.quantised,
         response.1.5.quantised,
         response.1.6.quantised,
         response.1.7.quantised,
         response.1.8.quantised,
         response.1.9.quantised,
         response.1.10.quantised,
         response.1.11.quantised,
         response.1.12.quantised,
         response.1.13.quantised,
         response.1.14.quantised,
         response.1.15.quantised,
         response.1.16.quantised,
         response.1.17.quantised,
         response.1.18.quantised) %>%
  rename(ppt=Participant.Public.ID)

#Removing 'p' in front of ppt
df_asrs$ppt <- gsub("p","",df_asrs$ppt)
df_asrs$ppt <- as.numeric(df_asrs$ppt)

#Ordering per ppt
df_asrs <- df_asrs[order(df_asrs$ppt),]

#Arranging ASRS scores to be 0-4 instead of 1-5
df_asrs[,c(2:19)] <- df_asrs[,c(2:19)]-1

#Calculating ASRS score per participant
df_asrs$asrs <- NA

for (i in c(1:nrow(df_asrs))){
  df_asrs$asrs[i] <- sum(df_asrs[i,c(2:19)])
}

#Finding mean and SD
mean(df_asrs$asrs)
sd(df_asrs$asrs)

#Plotting histogram to visualise data
p <- ggplot(df_asrs,aes(x=asrs))

p+geom_density(kernel="gaussian")

#Tidying df_asrs
df_asrs <- select(df_asrs,ppt,asrs)

#Merging asrs and ppt dfs
df_ppt <- merge(df_ppt,df_asrs)

#visualising asrs against sex, group, and age to identify any significant deviations
p1 <- ggplot(df_ppt,aes(x=sex,y=asrs))
p2 <- ggplot(df_ppt,aes(x=group,y=asrs))
p3 <- ggplot(df_ppt,aes(x=age,y=asrs))

p1+geom_boxplot()
p2+geom_boxplot()
p3+geom_point()+
  geom_smooth(method=lm)