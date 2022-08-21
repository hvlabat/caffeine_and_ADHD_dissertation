#Load libraries
library(here)
library(tidyverse)
library(ggpubr)

#Loading RVP pre-dose data for all experiment versions
df_rvp_pre <- read.csv(here("data","raw","gorilla","long","v34","long_v34_pre_rvp.csv"))
df_rvp_pre <- bind_rows(head(df_rvp_pre,-1),read.csv(here("data","raw","gorilla","long","v33","long_v33_pre_rvp.csv")))
df_rvp_pre <- bind_rows(head(df_rvp_pre,-1),read.csv(here("data","raw","gorilla","long","v31","long_v31_pre_rvp.csv")))
df_rvp_pre <- head(df_rvp_pre,-1)

#Cleaning up df
df_rvp_pre <- df_rvp_pre %>%
  select(Event.Index,
         Participant.Public.ID,
         Zone.Name,
         Zone.Type,
         Reaction.Time,
         Response,
         Attempt,
         Correct,
         Incorrect,display) %>%
  rename(ppt=Participant.Public.ID,
         react_time=Reaction.Time)

#Removing 'p' in front of ppt
df_rvp_pre$ppt <- gsub("p","",df_rvp_pre$ppt)

df_rvp_pre$ppt <- as.numeric(df_rvp_pre$ppt)

#Ordering results by ppt
df_rvp_pre <- df_rvp_pre[order(df_rvp_pre$ppt),]

#Filtering for the test responses
df_rvp_pre <- df_rvp_pre[grep("test",df_rvp_pre$display), ]

#Confirming that there are 735 stimuli per test
stim_num <- nrow(df_rvp_pre[grep("63",df_rvp_pre$ppt), ])


#Calculating mean response time (RT) (for correct responses), RT-variability (RTV),
#total omission error (OE), comission error (CE), response sensitivity (RS),
#and response bias (RB)
#for pre-break RVP per ppt

#First making blank sections in df_ppt
df_ppt$rvp_pre_RT <- NA
df_ppt$rvp_pre_RTV <- NA #note, RTV is a recording of standard deviation
df_ppt$rvp_pre_CC <- NA
df_ppt$rvp_pre_CO <- NA
df_ppt$rvp_pre_OE <- NA
df_ppt$rvp_pre_CE <- NA
df_ppt$rvp_pre_RS <- NA
df_ppt$rvp_pre_RB <- NA


#Creating separate df for RT
df_rvp_pre_RT <- df_rvp_pre[grep(1,df_rvp_pre$Correct),] #RT has same data as CC
df_rvp_pre_CO <- df_rvp_pre_RT[grep("^nogo$",df_rvp_pre_RT$Response),] #making CO group
df_rvp_pre_RT <- df_rvp_pre_RT[grep("^go$",df_rvp_pre_RT$Response),]

#for loop to calculate mean RT and RTV
for (i in c(1:nrow(df_ppt))){
  df_ppt$rvp_pre_RT[i] <- mean(df_rvp_pre_RT[grep(paste("^",i,"$",sep=""),df_rvp_pre_RT$ppt),"react_time"])
  df_ppt$rvp_pre_RTV[i] <- sd(df_rvp_pre_RT[grep(paste("^",i,"$",sep=""),df_rvp_pre_RT$ppt),"react_time"])
}



#Calculating RS & RB requires calculation of probability of a hit (h) and false positive (f)
#This requires information about correct comissions (CC) and omissions (CO)
# h = CC/(CC + OE)
# f = CE/(CE + CO)
#Then, RS = 0.5 + [(h − f) + (h − f)^2]/[4 × h × (1 − f)] , formula based on A' from Sahgal et al. 1987
# RB = [(h − h^2) − (f − f^2)]/[(h − h^2) + (f − f^2)] , formula based on B" from Sahgal et al. 1987
#Thus, these CC,CO,OE and CE must first be calculated

#df_rvp_pre_RT contains the CC data, and was used to make the CO data

#Creating separate dfs for OE and CE
df_rvp_pre_OE <- df_rvp_pre[grep(1,df_rvp_pre$Incorrect),]
df_rvp_pre_CE <- df_rvp_pre_OE[grep("^go$",df_rvp_pre_OE$Response),]
df_rvp_pre_OE <- df_rvp_pre_OE[grep("^nogo$",df_rvp_pre_OE$Response),]

#for loop to calculate sum of scores per ppt
for (i in c(1:nrow(df_ppt))){
  df_ppt$rvp_pre_CC[i] <- sum(df_rvp_pre_RT[grep(paste("^",i,"$",sep=""),df_rvp_pre_RT$ppt),"Correct"])
  df_ppt$rvp_pre_CO[i] <- sum(df_rvp_pre_CO[grep(paste("^",i,"$",sep=""),df_rvp_pre_CO$ppt),"Correct"])
  df_ppt$rvp_pre_CE[i] <- sum(df_rvp_pre_CE[grep(paste("^",i,"$",sep=""),df_rvp_pre_CE$ppt),"Incorrect"])
  df_ppt$rvp_pre_OE[i] <- sum(df_rvp_pre_OE[grep(paste("^",i,"$",sep=""),df_rvp_pre_OE$ppt),"Incorrect"])
  
  #Calculating h & f variables to create RS & RB
  h <- df_ppt$rvp_pre_CC[i]/(df_ppt$rvp_pre_CC[i]+df_ppt$rvp_pre_OE[i])
  f <- df_ppt$rvp_pre_CE[i]/(df_ppt$rvp_pre_CE[i]+df_ppt$rvp_pre_CO[i])
  
  #Calculating RS and RB
  df_ppt$rvp_pre_RS[i] <- (0.5+((h-f)+(h-f)^2)/(4*h*(1-f)))
  df_ppt$rvp_pre_RB[i] <- (((h-h^2)-(f-f^2))/((h-h^2)+(f-f^2)))
}







#Repeating the above, but for post-break rvp scores
#Loading RVP post-dose data for all experiment versions
df_rvp_post <- read.csv(here("data","raw","gorilla","long","v34","long_v34_post_rvp.csv"))
df_rvp_post <- bind_rows(head(df_rvp_post,-1),read.csv(here("data","raw","gorilla","long","v33","long_v33_post_rvp.csv")))
df_rvp_post <- bind_rows(head(df_rvp_post,-1),read.csv(here("data","raw","gorilla","long","v31","long_v31_post_rvp.csv")))
df_rvp_post <- head(df_rvp_post,-1)

#Cleaning up df
df_rvp_post <- df_rvp_post %>%
  select(Event.Index,
         Participant.Public.ID,
         Zone.Name,
         Zone.Type,
         Reaction.Time,
         Response,
         Attempt,
         Correct,
         Incorrect,display) %>%
  rename(ppt=Participant.Public.ID,
         react_time=Reaction.Time)

#Removing 'p' in front of ppt
df_rvp_post$ppt <- gsub("p","",df_rvp_post$ppt)

df_rvp_post$ppt <- as.numeric(df_rvp_post$ppt)

#Ordering results by ppt
df_rvp_post <- df_rvp_post[order(df_rvp_post$ppt),]

#Filtering for the test responses
df_rvp_post <- df_rvp_post[grep("test",df_rvp_post$display), ]

#Confirming that there are 735 stimuli per test
stim_num <- nrow(df_rvp_post[grep("63",df_rvp_post$ppt), ])


#Calculating mean response time (RT) (for correct responses), RT-variability (RTV),
#total omission error (OE), comission error (CE), response sensitivity (RS),
#and response bias (RB)
#for post-break RVP per ppt

#First making blank sections in df_ppt
df_ppt$rvp_post_RT <- NA
df_ppt$rvp_post_RTV <- NA #note, RTV is a recording of standard deviation
df_ppt$rvp_post_CC <- NA
df_ppt$rvp_post_CO <- NA
df_ppt$rvp_post_OE <- NA
df_ppt$rvp_post_CE <- NA
df_ppt$rvp_post_RS <- NA
df_ppt$rvp_post_RB <- NA


#Creating separate df for RT
df_rvp_post_RT <- df_rvp_post[grep(1,df_rvp_post$Correct),] #RT has same data as CC
df_rvp_post_CO <- df_rvp_post_RT[grep("^nogo$",df_rvp_post_RT$Response),] #making CO group
df_rvp_post_RT <- df_rvp_post_RT[grep("^go$",df_rvp_post_RT$Response),]

#for loop to calculate mean RT and RTV
for (i in c(1:nrow(df_ppt))){
  df_ppt$rvp_post_RT[i] <- mean(df_rvp_post_RT[grep(paste("^",i,"$",sep=""),df_rvp_post_RT$ppt),"react_time"])
  df_ppt$rvp_post_RTV[i] <- sd(df_rvp_post_RT[grep(paste("^",i,"$",sep=""),df_rvp_post_RT$ppt),"react_time"])
}



#Calculating RS & RB requires calculation of probability of a hit (h) and false positive (f)
#This requires information about correct comissions (CC) and omissions (CO)
# h = CC/(CC + OE)
# f = CE/(CE + CO)
#Then, RS = 0.5 + [(h − f) + (h − f)^2]/[4 × h × (1 − f)] , formula based on A' from Sahgal et al. 1987
# RB = [(h − h^2) − (f − f^2)]/[(h − h^2) + (f − f^2)] , formula based on B" from Sahgal et al. 1987
#Thus, these CC,CO,OE and CE must first be calculated

#df_rvp_post_RT contains the CC data, and was used to make the CO data

#Creating separate dfs for OE and CE
df_rvp_post_OE <- df_rvp_post[grep(1,df_rvp_post$Incorrect),]
df_rvp_post_CE <- df_rvp_post_OE[grep("^go$",df_rvp_post_OE$Response),]
df_rvp_post_OE <- df_rvp_post_OE[grep("^nogo$",df_rvp_post_OE$Response),]

#for loop to calculate sum of scores per ppt
for (i in c(1:nrow(df_ppt))){
  df_ppt$rvp_post_CC[i] <- sum(df_rvp_post_RT[grep(paste("^",i,"$",sep=""),df_rvp_post_RT$ppt),"Correct"])
  df_ppt$rvp_post_CO[i] <- sum(df_rvp_post_CO[grep(paste("^",i,"$",sep=""),df_rvp_post_CO$ppt),"Correct"])
  df_ppt$rvp_post_CE[i] <- sum(df_rvp_post_CE[grep(paste("^",i,"$",sep=""),df_rvp_post_CE$ppt),"Incorrect"])
  df_ppt$rvp_post_OE[i] <- sum(df_rvp_post_OE[grep(paste("^",i,"$",sep=""),df_rvp_post_OE$ppt),"Incorrect"])
  
  #Calculating h & f variables to create RS & RB
  h <- df_ppt$rvp_post_CC[i]/(df_ppt$rvp_post_CC[i]+df_ppt$rvp_post_OE[i])
  f <- df_ppt$rvp_post_CE[i]/(df_ppt$rvp_post_CE[i]+df_ppt$rvp_post_CO[i])
  
  #Calculating RS and RB
  df_ppt$rvp_post_RS[i] <- (0.5+((h-f)+(h-f)^2)/(4*h*(1-f)))
  df_ppt$rvp_post_RB[i] <- (((h-h^2)-(f-f^2))/((h-h^2)+(f-f^2)))
}