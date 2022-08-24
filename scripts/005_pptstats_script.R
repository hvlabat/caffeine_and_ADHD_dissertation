#Load libraries
library(here)
library(tidyverse)
library(ggpubr)
library(R.utils)

#T-tests of:
#age
#asrs
#daily_caff
#pre-break scores
#between groups A and B

for (i in c(5:9,12:15,24,25,28:31)){
  result_tt <- t.test(df_ppt[grep("Placebo",df_ppt$group),i],
                      df_ppt[grep("Caffeine",df_ppt$group),i])
  
  print(ggboxplot(df_ppt,x="group",y=print(colnames(df_ppt[i]))))
  
  if (result_tt$p.value<0.05){
    print(paste("Group scores differ significantly in ",
                colnames(df_ppt[i]),"mean=",mean(df_ppt[,i])))
  }
  else{
    print(paste("Group scores are similar in ",
                colnames(df_ppt[i]),"mean=",mean(df_ppt[,i])))
  }
}

#T-tests and boxplots suggest no significant deviations in all groups


#Chi-squared test of sex to identify deviations in groups A and B
chisq.test(df_ppt[grep("Caffeine",df_ppt$group),4],
           df_ppt[grep("Placebo",df_ppt$group)-1,4]) #removing one row of A due to 1 more ppt than B

ggboxplot(df_ppt,x="group",y="age")
#Chi-squared test suggest no significant deviations in groups

#Finding means and SDs.
mean(df_ppt$age)
sd(df_ppt$age)

mean(df_ppt$daily_caff)
sd(df_ppt$daily_caff)

mean(df_ppt$asrs)
sd(df_ppt$asrs)


#Finding ppt weight from CaffCo questionnaire
df_weights <- df_caffco[grep("response-5-unit|response-5-first|response-5-second",
               df_caffco$Question.Key),]

#Cleaning df
df_weights <- df_weights %>% rename(ppt=Participant.Public.ID)
df_weights <- select(df_weights,ppt,Response)

#Removing 'p' in front of ppt
df_weights$ppt <- gsub("p","",df_weights$ppt)

df_weights$ppt <- as.numeric(df_weights$ppt)

#Ordering results by ppt
df_weights <- df_weights[order(df_weights$ppt),]

#For script to extract weight in kg and change stones/pounds to kg
df_weights_extracted <- data.frame(df_ppt$ppt,df_ppt$group)
df_weights_extracted <- df_weights_extracted %>% rename(ppt=df_ppt.ppt,
                                                        group=df_ppt.group)

for (i in c(1:63)){
  if (df_weights[((i*3)-2),2]=="metric"){
    df_weights_extracted$weight[i] <- as.numeric(df_weights[((i*3)-1),2])
  }
  else if (df_weights[((i*3)-2),2]=="imperial"){
    df_weights_extracted$weight[i] <- (((as.numeric(df_weights[((i*3)-1),2]))*6.35029)+(as.numeric(df_weights[(i*3),2])*0.453592))
  }
}

#Removing zero and NA values
df_weights_extracted <- df_weights_extracted[!isZero(df_weights_extracted$weight),]
df_weights_extracted <- df_weights_extracted[!is.na(df_weights_extracted$weight),]

#T-test of weights
t.test(df_weights_extracted[grep("Placebo",df_weights_extracted$group),3],
       df_weights_extracted[grep("Caffeine",df_weights_extracted$group),3])
#Not significantly different!

mean(df_weights_extracted$weight)
sd(df_weights_extracted$weight)
