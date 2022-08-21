#Load libraries
library(here)
library(tidyverse)
library(ggpubr)
library(lme4)
library(flexplot)

#Creating new df to group scores by:
# Pre- and Post-dosed
# Groups A and B

df_ppt$dosed <- NA

#Order results by pre- and post-dosed
df_pre_full <- select(df_ppt,ppt,ref,sex,age,
                             daily_caff,group,asrs,dosed,
                             contains("pre"))
df_pre_full$dosed <- "Pre"
colnames(df_pre_full) <- c("ppt","ref","sex","age","daily_caff","group","asrs","dosed",
                           "rvp_RT","rvp_RTV","rvp_CC","rvp_CO","rvp_OE",
                           "rvp_CE","rvp_RS","rvp_RB",
                           "tova_RT","tova_RTV","tova_CC","tova_CO","tova_OE",
                           "tova_CE","tova_RS","tova_RB")

df_post_full <- select(df_ppt,ppt,ref,sex,age,
                              daily_caff,group,asrs,dosed,
                              contains("post"))
df_post_full$dosed <- "Post"
colnames(df_post_full) <- c("ppt","ref","sex","age","daily_caff","group","asrs","dosed",
                            "rvp_RT","rvp_RTV","rvp_CC","rvp_CO","rvp_OE",
                            "rvp_CE","rvp_RS","rvp_RB",
                            "tova_RT","tova_RTV","tova_CC","tova_CO","tova_OE",
                            "tova_CE","tova_RS","tova_RB")

#Merging into one
df_grouped <- rbind(df_pre_full,df_post_full)
df_grouped <- df_grouped[order(df_grouped$ppt),]
df_grouped$rvp_RB[is.nan(df_grouped$rvp_RB)] <- mean(!is.nan(df_grouped$rvp_RB))
df_grouped$tova_RB[is.nan(df_grouped$tova_RB)] <- mean(!is.nan(df_grouped$tova_RB))
#Replacements were to get rid of NaN values without affecting mean data


#Testing if data is parametric or not, to determine ideal statistical test
#This first requires df_grouped to be split into each group to be analysed
# E=prE dosed,O=pOst dosed
# A=Group A,B=Group B
#in the split dfs, columns 9 to 24 contain the test scores

df_EA <- df_grouped[grep("Pre",df_grouped$dosed),]
df_EA <- df_EA[grep("A",df_EA$group),]

df_EB <- df_grouped[grep("Pre",df_grouped$dosed),]
df_EB <- df_EB[grep("B",df_EB$group),]

df_OA <- df_grouped[grep("Post",df_grouped$dosed),]
df_OA <- df_OA[grep("A",df_OA$group),]

df_OB <- df_grouped[grep("Post",df_grouped$dosed),]
df_OB <- df_OB[grep("B",df_OB$group),]



#Testing parametry of data
for (i in 9:24){
  result_EA <- shapiro.test(df_EA[,i])
  result_EB <- shapiro.test(df_EB[,i])
  result_OA <- shapiro.test(df_OA[,i])
  result_OB <- shapiro.test(df_OB[,i])
  
  if (result_EA$p.value >= 0.05 &
      result_EB$p.value >= 0.05 &
      result_OA$p.value >= 0.05 &
      result_OB$p.value >= 0.05){
    print(paste(names(df_EA)[i],"is parametric"))
  }
  else{
    print(paste(names(df_EA)[i],"is non-parametric"))
  }
}

#TOVA and RVP RT are both parametric, and can be analysed using three-way ANOVA
#The rest are non-parametric, thus requiring non-parametric tests

#Analysing the RT data for both TOVA and RVP, using three-way ANOVA for each
#RVP RT
full <- aov(rvp_RT ~ group * dosed * asrs,
                  data=df_grouped)

reduced <- aov(rvp_RT ~ group + dosed + asrs,
               data=df_grouped)

model.comparison(full,reduced) #Comparing which model is best-fitting

visualize(reduced,plot="model")

summary(reduced)

#Indicates group and pre/post dose significantly influence the rvp_RT
#There are otherwise no other interactions

#Finding direction of interactions
df_grouped %>% group_by(group) %>% summarise(mean_RT=mean(rvp_RT))
boxplot(rvp_RT ~ group,data=df_grouped)
#Indicates Group B has a significantly lower RT

df_grouped %>% group_by(dosed) %>% summarise(mean_RT=mean(rvp_RT))
boxplot(rvp_RT ~ dosed,data=df_grouped)
#Indicates pre-dose has a significantly lower RT

#TOVA RT
full <- aov(tova_RT ~ group * dosed * asrs,
            data=df_grouped)

reduced <- aov(tova_RT ~ group + dosed + asrs,
               data=df_grouped)

model.comparison(full,reduced) #Comparing which model is best-fitting

visualize(reduced,plot="model")

summary(reduced)
#No significance





#~~~~~~~~~~~~~~~~~~

#Analysing the rest of the data for RVP & TOVA
#NOTE: all inputs are considered fixed

#GLM for rvp_CE
full <- glm(rvp_CE ~ group * asrs * dosed,
             data=df_grouped)

reduced <- glm(rvp_CE ~ group + asrs + dosed,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

summary(reduced)
#No significance

#GLM for rvp_OE
full <- glm(rvp_OE ~ group * asrs * dosed,
            data=df_grouped)

reduced <- glm(rvp_OE ~ group + asrs + dosed,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

summary(reduced)

df_grouped %>% group_by(group) %>% summarise(mean_OE=mean(rvp_OE))
#Group B has a significantly lower RVP OE than A


#GLM for rvp_RTV
full <- glm(rvp_RTV ~ group * asrs * dosed,
            data=df_grouped)

reduced <- glm(rvp_RTV ~ group + asrs + dosed,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

summary(reduced)
#No significance


#GLM for rvp_RS
full <- glm(rvp_RS ~ group * asrs * dosed,
            data=df_grouped)

reduced <- glm(rvp_RS ~ group + asrs + dosed,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

summary(reduced)
#No significance


#GLM for rvp_RB
full <- glm(rvp_RB ~ group * asrs * dosed,
            data=df_grouped)

reduced <- glm(rvp_RB ~ group + asrs + dosed,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

summary(reduced)
#No significance


#GLM for tova_CE
full <- glm(tova_CE ~ group * asrs * dosed,
            data=df_grouped)

reduced <- glm(tova_CE ~ group + asrs + dosed,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

summary(reduced)
#Significant positive effect of asrs on TOVA CE


#GLM for tova_OE
full <- glm(tova_OE ~ group * asrs * dosed,
            data=df_grouped)

reduced <- glm(tova_OE ~ group + asrs + dosed,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

summary(reduced)
#Significant positive effect from asrs on TOVA OE

df_grouped %>% group_by(group) %>% summarise(mean_OE=mean(tova_OE))
#Group B has a significantly lower OE than group A in TOVA


#GLM for tova_RTV
full <- glm(tova_RTV ~ group * asrs * dosed,
            data=df_grouped)

reduced <- glm(tova_RTV ~ group + asrs + dosed,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

summary(reduced)
#ASRS has a significant positive effect on TOVA RTV


#GLM for tova_RS
full <- glm(tova_RS ~ group * asrs * dosed,
            data=df_grouped)

reduced <- glm(tova_RS ~ group + asrs + dosed,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

summary(reduced)
#ASRS has a significant negative effect on TOVA RS


#GLM for tova_RB
full <- glm(tova_RB ~ group * asrs * dosed,
            data=df_grouped)

reduced <- glm(tova_RB ~ group + asrs + dosed,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

summary(reduced)
#No significance