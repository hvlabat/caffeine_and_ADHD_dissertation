#Load libraries
library(here)
library(tidyverse)
library(ggpubr)
library(moments)
library(dplyr)

#T-tests of age, total_asrs, daily_caff, and pre-break scores between groups A and B
for (i in c(5:9,15,16)){
  print(colnames(df_ppt)[i]) #to see which data we are testing
  print(t.test(df_ppt[grep("A",df_ppt$group),i],
         df_ppt[grep("B",df_ppt$group),i]))
}

ggboxplot(df_ppt,x="group",y="age")

ggboxplot(df_ppt,x="group",y="daily_caff")

ggboxplot(df_ppt,x="group",y="total_asrs")

ggboxplot(df_ppt,x="group",y="rvp_pre_rtime")

ggboxplot(df_ppt,x="group",y="rvp_pre_score")

#T-tests suggest no significant deviations in groups, though rvp_pre_rtime comes close


#Chi-squared test of sex to identify deviations in groups A and B
chisq.test(df_ppt[grep("B",df_ppt$group),4],
           df_ppt[grep("A",df_ppt$group)-1,4]) #removing one row of A due to 1 more ppt than B

#Chi-squared test suggest no significant deviations in groups


#Shapiro-Wilk test to see if change scores are parametric/normal
#p<0.05 (significant) means data is not normal
shapiro.test(df_ppt$mean_chng_tova)
gghistogram(data=df_ppt,x="mean_chng_tova")
#significant; non-parametric

shapiro.test(df_ppt$mean_chng_rvp)
gghistogram(data=df_ppt,x="mean_chng_rvp")
boxplot(df_ppt$mean_chng_rvp)
#significant; non-parametric

shapiro.test(df_ppt[grep("A",df_ppt$group),21])
gghistogram(data=df_ppt[grep("A",df_ppt$group),],x="mean_chng_tova")
boxplot(df_ppt$mean_chng_tova)
#non-significant; parametric

shapiro.test(df_ppt[grep("B",df_ppt$group),21])
gghistogram(data=df_ppt[grep("B",df_ppt$group),],x="mean_chng_tova")
#significant; non-parametric

shapiro.test(df_ppt[grep("A",df_ppt$group),14])
gghistogram(data=df_ppt[grep("A",df_ppt$group),],x="mean_chng_rvp")
#significant; non-parametric

shapiro.test(df_ppt[grep("B",df_ppt$group),14])
gghistogram(data=df_ppt[grep("B",df_ppt$group),],x="mean_chng_rvp")
#significant; non-parametric


#The test suggests the data is predominantly non-parametric
#it thus necessitates using Kruskal–Wallis one-way ANOVA

#Splitting ASRS scores into High ASRS group A (HAA) and group B (HAB)
#As well as Low " (LAA) and LAB.
df_HA <- df_ppt[df_ppt$total_asrs>median(df_ppt$total_asrs),]
df_LA <- df_ppt[df_ppt$total_asrs<=median(df_ppt$total_asrs),]

df_HAA <- df_HA[grep("A",df_HA$group),]
df_HAB <- df_HA[grep("B",df_HA$group),]

df_LAA <- df_LA[grep("A",df_LA$group),]
df_LAB <- df_LA[grep("B",df_LA$group),]

#Making df for stat processing of each test
max_length <- max(c(nrow(df_HAA),
                    nrow(df_HAB),
                    nrow(df_LAA),
                    nrow(df_LAB)))

#Stat test for rvp data
df_stats_rvp <- data.frame(HAA=c(df_HAA$mean_chng_rvp,
                                 rep(NA,max_length-nrow(df_HAA))),
                           HAB=c(df_HAB$mean_chng_rvp,
                                 rep(NA,max_length-nrow(df_HAB))),
                           LAA=c(df_LAA$mean_chng_rvp,
                                 rep(NA,max_length-nrow(df_LAA))),
                           LAB=c(df_LAB$mean_chng_rvp,
                                 rep(NA,max_length-nrow(df_LAB)))
                           )

boxplot(df_stats_rvp)

kruskal.test(df_stats_rvp)

#Stat test for tova data
df_stats_tova <- data.frame(HAA=c(df_HAA$mean_chng_tova,
                                 rep(NA,max_length-nrow(df_HAA))),
                           HAB=c(df_HAB$mean_chng_tova,
                                 rep(NA,max_length-nrow(df_HAB))),
                           LAA=c(df_LAA$mean_chng_tova,
                                 rep(NA,max_length-nrow(df_LAA))),
                           LAB=c(df_LAB$mean_chng_tova,
                                 rep(NA,max_length-nrow(df_LAB)))
)

boxplot(df_stats_tova)

kruskal.test(df_stats_tova)
