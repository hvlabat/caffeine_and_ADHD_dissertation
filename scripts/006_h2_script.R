#Load libraries
library(here)
library(tidyverse)
library(ggpubr)

#Hypothesis 2 is:
#There would be a greater improvement of test scores in
#caffeine-takers compared to placebo-takers.

#Thus, the relative change in test scores must be plotted against the group
#The test scores are OE, CE, RT, RTV, RS and RB


#Creating separate df for relative change scores
df_change <- data.frame(df_ppt$ppt,df_ppt$group,df_ppt$total_asrs)
df_change <- rename(df_change,ppt=df_ppt.ppt,group=df_ppt.group,asrs=df_ppt.total_asrs)

#For RVP
df_change$OE_rvp <- (df_ppt$rvp_post_OE-df_ppt$rvp_pre_OE)/df_ppt$rvp_pre_OE
df_change$CE_rvp <- (df_ppt$rvp_post_CE-df_ppt$rvp_pre_CE)/df_ppt$rvp_pre_CE
df_change$RT_rvp <- (df_ppt$rvp_post_RT-df_ppt$rvp_pre_RT)/df_ppt$rvp_pre_RT
df_change$RTV_rvp <- (df_ppt$rvp_post_RTV-df_ppt$rvp_pre_RTV)/df_ppt$rvp_pre_RTV
df_change$RS_rvp <- (df_ppt$rvp_post_RS-df_ppt$rvp_pre_RS)/df_ppt$rvp_pre_RS
df_change$RB_rvp <- (df_ppt$rvp_post_RB-df_ppt$rvp_pre_RB)/df_ppt$rvp_pre_RB

#For TOVA
df_change$OE_tova <- (df_ppt$tova_post_OE-df_ppt$tova_pre_OE)/df_ppt$tova_pre_OE
df_change$CE_tova <- (df_ppt$tova_post_CE-df_ppt$tova_pre_CE)/df_ppt$tova_pre_CE
df_change$RT_tova <- (df_ppt$tova_post_RT-df_ppt$tova_pre_RT)/df_ppt$tova_pre_RT
df_change$RTV_tova <- (df_ppt$tova_post_RTV-df_ppt$tova_pre_RTV)/df_ppt$tova_pre_RTV
df_change$RS_tova <- (df_ppt$tova_post_RS-df_ppt$tova_pre_RS)/df_ppt$tova_pre_RS
df_change$RB_tova <- (df_ppt$tova_post_RB-df_ppt$tova_pre_RB)/df_ppt$tova_pre_RB

#Getting rid of infinite or NaN results
df_change[sapply(df_change,is.infinite)] <- NA
df_change[sapply(df_change,is.nan)] <- NA


#RVP OE change Group A vs B
boxplot(df_change[grep("A",df_change$group),"OE_rvp"],
        df_change[grep("B",df_change$group),"OE_rvp"])
t.test(df_change[grep("A",df_change$group),"OE_rvp"],
       df_change[grep("B",df_change$group),"OE_rvp"])
#Insignificant

#RVP CE etc
boxplot(df_change[grep("A",df_change$group),"CE_rvp"],
        df_change[grep("B",df_change$group),"CE_rvp"])
t.test(df_change[grep("A",df_change$group),"CE_rvp"],
       df_change[grep("B",df_change$group),"CE_rvp"])
#Insignificant

#RVP RT etc
boxplot(df_change[grep("A",df_change$group),"RT_rvp"],
        df_change[grep("B",df_change$group),"RT_rvp"])
t.test(df_change[grep("A",df_change$group),"RT_rvp"],
       df_change[grep("B",df_change$group),"RT_rvp"])
#Insignificant

#RVP RTV etc
boxplot(df_change[grep("A",df_change$group),"RTV_rvp"],
        df_change[grep("B",df_change$group),"RTV_rvp"])
t.test(df_change[grep("A",df_change$group),"RTV_rvp"],
       df_change[grep("B",df_change$group),"RTV_rvp"])
#Insignificant

#RVP RS etc
boxplot(df_change[grep("A",df_change$group),"RS_rvp"],
        df_change[grep("B",df_change$group),"RS_rvp"])
t.test(df_change[grep("A",df_change$group),"RS_rvp"],
       df_change[grep("B",df_change$group),"RS_rvp"])
#Insignificant, but close! (p=0.05803)

#RVP RB etc
boxplot(df_change[grep("A",df_change$group),"RB_rvp"],
        df_change[grep("B",df_change$group),"RB_rvp"])
t.test(df_change[grep("A",df_change$group),"RB_rvp"],
       df_change[grep("B",df_change$group),"RB_rvp"])
#Insignificant



#tova OE change Group A vs B
boxplot(df_change[grep("A",df_change$group),"OE_tova"],
        df_change[grep("B",df_change$group),"OE_tova"])
t.test(df_change[grep("A",df_change$group),"OE_tova"],
       df_change[grep("B",df_change$group),"OE_tova"])
#Insignificant, but close! p=0.076

#tova CE etc
boxplot(df_change[grep("A",df_change$group),"CE_tova"],
        df_change[grep("B",df_change$group),"CE_tova"])
t.test(df_change[grep("A",df_change$group),"CE_tova"],
       df_change[grep("B",df_change$group),"CE_tova"])
#Insignificant, but close! p=0.095

#tova RT etc
boxplot(df_change[grep("A",df_change$group),"RT_tova"],
        df_change[grep("B",df_change$group),"RT_tova"])
t.test(df_change[grep("A",df_change$group),"RT_tova"],
       df_change[grep("B",df_change$group),"RT_tova"])
#SIGNIFICANT

#tova RTV etc
boxplot(df_change[grep("A",df_change$group),"RTV_tova"],
        df_change[grep("B",df_change$group),"RTV_tova"])
t.test(df_change[grep("A",df_change$group),"RTV_tova"],
       df_change[grep("B",df_change$group),"RTV_tova"])
#Insignificant

#tova RS etc
boxplot(df_change[grep("A",df_change$group),"RS_tova"],
        df_change[grep("B",df_change$group),"RS_tova"])
t.test(df_change[grep("A",df_change$group),"RS_tova"],
       df_change[grep("B",df_change$group),"RS_tova"])
#Insignificant

#tova RB etc
boxplot(df_change[grep("A",df_change$group),"RB_tova"],
        df_change[grep("B",df_change$group),"RB_tova"],ylim=c(-6,10))
t.test(df_change[grep("A",df_change$group),"RB_tova"],
       df_change[grep("B",df_change$group),"RB_tova"])
#Insignificant

#H2 CONCLUSION:
#There is a significant difference in RT change RT for TOVA between
#groups A & B. RS change for RVP, and OE & CE change for TOVA also got close to significance.

#We can accept the alternative hypothesis for RT change in TOVA, alone