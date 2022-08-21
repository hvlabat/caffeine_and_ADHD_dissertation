#Load libraries
library(here)
library(tidyverse)
library(ggpubr)

#Hypothesis 1 is:
#Participants with lower ASRS scores would have higher initial test
#scores than those with higher ASRS scores

#Thus, the pre-break test scores must be plotted against the ASRS scores
#The test scores are OE, CE, RT, RTV, RS and RB

#For RVP

#OE RVP
ggscatter(df_ppt,x="total_asrs",y="rvp_pre_OE",add="reg.line")
summary(lm(df_ppt$total_asrs~df_ppt$rvp_pre_OE))
#Insignificant

#CE RVP
ggscatter(df_ppt,x="total_asrs",y="rvp_pre_CE",add="reg.line")
summary(lm(df_ppt$total_asrs~df_ppt$rvp_pre_CE))
#Insignificant

#RT RVP
ggscatter(df_ppt,x="total_asrs",y="rvp_pre_RT",add="reg.line")
summary(lm(df_ppt$total_asrs~df_ppt$rvp_pre_RT))
#Insignificant

#RTV RVP
ggscatter(df_ppt,x="total_asrs",y="rvp_pre_RTV",add="reg.line")
summary(lm(df_ppt$total_asrs~df_ppt$rvp_pre_RTV))
#Insignificant

#RS RVP
ggscatter(df_ppt,x="total_asrs",y="rvp_pre_RS",add="reg.line")
summary(lm(df_ppt$total_asrs~df_ppt$rvp_pre_RS))
#Insignificant

#RB RVP
ggscatter(df_ppt,x="total_asrs",y="rvp_pre_RB",add="reg.line")
summary(lm(df_ppt$total_asrs~df_ppt$rvp_pre_RB))
#Insignificant


#For TOVA

#OE TOVA
ggscatter(df_ppt,x="total_asrs",y="tova_pre_OE",add="reg.line")
summary(lm(df_ppt$total_asrs~df_ppt$tova_pre_OE))
#Insignificant

#CE TOVA
ggscatter(df_ppt,x="total_asrs",y="tova_pre_CE",add="reg.line")
summary(lm(df_ppt$total_asrs~df_ppt$tova_pre_CE))
#Insignificant

#RT TOVA
ggscatter(df_ppt,x="total_asrs",y="tova_pre_RT",add="reg.line")
summary(lm(df_ppt$total_asrs~df_ppt$tova_pre_RT))
#Insignificant

#RTV TOVA
ggscatter(df_ppt,x="total_asrs",y="tova_pre_RTV",add="reg.line")
summary(lm(df_ppt$total_asrs~df_ppt$tova_pre_RTV))
#SIGNIFICANT

#RS TOVA
ggscatter(df_ppt,x="total_asrs",y="tova_pre_RS",add="reg.line")
summary(lm(df_ppt$total_asrs~df_ppt$tova_pre_RS))
#Insignificant

#RB TOVA
ggscatter(df_ppt,x="total_asrs",y="tova_pre_RB",add="reg.line")
summary(lm(df_ppt$total_asrs~df_ppt$tova_pre_RB))
#Insignificant


#H1 CONCLUSION:
#There is predominantly no significant correlation between test scores
#and ASRS scores, except in RTV for TOVA.

#We thus cannot accept this hypothesis.