#Load libraries
library(here)
library(tidyverse)
library(ggpubr)
library(lme4)
library(flexplot)
library(lmtest)
library(pwr)

#Creating new df to group scores by:
# Pre- and Post-dosage of drug (placebo or caffeine) (i.e. stage)
# Placebo and Caffeine

df_ppt$stage <- NA

#Order results by pre- and post-stage
df_pre_full <- dplyr::select(df_ppt,ppt,ref,sex,age,
                             daily_caff,group,asrs,stage,
                             contains("pre"))
df_pre_full$stage <- "Pre"
colnames(df_pre_full) <- c("ppt","ref","sex","age","daily_caff","group","asrs","stage",
                           "rvp_RT","rvp_RTV","rvp_CC","rvp_CO","rvp_OE",
                           "rvp_CE","rvp_RS","rvp_RB",
                           "tova_RT","tova_RTV","tova_CC","tova_CO","tova_OE",
                           "tova_CE","tova_RS","tova_RB")

df_post_full <- dplyr::select(df_ppt,ppt,ref,sex,age,
                              daily_caff,group,asrs,stage,
                              contains("post"))
df_post_full$stage <- "Post"
colnames(df_post_full) <- c("ppt","ref","sex","age","daily_caff","group","asrs","stage",
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
# E=prE stage,O=pOst stage
# A=Placebo,B=Caffeine (These were the original blinded groups)
#in the split dfs, columns 9 to 24 contain the test scores

df_EA <- df_grouped[grep("Pre",df_grouped$stage),]
df_EA <- df_EA[grep("Placebo",df_EA$group),]

df_EB <- df_grouped[grep("Pre",df_grouped$stage),]
df_EB <- df_EB[grep("Caffeine",df_EB$group),]

df_OA <- df_grouped[grep("Post",df_grouped$stage),]
df_OA <- df_OA[grep("Placebo",df_OA$group),]

df_OB <- df_grouped[grep("Post",df_grouped$stage),]
df_OB <- df_OB[grep("Caffeine",df_OB$group),]

#Testing parametry and homoscedasity of data
for (i in 9:24){
  para_result_EA <- shapiro.test(df_EA[,i])
  para_result_EB <- shapiro.test(df_EB[,i])
  para_result_OA <- shapiro.test(df_OA[,i])
  para_result_OB <- shapiro.test(df_OB[,i])
  
  if (para_result_EA$p.value >= 0.05 &
      para_result_EB$p.value >= 0.05 &
      para_result_OA$p.value >= 0.05 &
      para_result_OB$p.value >= 0.05){
    print(paste(names(df_EA)[i],"is parametric"))
  }
  else{
    print(paste(names(df_EA)[i],"is non-parametric"))
  }
}
  
#TOVA and RVP RT are both parametric, and can be analysed using GLM
#The rest are non-parametric, thus requiring non-parametric tests

#Analysing the RT data for both TOVA and RVP, using GLM for each
#RVP RT
full <- glm(rvp_RT ~ group * stage * asrs,
                  data=df_grouped)

reduced <- glm(rvp_RT ~ group + stage + asrs,
               data=df_grouped)

model.comparison(full,reduced) #Comparing which model is best-fitting

visualize(reduced,plot="model")

summary(reduced)
summary(full)

#Finding ideal sample size for 'reduced', where u=number of coefficiences
#v = n − u − 1
#Thus n = v + u + 1, where n is the sample size
#f2=0.35 is large effect size,0.15 is medium effect size,0.02 is small effect size
pwr.f2.test(u=3,f2=0.35,sig.level=0.05,power=0.8)
#v=31.3129
#Thus n>35
pwr.f2.test(u=3,f2=0.15,sig.level=0.05,power=0.8)
#v=72.70583
#Thus n>76 for significance
pwr.f2.test(u=3,f2=0.02,sig.level=0.05,power=0.8)
#Thus n>549

#Testing actual effect size
pwr.f2.test(u=3,v=59,sig.level=0.05,power=0.8)
#f2=0.185, which is a slightly larger-than-medium size

#Testing ideal sample size for coef of 7 (full model)
pwr.f2.test(u=7,f2=0.35,sig.level=0.05,power=0.8)
#n>48
pwr.f2.test(u=7,f2=0.15,sig.level=0.05,power=0.8)
#n>103
pwr.f2.test(u=7,f2=0.02,sig.level=0.05,power=0.8)
#n>725


gqtest(reduced) #Testing homoscedasticity

#Indicates caffeine/placego group and pre/post dose significantly influence the rvp_RT
#There are otherwise no other interactions

#Finding direction of interactions
df_grouped %>% group_by(group) %>% summarise(mean_RT=mean(rvp_RT),sd_RT=sd(rvp_RT))
boxplot(rvp_RT ~ group,data=df_grouped)
#Indicates Caffeine group has a significantly lower RT

df_grouped %>% group_by(stage) %>% summarise(mean_RT=mean(rvp_RT),sd_RT=sd(rvp_RT))
boxplot(rvp_RT ~ stage,data=df_grouped)
#Indicates pre-dose has a significantly lower RT

#Plotting these results
rvp_RT_caffeine <- ggscatter(data=data.frame(df_grouped[grep("Caffeine",df_grouped$group),]),
                             x="asrs",y="rvp_RT",
                             color="stage",palette=c("#009E73","#D55E00"),
                             add="reg.line",conf.int=TRUE,ylim=c(200,500),
                             ylab="RVP Response Time (ms)",xlab="ASRS Score",title="Caffeine")

rvp_RT_placebo <- ggscatter(data=data.frame(df_grouped[grep("Placebo",df_grouped$group),]),
                            x="asrs",y="rvp_RT",
                            color="stage",palette=c("#009E73","#D55E00"),
                            add="reg.line",conf.int=TRUE,ylim=c(200,500),
                            ylab="RVP Response Time (ms)",xlab="ASRS Score",title="Placebo")

rvp_RT_pre <- ggscatter(data=data.frame(df_grouped[grep("Pre",df_grouped$stage),]),
                        x="asrs",y="rvp_RT",
                        color="group",palette=c("#E69F00","#56B4E9"),
                        add="reg.line",conf.int=TRUE,ylim=c(200,500),
                        ylab="RVP Response Time (ms)",xlab="ASRS Score",title="Pre-dose")

rvp_RT_post <- ggscatter(data=data.frame(df_grouped[grep("Post",df_grouped$stage),]),
                         x="asrs",y="rvp_RT",
                         color="group",palette=c("#E69F00","#56B4E9"),
                         add="reg.line",conf.int=TRUE,ylim=c(200,500),
                         ylab="RVP Response Time (ms)",xlab="ASRS Score",title="Post-dose")

ggarrange(ggarrange(rvp_RT_pre,rvp_RT_post,ncol=2,
                    common.legend=TRUE,legend="right"),
          ggarrange(rvp_RT_caffeine,rvp_RT_placebo,ncol=2,
                    common.legend=TRUE,legend="right"),nrow=2)

rvp_RT_plot <- ggarrange(ggarrange(rvp_RT_pre,rvp_RT_post,ncol=2,
                                   common.legend=TRUE,legend="right"),
                         ggarrange(rvp_RT_caffeine,rvp_RT_placebo,ncol=2,
                                   common.legend=TRUE,legend="right"),nrow=2)

#Saving image
ggsave(here("outputs","rvp_RT_plot.png"),plot=rvp_RT_plot,width=12,height=8)



#TOVA RT
full <- glm(tova_RT ~ group * stage * asrs,
            data=df_grouped)

reduced <- glm(tova_RT ~ group + stage + asrs,
               data=df_grouped)

model.comparison(full,reduced) #Comparing which model is best-fitting

visualize(reduced,plot="model")

gqtest(reduced) #Testing homoscedasticity

summary(reduced)
#No significance


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Analysing the rest of the data for RVP & TOVA, plotting significant findings
#NOTE: all inputs are considered fixed

#GLM for rvp_CE
full <- glm(rvp_CE ~ group * asrs * stage,
             data=df_grouped)

reduced <- glm(rvp_CE ~ group + asrs + stage,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

gqtest(reduced) #Testing homoscedasticity

summary(reduced)
#No significance



#GLM for rvp_OE
full <- glm(rvp_OE ~ group * asrs * stage,
            data=df_grouped)

reduced <- glm(rvp_OE ~ group + asrs + stage,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

summary(reduced)

df_grouped %>% group_by(group) %>% summarise(mean_OE=mean(rvp_OE),sd_OE=sd(rvp_OE))
#Caffeine Group has a significantly lower RVP OE than Placebo

#Plotting these results
rvp_OE_caffeine <- ggscatter(data=data.frame(df_grouped[grep("Caffeine",df_grouped$group),]),
                              x="asrs",y="rvp_OE",
                              color="stage",palette=c("#009E73","#D55E00"),
                              add="reg.line",conf.int=TRUE,ylim=c(0,35),
                              ylab="RVP OE",xlab="ASRS Score",title="Caffeine")

rvp_OE_placebo <- ggscatter(data=data.frame(df_grouped[grep("Placebo",df_grouped$group),]),
                             x="asrs",y="rvp_OE",
                             color="stage",palette=c("#009E73","#D55E00"),
                             add="reg.line",conf.int=TRUE,ylim=c(0,35),
                             ylab="RVP OE",xlab="ASRS Score",title="Placebo")

rvp_OE_pre <- ggscatter(data=data.frame(df_grouped[grep("Pre",df_grouped$stage),]),
                        x="asrs",y="rvp_OE",
                        color="group",palette=c("#E69F00","#56B4E9"),
                        add="reg.line",conf.int=TRUE,ylim=c(0,35),
                        ylab="RVP OE",xlab="ASRS Score",title="Pre-dose")

rvp_OE_post <- ggscatter(data=data.frame(df_grouped[grep("Post",df_grouped$stage),]),
                         x="asrs",y="rvp_OE",
                         color="group",palette=c("#E69F00","#56B4E9"),
                         add="reg.line",conf.int=TRUE,ylim=c(0,35),
                         ylab="RVP OE",xlab="ASRS Score",title="Post-dose")

ggarrange(ggarrange(rvp_OE_pre,rvp_OE_post,ncol=2,
          common.legend=TRUE,legend="right"),
          ggarrange(rvp_OE_caffeine,rvp_OE_placebo,ncol=2,
          common.legend=TRUE,legend="right"),nrow=2)

rvp_OE_plot <- ggarrange(ggarrange(rvp_OE_pre,rvp_OE_post,ncol=2,
                                   common.legend=TRUE,legend="right"),
                         ggarrange(rvp_OE_caffeine,rvp_OE_placebo,ncol=2,
                                   common.legend=TRUE,legend="right"),nrow=2)

#Saving image
ggsave(here("outputs","rvp_OE_plot.png"),plot=rvp_OE_plot,width=12,height=8)



#GLM for rvp_RTV
full <- glm(rvp_RTV ~ group * asrs * stage,
            data=df_grouped)

reduced <- glm(rvp_RTV ~ group + asrs + stage,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

gqtest(reduced) #Testing homoscedasticity

summary(reduced)
#No significance



#GLM for rvp_RS
full <- glm(rvp_RS ~ group * asrs * stage,
            data=df_grouped)

reduced <- glm(rvp_RS ~ group + asrs + stage,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

summary(reduced)
#No significance



#GLM for rvp_RB
full <- glm(rvp_RB ~ group * asrs * stage,
            data=df_grouped)

reduced <- glm(rvp_RB ~ group + asrs + stage,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

gqtest(reduced) #Testing homoscedasticity

summary(reduced)
#No significance



#GLM for tova_CE
full <- glm(tova_CE ~ group * asrs * stage,
            data=df_grouped)

reduced <- glm(tova_CE ~ group + asrs + stage,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

gqtest(reduced) #Testing homoscedasticity

summary(reduced)
summary(full)
#Significant positive effect of asrs on TOVA CE
#Plotting these results
tova_CE_caffeine <- ggscatter(data=data.frame(df_grouped[grep("Caffeine",df_grouped$group),]),
                             x="asrs",y="tova_CE",
                             color="stage",palette=c("#009E73","#D55E00"),
                             add="reg.line",conf.int=TRUE,ylim=c(0,70),
                             ylab="TOVA CE",xlab="ASRS Score",title="Caffeine")

tova_CE_placebo <- ggscatter(data=data.frame(df_grouped[grep("Placebo",df_grouped$group),]),
                            x="asrs",y="tova_CE",
                            color="stage",palette=c("#009E73","#D55E00"),
                            add="reg.line",conf.int=TRUE,ylim=c(0,70),
                            ylab="TOVA CE",xlab="ASRS Score",title="Placebo")

tova_CE_pre <- ggscatter(data=data.frame(df_grouped[grep("Pre",df_grouped$stage),]),
                        x="asrs",y="tova_CE",
                        color="group",palette=c("#E69F00","#56B4E9"),
                        add="reg.line",conf.int=TRUE,ylim=c(0,70),
                        ylab="TOVA CE",xlab="ASRS Score",title="Pre-dose")

tova_CE_post <- ggscatter(data=data.frame(df_grouped[grep("Post",df_grouped$stage),]),
                         x="asrs",y="tova_CE",
                         color="group",palette=c("#E69F00","#56B4E9"),
                         add="reg.line",conf.int=TRUE,ylim=c(0,70),
                         ylab="TOVA CE",xlab="ASRS Score",title="Post-dose")

ggarrange(ggarrange(tova_CE_pre,tova_CE_post,ncol=2,
                    common.legend=TRUE,legend="right"),
          ggarrange(tova_CE_caffeine,tova_CE_placebo,ncol=2,
                    common.legend=TRUE,legend="right"),nrow=2)

tova_CE_plot <- ggarrange(ggarrange(tova_CE_pre,tova_CE_post,ncol=2,
                                   common.legend=TRUE,legend="right"),
                         ggarrange(tova_CE_caffeine,tova_CE_placebo,ncol=2,
                                   common.legend=TRUE,legend="right"),nrow=2)

#Saving image
ggsave(here("outputs","tova_CE_plot.png"),plot=tova_CE_plot,width=12,height=8)



#GLM for tova_OE
full <- glm(tova_OE ~ group * asrs * stage,
            data=df_grouped)

reduced <- glm(tova_OE ~ group + asrs + stage,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

gqtest(reduced) #Testing homoscedasticity

summary(reduced)
summary(full)
#Significant positive effect from asrs on TOVA OE

df_grouped %>% group_by(group) %>% summarise(mean_OE=mean(tova_OE),sd_OE=sd(tova_OE))
#Caffeine Group has a significantly lower OE than placebo group in TOVA

#Plotting these results
tova_OE_caffeine <- ggscatter(data=data.frame(df_grouped[grep("Caffeine",df_grouped$group),]),
                              x="asrs",y="tova_OE",
                              color="stage",palette=c("#009E73","#D55E00"),
                              add="reg.line",conf.int=TRUE,ylim=c(-10,50),
                              ylab="TOVA OE",xlab="ASRS Score",title="Caffeine")

tova_OE_placebo <- ggscatter(data=data.frame(df_grouped[grep("Placebo",df_grouped$group),]),
                             x="asrs",y="tova_OE",
                             color="stage",palette=c("#009E73","#D55E00"),
                             add="reg.line",conf.int=TRUE,ylim=c(-10,50),
                             ylab="TOVA OE",xlab="ASRS Score",title="Placebo")

tova_OE_pre <- ggscatter(data=data.frame(df_grouped[grep("Pre",df_grouped$stage),]),
                         x="asrs",y="tova_OE",
                         color="group",palette=c("#E69F00","#56B4E9"),
                         add="reg.line",conf.int=TRUE,ylim=c(-10,50),
                         ylab="TOVA OE",xlab="ASRS Score",title="Pre-dose")

tova_OE_post <- ggscatter(data=data.frame(df_grouped[grep("Post",df_grouped$stage),]),
                          x="asrs",y="tova_OE",
                          color="group",palette=c("#E69F00","#56B4E9"),
                          add="reg.line",conf.int=TRUE,ylim=c(-10,50),
                          ylab="TOVA OE",xlab="ASRS Score",title="Post-dose")

ggarrange(ggarrange(tova_OE_pre,tova_OE_post,ncol=2,
                    common.legend=TRUE,legend="right"),
          ggarrange(tova_OE_caffeine,tova_OE_placebo,ncol=2,
                    common.legend=TRUE,legend="right"),nrow=2)

tova_OE_plot <- ggarrange(ggarrange(tova_OE_pre,tova_OE_post,ncol=2,
                                    common.legend=TRUE,legend="right"),
                          ggarrange(tova_OE_caffeine,tova_OE_placebo,ncol=2,
                                    common.legend=TRUE,legend="right"),nrow=2)

#Saving image
ggsave(here("outputs","tova_OE_plot.png"),plot=tova_OE_plot,width=12,height=8)



#GLM for tova_RTV
full <- glm(tova_RTV ~ group * asrs * stage,
            data=df_grouped)

reduced <- glm(tova_RTV ~ group + asrs + stage,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

gqtest(reduced) #Testing homoscedasticity

summary(reduced)
summary(full)
#ASRS has a significant positive effect on TOVA RTV

#Plotting these results
tova_RTV_caffeine <- ggscatter(data=data.frame(df_grouped[grep("Caffeine",df_grouped$group),]),
                              x="asrs",y="tova_RTV",
                              color="stage",palette=c("#009E73","#D55E00"),
                              add="reg.line",conf.int=TRUE,ylim=c(40,400),
                              ylab="TOVA RTV (ms^2)",xlab="ASRS Score",title="Caffeine")

tova_RTV_placebo <- ggscatter(data=data.frame(df_grouped[grep("Placebo",df_grouped$group),]),
                             x="asrs",y="tova_RTV",
                             color="stage",palette=c("#009E73","#D55E00"),
                             add="reg.line",conf.int=TRUE,ylim=c(40,400),
                             ylab="TOVA RTV (ms^2)",xlab="ASRS Score",title="Placebo")

tova_RTV_pre <- ggscatter(data=data.frame(df_grouped[grep("Pre",df_grouped$stage),]),
                         x="asrs",y="tova_RTV",
                         color="group",palette=c("#E69F00","#56B4E9"),
                         add="reg.line",conf.int=TRUE,ylim=c(40,400),
                         ylab="TOVA RTV (ms^2)",xlab="ASRS Score",title="Pre-dose")

tova_RTV_post <- ggscatter(data=data.frame(df_grouped[grep("Post",df_grouped$stage),]),
                          x="asrs",y="tova_RTV",
                          color="group",palette=c("#E69F00","#56B4E9"),
                          add="reg.line",conf.int=TRUE,ylim=c(40,400),
                          ylab="TOVA RTV (ms^2)",xlab="ASRS Score",title="Post-dose")

ggarrange(ggarrange(tova_RTV_pre,tova_RTV_post,ncol=2,
                    common.legend=TRUE,legend="right"),
          ggarrange(tova_RTV_caffeine,tova_RTV_placebo,ncol=2,
                    common.legend=TRUE,legend="right"),nrow=2)

tova_RTV_plot <- ggarrange(ggarrange(tova_RTV_pre,tova_RTV_post,ncol=2,
                                    common.legend=TRUE,legend="right"),
                          ggarrange(tova_RTV_caffeine,tova_RTV_placebo,ncol=2,
                                    common.legend=TRUE,legend="right"),nrow=2)

#Saving image
ggsave(here("outputs","tova_RTV_plot.png"),plot=tova_RTV_plot,width=12,height=8)



#GLM for tova_RS
full <- glm(tova_RS ~ group * asrs * stage,
            data=df_grouped)

reduced <- glm(tova_RS ~ group + asrs + stage,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

gqtest(reduced) #Testing homoscedasticity

summary(reduced)
summary(full)
#ASRS has a significant negative effect on TOVA RS

#Plotting these results
tova_RS_caffeine <- ggscatter(data=data.frame(df_grouped[grep("Caffeine",df_grouped$group),]),
                               x="asrs",y="tova_RS",
                               color="stage",palette=c("#009E73","#D55E00"),
                               add="reg.line",conf.int=TRUE,ylim=c(0.87,1),
                               ylab="TOVA RS",xlab="ASRS Score",title="Caffeine")

tova_RS_placebo <- ggscatter(data=data.frame(df_grouped[grep("Placebo",df_grouped$group),]),
                              x="asrs",y="tova_RS",
                              color="stage",palette=c("#009E73","#D55E00"),
                              add="reg.line",conf.int=TRUE,ylim=c(0.87,1),
                              ylab="TOVA RS",xlab="ASRS Score",title="Placebo")

tova_RS_pre <- ggscatter(data=data.frame(df_grouped[grep("Pre",df_grouped$stage),]),
                          x="asrs",y="tova_RS",
                          color="group",palette=c("#E69F00","#56B4E9"),
                          add="reg.line",conf.int=TRUE,ylim=c(0.87,1),
                          ylab="TOVA RS",xlab="ASRS Score",title="Pre-dose")

tova_RS_post <- ggscatter(data=data.frame(df_grouped[grep("Post",df_grouped$stage),]),
                           x="asrs",y="tova_RS",
                           color="group",palette=c("#E69F00","#56B4E9"),
                           add="reg.line",conf.int=TRUE,ylim=c(0.87,1),
                           ylab="TOVA RS",xlab="ASRS Score",title="Post-dose")

ggarrange(ggarrange(tova_RS_pre,tova_RS_post,ncol=2,
                    common.legend=TRUE,legend="right"),
          ggarrange(tova_RS_caffeine,tova_RS_placebo,ncol=2,
                    common.legend=TRUE,legend="right"),nrow=2)

tova_RS_plot <- ggarrange(ggarrange(tova_RS_pre,tova_RS_post,ncol=2,
                                     common.legend=TRUE,legend="right"),
                           ggarrange(tova_RS_caffeine,tova_RS_placebo,ncol=2,
                                     common.legend=TRUE,legend="right"),nrow=2)

#Saving image
ggsave(here("outputs","tova_RS_plot.png"),plot=tova_RS_plot,width=12,height=8)



#GLM for tova_RB
full <- glm(tova_RB ~ group * asrs * stage,
            data=df_grouped)

reduced <- glm(tova_RB ~ group + asrs + stage,
               data=df_grouped)

model.comparison(full,reduced) #Comparing models and picking best fit

visualize(reduced,plot="model")

gqtest(reduced) #Testing homoscedasticity

summary(reduced)
#No significance