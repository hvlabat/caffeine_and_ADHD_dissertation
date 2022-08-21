#Script used to process data from Hampsey et al. (2019)

#loading libraries
library(here)
library(tidyverse)

#load the data
df_ms <- read.csv(here("data","raw","sacc","microsaccades_trial.csv"))

#microsaccdes per second
ms_per_sec <- df_ms$N/df_ms$trial_length

#add above data to df_ms
df_ms <- mutate(df_ms,ms_per_sec)

#ctrl+shift+m is pipe %>%

#separating path from file name
df_ms <- separate(data = df_ms, col = 1,
                  into = c(NA,NA,NA, "file_name"),sep = "/")

#getting participant number, condition, and trial number using str_sub, mutate,
# & parse_number
df_ms <- df_ms %>% mutate(ppt=str_sub(file_name,1,3)) %>% 
  mutate(cond=str_sub(file_name,4,7)) %>%
  mutate(cond=str_remove(cond,"_")) %>% #removes the _ in pre_
  mutate(trial=parse_number(str_sub(file_name,4)))

#tidying up using select, keeping only ppt, cond, trial and ms_per_sec in df
df_ms <- df_ms %>% select(ppt,cond,trial,ms_per_sec)

#using group_by and summarise to find the mean ms_per_sec per participant
df_ms %>% group_by(ppt) %>% 
  summarise(mean_ms_per_sec=mean(ms_per_sec))

#using group_by to count the number of trials per participant
df_ms %>% group_by(ppt) %>% 
  summarise(num_trials_per_ppt=mean(trial))

#finding mean ms_per_sec per trial
df_ms %>% group_by(trial) %>% 
  summarise(mean_ms_per_sec=mean(ms_per_sec))

#import asrs.csv which contains participant ASRS scores
df_ppt_asrs <- read.csv(here("data","raw","sacc","asrs.csv"))

#rename: change column/variable names to something short
df_ppt_asrs <- df_ppt_asrs %>% rename(inattn=Inattention.subscale) %>% 
  rename(hyperactv=Hyperactivity.subscale) %>% 
  rename(ppt=Participant.Number) %>% 
  rename(total_asrs=Total.ASRS.Score)

#create a df containing the mean ms_per_sec data, grouped by ppt and cond
df_ms_ppt_cond <- df_ms %>% group_by(ppt,cond) %>% 
  summarise(mean_ms_per_sec=mean(ms_per_sec))

#changing ppt in df_ms_ppt_cond into integers/numeric, to allow for merging
df_ms_ppt_cond <- transform(df_ms_ppt_cond,ppt=as.numeric(ppt))

#merging both dfs (via inner_join function, which only accepts the overlap of two dfs)
df_merged <- inner_join(df_ms_ppt_cond,df_ppt_asrs,by="ppt")

#Renaming df_merged column names to be presentation-friendly
df_merged <- rename(df_merged,Condition=cond)
df_merged$Condition <- ifelse(df_merged$Condition=="pre","Pre-Caffeine","Post-Caffeine")

#plotting the result, showing asrs score against microsaccade rate
p <- ggplot(df_merged,aes(x=total_asrs,y=mean_ms_per_sec,color=Condition,fill=Condition))

# Non-colourblind palette
p+geom_point()+
  geom_smooth(method=lm)+
  labs(x="ASRS Score", y="Mean Microsaccades per second")+
  scale_colour_manual(values=c("#E13A3F","#36B2EA"))+
  scale_fill_manual(values=c("#E13A3F","#36B2EA"))+
  theme(legend.position = c(0.85, 0.15),
        text=element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

# Colourblind palette
p+geom_point()+
  geom_smooth(method=lm)+
  labs(x="ASRS Score", y="Mean Microsaccades per second")+
  scale_colour_manual(values=c("#D55E00","#0072B2"))+
  scale_fill_manual(values=c("#D55E00","#0072B2"))+
  theme(legend.position = c(0.85, 0.15),
        text=element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

plot_fig <-p+geom_point()+
  geom_smooth(method=lm)+
  labs(x="ASRS Score", y="Mean Microsaccades per second")+
  scale_colour_manual(values=c("#D55E00","#0072B2"))+
  scale_fill_manual(values=c("#D55E00","#0072B2"))+
  theme(legend.position = c(0.85, 0.15),
        text=element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

ggsave(here("outputs","microsacc.png"),plot=plot_fig,width=9,height=7)
