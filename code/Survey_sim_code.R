# 1-5-16 
# Katie Palof, Alaska Department of Fish and Game
# Code for developing a simulation to deal with survey cuts to crab surveys

# Goal: to create a simulation that would allow you to remove each areas component to the survey biomass estimate
# and place it in the NON-survey expansion factor part of the regional estimate.

# this code deals with Tanner crab survey data - LEGAL biomass only.

library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(extrafont)
library(ggthemes)
library(rmarkdown)

#set graphing theme
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size = 12, base_family = 'Times New Roman')+ 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()))

#rm(list= ls())  clear workspace
#Load data
#####
#Data needed: 1) biomass for each area for each year ONLY LEGAL BIOMASS USED HERE
#             2) % survey area contribution based on historic catch 
# Regional biomass is the sum of the survey area biomasses and the non-surveyed areas expanded biomass
#   Biomass is expanding by the equation: 
#         total regional biomass = Survey area biomass/ % biomass in survey areas
# for 2015/2016 this is 2,142,529 / 0.66 = 3,246,255
getwd()
setwd("C:/Users/kjpalof/Documents/R projects/survey-cut-sim_kp/data")
dat <- read.csv("C:/Users/kjpalof/Documents/R projects/survey-cut-sim_kp/data/tanner crab data_legal.csv") # biomass by year and survey area
percent <- read.csv("C:/Users/kjpalof/Documents/R projects/survey-cut-sim_kp/data/tanner crab percent.csv") # file with the percent contribution for each area

head(dat) # just the biomass of legal crab 
head(percent)
#####
# need to summarize biomass by year but need to know which areas are included each year....not sure how to get 
# this last part.

#combine the two data sets
#####
dat %>%
  right_join(percent, by="SurveyArea") -> dat2
head(dat2)

#check to make sure that they survey areas add up to 0.7116 percent in years with all areas surveyed   
# true except for 2015 - this should be less due to the removal of port frederick and port camden
dat2 %>%
  group_by(Year_Survey) %>%
  mutate(percent_real = ifelse (Biomass_lb > 0, percent_hist_harvest, 0)) %>% # want to exclude areas that do not have biomass estimates
  summarize(expansion = sum (percent_real, na.rm=T)) # summarizes the expansion for each year based on the areas with  
                                                    # biomass estimates
#####

# calculate known biomass for each year and then add expansion biomass and total biomass columns
# using percent for each area from historic harvest - percent_hist_harvest
# how to creat known data data frames using either hist_harvest or survey area % 
##### 
dat2 %>%
  mutate(percent_real = ifelse (Biomass_lb > 0, percent_hist_harvest, 0)) %>%
  group_by(Year_Survey) %>%
   # want to exclude areas that do not have biomass estimates
  summarize(expansion = sum (percent_real, na.rm=T), survey_biomass = sum (Biomass_lb, na.rm =T)) %>%
  # summarizes the expansion for each year based on the areas with  biomass estimates
  mutate(non_biomass = (survey_biomass / expansion) - survey_biomass, total_lb = survey_biomass + non_biomass) -> known_dat_harvest
# using percent for each area from the last 10 years of the survey - percent_survey_area
dat2 %>%
  mutate(percent_real = ifelse (Biomass_lb > 0, percent_survey_area, 0)) %>%
  group_by(Year_Survey) %>%
  # want to exclude areas that do not have biomass estimates
  summarize(expansion = sum (percent_real, na.rm=T), survey_biomass = sum (Biomass_lb, na.rm =T)) %>%
  # summarizes the expansion for each year based on the areas with  biomass estimates
  mutate(non_biomass = (survey_biomass / expansion) - survey_biomass, total_lb = survey_biomass + non_biomass) -> known_dat_survey


#Need to figure out a way to choose which areas are included in the above calculation of expansion factor
dat2 %>%
  filter(Year_Survey ==2014) %>%
  mutate(percent_used = ifelse(SurveyArea %in% areas_2015, percent_surveyarea, 0)) #%>%
  summarise(expand2015 = sum(percent_used))
#####
  
# make sure you use the correct function 
  # survey_sim uses survey area % contribution from the last 10 years the other - survey_sim_harvest - uses historic harvest contribution

# input into function the areas you want to use in the simulation.
areas_all <- c("EI" , "GB" , "GLB" ,"HB"  ,"ICY" ,"LS",  "NJ" , "PB" , "PC" , "PF" , "PS",  "SC" , "SP" , "TB")
areas_all # all the areas 
areas_2015 <- c("EI" , "GB" , "GLB" ,"HB"  ,"ICY" ,"LS",  "NJ" , "PB" , "PS",  "SC" , "SP" , "TB")
  ### those used in 2015 
areas_RKC_only <-c("EI", "GB", "LS", "NJ", "PB", "PS", "SC", "SP")
  ### only the RKC survey areas.
areas_1 <- c("EI" , "GB" , "GLB" ,"ICY" ,"LS",  "NJ" , "PB" , "PS",  "SC" , "SP" )
  #2015 areas minus "TB", "HB" or leg 2

############################
#################### load survey_sim from function.R file ########################
######################## Simulations and figures ###################
##########################
sim2015 <- survey_sim (dat2, areas_2015)
ggplot(sim2015, aes(Year_Survey, total_lb, color = type)) + geom_point(size=2) +geom_smooth() +
  scale_color_tableau() +
  scale_x_continuous(breaks = seq(1996, 2016, 2))+ scale_y_continuous(breaks = seq(0, 55000000, 1000000))+
  coord_cartesian(ylim=c(0,5500000)) +
  xlab('Survey Year') + ylab('Total LEGAL biomass')+
  ggtitle('2015 survey areas simulation')
ggsave("sim2015.png")

#survey_sim(dat2, areas_all) # check to confirm that the difference is 0 when all areas are included. 

simRKC2 <- survey_sim (dat2, areas_RKC_only)
ggplot(simRKC2, aes(Year_Survey, total_lb, color = type)) + geom_point(size=2) +geom_smooth() +
  scale_color_tableau() +
  scale_x_continuous(breaks = seq(1996, 2016, 2))+ scale_y_continuous(breaks = seq(0, 55000000, 1000000))+
  coord_cartesian(ylim=c(0,5500000)) +
  xlab('Survey Year') + ylab('Total LEGAL biomass')+
  ggtitle('RKC survey only simulation')
ggsave("simRKC2.png")

sim_areas1 <- survey_sim(dat2, areas_1)
ggplot(sim_areas1, aes(Year_Survey, total_lb, color = type)) + geom_point(size=2) +geom_smooth() +
  scale_color_tableau() +
  scale_x_continuous(breaks = seq(1996, 2016, 2))+ scale_y_continuous(breaks = seq(0, 55000000, 1000000))+
  coord_cartesian(ylim=c(0,5500000)) +
  xlab('Survey Year') + ylab('Total LEGAL biomass')+
  ggtitle('sim using only leg 1 TCS and 2015 areas')
ggsave("sim_areas1.png")


######################################################################################
#########################  adjusted biomass to attempt to match historic #############
########################################################################################
# The biomass from Thomas Bay, Glacier Bay, and Port Camden in the years from 1997 to 2001, if it wasn't estimated
#   from surveys was estimated to be the "minimium of all consequetive years"  this was kept in place but not 
#   recalculated after 2012.  Therefore they are assumed to be the mininmium from *start to 2012.  They were calculated
#   that way in the "tanner crab data.xlxs" file in this folder.  The same simulations were done with this data below 
#   This was only done in an attempt to match historically reported total biomass (see annual stock health document - figure 1)

# LEGAL adjusted biomass 
dat_adj <- read.csv("C:/Users/kjpalof/Documents/R projects/survey-cut-sim_kp/data/tanner crab data_legal_adj.csv") # biomass by year and survey area
#percent <- read.csv("C:/Users/kjpalof/Documents/R projects/survey-cut-sim_kp/data/tanner crab percent.csv") # file with the percent contribution for each area

head(dat_adj) # just the biomass of legal crab 
head(percent)

#combine the two data sets
####
dat_adj %>%
  right_join(percent, by="SurveyArea") -> dat4
head(dat4)

# using percent for each area from the last 10 years of the survey - percent_survey_area
dat4 %>%
  mutate(percent_real = ifelse (Biomass_lb > 0, percent_survey_area, 0)) %>%
  group_by(Year_Survey) %>%
  # want to exclude areas that do not have biomass estimates
  summarize(expansion = sum (percent_real, na.rm=T), survey_biomass = sum (Biomass_lb, na.rm =T)) %>%
  # summarizes the expansion for each year based on the areas with  biomass estimates
  mutate(non_biomass = (survey_biomass / expansion) - survey_biomass, total_lb = survey_biomass + non_biomass) -> known_dat_survey_adj
ggplot(known_dat_survey_adj, aes(Year_Survey, total_lb)) + geom_point(size=2) +geom_smooth() +
  scale_color_tableau() +
  scale_x_continuous(breaks = seq(1996, 2016, 2))+ scale_y_continuous(breaks = seq(0, 60000000, 1000000))+
  coord_cartesian(ylim=c(0,6000000)) +
  xlab('Survey Year') + ylab('Total LEGAL biomass adj')+
  ggtitle('Tanner crab total LEGAL biomass (survey and non)')
ggsave("known_dat_adj.png")

#####################################
#####################  Simulations 
###################################
sim2015_adj <- survey_sim (dat4, areas_2015)
ggplot(sim2015_adj, aes(Year_Survey, total_lb, color = type)) + geom_point(size=2) +geom_smooth() +
  scale_color_tableau() +
  scale_x_continuous(breaks = seq(1996, 2016, 2))+ scale_y_continuous(breaks = seq(0, 55000000, 1000000))+
  coord_cartesian(ylim=c(0,5500000)) +
  xlab('Survey Year') + ylab('Total LEGAL biomass adj')+
  ggtitle('2015 survey areas simulation')
ggsave("sim2015_adj.png")

#survey_sim(dat2, areas_all) # check to confirm that the difference is 0 when all areas are included. 

simRKC2_adj <- survey_sim (dat4, areas_RKC_only)
ggplot(simRKC2_adj, aes(Year_Survey, total_lb, color = type)) + geom_point(size=2) +geom_smooth() +
  scale_color_tableau() +
  scale_x_continuous(breaks = seq(1996, 2016, 2))+ scale_y_continuous(breaks = seq(0, 55000000, 1000000))+
  coord_cartesian(ylim=c(0,5500000)) +
  xlab('Survey Year') + ylab('Total LEGAL biomass adj')+
  ggtitle('RKC survey only simulation')
ggsave("simRKC2_adj.png")

sim_areas1_adj <- survey_sim(dat4, areas_1)
ggplot(sim_areas1_adj, aes(Year_Survey, total_lb, color = type)) + geom_point(size=2) +geom_smooth() +
  scale_color_tableau() +
  scale_x_continuous(breaks = seq(1996, 2016, 2))+ scale_y_continuous(breaks = seq(0, 55000000, 1000000))+
  coord_cartesian(ylim=c(0,5500000)) +
  xlab('Survey Year') + ylab('Total LEGAL biomass adj')+
  ggtitle('sim using only leg 1 TCS and 2015 areas')
ggsave("sim_areas1_adj.png")


# table to summarize the average % difference  over all years for each simulation
#sim2015_adj$per_diff
#sim2015_adj$per_diff[20:38]
#mean (sim2015_adj$per_diff[20:38])

sim2015_adj_avg <- round(mean (sim2015_adj$per_diff[20:38]),4)*100
simRKC2_adj_avg <- round(mean (simRKC2_adj$per_diff[20:38]),4)*100
sim_areas1_adj_avg <- round(mean (sim_areas1_adj$per_diff[20:38]),4)*100

#per_diff <- c(sim2015_adj_avg, simRKC2_adj_avg, sim_areas1_adj_avg)
table1 <- matrix(c(sim2015_adj_avg, simRKC2_adj_avg, sim_areas1_adj_avg), ncol =3, byrow=TRUE)
colnames(table1) <- c("Sim 1", "Sim 2", "Sim 3")
rownames(table1) <- c("average % difference")
table1
####

