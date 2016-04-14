# 4-7-16
# Katie Palof, Alaska Department of Fish and Game
# Code for a simulation to deal with survey cuts to crab surveys

################
## This code has the data for doing the simulations with MATURE biomass 
  # see Survey_sim_code for legal biomass
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(extrafont)
library(ggthemes)
#set graphing theme
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size = 12, base_family = 'Times New Roman')+ 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()))

#Load data
#####
#Data needed: 1) biomass for each area for each year ONLY Mature BIOMASS USED HERE
#             2) % survey area contribution based on historic catch 
# Regional biomass is the sum of the survey area biomasses and the non-surveyed areas expanded biomass
#   Biomass is expanding by the equation: 
#         total regional biomass = Survey area biomass/ % biomass in survey areas
# for 2015/2016 this is 2,142,529 / 0.66 = 3,246,255 # this is legal biomass and won't match what's calculated here since
#                                                           #this 66% was calculated by subtracting and not in the way calculated here.
getwd()
setwd("C:/Users/kjpalof/Documents/R projects/survey-cut-sim_kp/data")
dat_mature <- read.csv("C:/Users/kjpalof/Documents/R projects/survey-cut-sim_kp/data/tanner crab data_mature.csv") # biomass by year and survey area
percent <- read.csv("C:/Users/kjpalof/Documents/R projects/survey-cut-sim_kp/data/tanner crab percent.csv") # file with the percent contribution for each area
dat_mature_adj <-read.csv("C:/Users/kjpalof/Documents/R projects/survey-cut-sim_kp/data/tanner crab data_mature_adj.csv") 
# adjusted to accomidate how early biomasses were calculated in areas not surveyed.

head(dat_mature) # just the biomass of legal crab 
head(percent)
#####
# need to summarize biomass by year but need to know which areas are included each year

#combine the two data sets
#####
dat_mature %>%
  right_join(percent, by="SurveyArea") -> dat3
head(dat3)

# calculate known biomass for each year and then add expansion biomass and total biomass columns
# using percent for each area from historic harvest - percent_hist_harvest
# how to creat known data data frames using either hist_harvest or survey area % 
##### 
dat3 %>%
  mutate(percent_real = ifelse (Biomass_lb > 0, percent_hist_harvest, 0)) %>%
  group_by(Year_Survey) %>%
  # want to exclude areas that do not have biomass estimates
  summarize(expansion = sum (percent_real, na.rm=T), survey_biomass = sum (Biomass_lb, na.rm =T)) %>%
  # summarizes the expansion for each year based on the areas with  biomass estimates
  mutate(non_biomass = (survey_biomass / expansion) - survey_biomass, total_lb = survey_biomass + non_biomass) -> known_mature_harvest
# using percent for each area from the last 10 years of the survey - percent_survey_area
dat3 %>%
  mutate(percent_real = ifelse (Biomass_lb > 0, mature_survey_percent, 0)) %>%
  group_by(Year_Survey) %>%
  # want to exclude areas that do not have biomass estimates
  summarize(expansion = sum (percent_real, na.rm=T), survey_biomass = sum (Biomass_lb, na.rm =T)) %>%
  # summarizes the expansion for each year based on the areas with  biomass estimates
  mutate(non_biomass = (survey_biomass / expansion) - survey_biomass, total_lb = survey_biomass + non_biomass) -> known_mature_survey

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

sim2015mature <- survey_sim_mature (dat3, areas_2015)
ggplot(sim2015mature, aes(Year_Survey, total_lb, color = type)) + geom_point(size=2) +geom_smooth() +
  scale_color_tableau() +
  scale_x_continuous(breaks = seq(1996, 2016, 2))+ scale_y_continuous(breaks = seq(0, 85000000, 1000000))+
  coord_cartesian(ylim=c(0,8500000)) +
  xlab('Survey Year') + ylab('Total MATURE biomass')+
  ggtitle('2015 survey areas simulation mature lb')
ggsave("sim2015mature.png")

#survey_sim(dat2, areas_all) # check to confirm that the difference is 0 when all areas are included. 

simRKC2mature <- survey_sim_mature (dat3, areas_RKC_only)
ggplot(simRKC2mature, aes(Year_Survey, total_lb, color = type)) + geom_point(size=2) +geom_smooth() +
  scale_color_tableau() +
  scale_x_continuous(breaks = seq(1996, 2016, 2))+ scale_y_continuous(breaks = seq(0, 85000000, 1000000))+
  coord_cartesian(ylim=c(0,8500000)) +
  xlab('Survey Year') + ylab('Total MATURE biomass')+
  ggtitle('RKC survey only simulation mature lb')
ggsave("simRKC2mature.png")

sim_areas1mature <- survey_sim_mature(dat3, areas_1)
ggplot(sim_areas1mature, aes(Year_Survey, total_lb, color = type)) + geom_point(size=2) +geom_smooth() +
  scale_color_tableau() +
  scale_x_continuous(breaks = seq(1996, 2016, 2))+ scale_y_continuous(breaks = seq(0, 85000000, 1000000))+
  coord_cartesian(ylim=c(0,8500000)) +
  xlab('Survey Year') + ylab('Total MATURE biomass')+
  ggtitle('sim using only leg 1 TCS and 2015 areas mature lb')
ggsave("sim_areas1mature.png")

require(gridExtra)
grid.arrange(a, b, ncol=2)


########################
################################
############################################
####   Uses adjusted biomass.
#combine the two data sets
#####
dat_mature_adj %>%
  right_join(percent, by="SurveyArea") -> dat5
head(dat5)

# calculate known biomass for each year and then add expansion biomass and total biomass columns
# using percent for each area from historic harvest - percent_hist_harvest
# how to creat known data data frames using either hist_harvest or survey area % 
##### 

# using percent for each area from the last 10 years of the survey - percent_survey_area
dat5 %>%
  mutate(percent_real = ifelse (Biomass_lb > 0, mature_survey_percent, 0)) %>%
  group_by(Year_Survey) %>%
  # want to exclude areas that do not have biomass estimates
  summarize(expansion = sum (percent_real, na.rm=T), survey_biomass = sum (Biomass_lb, na.rm =T)) %>%
  # summarizes the expansion for each year based on the areas with  biomass estimates
  mutate(non_biomass = (survey_biomass / expansion) - survey_biomass, total_lb = survey_biomass + non_biomass) -> known_mature_survey_adj
ggplot(known_mature_survey_adj, aes(Year_Survey, total_lb)) + geom_point(size=2) +geom_smooth() +
  scale_color_tableau() +
  scale_x_continuous(breaks = seq(1996, 2016, 2))+ scale_y_continuous(breaks = seq(0, 80000000, 1000000))+
  coord_cartesian(ylim=c(0,8000000)) +
  xlab('Survey Year') + ylab('Total MATURE biomass adj')+
  ggtitle('Tanner crab total MATURE biomass (survey and non)')
ggsave("known_mature_adj.png")
# make sure you use the correct function 
# survey_sim uses survey area % contribution from the last 10 years the other - survey_sim_harvest - uses historic harvest contribution


############################
#################### load survey_sim_mature from function.R file ########################

sim2015mature_adj <- survey_sim_mature (dat5, areas_2015)
ggplot(sim2015mature_adj, aes(Year_Survey, total_lb, color = type)) + geom_point(size=2) +geom_smooth() +
  scale_color_tableau() +
  scale_x_continuous(breaks = seq(1996, 2016, 2))+ scale_y_continuous(breaks = seq(0, 8000000, 1000000))+
  coord_cartesian(ylim=c(0,8000000)) +
  xlab('Survey Year') + ylab('Total MATURE biomass adj')+
  ggtitle('2015 survey areas simulation MATURE total lb')
ggsave("sim2015mature_adj.png")

#survey_sim(dat2, areas_all) # check to confirm that the difference is 0 when all areas are included. 

simRKC2mature_adj <- survey_sim_mature (dat5, areas_RKC_only)
ggplot(simRKC2mature_adj, aes(Year_Survey, total_lb, color = type)) + geom_point(size=2) +geom_smooth() +
  scale_color_tableau() +
  scale_x_continuous(breaks = seq(1996, 2016, 2))+ scale_y_continuous(breaks = seq(0, 8000000, 1000000))+
  coord_cartesian(ylim=c(0,8000000)) +
  xlab('Survey Year') + ylab('Total MATURE biomass adj')+
  ggtitle('RKC survey only simulation MATURE total lb')
ggsave("simRKC2mature_adj.png")

sim_areas1mature_adj <- survey_sim_mature(dat5, areas_1)
ggplot(sim_areas1mature_adj, aes(Year_Survey, total_lb, color = type)) + geom_point(size=2) +geom_smooth() +
  scale_color_tableau() +
  scale_x_continuous(breaks = seq(1996, 2016, 2))+ scale_y_continuous(breaks = seq(0, 8000000, 1000000))+
  coord_cartesian(ylim=c(0,8000000)) +
  xlab('Survey Year') + ylab('Total MATURE biomass adj')+
  ggtitle('sim using only leg 1 TCS and 2015 areas MATURE total lb')
ggsave("sim_areas1mature_adj.png")
