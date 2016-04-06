# 1-5-16 
# Katie Palof, Alaska Department of Fish and Game
# Code for developing a simulation to deal with survey cuts to crab surveys

# Goal: to create a simulation that would allow you to remove each areas component to the survey biomass estimate
# and place it in the NON-survey expansion factor part of the regional estimate.

library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(reshape2)

#Load data
#####
#Data needed: 1) biomass for each area for each year
#             2) % survey area contribution based on historic catch 
# Regional biomass is the sum of the survey area biomasses and the non-surveyed areas expanded biomass
#   Biomass is expanding by the equation: 
#         total regional biomass = Survey area biomass/ % biomass in survey areas
# for 2015/2016 this is 2,142,529 / 0.66 = 3,246,255
getwd()
setwd("C:/Users/kjpalof/Documents/R projects/survey-cut-sim_kp/code")
dat <- read.csv("C:/Users/kjpalof/Documents/R projects/survey-cut-sim_kp/data/tanner crab data.csv") # biomass by year and survey area
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

############################
#################### load survey_sim from function.R file ########################

sim2015 <- survey_sim (dat2, areas_2015)
ggplot(sim2015, aes(Year_Survey, total_lb, color = type)) + geom_point() +geom_smooth()

#survey_sim(dat2, areas_all) # check to confirm that the difference is 0 when all areas are included. 

simRKC2 <- survey_sim (dat2, areas_RKC_only)
ggplot(simRKC2, aes(Year_Survey, total_lb, color = type)) + geom_point() +geom_smooth()


