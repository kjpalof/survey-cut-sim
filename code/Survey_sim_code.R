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
#Data needed: 1) biomass for each area for each year
#             2) % survey area contribution based on historic catch 
# Regional biomass is the sum of the survey area biomasses and the non-surveyed areas expanded biomass
#   Biomass is expanding by the equation: 
#         total regional biomass = Survey area biomass/ % biomass in survey areas
# for 2015/2016 this is 2,142,529 / 0.66 = 3,246,255

dat <- read.csv("C:/Users/kjpalof/Documents/R projects/survey-cut-sim_kp/data/tanner crab data.csv") # biomass by year and survey area
percent <- read.csv("C:/Users/kjpalof/Documents/R projects/survey-cut-sim_kp/data/tanner crab percent.csv") # file with the percent contribution for each area

head(dat) # just the biomass of legal crab 
head(percent)

# need to summarize biomass by year but need to know which areas are included each year....not sure how to get 
# this last part.

#combine the two data sets
dat %>%
  right_join(percent, by="SurveyArea") -> dat2
head(dat2)

#check to make sure that they survey areas add up to 0.7116 percent in years with all areas surveyed   
# true except for 2015 - this should be less due to the removal of port frederick and port camden
dat2 %>%
  group_by(Year_Survey) %>%
  mutate(percent_real = ifelse (Biomass_lb > 0, percent_surveyarea, 0)) %>% # want to exclude areas that do not have biomass estimates
  summarize(expansion = sum (percent_real, na.rm=T)) # summarizes the expansion for each year based on the areas with  
                                                    # biomass estimates

dat2 %>%
  filter(Year_Survey == 1997)

# calculate known biomass for each year and then add expansion biomass and total biomass columns
dat2 %>%
  mutate(percent_real = ifelse (Biomass_lb > 0, percent_surveyarea, 0)) %>%
  group_by(Year_Survey) %>%
   # want to exclude areas that do not have biomass estimates
  summarize(expansion = sum (percent_real, na.rm=T), survey_biomass = sum (Biomass_lb, na.rm =T)) %>%
  # summarizes the expansion for each year based on the areas with  biomass estimates
  mutate(non_biomass = (survey_biomass / expansion) - survey_biomass, total_lb = survey_biomass + non_biomass) -> known_dat

#Need to figure out a way to choose which areas are included in the above calculation of expansion factor
dat2 %>%
  filter(Year_Survey ==2014) %>%
  mutate(percent_used = ifelse(SurveyArea %in% areas_2015, percent_surveyarea, 0)) #%>%
  summarise(expand2015 = sum(percent_used))


areas_all <- c("EI" , "GB" , "GLB" ,"HB"  ,"ICY" ,"LS",  "NJ" , "PB" , "PC" , "PF" , "PS",  "SC" , "SP" , "TB")
areas_all
areas_used <- c("EI" , "GB" , "GLB" ,"HB"  ,"ICY" ,"LS",  "NJ" , "PB" , "PS",  "SC" , "SP" , "TB")



survey_sim <- function(df, areas_used) {
  dat2 %>%
    mutate(percent_usedA = ifelse(SurveyArea %in% areas_used, percent_surveyarea, 0)) %>%
    mutate(percent_used = ifelse(Biomass_lb >0, percent_usedA, 0)) %>%
    #only want to use biomass from areas that are included in the sim
    mutate(biomass_used = ifelse(SurveyArea %in% areas_used, Biomass_lb, 0)) %>%
    group_by(Year_Survey) %>%
    # want to exclude areas that do not have biomass estimates
    summarize(sim_expansion = sum (percent_used, na.rm=T), survey_biomass = sum (biomass_used, na.rm =T)) %>%
    # summarizes the expansion for each year based on the areas with  biomass estimates
    mutate(non_biomass = (survey_biomass / sim_expansion) - survey_biomass, simtotal_lb = survey_biomass + non_biomass) -> sim
  df %>%
    mutate(percent_real = ifelse (Biomass_lb > 0, percent_surveyarea, 0)) %>%
    group_by(Year_Survey) %>%
    # want to exclude areas that do not have biomass estimates
    summarize(expansion = sum (percent_real, na.rm=T), survey_biomass = sum (Biomass_lb, na.rm =T)) %>%
    # summarizes the expansion for each year based on the areas with  biomass estimates
    mutate(non_biomass = (survey_biomass / expansion) - survey_biomass, total_lb = survey_biomass + non_biomass) -> known_dat
  known_dat %>%
    mutate(sim_expansion = sim$sim_expansion, sim_total_lb = sim$simtotal_lb)%>%
    mutate(diff_lb = (total_lb - sim$simtotal_lb))
    
}

survey_sim (dat2, areas_2015)
  