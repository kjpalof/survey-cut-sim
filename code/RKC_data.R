# 4-28-16

# Katie Palof, Alaska Department of Fish and Game
# Code for developing a simulation to deal with survey cuts to crab surveys

# This code build upon code developed to look at Tanner crab biomass estimation - see markdown file - tanner_crab_simulation.Rmd

# Code for Red King crab simulations 
# need to import data - biomass estimations for RKC for each survey area and their % contributions - similar to tanner - see
#   tanner crab data.xlsx for example.

#load packages
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

#####Load Data ----------------------------------------
getwd()

dat_RKC <- read.csv("./data/red crab data_legal.csv") # biomass by year and survey area
percent_RKC <- read.csv("./data/red crab percent.csv") # file with the percent contribution for each area

head(dat_RKC) # just the biomass of legal crab 
head(percent_RKC)

#combine the two data sets
#####
dat_RKC %>%
  right_join(percent_RKC, by="SurveyArea") -> dat2_RKC
head(dat2_RKC)

#check to make sure that they survey areas add up to 0.6529 percent (survey areas) and 0.6354 percent for historic
# harvest in years with all areas surveyed   
# true except for 2015 - this should be less due to the removal of port frederick 
dat2_RKC    %>%
  group_by(Year_Survey) %>%
  mutate(percent_real = ifelse (Biomass_lb > 0, percent_hist_harvest, 0)) %>% # want to exclude areas that do not have biomass estimates
  summarize(expansion = sum (percent_real, na.rm=T)) # summarizes the expansion for each year based on the areas with  
# biomass estimates
#####

# calculate known biomass for each year and then add expansion biomass and total biomass columns
# using percent for each area from historic harvest - percent_hist_harvest
# how to creat known data data frames using either hist_harvest or survey area % 
##### 
dat2_RKC %>%
  mutate(percent_real = ifelse (Biomass_lb > 0, percent_hist_harvest, 0)) %>%
  group_by(Year_Survey) %>%
  # want to exclude areas that do not have biomass estimates
  summarize(expansion = sum (percent_real, na.rm=T), survey_biomass = sum (Biomass_lb, na.rm =T)) %>%
  # summarizes the expansion for each year based on the areas with  biomass estimates
  mutate(non_biomass = (survey_biomass / expansion) - survey_biomass, total_lb = survey_biomass + non_biomass) -> known_datRKC_harvest
# using percent for each area from the last 10 years of the survey - percent_survey_area
dat2_RKC %>%
  mutate(percent_real = ifelse (Biomass_lb > 0, percent_survey_area, 0)) %>%
  group_by(Year_Survey) %>%
  # want to exclude areas that do not have biomass estimates
  summarize(expansion = sum (percent_real, na.rm=T), survey_biomass = sum (Biomass_lb, na.rm =T)) %>%
  # summarizes the expansion for each year based on the areas with  biomass estimates
  mutate(non_biomass = (survey_biomass / expansion) - survey_biomass, total_lb = survey_biomass + non_biomass) -> known_datRKC_survey

plot(known_datRKC_survey$Year_Survey, known_datRKC_survey$survey_biomass)
#Need to figure out a way to choose which areas are included in the above calculation of expansion factor

##   STOPPING HERE with red crab.  biomasss data for each area is not matching those in sigma plot files.  Not 
# sure how to know what data is correct.  Will revisit if needed in the future.


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