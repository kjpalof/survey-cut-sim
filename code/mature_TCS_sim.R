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
dat_mature <- read.csv("C:/Users/kjpalof/Documents/R projects/survey-cut-sim_kp/data/tanner crab data.csv") # biomass by year and survey area
percent <- read.csv("C:/Users/kjpalof/Documents/R projects/survey-cut-sim_kp/data/tanner crab percent.csv") # file with the percent contribution for each area

head(dat_mature) # just the biomass of legal crab 
head(percent)
#####
# need to summarize biomass by year but need to know which areas are included each year

#combine the two data sets
#####
dat_mature %>%
  right_join(percent, by="SurveyArea") -> dat3
head(dat3)