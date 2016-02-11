# 1-5-16 
# Katie Palof, Alaska Department of Fish and Game
# Code for developing a simulation to deal with survey cuts to crab surveys

# Goal: to create a simulation that would allow you to remove each areas component to the survey biomass estimate
# and place it in the NON-survey expansion factor part of the regional estimate.

#Load data
#Data needed: 1) biomass for each area for each year
#             2) % survey area contribution based on historic catch 
# Regional biomass is the sum of the survey area biomasses and the non-surveyed areas expanded biomass
#   Biomass is expanding by the equation: 
#         total regional biomass = Survey area biomass/ % biomass in survey areas
# for 2015/2016 this is 2,142,529 / 0.66 = 3,246,255

dat <- read.csv("tanner crab data.csv") # biomass by year and survey area
percent <- read.csv("tanner crab percent.csv") # file with the percent contribution for each area

head(dat)
head(percent)

#attempt to committ 2