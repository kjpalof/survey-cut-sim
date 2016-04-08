# 1-5-16
# Katie J Palof, ADF&G
# Function for survey cut simulations, use in R code.R


#using historic survey areas percentage
survey_sim_harvest <- function(df, areas_used) {
  df %>%
    mutate(percent_usedA = ifelse(SurveyArea %in% areas_used, percent_hist_harvest, 0)) %>%
    mutate(percent_used = ifelse(Biomass_lb >0, percent_usedA, 0)) %>%
    #only want to use biomass from areas that are included in the sim
    mutate(biomass_used = ifelse(SurveyArea %in% areas_used, Biomass_lb, 0)) %>%
    group_by(Year_Survey) %>%
    # want to exclude areas that do not have biomass estimates
    summarize(expansion = sum (percent_used, na.rm=T), survey_biomass = sum (biomass_used, na.rm =T)) %>%
    # summarizes the expansion for each year based on the areas with  biomass estimates
    mutate(non_biomass = (survey_biomass / expansion) - survey_biomass, total_lb = survey_biomass + non_biomass, type = "S") -> sim
  df %>%
    mutate(percent_real = ifelse (Biomass_lb > 0, percent_hist_harvest, 0)) %>%
    group_by(Year_Survey) %>%
    # want to exclude areas that do not have biomass estimates
    summarize(expansion = sum (percent_real, na.rm=T), survey_biomass = sum (Biomass_lb, na.rm =T)) %>%
    # summarizes the expansion for each year based on the areas with  biomass estimates
    mutate(non_biomass = (survey_biomass / expansion) - survey_biomass, total_lb = survey_biomass + non_biomass, type = "K") -> known_dat_harvest
  out <- rbind(known_dat_harvest, sim)
  out %>%
    mutate (diff = ifelse( type == "K", 0, (known_dat_harvest$total_lb - sim$total_lb))) 
  
}

survey_sim_harvest (dat2, areas_2015)


#using the last 10 years of survey biomass for each area to calculate percent contribution
survey_sim <- function(df, areas_used) {
  df %>%
    mutate(percent_usedA = ifelse(SurveyArea %in% areas_used, percent_survey_area, 0)) %>%
    mutate(percent_used = ifelse(Biomass_lb >0, percent_usedA, 0)) %>%
    #only want to use biomass from areas that are included in the sim
    mutate(biomass_used = ifelse(SurveyArea %in% areas_used, Biomass_lb, 0)) %>%
    group_by(Year_Survey) %>%
    # want to exclude areas that do not have biomass estimates
    summarize(expansion = sum (percent_used, na.rm=T), survey_biomass = sum (biomass_used, na.rm =T)) %>%
    # summarizes the expansion for each year based on the areas with  biomass estimates
    # need to add type for graphing K = known data
    mutate(non_biomass = (survey_biomass / expansion) - survey_biomass, total_lb = survey_biomass + non_biomass, type = "S") -> sim2
  df %>%
    mutate(percent_real = ifelse (Biomass_lb > 0, percent_survey_area, 0)) %>%
    group_by(Year_Survey) %>%
    # want to exclude areas that do not have biomass estimates
    summarize(expansion = sum (percent_real, na.rm=T), survey_biomass = sum (Biomass_lb, na.rm =T)) %>%
    # summarizes the expansion for each year based on the areas with  biomass estimates
    # need to add type for graphing 
    mutate(non_biomass = (survey_biomass / expansion) - survey_biomass, total_lb = survey_biomass + non_biomass, type = "K") -> known_dat_survey
  out <- rbind(known_dat_survey, sim2)
  out %>%
    mutate (diff = ifelse( type == "K", 0, (known_dat_survey$total_lb - sim2$total_lb))) 

}



# want to add the simulation expansion and total biomass 
#known_dat_survey %>%
 # mutate (type = "K") -> known_dat_survey
#sim2 %>%
#  mutate (type = "S")-> sim2

#test <- rbind(known_dat_survey, sim2)
# want to add the difference between the sim and the known in a column. 
#test %>%
 # mutate (diff = ifelse( type == "K", 0, (known_dat_survey$total_lb - sim2$total_lb))) -> test

ggplot(test, aes(Year_Survey, total_lb, color = type)) + geom_point() +geom_smooth()


