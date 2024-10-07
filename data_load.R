{
  library(RSQLite)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(plotly)
  library(ggimage)
  library(ggrepel)
  library(car)
}
# PREP FUNCTIONS ---------------------------------------------------------------
source('functions.R')

# DATA -------------------------------------------------------------------------

# rosters csv
rosters <- read.csv('data/rosters.csv') %>%
  mutate(height_inches = convert_to_inches(HEIGHT),
         height_inches = 
           # If a player has no height, then put the avg height of all pitchers
           ifelse(is.na(height_inches), mean(height_inches[grepl("P",POSITION)], na.rm = T), height_inches),
  )


# pitch data sample csv
pitch_data <- read.csv('data/pitch_data_raw.csv') %>%
  arm_angle_calc()

# LEAGUE AVERAGE PITCH DATA BY HANDEDNESS AND PITCH TYPE - FULL SEASON
pitch_data_lg_avg <- read.csv('data/league_avg.csv')%>% 
  mutate(TaggedPitchType = factor(TaggedPitchType, levels = c('FB', 'SI', 'CT', 'CB', 'SL', 'CH', 'SPL', 'KN', 'OT') ) )


# WORKING FUNCTION -------------------------------------------------------------
source('function_plot.R')

# plot_type <- c('movement', 'savant', 'arm_angle')

pitcher_plot('Cole Cook', 'arm_angle') 

