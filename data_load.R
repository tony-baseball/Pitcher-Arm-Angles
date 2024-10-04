library(RSQLite)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(ggimage)
library(ggrepel)

# PREP FUNCTIONS ---------------------------------------------------------------
source('functions.R')
 
# DATA -------------------------------------------------------------------------

# rosters csv
rosters <- read.csv('rosters.csv') %>%
  mutate(height_inches = convert_to_inches(HEIGHT),
         height_inches = 
           # If a player has no height, then put them at 6 feet tall
           ifelse(is.na(height_inches), mean(height_inches[grepl("P",POSITION)], na.rm = T), 72),
  )


# pitch data sample csv
pitch_data <- read.csv('pitch_data_raw.csv') %>%
  left_join(rosters %>% select(NAME, height_inches), by = c('Pitcher' = 'NAME'), relationship = "many-to-many") %>%
  arm_angle_calc()

# LEAGUE AVERAGE PITCH DATA BY HANDEDNESS AND PITCH TYPE
pitch_data_lg_avg <- read.csv('league_avg.csv')


# WORKING FUNCTION -------------------------------------------------------------
source('function_plot.R')


plot_type <- c('movement', 'savant', 'arm_angle')

pitcher_plot('Cole Cook', 'movement') # movement, savant, arm_angle

