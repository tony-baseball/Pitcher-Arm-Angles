library(RSQLite)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(ggimage)
library(ggrepel)
library(car)

# PREP FUNCTIONS ---------------------------------------------------------------
# calculate arm angle function
arm_angle_calc <- function(data_frame) {
  
  data_frame <- data_frame %>%
    mutate(height_inches = rosters$height_inches[match(Pitcher, rosters$NAME)],
           height_inches = ifelse(is.na(height_inches), mean(rosters$height_inches[grepl("P",rosters$POSITION)], na.rm = T), height_inches),
           arm_length = height_inches * .39
    ) %>%
    mutate(
      RelSide_in = RelSide * 12,
      RelHeight_in = RelHeight * 12,
      shoulder_pos = height_inches * 0.70,
      Adj = RelHeight_in - shoulder_pos,
      Opp = abs(RelSide_in),
      arm_angle_rad = atan2(Opp, Adj),
      arm_angle = arm_angle_rad * (180 / pi),
      # arm_angle2 = atan2(abs(RelSide * 12), ((RelHeight * 12) - (height_inches * 0.70))) * (180 / pi)
    ) %>%
    select(-Opp, -arm_angle_rad) %>%
    mutate(
      arm_angle_180 = case_when(
        PitcherThrows == 'Left' ~ 180 - arm_angle,
        PitcherThrows == 'Right' ~ 180 + arm_angle,
        T ~ arm_angle
      ),
      arm_angle_savant = case_when(
        between(arm_angle, 0,90) ~ 90 - arm_angle,
        arm_angle > 90 ~ (90 - arm_angle),
        T ~ NA
      ), 
      .after = arm_angle
    )
  
}

# convert height from feet to inches
convert_to_inches <- function(height) {
  # Create a named vector of patterns to replace with a common separator
  replacements <- c("-" = "'", "'" = "'", "''" = "'", "\"" = "", "/" = "'", "\\?" = "'", ":" = "'", "\\;" = "'")
  
  # Replace all patterns in the height string
  height <- str_replace_all(height, replacements)
  
  # Extract feet and inches
  parts <- str_split(height, "'", simplify = TRUE)
  feet <- as.numeric(parts[, 1])
  inches <- as.numeric(parts[, 2])
  
  # Convert to total inches
  total_inches <- feet * 12 + inches
  return(total_inches)
}

# function to categorize arm angle
arm_angle_categories <- function(df) {
  bins <- c(0, 30, 60, 90, 120, 180)
  labels <- c('Overhand', 'High Three-Quarters', 'Low Three-Quarters', 'Sidearm', 'Submarine')
  
  df <- df %>%
    mutate(
      arm_angle_type = cut(arm_angle, breaks = bins, labels = labels, right = FALSE)
    )
  
  return(df)
}

# circle function for plot arm angle savant circular
circleFun <- function(center = c(0, 0), radius = 24, npoints = 100) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  data.frame(
    x = center[1] + radius * cos(tt),
    y = center[2] + radius * sin(tt)
  )
}

# creats a circle for the SAVANT plot
circle <- circleFun(center = c(0, 0), radius = 24)

{ # code for the pitcher's mound in the ARM ANGLE plot
  df <- data.frame(x = 0.5, y = 0)
  theta <- seq(0, pi, length.out = 100)  # Change the range to create a semi-circle that is right-side up for the mound!
  r <- 40  # The horizontal range from -40 to 40 for the mound!
  # Calculate the x and y coordinates
  mound <- data.frame(
    x = r * cos(theta),
    y = 4 * sin(theta)
  )
}

# pitch colors table for SAVANT and MOVEMENT
pitch_colors = data.frame(TaggedPitchType = c("Fastball", "Sinker", "Cutter", "Curveball", 
                                              "Slider", "Changeup", "Splitter", "Knuckleball", "Other"),
                          PitchCode = c('FB', 'SI', 'CT', 'CB', 'SL', 'CH', 'SPL', 'KN', 'OT'),
                          Color = c('red', '#a34700', 'gold', 'darkgreen', 'cornflowerblue',
                                    'violet',  'black',  'black',  'black'))