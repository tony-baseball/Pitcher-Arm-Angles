library(dplyr)
library(baseballr) # for Statcast data
library(RSQLite)
library(stringr)

library(ggplot2)
library(grid)
library(gridExtra)
library(ggforce)
# DATA ------------------------------------------------ ----
espn_player_info <- read.csv('data/espnPlayerInfo.csv')

player_info <- read.csv('data/PlayerInfo.csv') %>%
  rename(pitcher_name = fullName, pitcher = id) %>%
  distinct(pitcher, .keep_all = TRUE)
teams <- read.csv('data/teams.csv')

pitchArmAngles <- read.csv('data/teams.csv')

overallArmAngles <- read.csv('data/overallArmAngles.csv')

# FUNCTIONS ------------------------------------------ ----
# Merge the Pitcher Info w/ Height from MLBStatsAPI
merge_pitcher_info <- function(df) {
  pitcher_info <- read.csv('data/playerInfo.csv', usecols = c('id', 'fullName', 'height')) %>%
    rename(pitcher_name = fullName, pitcher = id) %>%
    distinct(pitcher, .keep_all = TRUE)
  
  df <- df %>%
    left_join(pitcher_info, by = 'pitcher')
  
  return(df)
}

# Calculate the Arm Angle from Baseball Savant data
calculate_arm_angles <- function(df) {
  df <- df %>%
    mutate(
      release_pos_x = release_pos_x * 12,
      release_pos_z = release_pos_z * 12,
      shoulder_pos = height * 0.70,
      Adj = release_pos_z - shoulder_pos,
      Opp = abs(release_pos_x),
      arm_angle_rad = atan2(Opp, Adj),
      arm_angle = arm_angle_rad * (180 / pi)
    ) %>%
    select(-Opp, -arm_angle_rad)
  
  return(df)
}

# Categorize Arm Angles
arm_angle_categories <- function(df) {
  bins <- c(0, 30, 60, 90, 120, 180)
  labels <- c('Overhand', 'High Three-Quarters', 'Low Three-Quarters', 'Sidearm', 'Submarine')
  
  df <- df %>%
    mutate(
      arm_angle_type = cut(arm_angle, breaks = bins, labels = labels, right = FALSE)
    )
  
  return(df)
}

# DATA _---------------------------------- ----
# Retrieve Statcast data
# data <- statcast_search(start_date = '2024-03-01', end_date = '2024-03-31')
mlb_db <- dbConnect(RSQLite::SQLite(), "C:/Users/tdmed/OneDrive/R_Codes/MLB_Modeling/mlb_pitch.sqlite")

dbListTables(mlb_db)

data <- dbGetQuery(mlb_db, 'select * from mlb_data')  %>%
  mutate(Pitcher = str_split(Pitcher, ", ", simplify = TRUE)) %>%
  mutate(Pitcher = paste(Pitcher[,2], Pitcher[,1])) %>%
  filter(game_type == 'R')

# Apply the defined functions
df <- data %>%
  left_join(player_info, by = c('Pitcher' = 'pitcher_name')) %>% 
  # merge_pitcher_info() %>%
  calculate_arm_angles() %>%
  arm_angle_categories()

# Display the first few rows of the relevant columns
head(df %>% select(Pitcher, arm_angle, arm_angle_type))

grouped_df <- df %>%
  group_by(Pitcher, PitcherThrows) %>%
  summarise(height = mean(height, na.rm = T),
            shoulder_pos = mean(shoulder_pos, na.rm = T),
            release_pos_x = mean(release_pos_x, na.rm = T),
            release_pos_z = mean(release_pos_z, na.rm = T),
            arm_angle = mean(arm_angle, na.rm = T)
            ) %>%
  arm_angle_categories()


# PLOT --------------------------------------------------- ----
plot_pitcher_arm_angle <- function(pitcher_name, pitcher_data) {
  pitcher <- pitcher_data %>% filter(Pitcher == !!pitcher_name)
  player_team_name <- unique(teams$name[match(pitcher$teamId, teams$id)])
  arm_angle_type <- unique(pitcher$arm_angle_type)
  arm_angle_deg <- round(mean(pitcher$arm_angle),1)
  p_sholder_pos <- round(mean(pitcher$shoulder_pos),1) 
  avg_rel_z <- round(mean(pitcher$release_pos_z),2) 
  avg_rel_x <- round(mean(pitcher$release_pos_x),2) 
  
  
  theta <- seq(0, pi, length.out = 100)  # Change the range to create a semi-circle that is right-side up
  r <- 40  # The horizontal range from -40 to 40
  
  # Calculate the x and y coordinates
  mound <- data.frame(
    x = r * cos(theta),
    y = 4 * sin(theta)  # Adjusting the height to go up to 4
  )
  
  # Plot shoulder position and release position
  p <- ggplot() +
    geom_point(aes(x = 0, y = shoulder_pos), data = pitcher, size = 5, shape = 21, fill = "cornflowerblue", color = "black") +
    geom_point(aes(x = avg_rel_x, y = avg_rel_z), data = pitcher, size = 5, shape = 21, fill = "orange", color = "black") +
    
    # Draw lines between shoulder and release points
    geom_segment(aes(x = 0, xend = avg_rel_x, y = shoulder_pos, yend = avg_rel_z), 
                 data = pitcher, color = "black", size = 0.75) +
    
    # # Draw the mound as an ellipse
    # geom_ellipse(aes(x0 = 0, y0 = 0, a = 36, b = 3), fill = "brown", color = "black", size = 1) +
    geom_polygon(data = mound, aes(x = x, y = y), fill = "#8B4513") +
    
    # Draw the rubber as a rectangle
    geom_rect(aes(xmin = -12, xmax = 12, ymin = 3, ymax = 4), fill = "white", color = "black") +
    
    # Plot the distribution of league release points
    # geom_point(aes(x = release_pos_x, y = release_pos_z), data = pitcher_data, 
    #            color = "lightgray", alpha = 0.3, size = 3) +
    
    labs(
      title = paste(pitcher_name, "Arm Angle Plot"),
      subtitle = paste(player_team_name, "-", pitcher$PitcherThrows[1], "HP\n",
                       arm_angle_type, arm_angle_deg, "°"
                       ),
      caption = "Data: Baseball Savant | Methodology: Logan Mottely | Code: @TrevorThrash",
      x = "Horizontal Release (In.)", y = "Vertical Release (In.)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray"),
      plot.caption = element_text(hjust = 0.5, size = 8, color = "lightgray"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      panel.grid.major = element_line(color = "lightgray", linetype = "dashed")
    ) +
    coord_fixed(ratio = 1, xlim = c(-72, 72), ylim = c(0, 100))
  
  # Add annotation for the arm angle
  arm_angle <- pitcher$arm_angle[1]
  arm_angle_type <- pitcher$arm_angle_type[1]
  
  p <- p + annotate("text", x = 0, y = pitcher$shoulder_pos[1] + ifelse(arm_angle > 90, 5, -5), 
                    label = paste(arm_angle_deg, "°"), color = "black", size = 4, hjust = 0.5)
  
  # If an image exists for the pitcher, add it to the plot
  # img_path <- paste0('player_img/', pitcher_name, '.png')
  # if (file.exists(img_path)) {
  #   image <- png::readPNG(img_path)
  #   grid::grid.raster(image, x = 0.9, y = 1.12, width = unit(0.3, "npc"), just = c("center", "center"))
  # }
  
  print(p)
}

plot_pitcher_arm_angle('Chris Flexen', df)


