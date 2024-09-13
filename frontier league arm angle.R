# Convert FL heights to inches
db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Shiny/FLASH/flashdb.sqlite")

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

arm_angle_categories <- function(df) {
  bins <- c(0, 30, 60, 90, 120, 180)
  labels <- c('Overhand', 'High Three-Quarters', 'Low Three-Quarters', 'Sidearm', 'Submarine')
  
  df <- df %>%
    mutate(
      arm_angle_type = cut(arm_angle, breaks = bins, labels = labels, right = FALSE)
    )
  
  return(df)
}
rosters <- dbGetQuery(db, 'select * from rosters_')

rosters_ <- rosters %>%
  filter(!grepl('Sudden', NAME)) %>%
  mutate(height_in_inches = convert_to_inches(HEIGHT))
  
sum(is.na(rosters_$height_in_inches))


y <- dbGetQuery(db, 'select Pitcher, PitcherTeam, PitcherThrows, RelHeight, RelSide from pitch_data where SEASON = 2024' ) %>%
  left_join(rosters_ %>% select(NAME,height_in_inches), by = c('Pitcher' = 'NAME')) %>%
  # group_by(Pitcher) %>%
  mutate(
    RelSide = RelSide * 12,
    RelHeight = RelHeight * 12,
    shoulder_pos = height_in_inches * 0.70,
    Adj = RelHeight - shoulder_pos,
    Opp = abs(RelSide),
    arm_angle_rad = atan2(Opp, Adj),
    arm_angle = arm_angle_rad * (180 / pi)
  ) %>%
  select(-Opp, -arm_angle_rad)   %>%
  filter(RelHeight <= 7.6 * 12 | RelHeight > .9 * 12) %>%
  filter(abs(RelSide) <= 5.5 * 12 ) %>%
  arm_angle_categories()
  
# %>%
#   group_by(Pitcher, PitcherThrows) %>%
#   summarise(height_in_inches = mean(height_in_inches, na.rm = T),
#             shoulder_pos = mean(shoulder_pos, na.rm = T),
#             release_pos_x = mean(RelSide, na.rm = T),
#             release_pos_z = mean(RelHeight, na.rm = T),
#             arm_angle = mean(arm_angle, na.rm = T)
#   ) %>%
#   arm_angle_categories() 

y_grouped <- dbGetQuery(db, 'select Pitcher, PitcherTeam, PitcherThrows, RelHeight, RelSide from pitch_data' ) %>%
  left_join(rosters_ %>% select(NAME,height_in_inches), by = c('Pitcher' = 'NAME')) %>%
  # group_by(Pitcher) %>%
  mutate(
    RelSide = RelSide * 12,
    RelHeight = RelHeight * 12,
    shoulder_pos = height_in_inches * 0.70,
    Adj = RelHeight - shoulder_pos,
    Opp = abs(RelSide),
    arm_angle_rad = atan2(Opp, Adj),
    arm_angle = arm_angle_rad * (180 / pi)
  ) %>%
  select(-Opp, -arm_angle_rad)  %>%
  filter(RelHeight <= 7.6 * 12 | RelHeight > .9 * 12) %>%
  filter(abs(RelSide) <= 5 * 12 ) %>%
  arm_angle_categories() %>%
  group_by(Pitcher, PitcherThrows) %>%
  summarise(PitcherTeam = unique(PitcherTeam),
            height_in_inches = mean(height_in_inches, na.rm = T),
            shoulder_pos = mean(shoulder_pos, na.rm = T),
            release_pos_x = mean(RelSide, na.rm = T),
            release_pos_z = mean(RelHeight, na.rm = T),
            arm_angle = mean(arm_angle, na.rm = T)
  ) %>%
  arm_angle_categories()

plot_pitcher_arm_angle_yt <- function(pitcher_name, pitcher_data) {
  pitcher <- pitcher_data %>% filter(Pitcher == !!pitcher_name)
  player_team_name <- tail(pitcher$PitcherTeam)
  arm_angle_type <- y_grouped$arm_angle_type[match(pitcher$Pitcher, y_grouped$Pitcher)]
  arm_angle_deg <- round(mean(pitcher$arm_angle, na.rm = T),1)
  p_sholder_pos <- round(mean(pitcher$shoulder_pos, na.rm = T),1) 
  # p_sholder_pos <- round(mean(y$shoulder_pos[y$Pitcher=='Cole Cook'], na.rm = T),1)
  avg_rel_z <- round(mean(pitcher$RelHeight, na.rm = T),2) 
  avg_rel_x <- -round(mean(pitcher$RelSide, na.rm = T),2) 
  
  
  theta <- seq(0, pi, length.out = 100)  # Change the range to create a semi-circle that is right-side up
  r <- 40  # The horizontal range from -40 to 40
  
  # Calculate the x and y coordinates
  mound <- data.frame(
    x = r * cos(theta),
    y = 4 * sin(theta)  # Adjusting the height to go up to 4
  )
  
  # Plot shoulder position and release position
  p <- ggplot() +
    geom_point(aes(x = 0, y = p_sholder_pos), data = pitcher, size = 5, shape = 21, fill = "cornflowerblue", color = "black") +
    geom_point(aes(x = avg_rel_x, y = avg_rel_z), data = pitcher, size = 5, shape = 21, fill = "orange", color = "black") +
    
    # Draw lines between shoulder and release points
    geom_segment(aes(x = 0, xend = avg_rel_x, y = p_sholder_pos, yend = avg_rel_z), 
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
    coord_fixed(ratio = 1, xlim = c(-61, 61), ylim = c(0, 100))
  
  # Add annotation for the arm angle
  arm_angle <- pitcher$arm_angle[1]
  arm_angle_type <- pitcher$arm_angle_type[1]
  
  p <- p + annotate("text", x = 0, y = p_sholder_pos + ifelse(arm_angle > 90, 5, -5), 
                    label = paste(arm_angle_deg, "°"), color = "black", size = 4, hjust = 0.5)
  
  # If an image exists for the pitcher, add it to the plot
  # img_path <- paste0('player_img/', pitcher_name, '.png')
  # if (file.exists(img_path)) {
  #   image <- png::readPNG(img_path)
  #   grid::grid.raster(image, x = 0.9, y = 1.12, width = unit(0.3, "npc"), just = c("center", "center"))
  # }
  
  print(p)
}

plot_pitcher_arm_angle_yt('Michael McAvene', y)
