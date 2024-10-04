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
# change this
db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

# change this to a CSV
rosters <- dbGetQuery(db, 'select * from rosters_') %>%
  filter(!grepl('Sudden', NAME)) %>%
  mutate(height_inches = convert_to_inches(HEIGHT),
         height_inches = ifelse(
           is.na(height_inches), mean(height_inches[grepl("P",POSITION)], na.rm = T),
           height_inches
         ),
  )
# write.csv(dbGetQuery(db, 'select * from rosters_ where SEASON = 2024 and TEAM like "%Boomer%" and NAME not like "%Sudden%"'), 'rosters.csv', row.names = F, na='' )
# pitch data, change to sample csv
p <- dbGetQuery(db, 'select Date, Pitcher, PitcherThrows, PitcherTeam, TaggedPitchType, RelSpeed, InducedVertBreak, HorzBreak, SpinAxis,
                RelHeight, RelSide, Extension, SpinRate, yt_Efficiency, HomeTeamCode
                from pitch_data where PitcherTeam like "%Boomers%" and Season = 2024 and Date < "2024-06-15"')

# write.csv(p, 'pitch_data_raw.csv', row.names = F, na='')

# ALL PITCH DATA FOR SCHAUMBURG BOOMERS
pitch_data <- p %>%
  left_join(rosters %>% select(NAME, height_inches), by = c('Pitcher' = 'NAME'), relationship = "many-to-many") %>%
  arm_angle_calc()

# LEAGUE AVERAGE PITCH DATA BY HANDEDNESS AND PITCH TYPE
pitch_data_lg_Avg <- dbGetQuery(db, 'select PitcherThrows, TaggedPitchType, count(*) usage, avg(InducedVertBreak) InducedVertBreak, avg(HorzBreak) HorzBreak, avg(SpinAxis) SpinAxis
           from pitch_data where SEASON = 2024 AND PitcherThrows <>"" and TaggedPitchType is not NULL and TaggedPitchType <> ""
           group by PitcherThrows, TaggedPitchType') %>%
  mutate(TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball', 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup', 'Splitter', 'Knuckleball', 'Other') ),
         TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                  Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN', Splitter = 'SPL'  ) ) %>%
  group_by(PitcherThrows) %>%
  mutate(usage = round(usage / sum(usage),3)*100) %>%
  arrange(PitcherThrows, TaggedPitchType)


# MAIN FUNCTION ----------------------------------------------------------------
pitcher_plot <- function(pitcher_name, plot_type) {
  circle <- circleFun(center = c(0, 0), radius = 24)
  
  if(pitcher_name %in% pitch_data$Pitcher){
    # INDIVIDUAL PITCHER PITCH DATA
    p <- pitch_data %>%
      filter(Pitcher == pitcher_name) %>%
      filter(TaggedPitchType != '' | is.na(TaggedPitchType)) %>% 
      mutate(TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball', 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup', 'Splitter', 'Knuckleball', 'Other') ),
             TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                      Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN', Splitter = 'SPL'  ) ) 
    # INDIVIDUAL PITCHER AVERAGE PITCH METRICS 
    p_mean <- suppressMessages(
      pitch_data %>%
        filter(Pitcher == pitcher_name) %>%
        filter(TaggedPitchType != '' | is.na(TaggedPitchType)) %>% 
        mutate(TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball', 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup', 'Splitter', 'Knuckleball', 'Other') ),
               TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                        Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN', Splitter = 'SPL'  ) ) %>%
        group_by(Pitcher, PitcherThrows, TaggedPitchType) %>%
        summarise(across(c(InducedVertBreak, HorzBreak, SpinAxis), ~ mean(.,na.rm = T)),
                  usage = n()) %>%
        mutate(usage = round(usage / sum(usage),3)*100,
               scaled_usage = (usage - min(usage)) / (max(usage) - min(usage)) * (40 - 20) + 20)
    )
    
    # LEAGUE AVERAGE PITCH METRICS MATCHING INDIVIDUAL PITCHERS ARSENAL
    p_lg <- pitch_data_lg_Avg %>%
      filter(PitcherThrows %in% p_mean$PitcherThrows) %>%
      filter(TaggedPitchType %in% p_mean$TaggedPitchType)
    
    # INDIVIDUAL PITCHER AVERAGE ARM ANGLE AND RELEASE DATA
    p_arm <- suppressMessages(
      pitch_data %>%
        filter(Pitcher == pitcher_name) %>%
        group_by(Pitcher, PitcherThrows) %>%
        summarise(PitcherTeam = paste(unique(PitcherTeam), collapse = ', '),
                  height_inches = mean(height_inches, na.rm = T),
                  shoulder_pos = mean(shoulder_pos, na.rm = T),
                  release_pos_x = median(RelSide * 12, na.rm = T),
                  release_pos_z = median(RelHeight * 12, na.rm = T),
                  arm_angle = median(arm_angle, na.rm = T),
                  arm_angle_180 = median(arm_angle_180, na.rm = T),
                  arm_angle_savant = median(arm_angle_savant, na.rm = T)
        ) %>%
        arm_angle_categories() %>%
        mutate(relx = case_when(
          release_pos_x > 20 ~ 20,
          release_pos_z > 20 ~ 20 * (release_pos_x / (release_pos_z - shoulder_pos)),
          TRUE ~ release_pos_x
        ),
        relz = case_when(
          release_pos_x > 20 ~ 20 * ((release_pos_z - shoulder_pos) / release_pos_x),
          release_pos_z > 20 ~ 20,
          TRUE ~ release_pos_z
        ),
        arm_path = 'Arm Path'
        ) %>%
        mutate(arm_length = height_inches * .4,
               slope = (release_pos_z - shoulder_pos) / (release_pos_x - 0),
               arm_dist = sqrt((release_pos_x - 0)^2 + (release_pos_z - shoulder_pos)^2),
               arm_scale = arm_length / arm_dist,
               should_x = case_when(
                 arm_angle_savant >= 40 ~ 0,
                 between(arm_angle_savant, 10, 40)  ~ 0,
                 arm_angle_savant < 10 ~ 0,
               ),
               should_y = case_when(
                 arm_angle_savant >= 40 ~ 62.5,
                 between(arm_angle_savant, 10, 40)  ~ 56,
                 arm_angle_savant < 10 ~ 45,
                 
               ),
               rel_x = 0 + (arm_scale * (release_pos_x - 0)), 
               rel_z = shoulder_pos + (arm_scale * (release_pos_z - shoulder_pos)) + should_y - (shoulder_pos),
               arm_path = 'Arm Path'
        )
    )
    
    p_c <- pitch_colors %>%
      filter(PitchCode %in% p_mean$TaggedPitchType)
    
    p_c <- setNames(as.character(p_c$Color), p_c$PitchCode)
    
    
    if(nrow(p) > 0 & plot_type == 'savant') { # SAVANT PLOT CODE ----
      
      caption <- paste(
        'Speed:', round(p$RelSpeed,1),
        "\nSpin:", round(p$SpinRate),
        "\nAxis:", round(p$SpinAxis),
        "\nSpin Eff%:", round(p$yt_Efficiency),
        "\nDate:", p$Date,
        "\nBallpark:", p$HomeTeamCode
        
      )
      
      ggplotly(
        ggplot(data = p, aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
          geom_polygon(data = circle, aes(x = x, y = y), fill = "#e5f3f3", color = "#e5f3f3", inherit.aes = F) +
          # DEGREE ANNOTATION
          annotate('text', x = 26, y = 1, label = '0°', size = 3)+
          annotate('text', x = -26, y = 1, label = '0°', size = 3)+
          annotate('text', x = 0, y = 26, label = '90°', size = 3)+
          # BREAK ANNOTATION
          annotate('text', x = 10, y = -1.5, label = '12"', size = 3)+
          annotate('text', x = 22, y = -1.5, label = '24"', size = 3)+
          annotate('text', x = -4.5, y = -1.5, label = '6"', size = 3)+
          annotate('text', x = -10, y = -1.5, label = '12"', size = 3)+
          annotate('text', x = -16, y = -1.5, label = '18"', size = 3)+
          annotate('text', x = -22, y = -1.5, label = '24"', size = 3)+
          annotate('text', y = 10, x = -2, label = '12"', size = 3)+
          annotate('text', y = 22, x = -2, label = '24"', size = 3)+
          annotate('text', y = -10, x = -2, label = '12"', size = 3)+
          annotate('text', y = -22, x = -2, label = '24"', size = 3)+
          geom_path(data = data.frame(
            x = 6 * cos(seq(0, 2*pi, length.out = 100)),  y = 6 * sin(seq(0, 2*pi, length.out = 100)) ), 
            aes(x = x, y = y), linetype = "dashed", color = "gray", inherit.aes = F) +
          geom_path(data = data.frame(
            x = 12 * cos(seq(0, 2*pi, length.out = 100)), y = 12 * sin(seq(0, 2*pi, length.out = 100)) ), 
            aes(x = x, y = y), linetype = "solid", color = "gray", inherit.aes = F) +
          geom_path(data = data.frame(
            x = 18 * cos(seq(0, 2*pi, length.out = 100)),  y = 18 * sin(seq(0, 2*pi, length.out = 100)) ), 
            aes(x = x, y = y), linetype = "dashed", color = "gray", inherit.aes = F) +
          geom_path(data = data.frame(
            x = 24 * sin(seq(0, 2*pi, length.out = 100)),  y = 24 * cos(seq(0, 2*pi, length.out = 100)) ), 
            aes(x = x, y = y), linetype = "solid", color = "gray", inherit.aes = F) +
          coord_fixed()+
          geom_segment(x = 0, y = -25, xend = 0, yend = 25, linewidth = .5, color = "grey55") +
          geom_segment(x = -25, y = 0, xend = 25, yend = 0, linewidth = .2, color = "grey55") +
          geom_segment(x = 25, y = 0, xend = 25, yend = 0, linewidth = .2, color = "grey55") +
          geom_segment(y=19.91,yend=21.65, x= 11.5, xend=12.5, color='grey55')+
          geom_segment(x=19.91,xend=21.65, y= 11.5, yend=12.5, color='grey55')+
          geom_segment(y=19.91,yend=21.65, x= -11.5, xend=-12.5, color='grey55')+
          geom_segment(x=-19.91,xend=-21.65, y= 11.5, yend=12.5, color='grey55')+
          annotate('text', x = 22.5, y = 13.5, label = '30°', size = 3)+
          annotate('text', x = -22.5, y = 13.5, label = '30°', size = 3)+
          annotate('text', y = 22.5, x = 13.5, label = '60°', size = 3)+
          annotate('text', y = 22.5, x = -13.5, label = '60°', size = 3)+
          theme(legend.position = "left",
                panel.background = element_blank(),
                legend.text = element_text(size = 8),
                axis.title = element_text(size = 10),
                panel.grid = element_blank(),
                plot.title = element_text(hjust = .5),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
          ) + 
          guides(color = "none")+
          scale_size_continuous(range = c(4, 9), guide = 'none') +
          labs(color = "", fill = 'Pitch',x = "", y = "", 
               title = paste0(round(p_arm$arm_angle_savant),"° ", p_arm$arm_angle_type) ),
        color = ~TaggedPitchType,
        
      ) %>%
        plotly::style(hoverinfo = "none", traces = 1:29)%>%
        plotly::layout(autosize = T,
                       title = list( text = paste0("Pitch Movement and Arm Angle" ),
                                     x = 0.5,xanchor = 'center') ,showlegend = TRUE)  %>%
        # INDIVIDUAL POINTS
        add_markers(data = p,
                    x = ~HorzBreak,  y = ~InducedVertBreak,  color = ~TaggedPitchType, colors = p_c,
                    marker = list(symbol = "circle",  opacity = 0.7, size = 8,    
                                  line = list(  color = 'black',   width = .5   )   ),
                    showlegend = TRUE,  legendgroup = "group1", visible = T ,
                    legendgrouptitle = list(text = "Pitches", font = list(size = 10)), hovertext = caption) %>%
        # GROUP POINTS
        plotly::add_markers(data = p_mean,
                            x = ~HorzBreak,  y = ~InducedVertBreak,  color = ~TaggedPitchType, colors = p_c,
                            marker = list(
                              symbol = "circle-dot",  opacity = 0.5, size = p_mean$scaled_usage,
                              sizeref = 0.0,   sizemode = "area",   
                              line = list(color = 'black', width = 2) ),  
                            showlegend = TRUE,  legendgroup = "group2", visible = "legendonly", 
                            legendgrouptitle = list(text = "Pitch Avg", font = list(size = 10))
        ) %>%
        # Arm Path
        add_segments(x = 0, xend = p_arm$relx, y = 0, yend = p_arm$relz,
                     line = list(color = 'grey', width = 7), opacity = .5, name = 'Arm',
                     legendgroup = "group4", visible = T ) %>%
        # Release Point
        add_markers(x = p_arm$relx, y = p_arm$relz,
                    marker = list(  symbol = "diamond",  size = 15,   color = 'orange', opacity = .7,    
                                    line = list(  color = 'black',   width = 1.5   )  ),
                    name = "Release",
                    legendgroup = "group4", visible = T)  %>%
        # LEAGUE AVERAGE
        add_markers(data = p_lg,
                    x = ~HorzBreak, y = ~InducedVertBreak,
                    color = ~TaggedPitchType, colors = p_c,
                    marker = list(
                      symbol = "circle-x-open", opacity = 0.5, size = 35,
                      line = list(  color = 'black',   width = 2   )
                    ),
                    showlegend = T,  legendgroup = "group3", visible = "legendonly",
                    legendgrouptitle = list(text = "League Avg", font = list(size = 10))
        ) %>%
        layout(legend = list(itemsizing = 'constant'))  %>%
        # Arm angle caption
        add_annotations(
          x = 0,  y = 28, xref = "x", yref = "y",
          text = paste0(round(p_arm$arm_angle_savant),"° Arm Angle - ", gsub("Three-Quarters", "3/4", p_arm$arm_angle_type)),
          showarrow = FALSE,
          font = list(size = 11, color = "#731209", face = 'bold')
        ) %>%
        # point size caption
        add_annotations(
          x = -23,  y = -20, xref = "x", yref = "y",
          text = "Pitch Avg point size\nrelative to usage",
          showarrow = FALSE,
          font = list(size = 11, color = "#731209", face = 'bold')
        ) 
      
      
    } else if(nrow(p) > 0 & plot_type == 'movement') {# MOVEMENT PLOT CODE ----
      
      caption <- paste(
        'Speed:', round(p$RelSpeed,1),
        "\nSpin:", round(p$SpinRate),
        "\nAxis:", round(p$SpinAxis),
        "\nSpin Eff%:", round(p$yt_Efficiency),
        "\nDate:", p$Date,
        "\nBallpark:", p$HomeTeamCode
        
      )
      
      ggplotly(
        ggplot(data = p, aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
          labs(color = "",x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)" ) + 
          xlim(-25, 25) + ylim(-25, 25) +
          geom_segment(x = 0, y = -25, xend = 0, yend = 25, linewidth = 1, color = "grey55") +
          geom_segment(x = -25, y = 0, xend = 25, yend = 0, linewidth = 1, color = "grey55") +
          stat_ellipse(aes(fill = TaggedPitchType),level = .99, alpha = 0.3, geom = "polygon") +  # Adds a filled ellipse with transparency
          geom_segment(x = 0, xend = p_arm$release_pos_x, y = 0, yend = p_arm$release_pos_z - p_arm$shoulder_pos, 
                       color = "grey", size = 0.75, alpha = .3) +
          geom_point(fill = 'orange', color = 'black', x=0, y=0, pch = 21, size = 4, alpha = .01, show.legend = T) + # shoulder
          geom_point(fill = 'cornflowerblue', color = 'black', x=p_arm$release_pos_x  , y= p_arm$release_pos_z - p_arm$shoulder_pos , 
                     pch = 21, size = 4, alpha = .01, show.legend = T) + # shoulder
          geom_point(aes(color = TaggedPitchType,
                         text = caption), # paste('Spin: ', SpinRate)), 
                     size = 1, alpha = .5) +
          scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                        'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) +
          scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                       'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) +
          theme(legend.position = "left", 
                legend.text = element_text(size = 8), 
                axis.title = element_text(size = 10)
          ) +
          guides(fill = "none")     
      ) %>% 
        
        plotly::layout(autosize = T #,
                       # showlegend = TRUE,
                       # legend=list(x=0, 
                       #             xanchor='left',
                       #             yanchor='left',
                       #             orientation='v')
        ) 
    } else if (nrow(p) > 0 & plot_type == 'arm_angle') { # ARM ANGLE PLOT CODE ----
      
      
      df <- data.frame(x = 0.5, y = 0)
      
      theta <- seq(0, pi, length.out = 100)  # Change the range to create a semi-circle that is right-side up
      r <- 40  # The horizontal range from -40 to 40
      
      # Calculate the x and y coordinates
      mound <- data.frame(
        x = r * cos(theta),
        y = 4 * sin(theta)  # Adjusting the height to go up to 4
      )
      
      rubber_xmin <- -9  # Left side of the rubber
      rubber_xmax <- 9   # Right side of the rubber
      rubber_ymin <- 4   # The bottom y-coordinate of the rubber, same as the top of the mound
      rubber_ymax <- 4.5 # The top y-coordinate of the rubber, make it small in height
      
      base_plot <- ggplot(df, aes(x, y)) + 
        geom_polygon(data = mound, aes(x = x, y = y), fill = "#8B4513") +
        xlim(-50,50) + ylim(0,100)+
        geom_rect(aes(xmin = rubber_xmin, xmax = rubber_xmax, ymin = rubber_ymin, ymax = rubber_ymax), 
                  fill = "white", color = "black") 
      
      if(p_arm$PitcherThrows == 'Right') {
        if(p_arm$arm_angle_savant >= 40) { 
          
          image_path <- "savant/SavantPitchers_top_right_front-svg.png"
          
          base_plot +
            # geom_hline(yintercept = p_arm$should_y)+
            # geom_vline(xintercept = 0)+
            geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(x=0, y =  p_arm$shoulder_pos + (p_arm$should_y - p_arm$shoulder_pos), 
                         xend = -p_arm$rel_x, 
                         yend = p_arm$rel_z , 
                         size = 8, color = "#6892a2", alpha = .5) +
            geom_point(x = -p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =7, stroke = 2)+
            # geom_point(data = p_mean, aes(x = -HorzBreak * 2.25, y = (InducedVertBreak*1.5)+p_arm$should_y, fill = TaggedPitchType), # THIS ONE
            #            size = 15, alpha = .4, pch = 21, color = 'black')+
            # scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
            #                              'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black' )) +
            # geom_label_repel(data = p_mean, aes(x = -HorzBreak * 2.25, y = (InducedVertBreak * 1.5) + p_arm$should_y, 
            #                                     label = paste('IVB:', round(InducedVertBreak,1),"\n",'HB:', round(-HorzBreak,1))), 
            #                  size = 3, color = 'black') +
            labs(
              title = paste(p_arm$Pitcher),
              subtitle = paste0('Arm Angle: ', round(p_arm$arm_angle_savant), "° - ", gsub('Three-Quarters','3/4', p_arm$arm_angle_type))
            )  +
            theme_void()+
            theme(plot.title = element_text(hjust = .5),
                  plot.subtitle = element_text(hjust = .5))
          
          
        } else if (between(p_arm$arm_angle_savant, 10,40)) {
          image_path <- "savant/SavantPitchers_mid_right_front-svg.png"
          
          base_plot +
            # geom_hline(yintercept = p_arm$should_y)+
            # geom_vline(xintercept = 0)+
            geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(x=0, y =  p_arm$shoulder_pos + (p_arm$should_y - p_arm$shoulder_pos), 
                         xend = -p_arm$rel_x, 
                         yend = p_arm$rel_z , 
                         size = 8, color = "#6892a2", alpha = .5) +
            geom_point(x = -p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =7, stroke = 2)+
            # geom_point(data = p_mean, aes(x = -HorzBreak * 2.25, y = (InducedVertBreak*1.5)+p_arm$should_y, fill = TaggedPitchType), # THIS ONE
            #            size = 15, alpha = .4, pch = 21, color = 'black')+
            # scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
            #                              'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black' )) +
            # geom_label_repel(data = p_mean, aes(x = -HorzBreak * 2.25, y = (InducedVertBreak * 1.5) + p_arm$should_y, 
            #                                     label = paste('IVB:', round(InducedVertBreak,1),"\n",'HB:', round(-HorzBreak,1))), 
            #                 size = 3, color = 'black') +
            labs(
              title = paste(p_arm$Pitcher),
              subtitle = paste0('Arm Angle: ', round(p_arm$arm_angle_savant), "° - ", gsub('Three-Quarters','3/4', p_arm$arm_angle_type))
            )  +
            theme_void()+
            theme(plot.title = element_text(hjust = .5),
                  plot.subtitle = element_text(hjust = .5))
          
        } else if(p_arm$arm_angle_savant < 10){
          image_path <- "savant/SavantPitchers_low_right_front-svg.png"
          
          base_plot +
            # geom_hline(yintercept = p_arm$should_y)+
            # geom_vline(xintercept = 0)+
            geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(x=0, y =  p_arm$shoulder_pos + (p_arm$should_y - p_arm$shoulder_pos), 
                         xend = -p_arm$rel_x, 
                         yend = p_arm$rel_z , 
                         size = 8, color = "#6892a2", alpha = .5) +
            geom_point(x = -p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =7, stroke = 2)+
            # geom_point(data = p_mean, aes(x = -HorzBreak * 2.25, y = (InducedVertBreak*1.5)+p_arm$should_y, fill = TaggedPitchType), # THIS ONE
            #            size = 15, alpha = .4, pch = 21, color = 'black')+
            # scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
            #                              'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black' )) +
            # geom_label_repel(data = p_mean, aes(x = -HorzBreak * 2.25, y = (InducedVertBreak * 1.5) + p_arm$should_y, 
            #                                     label = paste('IVB:', round(InducedVertBreak,1),"\n",'HB:', round(-HorzBreak,1))), 
            #                  size = 3, color = 'black') +
            labs(
              title = paste(p_arm$Pitcher),
              subtitle = paste0('Arm Angle: ', round(p_arm$arm_angle_savant), "° - ", gsub('Three-Quarters','3/4', p_arm$arm_angle_type))
            ) +
            theme_void()+
            theme(plot.title = element_text(hjust = .5),
                  plot.subtitle = element_text(hjust = .5))
          
        }
      } else if(p_arm$PitcherThrows == 'Left'){
        if(p_arm$arm_angle_savant >= 40) { 
          
          image_path <- "savant/SavantPitchers_top_left_front-svg.png"
          
          base_plot +
            # geom_hline(yintercept = p_arm$should_y)+
            # geom_vline(xintercept = 0)+
            geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(x=0, y =  p_arm$shoulder_pos + (p_arm$should_y - p_arm$shoulder_pos), 
                         xend = -p_arm$rel_x, 
                         yend = p_arm$rel_z , 
                         size = 8, color = "#6892a2", alpha = .5) +
            geom_point(x = -p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =7, stroke = 2)+
            # geom_point(data = p_mean, aes(x = -HorzBreak * 2.25, y = (InducedVertBreak*1.5)+p_arm$should_y, fill = TaggedPitchType), # THIS ONE
            #            size = 15, alpha = .4, pch = 21, color = 'black')+
            # scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
            #                              'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black' )) +
            # geom_label_repel(data = p_mean, aes(x = -HorzBreak * 2.25, y = (InducedVertBreak * 1.5) + p_arm$should_y, 
            #                                     label = paste('IVB:', round(InducedVertBreak,1),"\n",'HB:', round(-HorzBreak,1))), 
            #                  size = 3, color = 'black') +
            labs(
              title = paste(p_arm$Pitcher),
              subtitle = paste0('Arm Angle: ', round(p_arm$arm_angle_savant), "° - ", gsub('Three-Quarters','3/4', p_arm$arm_angle_type))
            )  +
            theme_void()+
            theme(plot.title = element_text(hjust = .5),
                  plot.subtitle = element_text(hjust = .5))
          
        } else if (between(p_arm$arm_angle_savant, 10,40)) {
          image_path <- "savant/SavantPitchers_mid_left_front-svg.png"
          
          base_plot +
            # geom_hline(yintercept = p_arm$should_y)+
            # geom_vline(xintercept = 0)+
            geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(x=0, y =  p_arm$shoulder_pos + (p_arm$should_y - p_arm$shoulder_pos), 
                         xend = -p_arm$rel_x, 
                         yend = p_arm$rel_z , 
                         size = 8, color = "#6892a2", alpha = .5) +
            geom_point(x = -p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =7, stroke = 2)+
            # geom_point(data = p_mean, aes(x = -HorzBreak * 2.25, y = (InducedVertBreak*1.5)+p_arm$should_y, fill = TaggedPitchType), # THIS ONE
            #            size = 15, alpha = .4, pch = 21, color = 'black')+
            # scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
            #                              'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black' )) +
            # geom_label_repel(data = p_mean, aes(x = -HorzBreak * 2.25, y = (InducedVertBreak * 1.5) + p_arm$should_y, 
            #                                     label = paste('IVB:', round(InducedVertBreak,1),"\n",'HB:', round(-HorzBreak,1))), 
            #                  size = 3, color = 'black') +
            labs(
              title = paste(p_arm$Pitcher),
              subtitle = paste0('Arm Angle: ', round(p_arm$arm_angle_savant), "° - ", gsub('Three-Quarters','3/4', p_arm$arm_angle_type))
            )  +
            theme_void()+
            theme(plot.title = element_text(hjust = .5),
                  plot.subtitle = element_text(hjust = .5))
          
        } else if(p_arm$arm_angle_savant < 10){
          image_path <- "savant/SavantPitchers_low_left_front-svg.png"
          
          base_plot +
            # geom_hline(yintercept = p_arm$should_y)+
            # geom_vline(xintercept = 0)+
            geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(x=0, y =  p_arm$shoulder_pos + (p_arm$should_y - p_arm$shoulder_pos), 
                         xend = -p_arm$rel_x, 
                         yend = p_arm$rel_z , 
                         size = 8, color = "#6892a2", alpha = .5) +
            geom_point(x = -p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =7, stroke = 2)+
            # geom_point(data = p_mean, aes(x = -HorzBreak * 2.25, y = (InducedVertBreak*1.5)+p_arm$should_y, fill = TaggedPitchType), # THIS ONE
            #            size = 15, alpha = .4, pch = 21, color = 'black')+
            # scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
            #                              'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black' )) +
            # geom_label_repel(data = p_mean, aes(x = -HorzBreak * 2.25, y = (InducedVertBreak * 1.5) + p_arm$should_y, 
            #                                     label = paste('IVB:', round(InducedVertBreak,1),"\n",'HB:', round(-HorzBreak,1))), 
            #                  size = 3, color = 'black') +
            labs(
              title = paste(p_arm$Pitcher),
              subtitle = paste0('Arm Angle: ', round(p_arm$arm_angle_savant), "° - ", gsub('Three-Quarters','3/4', p_arm$arm_angle_type))
            ) +
            theme_void()+
            theme(plot.title = element_text(hjust = .5),
                  plot.subtitle = element_text(hjust = .5))
          
          
        }
      } 
      
      
    } else {
      print('Plot Type or Pitcher not found. Try again!')
    }
    
  } else {
    print('Pitcher not found. Try again!')
  }
  
}

# WORKING FUNCTION -------------------------------------------------------------

pitcher_name <- sort(unique(pitch_data$Pitcher))

plot_type <- c('movement', 'savant', 'arm_angle')

pitcher_plot('Cole Cook', 'movement') # movement, savant, arm_angle

