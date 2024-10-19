library(dplyr)
library(ggplot2)
library(ggrepel)
library(teamcolors)

stats <- read.table(text = "
Rk,Team,Age,W,L,PW,PL,MOV,SOS,SRS,ORtg,DRtg,NRtg,Pace,FTr,3PAr,TS%,,eFG%,TOV%,ORB%,FT/FGA,,eFG%,TOV%,DRB%,FT/FGA,,Arena
1,New York Liberty*,28.5,32,8,33,7,9.15,-1.09,8.06,109.6,97.9,+11.7,78.1,.250,.422,.561,,.521,14.2,25.1,.203,,.476,14.5,79.2,.167,,
2,Connecticut Sun*,28.9,28,12,31,9,6.50,-0.75,5.75,105.0,96.4,+8.6,75.8,.317,.273,.533,,.488,13.8,25.2,.239,,.482,17.6,78.2,.204,,
3,Minnesota Lynx*,27.9,30,10,30,10,6.38,-0.74,5.64,104.6,96.5,+8.1,77.7,.230,.371,.553,,.518,15.3,22.3,.182,,.460,16.5,74.2,.181,,
4,Las Vegas Aces*,29.6,27,13,29,11,5.48,-0.70,4.77,108.0,101.2,+6.8,79.7,.269,.388,.567,,.523,12.4,16.6,.223,,.488,14.1,79.5,.189,,
5,Seattle Storm*,29.1,25,15,27,13,4.48,-0.56,3.92,104.2,98.6,+5.6,79.4,.252,.295,.525,,.478,13.5,24.2,.211,,.477,16.5,74.6,.210,,
6,Indiana Fever*,25.5,20,20,16,24,-2.68,0.49,-2.19,106.1,109.5,-3.4,79.8,.250,.377,.559,,.523,15.7,24.6,.194,,.507,12.8,76.7,.228,,
7,Atlanta Dream*,27.7,15,25,15,25,-2.75,0.36,-2.39,99.0,102.5,-3.5,77.1,.294,.285,.501,,.452,14.0,25.4,.227,,.488,14.3,78.3,.208,,
8,Washington Mystics,27.4,14,26,15,25,-2.98,0.54,-2.44,99.7,103.4,-3.7,79.1,.224,.397,.538,,.506,17.0,20.6,.172,,.502,16.7,75.2,.241,,
9,Phoenix Mercury*,30.4,19,21,15,25,-3.30,0.33,-2.97,103.6,107.8,-4.2,78.1,.278,.394,.547,,.503,15.1,20.0,.222,,.496,13.1,71.6,.189,,
10,Chicago Sky,26.1,13,27,12,28,-5.13,0.46,-4.67,99.1,105.6,-6.5,78.2,.252,.212,.495,,.457,14.3,29.5,.187,,.499,14.2,76.8,.228,,
11,Los Angeles Sparks,27.0,8,32,9,31,-7.23,0.92,-6.30,98.6,107.7,-9.1,79.2,.295,.340,.523,,.478,16.6,22.0,.226,,.522,14.2,76.1,.211,,
12,Dallas Wings,26.8,9,31,9,31,-7.93,0.75,-7.18,104.2,114.0,-9.8,80.0,.263,.270,.531,,.490,15.7,30.2,.206,,.536,13.8,73.4,.236,,
,League Average,27.9,,,20,20,0.00,0.00,0.00,103.5,103.5,,78.5,.264,.335,.536,,.495,14.8,23.9,.207,,.495,14.8,76.1,.207,,
", header = TRUE, sep = ",", fill = TRUE, stringsAsFactors = FALSE)

stats <- stats %>%
	filter(Team != "League Average") %>%
	select("Team", "Pace", "eFG.", "W")

stats$Team <- gsub("\\*", "", stats$Team)

colors <- subset(teamcolors, league=="wnba")

stats_colors <- merge(stats, colors[, c("name", "primary")], by.x = "Team", by.y = "name", all.x = TRUE)
stats_colors$primary[stats_colors$Team == "Chicago Sky"] <- "#418FDE"
stats_colors$primary[stats_colors$Team == "Dallas Wings"] <- "#C4D600"
stats_colors$primary[stats_colors$Team == "Las Vegas Aces"] <- "#85714D"
stats_colors$primary[stats_colors$Team == "Dallas Wings"] <- "#C4D600"
stats_colors$primary[stats_colors$Team == "Phoenix Mercury"] <- "#CB6015"
stats_colors$primary[stats_colors$Team == "Seattle Storm"] <- "#2C5234"
stats_colors$primary[stats_colors$Team == "Washington Mystics"] <- "#C8102E"
stats_colors$primary[stats_colors$Team == "Connecticut Sun"] <- "#DC4405"
stats_colors$primary[stats_colors$Team == "Minnesota Lynx"] <- "#236192"

ggplot(stats_colors, aes(x=Pace, y=eFG., size = W)) +
    geom_point(alpha=0.9, shape=21, color="black")

ggplot(stats_colors, aes(x=Pace, y=eFG., size = W)) +
    geom_point(aes(fill= primary), alpha=0.9, shape=21, color="black") +
    geom_text_repel(aes(label = Team), size = 3, show.legend = FALSE) +
    scale_size_continuous(name = "Wins") +
    guides(fill = "none") +
    ylab("Accuracy (effective FG%)") +
    xlab("Speed (pace)") +
    scale_fill_identity() +
    scale_x_reverse() + 
    ggtitle("Speed vs Accuracy for WNBA Teams (2024)") +
    theme_minimal()

#fancier (thanks perplexity.ai)
ggplot(stats_colors, aes(x=Pace, y=eFG., size = W)) +
    geom_point(aes(fill= primary), alpha=0.9, shape=21, color="black") +
    geom_text_repel(aes(label = Team), size = 3, show.legend = FALSE) +
    scale_size_continuous(name = "Wins", range = c(3, 15)) +  # Adjust point size range
    scale_fill_identity() +
    scale_x_reverse() +
    labs(
      y = "Accuracy (effective FG%)",
      x = "Speed (pace)",
      title = "Speed vs Accuracy for WNBA Teams (2024)"
    ) +
    theme_minimal(base_size = 14) +  # Base font size
    theme(
        plot.background = element_rect(fill = "#F5F5DC", color = NA),  # Beige background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_rect(fill = "#F5F5DC", color = NA),
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centered title
        legend.position = "right"  # Position legend on the right
    ) +
    guides(size = guide_legend(title.position="top"))  # Legend title position
