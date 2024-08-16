#inspo and original code: https://github.com/gkaramanis/FIBA-Basketbal-World-Cup/blob/master/R/shooting-stats.R

library(tidyverse)
library(ggtext)
library(lemon)
library(teamcolors)
library(extrafont)
font_import()  # Import all fonts into R
loadfonts()    # Load the fonts

totalstats <- read.csv("~/Desktop/wehoop/2024-8-16 team shooting stats/shot_stats.csv", header=TRUE)

shot_stats <- totalstats %>%
  filter(Team != "League Average") %>%  # Filter out "League Average"
  select(Team, freethrows_percentage, twopoints_percentage, threepoints_percentage) %>% 
  gather("type", "percentage", freethrows_percentage, twopoints_percentage, threepoints_percentage) %>% 
  mutate(
    team_color = case_when(
      Team == "Minnesota Lynx" ~ "#236192", #colors for highlighting specific teams
      Team == "Seattle Storm" ~ "#2C5234",
      Team == "New York Liberty" ~ "#6ECEB2",
      Team == "Las Vegas Aces" ~ "#BA0C2F",
      TRUE ~ "grey60" #make all the rest grey
    )
  )

shot_stats$type <- factor(shot_stats$type, c("freethrows_percentage", "twopoints_percentage", "threepoints_percentage"))
 
ggplot(shot_stats, aes(x = type, y = percentage, group = Team, color = team_color, alpha = (team_color != "grey60"))) +
  geom_pointline(stroke = 0, linesize = 4, size = 5, linejoin = "mitre") +
  # geom_point() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_x_discrete(labels = c("Free Throws", "Two-pointers", "Three-pointers")) +
  scale_color_identity() +
  scale_alpha_manual(values = c(0.15, 1)) +
  labs(
    title = "Shooting percentages in WNBA 2024, Pre Olympic Break<br>Team Shooting Specialization",
    subtitle = "<span style='color:#236192'>**Minnesota Lynx**</span> lead the WNBA in three-point percentage (38.4%)<br>but rank only 10th in two-point percentage (46.8%).<br><span style='color:#2C5234'>**Seattle Storm**</span> own the worst three-point percentage (29.2%)<br>but has the second-highest free throw percentage (83.2%).<br><span style='color:#6ECEB2'>**New York Liberty**</span> have the best two-point percentage (53.5%)<br>and trail only the <span style='color:#BA0C2F'>**Las Vegas Aces**</span> (45.4%) in overall FG%.",
    caption = "Source: basketball-reference.com | Graphic: @wnbadata | Adapted from original by Georgios Karamanis "
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 30, 20, 20),
    plot.background = element_rect(fill = "grey90", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 12), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_markdown(size = 16, lineheight = 1.1, color = "grey20"),
    plot.subtitle = element_markdown(size = 14, lineheight = 1.1, color = "grey40", margin = margin(0, 0, 20, 0)),
    plot.caption = element_markdown(size = 8, color = "grey60", margin = margin(20, 0, 0, 0))
  ) #+
  ggsave(here::here("figures", "shooting-stats.png"),
         height = 12, width = 8)