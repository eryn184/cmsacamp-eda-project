#PURPOSE:EDA of Shot Attempts from the 2021 NHL Playoffs
#Author: Nicole Tucker



# Load Packages -----------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(ggridges)

# Load and Clean Data -----------------------------------------------------

# Accessed NHL shots from 2020-2021 season from MoneyPuck.com
# Filter to the playoff game shots (as of June 5th)
playoff_shot_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2021/materials/data/xy_examples/moneypuck_shots_2020.csv") %>%
  dplyr::filter(isPlayoffGame == 1)

# Now only select columns to work with for this task:
playoff_shot_data <- playoff_shot_data %>%
  dplyr::select(# Player info attempting the shot
    shooterPlayerId, shooterName, team, shooterLeftRight, 
    shooterTimeOnIce, shooterTimeOnIceSinceFaceoff,
    # Info about the shot:
    event, location, shotType, shotAngle, shotAnglePlusRebound, 
    shotDistance, shotOnEmptyNet, shotRebound, shotRush, 
    shotWasOnGoal, shotGeneratedRebound, shotGoalieFroze,
    # Adjusted for arena locations
    arenaAdjustedShotDistance, 
    arenaAdjustedXCord, arenaAdjustedYCord,
    # Goalie info:
    goalieIdForShot, goalieNameForShot,
    # Team context
    teamCode, isHomeTeam, homeSkatersOnIce, awaySkatersOnIce,
    # Game context
    game_id, homeTeamCode, awayTeamCode, homeTeamGoals,
    awayTeamGoals, time, period)


# Exploring the Data Structure --------------------------------------------

dim(playoff_shot_data)
nrow(playoff_shot_data)
ncol(playoff_shot_data)

class(playoff_shot_data)


head(playoff_shot_data)
colnames(playoff_shot_data)



# Set Theme ---------------------------------------------------------------

theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14)
    )
}


#Looking at how shot distance affects types of shots--------------------------
playoff_shot_data %>%
  ggplot(aes(x = shotType, y = shotDistance)) +
  geom_violin(aes(fill = shotType), draw_quantiles = c(0.25, 0.5, 0.75)) +
  #geom_jitter(aes(color = shotType), width = 0.20, alpha = 0.10) +
  theme_reach() +
  labs(x = "Shot Type",
       y = "Shot Distance",
       title = "How Shot Distance Affects Shot Type",
       subtitle = "Shots used from the 2021 Stanley Cup Playoffs")




# New Variables- Goals/Shots ----------------------------------------------

playoff_shot_data <- playoff_shot_data %>%
  mutate(goal = ifelse(event == "GOAL", 1, 0))

team_shots <- playoff_shot_data %>%
  group_by(teamCode) %>%
  summarize(shots = n(),
            avg_distance = mean(shotDistance, na.rm = T))

team_goals <- playoff_shot_data %>%
  filter(goal == 1) %>%
  group_by(teamCode) %>%
  summarize(goals = sum(goal))

team_shot_perc <- team_shots %>%
  left_join(team_goals, by = "teamCode") %>%
  mutate(shot_perc = goals / shots)




# Relationship Between Shot Distance and Goals Made -----------------------

team_shot_perc %>%
  ggplot(aes(x = avg_distance, y = shot_perc)) +
  geom_point(aes(size = shots), shape = 21, fill = "orange", color = "black", alpha = 0.8) +
  geom_smooth(method = "lm", color = "gray", se = FALSE) +
  ggrepel::geom_text_repel(aes(label = teamCode)) +
  theme_reach() +
  labs(y = "Goal Percentage",
       x = "Average Shot Distance (Feet)",
       title = "No Correlation Between Shot Distance and Goal Percentage",
       subtitle = "2021 Stanley Cup Playoffs, bubble size is amount of shots")




# Exploring Shot Angle ----------------------------------------------------

hist(playoff_shot_data$shotAngle, breaks = 100)

player_shot_angles <- playoff_shot_data %>%
  group_by(shooterName) %>%
  summarize(shots = n(),
            shot_angle_sd = sd(shotAngle)) %>%
  filter(shots >= 10)

playoff_shot_data <- playoff_shot_data %>%
  mutate(is_overtime = ifelse(period > 3, 1, 0))

playoff_shot_data %>%
  ggplot(aes(x = as.factor(goal), y = abs(shotDistance))) +
  geom_violin(aes(fill = as.factor(is_overtime)), draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_reach() +
  coord_flip()



# Exploring Shooter Time on Ice and Shots Taken ---------------------------

playoff_shot_data %>%
  filter(shooterTimeOnIce <= 150) %>%
  ggplot(aes(x = shooterTimeOnIce)) +
  geom_density(fill = "darkgreen", alpha = 0.8) +
  geom_vline(xintercept = 16, linetype = "dashed") +
  geom_vline(xintercept = 28, linetype = "dashed") +
  geom_vline(xintercept = 42, linetype = "dashed") +
  #geom_histogram(fill = "darkgreen", color = "black", bins = 50) +
  theme_reach() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))



