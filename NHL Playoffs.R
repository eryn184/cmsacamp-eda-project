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



# Exploring Shooter Time on Ice and Shots Taken ---------------------------

#Hypothesis: player's come on the ice and shoot right away because that's when they have energy and are excited

#Conclusion: the data backs up the hypothesis as most players shoot within a minute of getting on the ice.

playoff_shot_data %>%
  filter(shooterTimeOnIce <= 150) %>%
  ggplot(aes(x = shooterTimeOnIce)) +
  geom_density(fill = "darkorange", alpha = 0.8) +
  geom_vline(xintercept = quantile(playoff_shot_data$shooterTimeOnIce,0.25), linetype = "dashed") +
  geom_vline(xintercept = quantile(playoff_shot_data$shooterTimeOnIce,0.50), linetype = "dashed") +
  geom_vline(xintercept = quantile(playoff_shot_data$shooterTimeOnIce,0.75), linetype = "dashed") +
  theme_reach() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Player's Time on Ice",
       y = "Density",
       title = "Player's Shoot Mostly Between 16 and 42 Seconds On Ice")



# Shot Distance vs Rebounds -----------------------------------------------

#Hypothesis: taking a page out of basketball analytics, we hypothesized that longer
#shot distance would lead to more rebounds.

#Conclusion: the opposite was true as a shorter shot distance actually led to a higher chance of a reboud. 
playoff_shot_data %>%
  ggplot(aes(x = arenaAdjustedShotDistance, color = as.factor(shotGeneratedRebound))) +
  stat_ecdf(size = 1.5) +
  theme_reach() +
  scale_color_manual("Shot Generated Rebound",values=c("darkorange","darkblue")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "red") +
  labs(x = "Arena Adjusted Shot Distance",
       y = "Percent of Rebounds",
       fill = "Shot Generated Rebound",
       title = "Shots Closer to the Goal Generate More Rebounds") +
  theme(legend.position = "bottom")



# Amount of Shots and Goals vs Period of Game ----------------------------

#Hypothesis: we thought that the period would affect how the outcome of shots taken. 
#for example, we thought that teams would be aggressive at the end of games and take more shots

#Conclusion:however that isn't the case as shot-taking actually goes down while shot-making goes up in the end of the 3rd period.

playoff_shot_data %>%
  filter(time<=3600) %>% 
  ggplot(aes(x = time)) + 
  geom_histogram(aes(fill = event)) +
  theme_reach() +
  scale_fill_brewer(palette = "Paired") +
  geom_vline(xintercept = 1200) +
  geom_vline(xintercept = 2400) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_grid(event ~., margins = TRUE, scales = "free_y") +
  labs(x = "Time (seconds)",
       y = "Count",
       title = "Different Event Frequencies",
       subtitle = "GOAL = goal, MISS = missed net, SHOT = shot on target") 



#Looking at how shot distance affects types of shots--------------------------

#Hypothesis: shot distance does affect shot type as players need to do different things in different areas to score

#Conclusion: This holds true in the plot as there are disparities in shot distance between types of shots


playoff_shot_data %>%
  ggplot(aes(x = shotType, y = shotDistance)) +
  geom_violin(aes(fill = shotType), draw_quantiles = c(0.25, 0.5, 0.75)) +
  #geom_jitter(aes(color = shotType), width = 0.20, alpha = 0.10) +
  theme_reach() +
  labs(x = "Shot Type",
       y = "Shot Distance",
       title = "How Shot Distance Affects Shot Type",
       subtitle = "Shots used from the 2021 Stanley Cup Playoffs") +
    coord_flip()





############################ ADDITIONAL VISUALIZATION AND EXPLORATION ###################################### 

# Relationship Between Shot Distance and Goals Made -----------------------

# New Variables- Goals/Shots 

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


# Visulaization

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

#Histogram
hist(playoff_shot_data$shotAngle, breaks = 100)

#Shot Angle sd
player_shot_angles <- playoff_shot_data %>%
  group_by(shooterName) %>%
  summarize(shots = n(),
            shot_angle_sd = sd(shotAngle)) %>%
  filter(shots >= 10)

#Overtime- 1, Regular Time- 0
playoff_shot_data <- playoff_shot_data %>%
  mutate(is_overtime = ifelse(period > 3, 1, 0))

#Violin plot
playoff_shot_data %>%
  ggplot(aes(x = as.factor(goal), y = abs(shotDistance))) +
  geom_violin(aes(fill = as.factor(is_overtime)), draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_reach() +
  coord_flip()



#Frequency of Events & Shot Types- Tile Plot & Mosaic Plot---------------------------

playoff_shot_data %>%
  group_by(shotType, event) %>%
  summarize(count = n(),
            joint_prob = count / nrow(playoff_shot_data)) %>%
  ggplot(aes(x = shotType, y = event)) +
  geom_tile(aes(fill = count), color = "white") +
  geom_text(aes(label = round(joint_prob, digits = 3)), color = "white") +
  scale_fill_gradient(high = "darkorange", low = "darkblue") +
  theme_reach() + 
  labs(x = "Shot Type",
       y = "Event",
       fill = "Count",
       title = "Frequency of Events and Shot Type",
       subtitle = "2021 Stanley Cup Playoffs") +
  theme(legend.position = "bottom")

#checks independence
mosaic(~ shotType + event, data = playoff_shot_data)



# Early Experimentation with Shots vs Period ------------------------------

#simple histogram of all shots
playoff_shot_data %>%
  filter(time <= 3600) %>%
  ggplot(aes(x = time)) +
  #geom_density(aes(x = time), fill = "darkblue", alpha = 0.6) +
  geom_histogram(fill = "darkorange", alpha = 0.6, binwidth = 80) +
  geom_vline(xintercept = 1200) +
  geom_vline(xintercept = 2400) +
  theme_reach() 

#Specifiying Goals
goals <- playoff_shot_data %>%
  filter(event == "GOAL")

#simple histogram of all goals
goals %>%
  filter(time <= 3600) %>%
  ggplot(aes(x = time)) +
  #geom_density(aes(x = time), fill = "darkblue", alpha = 0.6) +
  geom_histogram(fill = "darkblue", alpha = 0.6, binwidth = 80) +
  geom_vline(xintercept = 1200) +
  geom_vline(xintercept = 2400) +
  theme_reach() 

#freqpoly of event vs time
playoff_shot_data %>%
  filter(time<=3600) %>% 
  ggplot(aes(x = time,
             color = event)) +
  geom_freqpoly() +
  theme_bw() +
  theme(legend.position = "bottom")


# Clustering with player's stats ------------------------------

#getting our player-level data ready for clustering
player_shot_stats <- playoff_shot_data %>%
  group_by(shooterName) %>%
  summarize(shots = n(),
            avg_shot_distance = mean(shotDistance, na.rm = T),
            avg_shot_angle = mean(shotAngle, na.rm = T)) %>%
  filter(shots >= 10)

#creating a basic clustering algorithm
init_nhl_kmeans <- 
  kmeans(dplyr::select(player_shot_stats, avg_shot_distance, avg_shot_angle),
         algorithm = "Lloyd", centers = 4, nstart = 100)

#plotting the basic clustering algorithm
player_shot_stats %>%
  mutate(player_clusters = as.factor(init_nhl_kmeans$cluster)) %>%
  ggplot(aes(x = avg_shot_distance, y = avg_shot_angle,
             color = player_clusters)) +
  geom_jitter(alpha = 0.5, size = 3) +
  theme_bw() +
  ggthemes::scale_color_colorblind()

#Using K Means++ 
nhl_kmeanspp <- 
  kcca(dplyr::select(player_shot_stats,
                     avg_shot_distance, avg_shot_angle), k = 4,
       control = list(initcent = "kmeanspp"))

#plotting K-Means++
player_shot_stats %>%
  mutate(Cluster = 
           as.factor(nhl_kmeanspp@cluster)) %>% #must use the @ symbol 
  ggplot(aes(x = avg_shot_distance, y = avg_shot_angle,
             color = Cluster)) +
  geom_jitter(alpha = 0.7, size = 3) + 
  scale_fill_brewer(palette = "Paired") +
  theme_reach() +
  labs(x = "Average Shot Distance",
       y = "Average Shot Angle",
       title = "NHL Player Clustering Based on Shot Distance and Shot Angle",
       fill = "Cluster") +
  theme(legend.position = "bottom")



