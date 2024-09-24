library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)

data <- read_csv("./CWC23_all_innings.csv")

data$start_date <- as.Date(data$start_date, format = '%d-%b-%y')  # Convert date column to Date objects
data <- data[order(data$start_date), ]  # Sort data by date

# Separate batting and bowling data
batting_data <- data %>% filter(bat_or_bowl == "bat")
bowling_data <- data %>% filter(bat_or_bowl == "bowl")

# Create match results summarizing table
inning_summary <- bowling_data %>%
  group_by(team, opposition, ground, start_date, inns) %>%
  summarise(
    total_runs_scored = sum(runs, na.rm = TRUE),
    total_wickets_taken = sum(wkts, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(start_date)

# Separate data for each inning into first and second innings
first_innings <- inning_summary %>%
  filter(inns == 1) %>%
  rename(
    team2 = team,
    team1_runs = total_runs_scored,
    team1_wickets = total_wickets_taken
  )

second_innings <- inning_summary %>%
  filter(inns == 2) %>%
  rename(
    team1 = team,
    team2_runs = total_runs_scored,
    team2_wickets = total_wickets_taken
  )

# Merge first and second innings data into a single record for each match
merged_matches <- first_innings %>%
  inner_join(second_innings, by = c("opposition" = "team1", "team2" = "opposition", "ground", "start_date")) %>%
  select(opposition, team2, ground, start_date, team1_runs, team2_runs, team1_wickets, team2_wickets)

# Rename columns for clarity
names(merged_matches) <- c("Team_1", "Team_2", "ground", "start_date", "Team1_total", "Team2_total", "Team1_wickets_fallen", "Team2_wickets_fallen")

# Add 'match_total_runs' field to indicate the sum of both teams' runs
merged_matches <- merged_matches %>%
  mutate(
    match_total_runs = Team1_total + Team2_total,
    match_total_wickets = Team1_wickets_fallen + Team2_wickets_fallen
  )

# Add a 'winner' field to indicate the winning team
merged_matches <- merged_matches %>%
  mutate(winner = case_when(
    Team1_total > Team2_total ~ Team_1,
    Team2_total > Team1_total ~ Team_2,
    TRUE ~ "Tie"
  ))

# Determine if the winning team batted first or second
merged_matches <- merged_matches %>%
  mutate(
    batting_first_winner = case_when(
      winner == Team_1 ~ "1st Innings",
      winner == Team_2 ~ "2nd Innings",
      TRUE ~ NA_character_
    )
  )

# Calculate win percentages for teams batting 1st vs. 2nd at each ground
win_summary <- merged_matches %>%
  filter(!is.na(batting_first_winner)) %>%
  group_by(ground, batting_first_winner) %>%
  summarise(
    wins = n(),
    total_matches = n_distinct(start_date)
  ) %>%
  mutate(
    win_percentage = (wins / sum(wins)) * 100
  ) %>%
  ungroup()

# Calculate win percentages by ground and inning
win_summary <- merged_matches %>%
  mutate(winning_inning = case_when(
    winner == Team_1 ~ "1st Inning",
    winner == Team_2 ~ "2nd Inning",
    TRUE ~ "Tie"
  )) %>%
  group_by(ground, winning_inning) %>%
  summarise(win_count = n()) %>%
  ungroup() %>%
  group_by(ground) %>%
  mutate(win_percentage = (win_count / sum(win_count)) * 100)

# Filter out any "Tie" matches if you don't want them in the plot
win_summary <- win_summary %>%
  filter(winning_inning != "Tie")

# Create columns to identify teams setting the target and chasing
win_summary <- merged_matches %>%
  mutate(
    setting_target = Team_1,
    chasing_target = Team_2
  ) %>%
  mutate(winning_strategy = case_when(
    winner == setting_target ~ "Target Setting",
    winner == chasing_target ~ "Target Chasing",
    TRUE ~ "Tie"
  ))

# Calculate the win percentage by strategy (target setting or chasing) for each team
win_percentage <- win_summary %>%
  group_by(Team_1, winning_strategy) %>%
  summarise(win_count = n()) %>%
  ungroup() %>%
  group_by(Team_1) %>%
  mutate(win_percentage = (win_count / sum(win_count)) * 100)

# Filter out any "Tie" matches if you don't want them in the plot
win_percentage <- win_percentage %>%
  filter(winning_strategy != "Tie")

# Aggregate bowling performance data for each team
batting_team_perfomance <- inning_summary %>%
  group_by(opposition) %>%
  summarise(
    total_batting_runs = sum(total_runs_scored, na.rm = TRUE),
    matches_played_batting = n_distinct(start_date)
  )
batting_team_perfomance <- batting_team_perfomance %>% rename(team = opposition)

bowling_team_perfomance <- inning_summary %>%
  group_by(team) %>%
  summarise(
    total_runs_against = sum(total_runs_scored, na.rm = TRUE),
    total_wickets_taken = sum(total_wickets_taken, na.rm = TRUE),
    matches_played_bowling = n_distinct(start_date)
  )

# Merge batting and bowling summaries to create a complete team performance table
team_performance <- batting_team_perfomance %>%
  left_join(bowling_team_perfomance, by = "team") %>%
  mutate(
    matches_played = coalesce(matches_played_batting, matches_played_bowling),
    batting_score_average = total_batting_runs / matches_played,
    bowling_score_average = total_runs_against / matches_played,
    wicket_taking_average = total_runs_against / total_wickets_taken
  ) %>%
  select(
    team,
    total_batting_runs,
    total_runs_against,
    total_wickets_taken,
    batting_score_average,
    bowling_score_average,
    wicket_taking_average,
    matches_played
  )

# Calculate total wins for each team
team_wins_count <- merged_matches %>%
  group_by(winner) %>%
  summarise(
    win_count = n_distinct(start_date)
  )

# Count the number of matches hosted by each ground
ground_counts <- merged_matches %>%
  count(ground)

# Calculate batting score average for each team
batting_avg <- data %>%
  group_by(team) %>%
  summarise(batting_score_avg = mean(runs, na.rm = TRUE))

# Calculate bowling score average for each team
bowling_avg <- data %>%
  filter(bat_or_bowl == "bowl") %>%
  group_by(team) %>%
  summarise(bowling_score_avg = mean(runs, na.rm = TRUE))

# Calculate average runs by ground and inning
runs_summary <- data %>%
  group_by(ground, inns) %>%
  summarise(avg_runs = mean(runs, na.rm = TRUE))

# Calculate average wickets by ground and inning
wickets_summary <- merged_matches %>%
  select(ground, Team1_wickets_fallen, Team2_wickets_fallen) %>%
  mutate(
    first_inning_wickets = Team2_wickets_fallen,
    second_inning_wickets = Team1_wickets_fallen
  ) %>%
  group_by(ground) %>%
  summarise(
    avg_first_inning_wickets = mean(first_inning_wickets, na.rm = TRUE),
    avg_second_inning_wickets = mean(second_inning_wickets, na.rm = TRUE)
  )

# Reshape wickets summary data for plotting
wickets_summary_long <- wickets_summary %>%
  pivot_longer(cols = c("avg_first_inning_wickets", "avg_second_inning_wickets"),
               names_to = "inning", values_to = "avg_wickets") %>%
  mutate(inning = case_when(
    inning == "avg_first_inning_wickets" ~ "1st Inning",
    inning == "avg_second_inning_wickets" ~ "2nd Inning"
  ))

# Calculate player contribution by runs
player_runs_contribution <- bowling_data %>%
  group_by(player, team) %>%
  summarise(
    total_runs = sum(runs, na.rm = TRUE),
    total_wickets = sum(wkts, na.rm = TRUE)
  ) %>%
  ungroup()

# Reshape player contribution data for plotting
player_contribution_long <- player_runs_contribution %>%
  pivot_longer(cols = c("total_runs", "total_wickets"),
               names_to = "contribution_type", values_to = "total_contribution") %>%
  mutate(contribution_type = case_when(
    contribution_type == "total_runs" ~ "Runs",
    contribution_type == "total_wickets" ~ "W ickets"
  ))

# Calculate top run scorers
top_run_scorers <- batting_data %>%
  group_by(player) %>%
  summarise(total_runs = sum(runs, na.rm = TRUE)) %>%
  arrange(desc(total_runs)) %>%
  slice_max(total_runs, n = 20)

# Calculate top strike rates
strike_rate_data <- batting_data %>%
  group_by(player) %>%
  summarise(
    total_runs = sum(runs, na.rm = TRUE),
    total_balls_faced = sum(bb_bf, na.rm = TRUE)
  ) %>%
  filter(total_runs > 100) %>%
  mutate(strike_rate = (total_runs / total_balls_faced) * 100) %>%
  arrange(desc(strike_rate)) %>%
  slice_max(strike_rate, n = 20)

# Calculate top batting averages
batting_average_data <- batting_data %>%
  group_by(player) %>%
  summarise(
    total_runs = sum(runs, na.rm = TRUE),
    innings_played = n(),
    not_outs = sum(not_out, na.rm = TRUE)
  ) %>%
  filter(innings_played > 3) %>%
  mutate(batting_average = total_runs / (innings_played - not_outs)) %>%
  arrange(desc(batting_average)) %>%
  slice_max(batting_average, n = 20)

# Calculate top wicket takers
wicket_takers_data <- bowling_data %>%
  group_by(player) %>%
  summarise(
    total_wickets = sum(wkts, na.rm = TRUE)
  ) %>%
  arrange(desc(total_wickets)) %>%
  slice_max(total_wickets, n = 20)

# Calculate top economical bowlers
economical_bowlers_data <- bowling_data %>%
  group_by(player) %>%
  summarise(
    total_runs_conceded = sum(runs, na.rm = TRUE),
    total_balls_bowled = sum(bb_bf, na.rm = TRUE),
    total_overs_bowled = total_balls_bowled / 6
  ) %>%
  filter(total_overs_bowled > 15) %>%
  mutate(economy_rate = total_runs_conceded / total_overs_bowled) %>%
  arrange(economy_rate) %>%
  slice_min(economy_rate, n = 20)

# Calculate top bowling averages
bowling_average_data <- bowling_data %>%
  group_by(player) %>%
  summarise(
    total_runs_conceded = sum(runs, na.rm = TRUE),
    total_wickets = sum(wkts, na.rm = TRUE)
  ) %>%
  filter(total_wickets > 5) %>%
  mutate(bowling_average = total_runs_conceded / total_wickets) %>%
  arrange(bowling_average) %>%
  slice_min(bowling_average, n = 20)

# Create plots
# ...

# Create a histogram of innings total runs to show the distribution
ggplot(inning_summary, aes(x = total_runs_scored)) + 
  geom_histogram(binwidth = 20, fill = "steelblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Innings Total Runs in the Tournament",
       x = "Innings Total Runs",
       y = "Frequency") + 
  theme_minimal()

# Summary statistics for innings total runs
summary(inning_summary$total_runs_scored)

# Create a histogram of 1st-innings total runs to show the distribution
ggplot(first_innings, aes(x = team1_runs)) + 
  geom_histogram(binwidth = 20, fill = "steelblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Innings Total Runs in the Tournament",
       x = "Innings Total Runs",
       y = "Frequency") + 
  theme_minimal()

# Summary statistics for 1st-innings total runs
summary(first_innings$team1_runs)

# Create a histogram of 2nd-innings total runs to show the distribution
ggplot(second_innings, aes(x = team2_runs)) + 
  geom_histogram(binwidth = 20, fill = "steelblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Innings Total Runs in the Tournament",
       x = "Innings Total Runs",
       y = "Frequency") + 
  theme_minimal()

# Summary statistics for 2nd-innings total runs
summary(second_innings$team2_runs)
#-------------------------

# Create a histogram of innings total wickets to show the distribution
ggplot(inning_summary, aes(x = total_wickets_taken)) + 
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Innings Total Wickets in the Tournament",
       x = "Innings Total Wickets",
       y = "Frequency") + 
  theme_minimal()

# Summary statistics for innings total wickets
summary(inning_summary$total_wickets_taken)

# Create a histogram of 1st-innings total wickets to show the distribution
ggplot(first_innings, aes(x = team1_wickets)) + 
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of 1st-Innings Total Wickets taken in the Tournament",
       x = "Innings Total Wickets",
       y = "Frequency") + 
  theme_minimal()

# Summary statistics for 1st-innings total wickets
summary(first_innings$team1_wickets)

# Create a histogram of 2nd-innings total wickets to show the distribution
ggplot(second_innings, aes(x = team2_wickets)) + 
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of 2nd-Innings Total Wickets taken in the Tournament",
       x = "Innings Total Wickets",
       y = "Frequency") + 
  theme_minimal()

# Summary statistics for 2nd-innings total wickets
summary(second_innings$team2_wickets)
#----------------------------------

# Create a pie chart to show the distribution of matches hosted by each ground
ggplot(ground_counts, aes(x = "", y = n, fill = ground)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar(theta = "y") + 
  labs(title = "Distribution of Matches Hosted by Ground") + 
  theme_void() + 
  scale_fill_brewer(palette = "Set3") +  # Optional: Choose different color palettes
  geom_text(aes(label = n), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)  # Adjust text color and size as needed
#-------------------------------------------

# Create a box plot to compare runs per match across different grounds
ggplot(merged_matches, aes(x = reorder(ground, -match_total_runs, FUN = median), y = match_total_runs, fill = ground)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 2, alpha = 0.7) + 
  scale_fill_brewer(palette = "Set3") +  # Adding color palette
  theme_minimal(base_size = 14) +  # Increase font size for better readability
  labs(
    title = "Comparison of Runs per Match by Ground",
    x = "Ground",
    y = "Total Runs per Match",
    fill = "Ground"
  ) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels for readability
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the title
    legend.position = "none"  # Remove legend for cleaner look
  ) 
#----------------------------------------------

# Create a box plot to compare wickets per match across different grounds
ggplot(merged_matches, aes(x = ground, y = match_total_wickets, fill = ground)) + 
  geom_boxplot(outlier.shape = NA, color = "black") +  # Box plot with no outlier points shown
  labs(
    title = "Comparison of Total Wickets Per Match Across Grounds",
    x = "Ground",
    y = "Total Wickets Per Match"
  ) + 
  scale_fill_brewer(palette = "Set3") +  # Attractive color palette
  theme_minimal() +                      # Clean minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability
    plot.title = element_text(size = 14, face = "bold") # Enhance title font
  )
#------------------------------------------------

# Create bar plot of Batting Score Averages by team
ggplot(team_performance, aes(x = reorder(team, batting_score_average), y = batting_score_average, fill = team)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Batting Score Averages of Teams",
    x = "Team",
    y = "Batting Score Average"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# Create bar plot of Bowling Score Averages by team
ggplot(team_performance, aes(x = reorder(team, -bowling_score_average), y = bowling_score_average, fill = team)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Score Averages against Teams",
    x = "Team vs.",
    y = "Bowling Score Average"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# Create bar plot of total runs scored by each team
ggplot(team_performance, aes(x = reorder(team, -total_batting_runs), y = total_batting_runs, fill = team)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(
    title = "Total Runs Scored by Each Team",
    x = "Team",
    y = "Total Runs Scored"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# Create bar plot of total wickets taken by each team
ggplot(team_performance, aes(x = reorder(team, -total_wickets_taken), y = total_wickets_taken, fill = team)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(
    title = "Total Wickets Taken by Each Team",
    x = "Team",
    y = "Total Wickets Taken"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# Create bar plot of Wicket-Taking Average by team
ggplot(team_performance, aes(x = reorder(team, -wicket_taking_average), y = wicket_taking_average, fill = team)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Wicket-Taking Averages of Teams",
    x = "Team",
    y = "Wicket-Taking Average"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")

# Create bar plot will show the total number of wins for each team
ggplot(team_wins_count, aes(x = reorder(winner, -win_count), y = win_count, fill = winner)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(
    title = "Total wins of Teams",
    x = "Team",
    y = "Win Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Paired")

# Create bar graph of the top run scorers
ggplot(top_run_scorers, aes(x = reorder(player, total_runs), y = total_runs, fill = total_runs)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Run Scorers of the Tournament", x = "Player Name", y = "Total Runs") +
  theme_minimal() +
  scale_fill_gradient(low = "lightblue", high = "darkblue")

# Create bar graph for top strike rates
ggplot(strike_rate_data, aes(x = reorder(player, strike_rate), y = strike_rate, fill = strike_rate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Players by Strike Rate (Runs > 100)", x = "Player Name", y = "Strike Rate") +
  theme_minimal() +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen")

# Create bar graph for top averages
ggplot(batting_average_data, aes(x = reorder(player, batting_average), y = batting_average, fill = batting_average)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Players by Batting Average (Innings > 3)", x = "Player Name", y = "Batting Average") +
  theme_minimal() +
  scale_fill_gradient(low = "lightcoral", high = "darkred")

# Create bar graph for top wicket-takers
ggplot(wicket_takers_data, aes(x = reorder(player, total_wickets), y = total_wickets, fill = total_wickets)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Wicket Takers of the Tournament", x = "Player Name", y = "Total Wickets") +
  theme_minimal() +
  scale_fill_gradient(low = "lightblue", high = "darkblue")

# Create bar graph for top economical bowlers
ggplot(economical_bowlers_data, aes(x = reorder(player, -economy_rate), y = economy_rate, fill = economy_rate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Economical Bowlers (Bowled > 15 Overs)", x = "Player Name", y = "Economy Rate") +
  theme_minimal() +
  scale_fill_gradient(low = "lightcoral", high = "darkred")

# Create bar graph for top bowling averages
ggplot(bowling_average_data, aes(x = reorder(player, -bowling_average), y = bowling_average, fill = bowling_average)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Bowlers by Bowling Average (More Than 5 Wickets)", x = "Player Name", y = "Bowling Average") +
  theme_minimal () +
  scale_fill_gradient(low = "lightcoral", high = "darkred")

# Create bar plot for batting average by Ground
ggplot(runs_summary, aes(x = ground, y = avg_runs, fill = factor(inns))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Runs in 1st vs 2nd Innings by Ground", x = "Ground", y = "Average Runs", fill = "Inning") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create scatter plot of 1st innings runs vs 2nd innings runs for each ground
ggplot(merged_matches, aes(x = Team1_total, y = Team2_total, color = ground)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Runs in 1st Innings vs 2nd Innings by Ground", x = "Runs in 1st Innings", y = "Runs in 2nd Innings", color = "Ground") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14), legend.position = "right")

# Create bar plot for win percentage by ground and winning inning
ggplot(win_summary, aes(x = ground, y = win_percentage, fill = winning_inning)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Win Percentage of 1st vs 2nd Innings by Ground", x = "Ground", y = "Win Percentage", fill = "Winning Inning") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create bar chart showing win percentages for teams batting 1st vs 2nd at each ground
ggplot(win_summary, aes(x = ground, y = win_percentage, fill = batting_first_winner)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Win Percentages for Teams Batting 1st vs 2nd at Each Ground", x = "Ground", y = "Win Percentage (%)", fill = "Batting Order") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5, size = 14)) +
  scale_fill_manual(values = c("1st Innings" = "blue", "2nd Innings" = "red"))

# Create bar plot for wickets per inning by ground
ggplot(wickets_summary_long, aes(x = ground, y = avg_wickets, fill = inning)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Wickets in 1st vs 2nd Inning by Ground", x = "Ground", y = "Average Wickets Taken", fill = "Inning") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create scatter plot of wickets fallen in 1st innings vs 2nd innings for each ground
ggplot(merged_matches, aes(x = Team1_wickets_fallen, y = Team2_wickets_fallen, color = ground)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Wickets Fallen in 1st Innings vs 2nd Innings by Ground", x = "Wickets Fallen in 1st Innings", y = "Wickets Fallen in 2nd Innings", color = "Ground") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14), legend.position = "right")

# Create bar plot for win percentage by strategy (target setting or chasing)
ggplot(win_percentage, aes(x = Team_1, y = win_percentage, fill = winning_strategy)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Win Percentage by Target Setting vs Target Chasing", x = "Team", y = "Win Percentage", fill = "Strategy") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
