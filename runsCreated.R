# Start date: Tuesday, August 15th

# Running simulation from Mathletics to practice data wrangling, cleaning, 
# and modeling. Variety of ways we can go from there to explore further.

library(tidyverse)
library(dplyr)

#####################################################################

# First lets start by calculating the runs created (rc) for each MLB team
# in the 2021 season using Bill James' original equation.

team_stats <- read.csv("2021_team_stats.csv")
head(team_stats)

team_stats <- team_stats %>%
  mutate(RC = ((H+BB+HBP+IBB)*TB)/(AB+BB+IBB+HBP),
         RC_diff = RC-R) %>%
  arrange(RC)

# What is this complicated formula? Let's think of it like this: 
# We are multiplying (H+BB+HBP+IBB) by TB / (AB+BB+IBB+HBP).This fraction is 
# really a rate of bases per at bat. And we multiply this by total amount
# of actual base runners, so it is ultimately # of base runners * rate
# they are advanced. Pretty neat.

# Plotting both results
team_stats$RC <- factor(team_stats$RC, levels = team_stats$RC)
ggplot(team_stats, aes(x=Tm, y=RC)) +
  geom_point()

# Calculate mean average deviation for Runs Scored vs prediction
mad_rc <- (sum(abs(team_stats$RC_diff)) / 30) / mean(team_stats$R)

# On average this simple prediction is off by about 24.6 runs compared to the
# actual runs scored. Since the average runs scored was 733.6, our prediction
# has an average error of only 3.4%.

#####################################################################

# Now the discussion turns to evaluating players. How can we use this information
# to determine the runs created by an individual player and how can we use this
# to compare players?

player_stats <- read.csv("2021_batter_stats.csv")
head(player_stats)

# Using the formula from above to get total runs created for specific players
player_stats <- player_stats %>%
  filter(last_name %in% c("Goldschmidt",
                          "Arenado",
                          "Ohtani",
                          "Harper",
                          "Ramirez")) %>%
  mutate(RC = ((b_total_hits+b_walk+b_hit_by_pitch+b_intent_walk)*b_total_bases)
         /(b_ab+b_walk+b_hit_by_pitch+b_intent_walk))








