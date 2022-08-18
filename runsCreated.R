# Start date: Tuesday, August 15th

# Running simulation from Mathletics to practice data wrangling, cleaning, 
# and modeling. Variety of ways we can go from there to explore further.

library(tidyverse)
library(dplyr)
library(stringr)
library(reshape2)
setwd("/Users/maxclowdus/Desktop/R_Files/Mathletics")

#####################################################################

# First lets start by calculating the runs created (RC) for each MLB team
# in the 2021 season using Bill James' original, simplest equation.

team_stats <- read.csv("2021_team_stats.csv", stringsAsFactors = FALSE)
head(team_stats)

team_stats <- team_stats %>%
  mutate(RC = ((H+BB+HBP)*TB)/(AB+BB+HBP),
         RC_diff = RC-R)

# What is this complicated formula? Let's think of it like this: 
# We are multiplying (H+BB+HBP+IBB) by TB / (AB+BB+IBB+HBP).This fraction is 
# really a rate of bases per at bat. And we multiply this by total amount
# of actual base runners, so it is ultimately # of base runners * rate
# they are advanced. Pretty neat.

# Plotting both results
team_stats <- team_stats %>%
  dplyr::select(Tm, R, RC) %>%
  melt(id.vars = "Tm")

ggplot(team_stats, aes(x=reorder(Tm, value), y=value)) +
  geom_point(aes(colour = variable)) +
  xlab("Team") +
  ylab("Runs Created 2021") +
  scale_x_discrete(guide = guide_axis(n.dodge=3))

# Calculate mean average deviation for Runs Scored vs prediction
mad_rc <- (sum(abs(team_stats$RC_diff)) / 30) / mean(team_stats$R)

# On average this simple prediction is off by about 28.5 runs compared to the
# actual runs scored. Since the average runs scored was 733.6, our prediction
# has an average error of only 3.9%.

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
  mutate(RC = ((H+BB+HBP)*TB)/(AB+BB+HBP) )

# This RC value gives us the total number of runs created using our formula
# for the entire season. Now lets look at runs created per game for each of
# these players.






