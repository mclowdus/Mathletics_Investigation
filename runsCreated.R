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

team_stats <- read.csv("2021_team_stats.csv")
head(team_stats)

team_stats <- team_stats %>%
  mutate(RC = ((H+BB+HBP)*TB)/(AB+BB+HBP),
         err = abs(RC-R))

# Calculate mean absolute deviation for Runs Scored vs prediction
mad_rc <- mean(team_stats$err)
# On average this simple prediction is off by about 28.5 runs compared to the
# actual runs scored. Since the average runs scored was 733.6, our prediction
# has an average error of only 3.9%.

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

# Notice how with this simple formula the RC nearly always underestimates the
# actual number of runs scored. This will be something to keep our eye on.


#####################################################################


# Using basic RC formula for players

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

# This RC value gives us the total number of runs created, using our formula,
# for the entire season. Now lets look at runs created per game for each of
# these players.

# To get RC/G we cannot just divide runs created by 162 because that would
# not be an accurate representation of the players influence and impact on
# games throughout the season. Sometime he hits more sometimes he hits less in
# each game.

# We can, however get the number of outs the player "consumed" during the season.
# Then we can determine how many runs a player created compared to the number
# of outs he creates. And since we know the number of outs in a game we can get
# RC/G.

# Outs is really AB - H - 0.018*AB where the 0.18 comes from a 1.8% chance of 
# an error. Then we add in additional outs which come from SB, SF, CS, and GIDP.
# Lets get number of outs for each player:

player_stats <- player_stats %>%
  mutate(Outs =  0.982*AB-H+GIDP+SF+SB+CS,
         Games_used = Outs / 27,
         RC_G = RC/Games_used)

# Note: This stats indicates the number of runs we would expect a team to score
# if it was made up entirely of the player in question. Which, obviously is 
# not the case. It is an interesting metric to compare players however.


#####################################################################


# Using Linear Weights approach
lw_model <- lm()













