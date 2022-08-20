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
# from 2011 to 2021 using Bill James' original, simplest equation.

team_stats <- read.csv("11-21_team_stats.csv")
current_teams <- read.csv("MLB_teams.csv")
head(team_stats)

team_stats <- team_stats %>%
  filter(Season != 2020) %>%
  mutate(RC = ((H+BB+HBP)*TB)/(AB+BB+HBP),
         err = abs(RC-R))
team_stats <- merge(team_stats, current_teams, by="Team")

# What is this complicated formula? Let's think of it like this: 
# We are multiplying (H+BB+HBP+IBB) by TB / (AB+BB+IBB+HBP).This fraction is 
# really a rate of bases per at bat. And we multiply this by total amount
# of actual base runners, so it is ultimately # of base runners * rate
# they are advanced. Pretty neat.

# Calculate mean absolute deviation for Runs Scored vs prediction
avg_runs <- mean(team_stats$R)
mad_rc <- mean(team_stats$err)
perc_err <- mad_rc / avg_runs

# On average this simple prediction is off by about 24.4 runs over the 10
# year period. Since the average runs scored was 713.07, our prediction
# has an average error of only 3.4%.

# Plotting both results
team_stats_plot <- team_stats %>%
  dplyr::select(Tm, R, RC) %>%
  melt(id.vars = "Tm")

ggplot(team_stats_plot, aes(x=reorder(Tm, value), y=value)) +
  geom_point(aes(colour = variable)) +
  xlab("Team") +
  ylab("Runs Created 2011-2021") +
  scale_x_discrete(guide = guide_axis(n.dodge=3))

# Notice how with this simple formula the RC nearly always underestimates the
# actual number of runs scored. This will be something to keep our eye on.


#####################################################################


# Using basic RC formula for players in 2021

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
                          "Ramirez",
                          "Guerrero Jr.")) %>%
  mutate(RC = ((H+BB+HBP)*TB)/(AB+BB+HBP) )

# This RC value gives us the total number of runs created, using our formula,
# for the entire season. Now lets look at runs created per game for each of
# these players.

# To get RC/G we cannot just divide runs created by 162 because that would
# not be an accurate representation of the players influence and impact on
# games throughout the season. Sometimes he hits more sometimes he hits less in
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
         Games_used = Outs / 26.72,
         RC_G = RC/Games_used)

# Note: This stats indicates the number of runs we would expect a team to score
# if it was made up entirely of the player in question. Which, obviously is 
# not the case. It is an interesting metric to compare players however.


#####################################################################

# Now lets use the linear weights apprach to create a regression model to 
# project player and team RC and see how to compares to the previous basic 
# RC approach.

# Using Linear Weights approach
lw_model <- lm(R ~ X1B+X2B+X3B+HR+BB+HBP+SB, data = team_stats)
summary(lw_model)

# Here we are creating a regression model to get how much value each offensive
# result contributes to a run being scored (dependent variable).

# We can see from the summary that a HR provides the most weight followed by
# triple, double and single. As expected right? Now lets get projection.

team_stats <- team_stats %>%
  mutate(lw_RC = predict(lw_model),
         lw_err = abs(lw_RC - R))
lw_mad_rc <- mean(team_stats$lw_err)
lw_perc_err <- lw_mad_rc / avg_runs

# Here we see an average error of only 18.3 runs across the 10 year span! This
# reduces are avg % error to only 2.5% as compared to 3.4% before!

# To do this for players we will think about, for example, how many HRs a player
# will hit per out and then we can extrapolate to get how many HRs a team of
# of this player should hit per season. Then we can compare to our previous
# prediction.

lw_player_projections <- player_stats %>%
  mutate(scale_factor = 4329 / Outs) %>%
  select(-c(player_id, year, Age, SLG, OBP, OPS, AVG, R, K., BB., OBA))

lw_player_projections[,seq(6, 19)] = lw_player_projections$scale_factor * 
  lw_player_projections[,seq(6, 19)]

lw_player_projections <- lw_player_projections %>%
    mutate(lw_RC = predict(lw_model, newdata = .),
           lw_RC_G = lw_RC / 162)






