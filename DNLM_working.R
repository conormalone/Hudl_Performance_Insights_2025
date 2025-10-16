library(arrow)
library(tidyverse)
library(dlnm)
library(mgcv)
library(lubridate)
library(ggplot2)
library(car)
###################
library(dlnm)
library(splines)

df<- read.csv("team_agg_dlnm2.csv")
#df<- read.csv("shooter_dlnm.csv")
#df$match_date <- as.Date(df$match_date)
df$week_start <- as.Date(df$week_start)

# Create a lagged variable for the exposure (e.g., ACWR_mean)
# This creates multiple lags at once
df <- df %>%
  group_by(shot_team_id) %>%
  mutate(
    acwr_lag1 = lag(ACWR, 1),
    acwr_lag2 = lag(ACWR, 2), 
    acwr_lag3 = lag(ACWR, 3),
    acwr_lag4 = lag(ACWR, 4)
  ) %>%
  ungroup()



model_simple <- glm(statsbomb_xg ~ acwr_lag1,
                    data = df, family = gaussian())

summary(model_simple)

# Check which lags are significant
car::Anova(model_simple)
summary(df$acwr_lag1)

###################################################################################
df<- read.csv("shooter_dlnm.csv")
#df$match_date <- as.Date(df$match_date)
df$week_start <- as.Date(df$week_start)

# Create a lagged variable for the exposure (e.g., ACWR_mean)
# This creates multiple lags at once
df <- df %>%
  group_by(player_id) %>%
  mutate(
    acwr_lag1 = lag(ACWR, 1),
    acwr_lag2 = lag(ACWR, 2), 
    acwr_lag3 = lag(ACWR, 3),
    acwr_lag4 = lag(ACWR, 4)
  ) %>%
  ungroup()



model_simple <- glm(statsbomb_xg ~ acwr_lag3,
                    data = df, family = gaussian())

summary(model_simple)

# Check which lags are significant
car::Anova(model_simple)
summary(df$acwr_lag3)
