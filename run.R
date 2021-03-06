# Load packages
library(retrosheet)
library(dplyr)
library(tidyr)
library(ggplot2)

# Pull data
years <- 1996:2014
game.logs.raw <- lapply(years, getRetrosheet, type = "game")

# Reformat data
game.logs.list <- lapply(game.logs.raw, subset, select = c("VisTm", "HmTm", "VisTmGNum", "HmTmGNum", "VisRuns", "HmRuns"))
game.logs.list <- mapply(FUN = cbind, game.logs.list, year = years, SIMPLIFY = FALSE)
game.logs <- rbind_all(game.logs.list)

# Create data frame with one row per team game
visitors <- game.logs %>% 
  mutate(won = VisRuns > HmRuns) %>%
  select(team = VisTm, team.game.num = VisTmGNum, year, won)
home <- game.logs %>% 
  mutate(won = HmRuns > VisRuns) %>%
  select(team = HmTm, team.game.num = HmTmGNum, year, won)
team.games <- rbind(visitors, home) %>%
  arrange(team, year, team.game.num)

# Calculate year to date and rest of season win percentages
rolling.win.pcts <- team.games %>% 
  group_by(team, year) %>%
  mutate(ytd.win.pct = cummean(won), ros.win.pct = (sum(won) - cumsum(won)) / (n() - team.game.num)) %>%
  ungroup() %>%
  select(-won)

# Join with previous year's win % and create data frame for model fitting
model.data <- team.games %>%
  group_by(team, prev.year = year - 1) %>%
  summarize(prev.win.pct = mean(won)) %>%
  ungroup() %>%
  inner_join(rolling.win.pcts, by = c("prev.year" = "year", "team" = "team")) %>%
  select(-team, -prev.year) %>%
  filter(between(team.game.num, 0, 157))

# Fit linear model for each team game
lm.models <- model.data %>%
  group_by(team.game.num) %>%
  do(model = lm(ros.win.pct ~ prev.win.pct + ytd.win.pct, data = .))

# Extract coefficients
lm.coefs <- data.frame(team.game.num = lm.models$team.game.num, 
                       t(sapply(lm.models$model, coef)))
colnames(lm.coefs)[2] <- "intercept"

# Visualize coefficients over course of season (multiply intercept by 2 so they are on same scale)
lm.coefs %>% 
  mutate("intercept*2" = intercept * 2) %>%
  select(-intercept) %>%
  gather(term, value, -team.game.num) %>%
  ggplot(aes(x = team.game.num, y = value, group = term, colour = term)) +
  geom_line()
ggsave("coefs_plot.png")

# Extract r squared
lm.r.squared <- data.frame(team.game.num = lm.models$team.game.num, 
                           r.squared = sapply(lm.models$model, function(x) summary(x)$r.squared))

# Visualize r squared over course of season
lm.r.squared %>%
  ggplot(aes(x = team.game.num, y = r.squared)) +
  geom_line()
