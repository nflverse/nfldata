# libraries
library(glue)
library(tidyverse)
library(nflreadr)

# games
games <- load_schedules(get_current_season()) |>
  filter(!is.na(result) & result != 0) |>
  mutate(
    winner = ifelse(result < 0, away_team, home_team),
    loser = ifelse(result < 0, home_team, away_team),
    winner_score = ifelse(result < 0, away_score, home_score),
    loser_score = ifelse(result < 0, home_score, away_score)
  ) |>
  group_by(winner, loser) |>
  slice(1) |>
  ungroup()

# any winless teams?
if (any(!is.element(games$winner, unique(games$loser))))
{
  stop("There is no circle as at least one team has zero wins")
}

# any undefeated teams?
if (any(!is.element(games$loser, unique(games$winner))))
{
  stop("There is no circle as at least one team has zero losses")
}

# beaten list
teams <- sort(unique(games$winner))
beaten <- list()
for (t in teams)
{
  beaten[[t]] <- sort(unique(games[games$winner == t,]$loser))
}

find_circle <- function(path)
{
  if (!length(path)) stop("Must pass in a starting team!")
  print(path)
  start <- path[1]
  end <- path[length(path)]
  if (is.element(start, beaten[[end]]) && length(path) == length(teams))
  {  
    return(c(path, start))
  }
  for (t in beaten[[end]])
  {
    if (!is.element(t, path))
    {
      result <- find_circle(c(path, t))
      if (length(result) > 0) return(result)
    }
  }
  return(NULL)
}

# start with a team with smallest number of wins
start_team <- games |>
  group_by(winner) |>
  summarize(wins = n()) |>
  ungroup() |>
  filter(wins == min(wins)) |>
  pull(winner)

# calculate circle
circle <- find_circle(start_team[1])

# output
output <- tibble(winner = circle) %>% 
  mutate(loser = lead(winner)) %>% 
  inner_join(games, by = c("winner", "loser")) %>% 
  mutate(str = glue("{winner} > {loser} ({winner_score}-{loser_score} W{week})"))
print(output$str)
