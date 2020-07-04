############# LIBRARIES ##############

library(tidyverse)
library(ggplot2)
library(ggimage)
library(glue)
library(janitor)
library(nflfastR)

########## HELPER FUNCTIONS ##########

# report progress to console
report <- function(msg)
{
  message(paste0(Sys.time(),": ",msg))
}

# read csv silently
silent_csv <- function(location)
{
  return(suppressWarnings(suppressMessages(read_csv(location))))
}

# read csv silently
silent_readLines <- function(location)
{
  return(suppressWarnings(suppressMessages(readLines(location))))
}

# look for text in names of columns of data frame
grep_col <- function(x,df=pbp)
{
  return(colnames(df)[grepl(x,colnames(df))])
}

# conditionally apply a function to data
## credit for this function goes to RStudio community member alistaire
provided <- function(data,condition,call)
{
  if (rlang::eval_tidy(enquo(condition),data))
  {
    return(rlang::eval_tidy(rlang::quo_squash(quo(data %>% !!enquo(call)))))
  } else {
    return(data)
  } 
}

# express values as percentages
## character vector with the "%" sign at the end
## designed for labeling axes in plots
make_pct <- function(x)
{
  return(paste0(round(100*x,0),"%"))
}

# apply logos and colors
apply_colors_and_logos <- function(p,team_col="")
{
  # default team_col values
  team_col <- case_when(
    team_col != "" ~ team_col,
    "team" %in% colnames(p) ~ "team",
    "posteam" %in% colnames(p) ~ "posteam",
    "defteam" %in% colnames(p) ~ "defteam",
    TRUE ~ team_col
  )
  
  # raise error if column not present
  if (!(team_col %in% colnames(p))) stop(glue("Column {team_col} is not present"))
  
  # load data
  if (!exists("team_colors"))
    team_colors <- silent_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/teamcolors.csv")
  if (!exists("team_logos"))
    team_logos <- silent_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/logos.csv")
  
  # function to determine the brightness of a color
  brightness <- function(hex)
  {
    result <- rep(0,length(hex))
    for (i in 2:7)
    {
      ch <- substr(hex,i,i)
      result <- result + ifelse(i %% 2 == 0,16,1) * case_when(
        ch == "0" ~ 0, ch == "1" ~ 1, ch == "2" ~ 2, ch == "3" ~ 3, ch == "4" ~ 4,
        ch == "5" ~ 5, ch == "6" ~ 6, ch == "7" ~ 7, ch == "8" ~ 8, ch == "9" ~ 9,        
        ch == "a" | ch == "A" ~ 10,
        ch == "b" | ch == "B" ~ 11,
        ch == "c" | ch == "C" ~ 12,
        ch == "d" | ch == "D" ~ 13,
        ch == "e" | ch == "E" ~ 14,
        ch == "f" | ch == "F" ~ 15,
        TRUE ~ 0
      )
    }
    return(result)
  }
  
  # use the primary color if brightness > 128, else grab secondary
  team_colors <- team_colors %>% 
    mutate(use_color=ifelse(brightness(color) > 140,color,color2)) %>% 
    select(team,use_color)
  
  # add to p
  p <- p %>% 
    inner_join(team_colors,by=setNames("team",team_col)) %>% 
    inner_join(team_logos,by=setNames("team",team_col))
}

########## FUNCTIONS TO APPLY ADDITIONAL DATA ##########

# apply Lee Sharpe mutations
apply_sharpe_mutations <- function(p)
{
  p <- p %>% 
    mutate(
      play_type=case_when(
        is.na(posteam) ~ "note",
        substr(desc,1,8) == "Timeout " ~ "note",
        desc == "*** play under review ***" ~ "note",
        TRUE ~ play_type
      ))
}

########## FUNCTIONS TO HELP TRANSFORM DATA OR MAKE PLOTS ##########

# double games
## takes input where each game has one row with teams as `away_team` and `home_team`
## returns with each game having two rows with teams listed as `team` and `opp`
double_games <- function(g)
{
  g1 <- g %>% 
    rename(team=away_team,team_score=away_score,
           opp=home_team,opp_score=home_score,
           team_rest=away_rest,opp_rest=home_rest,
           team_moneyline=away_moneyline,opp_moneyline=home_moneyline,
           team_spread_odds=away_spread_odds,opp_spread_odds=home_spread_odds,
           team_coach=away_coach,opp_coach=home_coach) %>% 
    mutate(location=ifelse(location == "Home","Away",location),
           result=-1*result,spread_line=-1*spread_line)
  g2 <- g %>% 
    rename(team=home_team,team_score=home_score,
           opp=away_team,opp_score=away_score,
           team_rest=home_rest,opp_rest=away_rest,
           team_moneyline=home_moneyline,opp_moneyline=away_moneyline,
           team_spread_odds=home_spread_odds,opp_spread_odds=away_spread_odds,           
           team_coach=home_coach,opp_coach=away_coach)
  g <- bind_rows(g1,g2) %>% 
    arrange(gameday,gametime,old_game_id,location)
  return(g)
}

# save plot
## takes input as a filename
## applies my default settings for the file saved
save_plot <- function(fn)
{
  ggsave(filename=fn,plot=last_plot(),dpi=500,units="in",width=12,height=9)
}

########## EARLY DATA ##########

team_colors <- silent_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/teamcolors.csv")
team_logos <- silent_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/logos.csv")
