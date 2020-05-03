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

# fix inconsistent data types
fix_inconsistent_data_types <- function(p)
{
  p <- p %>% 
    mutate(game_id=as.character(game_id),
           play_id=as.numeric(play_id),
           time=substr(as.character(time),1,5),
           down=as.numeric(down),
           air_yards=as.numeric(air_yards),
           yards_after_catch=as.numeric(yards_after_catch),
           comp_air_epa=as.numeric(comp_air_epa),
           comp_yac_epa=as.numeric(comp_yac_epa),
           air_wpa=as.numeric(air_wpa),
           yac_wpa=as.numeric(yac_wpa),
           blocked_player_id=as.character(blocked_player_id),
           fumble_recovery_2_yards=as.numeric(fumble_recovery_2_yards),
           fumble_recovery_2_player_id=as.character(fumble_recovery_2_player_id),
           forced_fumble_player_2_player_id=as.character(forced_fumble_player_2_player_id),
           tackle_for_loss_2_player_id=as.character(forced_fumble_player_2_player_id))
  return(p)
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

# game data
apply_game_data <- function(p)
{
  if (!("alt_game_id" %in% colnames(p)))  # already included, don't reapply
  {
    report("Applying game data")
    if (!exists("games"))
      games <- silent_csv("http://www.habitatring.com/games.csv") %>% 
        mutate(game_id=as.character(game_id)) %>% 
    p <- p %>% 
      fix_inconsistent_data_types() %>% 
      inner_join(games,by=c("game_id","season","week","game_type","away_team","home_team"))
  }
  return(p)
}

# apply Lee Sharpe mutations
apply_sharpe_mutations <- function(p)
{
  report("Applying Lee Sharpe mutations")
  p <- p %>% 
    apply_game_data() %>% 
    mutate(
      play_type=case_when(
        is.na(posteam) ~ "note",
        substr(desc,1,8) == "Timeout " ~ "note",
        desc == "*** play under review ***" ~ "note",
        TRUE ~ play_type
      ),
      special=ifelse(play_type %in% 
                       c("extra_point","field_goal","kickoff","punt"),1,0))
}

# apply name fixes
apply_name_fixes <- function(p)
{
  report("Applying name fixes")
  p <- p %>% mutate(
    name=case_when(
      name == "Ryan" ~ "M.Ryan",
      name == "Matt.Moore" ~ "M.Moore",
      name == "Alex Smith" ~ "A.Smith",
      name == "R.Griffin III" ~ "R.Griffin",
      name == "Jos.Allen" ~ "J.Allen",
      name == "G.Minshew II" ~ "G.Minshew",
      TRUE ~ name
    ),
    passer_player_name=ifelse(!is.na(passer_player_name),name,NA),
    rusher_player_name=ifelse(!is.na(rusher_player_name),name,NA),
    receiver_player_name=case_when(
      receiver_player_name == "D.Chark Jr." ~ "D.Chark",
      TRUE ~ receiver_player_name
    )
  )
  return(p)
}

# update rosters
fetch_rosters <- function(s)
{
  report(glue("Scraping roster data from {s} season"))
  
  teams <- silent_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/teams.csv") %>% 
    filter(season == s) %>% 
    pull(nfl_team_id)
  
  rosters <- fast_scraper_roster(teams,s) %>% 
    as_tibble() %>% 
    clean_names()
  colnames(rosters) <- colnames(rosters) %>% 
    str_replace("team_","") %>% 
    str_replace("players_","")
  rosters <- rosters %>% 
    mutate(position=ifelse(position == "SAF","S",position),
           height=12*as.integer(substr(height,1,1))+as.integer(substr(height,3,4)),
           birth_date=paste(substr(birth_date,7,10),
                            substr(birth_date,1,2),
                            substr(birth_date,4,5),
                            sep="-"),
           birth_date=as.Date(birth_date)) %>% 
    rename(team=abbr,
           id=gsis_id,
           jersey=jersey_number,
           name=display_name,
           headshot=headshot_url) %>% 
    arrange(team,jersey,last_name,first_name) %>% 
    select(season,team,id,position,jersey,name,birth_date,height,weight,headshot)
  return(rosters)
}

# apply positions
apply_positions <- function(p,r)
{
  # trim roster info down
  r <- r %>%
    filter(!is.na(id)) %>% 
    select(id,position) %>% 
    group_by(id) %>% 
    slice(1) %>% 
    ungroup()
  
  # id columns
  id_cols <- grep_col("_player_id",p)
  
  # add new columns
  for (col in id_cols)
  {
    p <- p %>% 
      left_join(r,by=setNames("id",col))
    colnames(p)[ncol(p)] <- str_replace(col,"_player_id","_player_position")
  }
    
  return(p)
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
           team_coach=away_coach,opp_coach=home_coach) %>% 
    mutate(location=ifelse(location == "Home","Away",location),
           result=-1*result,spread_line=-1*result)
  g2 <- g %>% 
    rename(team=home_team,team_score=home_score,
           opp=away_team,opp_score=away_score,
           team_coach=home_coach,opp_coach=away_coach)
  g <- bind_rows(g1,g2) %>% 
    arrange(gameday,gametime,game_id,location)
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