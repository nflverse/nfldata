############# LIBRARIES ##############

library(tidyverse)
library(ggplot2)
library(ggimage)
library(glue)
library(nflscrapR)
library(mgcv)

############ HELPER DATA #############

team_colors <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/teamcolors.csv")
team_logos <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/logos.csv")

########## HELPER FUNCTIONS ##########

# report progress to console
report <- function(msg)
{
  print(paste0(Sys.time(),": ",msg))
}

# look for text in names of columns of data frame
grep_col <- function(x,df=plays)
{
  return(colnames(df)[grepl(x,colnames(df))])
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
    team_colors <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/teamcolors.csv")
  if (!exists("team_logos"))
    team_logos <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/logos.csv")
  
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

# fix team abbreviations
## by default this just makes every team have the same abbreviation all season
## use old_to_new=TRUE to make teams that have moved use the new abbreviation in the past
fix_team_abbreviations <- function(p,old_to_new=FALSE)
{
  for (col in grep_col("team",p))
  {
    x <- p %>% pull(col)
    if (typeof(x) == "character")
    {
      p[,col] <- case_when(
        x == "JAC" ~ "JAX",
        x == "LA" ~ "LAR",
        x == "SD" & old_to_new ~ "LAC",
        x == "STL" & old_to_new ~ "LAR",
        x == "OAK" & old_to_new ~ "LV",
        TRUE ~ x)
    }
  }
  return(p)
}

########## FUNCTIONS TO APPLY ADDITIONAL DATA ##########

# game data
apply_game_data <- function(p)
{
  if (!("alt_game_id" %in% colnames(p)))  # already included, don't reapply
  {
    report("Applying game data")
    if (!exists("games"))
      games <- read_csv("http://www.habitatring.com/games.csv")
    games <- games %>%
      mutate(game_id=as.character(game_id))
    p <- p %>% 
      fix_inconsistent_data_types() %>% 
      inner_join(games,by=c("game_id","away_team","home_team"))
  }
  return(p)
}

# apply mutations from Ben Baldwin (and some code from Keegan Abdoo)
## taken from https://github.com/guga31bb/nflstats/blob/master/helpers.R
apply_baldwin_mutations <- function(p)
{
  report("Applying Ben Baldwin mutations")
  p <- p %>% 
    mutate(
      # identify passes and rushes
      ## note this treats qb scrambles as passes since a pass was called
      pass=ifelse(str_detect(desc,"( pass)|(sacked)|(scramble)"),1,0),
      rush=ifelse(str_detect(desc,"(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0,1,0),
      # plays are defined as successful when EPA is positive
      success=ifelse(is.na(epa),NA,ifelse(epa>0,1,0)),
      # fix player name fields so they aren't NA on penalty plays
      ## code for this from Keenan Abdoo
      passer_player_name=ifelse(play_type == "no_play" & pass == 1, 
                                str_extract(desc,"(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                passer_player_name),
      receiver_player_name=ifelse(play_type == "no_play" & str_detect(desc," pass"), 
                                  str_extract(desc,"(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
      rusher_player_name=ifelse(play_type == "no_play" & rush == 1, 
                                str_extract(desc,"(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name),
      # this is shorthand so "name" is the QB (if pass) or runner (if run)
      name=ifelse(!is.na(passer_player_name),passer_player_name,rusher_player_name),
      # set yards_gained to be NA on penalties rather then 0
      yards_gained=ifelse(play_type == "no_play" | play_type == "note",NA,yards_gained),
      # easy filter: play is 1 if a "normal" play (including penalties), or 0 otherwise
      play=ifelse(!is.na(epa) & !is.na(posteam) & 
                    desc != "*** play under review ***" &
                    substr(desc,1,8) != "Timeout " &
                    play_type %in% c("no_play","pass","run"),1,0))
  
  # passer_ep(a)
  ## adjust epa for WR catches-and-fumbles as if tackled at spot of fumble
  wr_fumbled <- p %>% 
    filter(complete_pass == 1 & fumble_lost == 1 & !is.na(epa)) %>%
    select(desc, game_id, play_id, epa, posteam, half_seconds_remaining, yardline_100, down, ydstogo, yards_gained, goal_to_go, ep) %>%
    mutate(
      #save old stuff for testing/checking
      down_old=down,ydstogo_old=ydstogo,epa_old=epa,
      #update yard line, down, yards to go from play result
      yardline_100=yardline_100 - yards_gained,
      down=ifelse(yards_gained >= ydstogo,1,down+1),
      #if the fumble spot would have resulted in turnover on downs, need to give other team the ball and fix
      change = ifelse(down == 5,1,0),down=ifelse(down == 5,1,down),
      #yards to go is 10 if its a first down, update otherwise
      ydstogo = ifelse(down == 1,10,ydstogo - yards_gained), 
      #fix yards to go for goal line (eg can't have 1st & 10 inside opponent 10 yard line)
      ydstogo = ifelse(yardline_100 < ydstogo,yardline_100,ydstogo), 
      #10 yards to go if possession change
      ydstogo = ifelse(change == 1,10,ydstogo),
      #flip field for possession change
      yardline_100 = ifelse(change == 1,100 - yardline_100,yardline_100),
      goal_to_go = ifelse(yardline_100 == ydstogo,1,0),
      ep_old = ep) %>% 
    select(-ep,-epa)
  # were there any WR fumbles?
  if (nrow(wr_fumbled) > 0)
  {
    # calculate epa as if reciever did not fumble
    wr_fumbled_ep <- calculate_expected_points(wr_fumbled,
                                    "half_seconds_remaining", "yardline_100", 
                                    "down", "ydstogo", "goal_to_go") %>%
      mutate(passer_ep=ifelse(change == 1,-ep,ep),passer_epa=ep-ep_old) %>%
      select(game_id,play_id,passer_epa)
    # add to p
    p <- p %>%
      left_join(wr_fumbled_ep,by=c("game_id","play_id")) %>% 
      mutate(passer_epa=ifelse(!is.na(passer_epa),passer_epa,
                               ifelse(!is.na(epa) & pass == 1,epa,NA)))
    if ("passer_epa.y" %in% colnames(p))
    {
      p <- p %>% 
        rename(passer_epa=passer_epa.y) %>% 
        select(-passer_epa.x)
    }
  } else {
    p$passer_epa <- p$epa
  }
  
  # return p
  return(p)
}

# apply Lee Sharpe mutations
apply_sharpe_mutations <- function(p)
{
  report("Applying Lee Sharpe mutations")
  p <- p %>% 
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

# apply completion probability
## formula from Ben Baldwin
apply_completion_probability <- function(p,all_plays) 
{
  report("Applying completion probability")
  
  # sort p and create cp column
  p$cp <- NA
  
  # season loop
  ## since our data only goes back to 2007, no CP for 2007
  seasons <- unique(p$season[p$season > 2007])
  for (s in seasons)
  {
    # get data from previous three seasons
    old_data <- all_plays %>%
      filter(play == 1 & season >= s-3 & season <= s-1) %>% 
      filter(complete_pass == 1 | incomplete_pass == 1 | interception == 1) %>% 
      filter(air_yards >= -10 & !is.na(receiver_player_id) & !is.na(pass_location)) %>% 
      mutate(air_is_zero=ifelse(air_yards,0,1))
    
    # determine CPOE formula
    cp_model <- gam(complete_pass ~ s(air_yards) + air_is_zero + factor(pass_location),
                      data=old_data,method="REML")
    
    # apply CPOE to current season
    new_data <- p %>%
      filter(play == 1 & season == s) %>% 
      filter(complete_pass == 1 | incomplete_pass == 1 | interception == 1) %>% 
      filter(air_yards >= -10 & !is.na(receiver_player_id) & !is.na(pass_location)) %>% 
      mutate(air_is_zero=ifelse(air_yards,0,1))
    new_data$cp <- predict.gam(cp_model,new_data)
    new_data <- new_data %>% 
      select(game_id,play_id,cp)
    
    # merge into p
    p <- p %>%
      left_join(new_data,by=c("game_id","play_id")) %>% 
        mutate(cp=ifelse(!is.na(cp.y),cp.y,cp.x)) %>% 
        select(-cp.x,-cp.y)
  }
  return(p)
}

# apply series data
## series = 
##  starts at 1, each new first down increments, numbers shared across both teams
##  NA: kickoffs, extra point/two point conversion attempts, non-plays, no posteam
##  Note: Also is broken for 2013 @ CLE gamesin Week 12 & 13 games due to nflscrapR data issues
## series_success =
##  1: scored touchdown, gained enough yards for first down
##  0: punt, interception, fumble lost, turnover on downs, 4th down FG attempt or punt
##  NA: series is NA, series contains QB spike/kneel, half ended with none of above
apply_series_data <- function(p)
{
  report("Applying series and series success")
  
  # identify broken games
  broken_games <- unique(p$game_id[is.na(p$yards_gained)
                    & !(is.na(p$play_type) |
                          p$play_type %in% c("note","no_play"))])
  
  # add in series and series_success variables
  p <- p %>% mutate(series=NA,series_success=0)
  
  # initialize loop trackers
  p$series[min(which(p$play_type != "kickoff"))] <- 1
  p$series_success[1:(min(which(p$play_type != "kickoff"))-1)] <- NA
  series <- 1
  lb <- 1
  
  # play loop
  for (r in (min(which(p$play_type != "kickoff"))+1):nrow(p))
  {
    
    # progress report
    if (r %% 10000 == 0) 
    {
      report(paste("Series Data:",r,"of",nrow(p),"plays"))
      saveRDS(p,file=plays_filename)
    }
    
    # skip broken games or no-description plays
    if (p$game_id[r] %in% broken_games || is.na(p$desc[r]))
    {
      lb <- lb + 1
      next
    }
    
    # if posteam is not defined or is a timeout, mark as a non-series and skip
    if (p$play_type[r] == "note")
    {
      p$series[r] <- NA
      p$series_success[r] <- NA
      lb <- lb + 1
      next
    }
    
    # game has changed
    if (p$game_id[r] != p$game_id[r-lb]) 
    {
      if (p$yards_gained[r-lb] >= p$ydstogo[r-lb])
      {
        p$series_success[p$game_id == p$game_id[r-lb] & p$series == series] <- 1
      } else if (any(p$play_type[p$game_id == p$game_id[r] & p$series == series]
                     %in% c("qb_kneel","qb_spike"))) {
        p$series_success[p$game_id == p$game_id[r] & p$series == series] <- NA        
      } else if (p$down[r-lb] == 4) {
        p$series_success[p$game_id == p$game_id[r-lb] & p$series == series] <- 0
      } else {
        p$series_success[p$game_id == p$game_id[r-lb] & p$series == series] <- NA
      }
      series <- 1
    # beginning of 2nd half or overtime
    } else if (p$qtr[r] != p$qtr[r-lb] && (p$qtr[r] == 3 || p$qtr[r] >= 5)) {
      if (p$yards_gained[r-lb] >= p$ydstogo[r-lb])
      {
        p$series_success[p$game_id == p$game_id[r] & p$series == series] <- 1
      } else if (any(p$play_type[p$game_id == p$game_id[r] & p$series == series]
                     %in% c("qb_kneel","qb_spike"))) {
        p$series_success[p$game_id == p$game_id[r] & p$series == series] <- NA
      } else if (p$down[r-lb] == 4) {
        p$series_success[p$game_id == p$game_id[r-lb] & p$series == series] <- 0        
      } else {
        p$series_success[p$game_id == p$game_id[r] & p$series == series] <- NA
      }
      series <- series + 1
    # or drive has changed  
    } else if (p$drive[r] != p$drive[r-lb]) {
      if (p$yards_gained[r-lb] >= p$ydstogo[r-lb])
      {
        p$series_success[p$game_id == p$game_id[r] & p$series == series] <- 1
      } else if (any(p$play_type[p$game_id == p$game_id[r] & p$series == series]
                   %in% c("qb_kneel","qb_spike"))) {
        p$series_success[p$game_id == p$game_id[r] & p$series == series] <- NA
      }
      series <- series + 1
    # first down or NA down with last play having enough yards or defensive penalty
    } else if ((is.na(p$down[r]) || p$down[r] == 1) &&
              ((!is.na(p$yards_gained[r-lb]) && p$yards_gained[r-lb] >= p$ydstogo[r-lb])
               || any(p$first_down_penalty[(r-lb):(r-1)] == 1,na.rm=TRUE))) {
      if (p$play_type[r-lb] != "kickoff" ||
          any(p$first_down_penalty[(r-lb):(r-1)] == 1,na.rm=TRUE))
      {
        p$series_success[p$game_id == p$game_id[r] & p$series == series] <- 1
      }
      series <- series + 1
    # blocked field goal the same team recovered
    } else if (!is.na(p$down[r]) && p$down[r] == 1 &&
               !is.na(p$field_goal_result[r-lb]) &&
               p$field_goal_result[r-lb] == "blocked" &&
               p$posteam[r-lb] == p$posteam[r]) {
      p$series_success[p$game_id == p$game_id[r] & p$series == series] <- 0
      series <- series + 1
    }
    
    # mark series for kickoffs as NA
    if (!is.na(p$play_type[r]) && p$play_type[r] == "kickoff")
    {
      p$series_success[r] <- NA
      series <- series - 1  # otherwise it would advance 2, want to advance 1
    } else if ((!is.na(p$play_type[r]) && p$play_type[r] == "extra_point") ||
               (!is.na(p$two_point_attempt[r]) && p$two_point_attempt[r] == 1)) {
      p$series_success[r] <- NA
      series <- series - 1  # otherwise it would advance 2, want to advance 1
    # mark series for all other p
    } else {
      p$series[r] <- series
    }
    
    # if this is a real play, reset lookback to 1, otherwise increment it
    ## the looback defines the "previous" play
    ## we want to skip this for p that don't actually affect series data
    if (is.na(p$play_type[r]) ||
        p$play_type[r] %in% c("no_play","extra_point","note") ||
        (!is.na(p$two_point_attempt[r]) && p$two_point_attempt[r] == 1))
    {
      lb <- lb + 1
    } else {
      lb <- 1
    }
    
  }
  
  # handle final series in the data
  lb <- 0
  while(is.na(p$play_type[nrow(p)-lb]) ||
        p$play_type[nrow(p)-lb] %in% c("no_play","note"))
  {
    lb <- lb + 1
  }
  if (p$yards_gained[nrow(p)-lb] >= p$ydstogo[nrow(p)-lb])
  {
    p$series_success[p$game_id == p$game_id[nrow(p)-lb] & p$series == series] <- 1
  } else if (any(p$play_type[p$game_id == p$game_id[r] & p$series == series]
                 %in% c("qb_kneel","qb_spike"))) {
    p$series_success[p$game_id == p$game_id[r] & p$series == series] <- NA    
  } else if (p$down[nrow(p)-lb] == 4) {
    p$series_success[p$game_id == p$game_id[nrow(p)-lb] & p$series == series] <- 0
  } else {
    p$series_success[p$game_id == p$game_id[nrow(p)] & p$series == series] <- NA
  }
  
  report(paste("Series Data Complete!"))
  return(p)
}

########## FUNCTIONS TO HELP TRANSFORM DATA OR MAKE PLOTS ##########

# get data from over the cap
fetch_tankathon_draft_order <- function()
{
  teams <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/teams.csv")
  otc_raw <- readLines("https://overthecap.com/salary-cap-space/")
  otc_text <- otc_raw[56]
  otc_text <- gsub('</tr></thead><tbody><tr class="sortable">',"\n",otc_text)
  otc_text <- gsub('</tr><tr class="sortable">',"\n",otc_text)
  otc_text <- gsub("<[^>]+>","%",otc_text)
  otc_text <- gsub("%+","%",otc_text)
  otc_text <- gsub(",","",otc_text)
  otc_text <- gsub("\\$","",otc_text)
  otc_text <- gsub("\\(","-",otc_text)
  otc_text <- gsub(")","",otc_text)
  otc_split <- str_split(otc_text,"\n")[[1]][2:33]
  otc_split <- gsub("^%","",otc_split)
  otc_split <- gsub("%$","",otc_split)
  cap <- as_tibble(str_split_fixed(otc_split,"%",6)) %>% 
    mutate(V2=as.numeric(V2),V3=as.numeric(V3),V4=as.numeric(V4),
           V5=as.numeric(V5),V6=as.numeric(V6)) %>% 
    rename(team=V1,cap_space_actual=V2,cap_space=V3,
           num_players=V4,active_cap_spend=V5,dead_money=V6)
  result <- teams %>% 
    filter(season == max(season)) %>% 
    select(team,nickname) %>% 
    inner_join(cap,by=c("nickname"="team")) %>% 
    select(-nickname)
  return(result)
}

# get data from over the cap
fetch_otc_cap_data <- function()
{
  teams <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/teams.csv")
  otc_raw <- readLines("https://overthecap.com/salary-cap-space/")
  otc_text <- paste(otc_raw[51:54],collapse="")
  otc_text <- gsub('</tr></thead><tbody><tr class="sortable">',"\n",otc_text)
  otc_text <- gsub('</tr><tr class="sortable">',"\n",otc_text)
  otc_text <- gsub("<[^>]+>","%",otc_text)
  otc_text <- gsub("%+","%",otc_text)
  otc_text <- gsub(",","",otc_text)
  otc_text <- gsub("\\$","",otc_text)
  otc_text <- gsub("\\(","-",otc_text)
  otc_text <- gsub(")","",otc_text)
  otc_split <- str_split(otc_text,"\n")[[1]][2:33]
  otc_split <- gsub("^%","",otc_split)
  otc_split <- gsub("%$","",otc_split)
  cap <- as_tibble(str_split_fixed(otc_split,"%",6)) %>% 
    mutate(V2=as.numeric(V2),V3=as.numeric(V3),V4=as.numeric(V4),
           V5=as.numeric(V5),V6=as.numeric(V6)) %>% 
    rename(team=V1,cap_space_actual=V2,cap_space=V3,
           num_players=V4,active_cap_spend=V5,dead_money=V6)
  result <- teams %>% 
    filter(season == max(season)) %>% 
    select(team,nickname) %>% 
    inner_join(cap,by=c("nickname"="team")) %>% 
    select(-nickname)
  return(result)
}

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