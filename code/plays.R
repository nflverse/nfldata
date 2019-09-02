source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays-functions.R")

########## INPUTS ##########

# filename to store plays
plays_filename = "playdata" # replace this with file where you want to play data
baldwin_mutations <- TRUE   # do you want to apply Ben Baldwin's mutations?
series_data <- TRUE         # do you want to apply series data?

########## LOAD DATA ##########

# load game data
report("Loading game data")
games <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv")
games <- games %>%
  filter(season >= 2009 & !is.na(result)) %>% 
  mutate(game_id=as.character(game_id))

# load previous data
report("Loading existing plays data")
old_warning_level <- getOption("warn")
options(warn=-1)
tryCatch(plays <- readRDS(plays_filename),error=report_error)
options(warn=old_warning_level)

########## ADD MISSING GAMES ##########

# update plays
if (exists("plays"))
{
  # we have data! identify any missing games
  pulled_games <- plays %>% pull(game_id) %>% unique()
  missing <- games %>% filter(!(game_id %in% pulled_games)) %>% pull(game_id)
  
  # handle missing games
  if (length(missing) > 0)
  {
    # get new plays
    new_plays <- NULL
    for (g in missing)
    {
      report(paste0("Scraping from plays from game: ",g))
      game_plays <- scrape_json_play_by_play(g)
      game_plays <- game_plays %>%
        fix_inconsistent_data_types() %>% 
        fix_team_abbreviations()
      new_plays <- bind_rows(new_plays,game_plays)
    }
    
    # 
    report("Adding in game data for new plays")
    new_plays <- new_plays %>%
      apply_game_data()
    
    # additional optional modifications
    if (baldwin_mutations) new_plays <- apply_baldwin_mutations(new_plays)
    if (series_data) new_plays <- apply_series_data(new_plays)
    
    # finally merge things together
    report("Merging existing plays and new plays")
    plays <- bind_rows(plays,new_plays) %>% arrange(game_id,play_id)
    saveRDS(plays,plays_filename)
    rm(new_plays)  # no need for this to take up memory anymore
    
  }
  
########## FRESH DOWNLOAD ##########
  
} else {
  
  # no plays variable, so we're from scratch
  report("No play data found, loading plays from scratch")
  seasons <- games %>% filter(!is.na(result)) %>% pull(season) %>% unique()
  plays <- NULL
  
  # season loop
  for (s in seasons)
  {
    # regular season
    report(paste("Importing",s,"regular season"))
    reg <- read_csv(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_",s,".csv"))
    reg <- reg %>% fix_inconsistent_data_types()
    plays <- bind_rows(plays,reg)
    # playoffs
    report(paste("Importing",s,"playoffs"))
    post <- read_csv(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/post_season/post_pbp_",s,".csv"))
    post <- post %>% fix_inconsistent_data_types()
    plays <- bind_rows(plays,post)
  }
  saveRDS(plays,file=plays_filename)
  
  # remove these, no need to take up memory, it's all in plays now
  rm(reg)
  rm(post)
  
  # fix team abbreviations and merge with game data
  report("Merging play and game data")
  plays <- plays %>%
    fix_team_abbreviations() %>% 
    apply_game_data()
  
  # additional optional modifications
  if (baldwin_mutations) plays <- apply_baldwin_mutations(plays)
  if (series_data) plays <- apply_series_data(plays)
  
}

