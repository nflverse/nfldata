source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays-functions.R")

########## INPUTS ##########

# filename to store plays
plays_filename = "playdata" # replace this with file where you want to play data
baldwin_mutations <- TRUE   # do you want to apply Ben Baldwin's mutations?
sharpe_mutations <- TRUE    # do you want to apply Lee Sharpe's mutations?
name_fixes <- TRUE          # do you want to apply name fixes?
comp_prob <- TRUE           # do you want to apply completion probability?
series_data <- TRUE         # do you want to apply series data?

## NOTE: you must set baldwin_mutations to TRUE to get comp_prob

########## LOAD EXISTING DATA ##########

# load game data
report("Loading game data")
games <- read_csv("http://www.habitatring.com/games.csv")
games <- games %>%
  filter(season >= 2009 & !is.na(result)) %>% 
  mutate(game_id=as.character(game_id))

# load previous data
report("Loading existing plays data")
old_warning_level <- getOption("warn")
options(warn=-1)
tryCatch(plays <- readRDS(plays_filename),error=report)
options(warn=old_warning_level)

########## FRESH DOWNLOAD OF COMPLETED SEASONS ##########

if (!exists("plays"))
{
  
  # no plays variable, so we're from scratch
  report("No play data found, loading plays from scratch")
  seasons <- games %>%
    group_by(season) %>% 
    summarize(count=n()) %>% 
    ungroup() %>% 
    filter(count == 267) %>% 
    pull(season)
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
  if (sharpe_mutations) plays <- apply_sharpe_mutations(plays)
  if (name_fixes) plays <- apply_name_fixes(plays)
  if (baldwin_mutations & comp_prob)
    plays <- apply_completion_probability(plays,plays)
  if (series_data) plays <- apply_series_data(plays)
}

########## INITIAL COMP PROBABILITY ##########

# temporarily here to help people who want to add cp to existing data
if (exists("plays") & baldwin_mutations & comp_prob &
    !("cp" %in% colnames(plays)))
{
  plays <- apply_completion_probability(plays)
  saveRDS(plays,plays_filename)
}

########## SCRAPE GAMES IN CURRENT SEASON ##########

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
    # scrape
    report(paste0("Scraping plays from game: ",g))
    game_plays <- scrape_json_play_by_play(g)
    
    # add in basic data
    new_plays <- game_plays %>%
      fix_inconsistent_data_types() %>% 
      fix_team_abbreviations() %>% 
      apply_game_data()
  
    # additional optional modifications
    if (baldwin_mutations) new_plays <- apply_baldwin_mutations(new_plays)
    if (sharpe_mutations) new_plays <- apply_sharpe_mutations(new_plays)
    if (name_fixes) new_plays <- apply_name_fixes(new_plays)
    if (baldwin_mutations & comp_prob)
      new_plays <- apply_completion_probability(new_plays,plays)
    if (series_data) new_plays <- apply_series_data(new_plays)
    
    # finally merge things together
    report("Merging existing plays and new plays")
    plays <- bind_rows(plays,new_plays) %>% arrange(game_id,play_id)
    
    # save after each game
    saveRDS(plays,plays_filename)
  }
  
  # no need for this to take up memory anymore
  rm(game_plays)
  rm(new_plays)
}
