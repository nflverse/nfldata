source("~/nfl/plays-functions.R")

########## INPUTS ##########

# filename to store plays
pbp_filename = "pbp.rds"    # replace this with file where you want to play data
sharpe_mutations <- TRUE    # do you want to apply Lee Sharpe's mutations?
name_fixes <- TRUE          # do you want to apply name fixes?
add_positions <- TRUE       # do you want to add columns with player's positions?

## NOTE: you must set baldwin_mutations to TRUE to get comp_prob

########## LOAD EXISTING DATA ##########

# load game data
report("Loading game data")
games <- silent_csv("http://www.habitatring.com/games.csv") %>%
  mutate(game_id=as.character(game_id)) %>% 
  filter(season >= 2000 & !is.na(result))

# load previous data
report("Loading existing plays data")
tryCatch(pbp <- suppressWarnings(readRDS(pbp_filename)),error=
           function(e) { report("No existing pbp data found") })

# get all games from scratch
if (!exists("pbp"))
{
  pbp <- NULL
  rosters <- NULL
  for (s in unique(games$season))
  {
    report(glue("Downloading play by play from {s} season"))
    season_pbp <- readRDS(url(glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{s}.rds"))) %>% 
      fix_inconsistent_data_types() %>% 
      provided(add_positions && s >= 2002,apply_positions(fetch_rosters(s)))
    pbp <- pbp %>% 
      bind_rows(season_pbp)
  }
  report("Cleaning new data")
  pbp <- pbp %>% 
    clean_pbp() %>% 
    fix_fumbles() %>% 
    provided(sharpe_mutations,apply_sharpe_mutations) %>% 
    provided(name_fixes,apply_name_fixes)
  saveRDS(pbp,file=pbp_filename)
  rm(season_pbp)
# get only what you are missing
} else {
  pulled_games <- pbp %>% 
    pull(game_id) %>%
    unique()
  missing_games <- games %>% 
    filter(!(game_id %in% pulled_games)) %>% 
    pull(game_id)
  if (length(missing_games) > 0)
  {
    report(paste("Scraping missing games:",paste(missing_games,collapse=", ")))
    missing_pbp <- fast_scraper(missing_games,pp=(length(missing_games) > 1))
    report("Cleaning new data")
    missing_pbp <- missing_pbp %>% 
      fix_inconsistent_data_types() %>% 
      clean_pbp() %>% 
      fix_fumbles() %>% 
      provided(sharpe_mutations,apply_sharpe_mutations) %>% 
      provided(name_fixes,apply_name_fixes) %>% 
      provided(add_positions,apply_positions(fetch_rosters(max(games$season))))
    pbp <- pbp %>% 
      bind_rows(missing_pbp)
    saveRDS(pbp,file=pbp_filename)
    rm(missing_pbp)
  } else {
    report("No missing games")
  }
}