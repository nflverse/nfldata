# libraries
library(tidyverse)
library(glue)

# load sharpe data
load_sharpe_data <- function(file_name) {
  url <- glue("https://raw.githubusercontent.com/nflverse/nfldata/master/data/{file_name}.csv")
  suppressWarnings({ df <- read_csv(url, col_types = cols()) })
  return(df)
}

# teams data
teams <- load_sharpe_data("teams") %>% 
  filter(season == max(season)) %>% 
  select(team, full, draft_kings)

# games data
games <- load_sharpe_data("games") %>%
  filter(is.na(result))

# standings data
standings <- load_sharpe_data("standings") %>%
  filter(season == max(season)) %>% 
  select(team, conf, division)

# draft kings game labels
dk_games <- games %>% 
  inner_join(teams, by = c("away_team" = "team")) %>% 
  rename(away_full = full) %>%
  inner_join(teams, by = c("home_team" = "team")) %>% 
  rename(home_full = full) %>%
  mutate(dk_label = glue("{away_full} @ {home_full}")) %>% 
  select(game_id, dk_label)

# draft kings query
response <- "https://sportsbook.draftkings.com/leagues/football/3" %>% 
  utils::URLdecode() %>% 
  httr::GET() %>%
  httr::content(as = "text", encoding = "UTF-8") %>% 
  stringi::stri_trans_general("latin-ascii") %>% 
  stringi::stri_split_lines1()

# extract betting json
bets_str <- response[nchar(response) == max(nchar(response))]
start_char <- str_locate(bets_str, "\\{")
dk_bets <- substr(bets_str, start_char, nchar(bets_str)-1) %>% 
  jsonlite::fromJSON(flatten=TRUE)

# collecting event names
dk_events <- tibble(
  event_id=names(dk_bets$eventGroups$`3`$events),
  about=NA
)
for (r in 1:nrow(dk_events)) {
  dk_events$about[r] <- dk_bets$eventGroups$`3`$events[[dk_events$event_id[r]]]$name
}

# collect lines
dk_lines <- NULL
for (nm in names(dk_bets$offers$`3`)) {
  
  # if offers available for this event
  if (!is.null(colnames(dk_bets$offers$`3`[[nm]]$outcomes)) &&
      !is.null(dk_bets$offers$`3`[[nm]]$label) &&
      "label" %in% colnames(dk_bets$offers$`3`[[nm]]$outcomes)) {
    
    # break offers into lines
    lines <- dk_bets$offers$`3`[[nm]]$outcomes %>% 
      as_tibble() %>% 
      rename(side = label, odds = oddsAmerican) %>% 
      mutate(id = nm,
             type = dk_bets$offers$`3`[[nm]]$label,
             event_id = as.character(dk_bets$offers$`3`[[nm]]$providerEventId),
             odds = as.numeric(odds)) %>% 
      inner_join(dk_events,by="event_id")
    
    # add to dk_lines
    dk_lines <- bind_rows(dk_lines,lines)
    
  }
}

# draft kings lines organizing
dk_lines <- dk_lines %>% 
  select(type, about, side, line, odds) %>% 
  mutate(
    type = case_when(
      type == "NFL Championship Winner" ~ "SUPERBOWL",
      type == "Conference Winner" ~ "CONFERENCE",
      type == "Division Winner" ~ "DIVISION",
      type == "Number of Games won in the Regular Season" ~ "WINS",
      type == "To make the playoffs" ~ "PLAYOFFS",
      type == "Division Finishing Position" ~ "DIV_PLACE",
      type == "Week 1 Starting Quarterback for the Team" ~ "START_QB",
      type == "Point Spread" ~ "SPREAD",
      type == "Moneyline" ~ "MONEYLINE",
      type == "Total Points" ~ "TOTAL",
      type == "MVP Award Winner" ~ "MVP",
      type == "Offensive Player of the Year Winner" ~ "OFF_PLAYER",
      type == "Defensive Player of the Year Winner" ~ "DEF_PLAYER",
      type == "Offensive Rookie of the Year Winner" ~ "OFF_ROOKIE",
      type == "Defensive Rookie of the Year Winner" ~ "DEF_ROOKIE",
      type == "Comeback Player of the Year" ~ "COMEBACK",
      type == "Coach of the Year" ~ "COACH",
      type == "Player's Total Passing Yards - Regular Season" ~ "YDS_PASS",
      type == "Player's Total Rushing Yards - Regular Season" ~ "YDS_RUSH",
      type == "Player's Total Receiving Yards - Regular Season" ~ "YDS_REC",
      type == "Player Total Rushing & Receiving Yards - Excluding Preseason & Playoffs" ~ "YDS_TOTAL",
      type == "Player's Total Passing Touchdowns - Regular Season" ~ "TDS_PASS",
      type == "Player's Total Rushing Touchdowns - Regular Season" ~ "TDS_RUSH",
      type == "Player's Total Receiving Touchdowns - Regular Season" ~ "TDS_REC",    
      type == "Player's Total Sacks - Regular Season" ~ "SACKS",
      type == "Team to have a 17-0 record" ~ "REG_ALL_WINS",
      type == "Team to have an 0-17 record" ~ "REG_ALL_LOSSES",
      type == "Team to win Least Games in the Regular Season" ~ "LEAST_WINS",
      type == "Team to win Most Games in the Regular Season" ~ "MOST_WINS",
      type == "Winning Division" ~ "SB_WIN_DIV",
      type == "State of NFL Championship Winner" ~ "SB_WIN_STATE",
      TRUE ~ type
    ),
    line = case_when(
      substr(side, 1, 5) == "Over " ~ as.numeric(substr(side, 6, 1000)),
      substr(side, 1, 6) == "Under " ~ as.numeric(substr(side, 7, 1000)),
      TRUE ~ line
    ),
    side = case_when(
      substr(side, 1, 5) == "Over " ~ "Over",
      substr(side, 1, 6) == "Under " ~ "Under",
      type == "SB_WIN_STATE" ~ substr(side, 1, 2),
      TRUE ~ side
    ),
    line = ifelse(type %in% 
      c("SACKS", "SPREAD", "TDS_PASS", "TDS_REC", "TOTAL", "WINS",
        "YDS_PASS", "YDS_REC", "YDS_RUSH", "YDS_TOTAL"), 
      line, NA),
    about = str_replace(about, " \\d{4}\\/\\d{4}", ""),
    about = str_replace(about, " Specials", "")
  ) %>% 
  left_join(teams, by = c("about" = "full")) %>% 
  mutate(about = ifelse(!is.na(team), team, about)) %>% 
  select(-team, -draft_kings) %>% 
  left_join(teams, by = c("side" = "draft_kings")) %>% 
  mutate(side = ifelse(!is.na(team), team, side)) %>% 
  select(-team, -full) %>% 
  left_join(dk_games, by = c("about" = "dk_label")) %>% 
  mutate(about = ifelse(!is.na(game_id), game_id, about)) %>% 
  select(-game_id) %>% 
  mutate(about = case_when(
    type == "CONFERENCE" ~ substr(about,1,3),
    type %in% c("SUPERBOWL", "MVP", "OFF_PLAYER", "DEF_PLAYER",
                "OFF_ROOKIE","DEF_ROOKIE", "COMEBACK", "COACH",
                "LEAST_WINS", "MOST_WINS", "SB_WIN_DIV", "SB_WIN_STATE")
      ~ NA_character_,
    TRUE ~ about
  ))