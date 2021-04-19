library(tidyverse)
library(glue)
library(gt)
library(webshot)

# load data
trades <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/trades.csv")
draft_values <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/draft_values.csv")
draft_picks <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/draft_picks.csv")

# identify pick trades
pick_trades <- trades %>% 
  # filter to trades for picks only
  group_by(trade_id, season, trade_date) %>% 
  filter(all(!is.na(pick_season))) %>%   
  ungroup() %>% 
  # filter out conditional picks that never got made
  filter(!is.na(pick_number) | pick_season >= max(season)) %>% 
  # assume unknown-placed picks in middle of the round
  mutate(pick_number =
           ifelse(!is.na(pick_number), pick_number, 32*(pick_round-1) + 16)) %>% 
  # identify pick values
  left_join(draft_values, by = c("pick_number" = "pick")) %>% 
  arrange(trade_date, trade_id, pick_season, pick_round, pick_number) %>%
  group_by(trade_id, season, trade_date) %>% 
  # identify the top pick involved in the trade
  mutate(top_pick = (row_number() == 1)) %>% 
  # discount pick value for future years
  mutate_at(vars(stuart, johnson, hill, otc), ~case_when(
    pick_season == season ~ .x * 1,
    min(pick_season) - season == 1 & pick_season - season == 1 ~ .x * 0.9,
    min(pick_season) - season == 1 & pick_season - season == 2 ~ .x * 0.8,
    min(pick_season) - season == 0 & pick_season - season == 1 ~ .x * 0.8,
    min(pick_season) - season == 0 & pick_season - season == 2 ~ .x * 0.7,
    TRUE ~ 100000
  )) %>%   
  rename(
    trade_up = received,
    trade_down = gave,
    player = pfr_name
  ) %>% 
  # pick values are negative if received by team trading up
  mutate_at(vars(stuart, johnson, hill, otc), ~case_when(
    trade_up == max(ifelse(top_pick, trade_up, "")) ~ .x * -1,
    TRUE ~ .x
  )) %>% 
  # how to represent the pick as a string
  mutate(note = case_when(
    pick_season > season ~ glue("+{pick_season-season} R{pick_round}"),
    TRUE ~ glue("{pick_number}")
  )) %>% 
  # actual player drafted in the pick
  left_join(draft_picks, by = c(
    "pick_season" = "season",
    "pick_round" = "round",
    "pick_number" = "pick",
    "pfr_id" = "pfr_id"
  )) %>% 
  # reduce to one line per trade
  summarize(
    trade_up = max(ifelse(top_pick, trade_up, "")),
    trade_down = max(ifelse(top_pick, trade_down, "")),
    for_qb = (max(ifelse(top_pick, position, "")) == "QB"),
    picks_up = paste(note[otc < 0], collapse = ","),
    picks_down = paste(note[otc > 0], collapse = ","),
    player = max(ifelse(top_pick, player, "")),
    johnson = sum(ifelse(johnson > 0, johnson, 0)) / -sum(ifelse(johnson < 0, johnson, 0)),    
    stuart = sum(ifelse(stuart > 0, stuart, 0)) / -sum(ifelse(stuart < 0, stuart, 0)),
    hill = sum(ifelse(hill > 0, hill, 0)) / -sum(ifelse(hill < 0, hill, 0)),
    otc = sum(ifelse(otc > 0, otc, 0)) / -sum(ifelse(otc < 0, otc, 0)),
    earliest = max(ifelse(top_pick, pick_number, 0))
  ) %>% 
  ungroup() %>% 
  # special override for 2021 SF/MIA trade since it hasn't happened yet
  mutate(for_qb = ifelse(trade_id == 1552, TRUE, for_qb)) %>% 
  # mark players yet to be drafted as TBD
  mutate(player = ifelse(is.na(player), "[TBD]", player))


# create table to display
featured_trades <- pick_trades %>% 
  # only trade ups for a QB
  filter(for_qb) %>% 
  # only trade ups earlier than this pick
  filter(earliest <= 32) %>% 
  select(player, season, trade_up, trade_down, picks_up,
         picks_down, johnson, stuart, otc) %>% 
  arrange(desc(otc)) %>% 
  # create table
  gt(rowname_col = "player") %>% 
  # format table
  fmt_percent(vars(johnson, stuart, otc), decimals = 0) %>% 
  data_color(
    autocolor_text = FALSE,
    columns = vars(johnson, stuart, otc),
    colors = scales::col_numeric(
      palette = c("blue","white","red"),
      domain = c(-0.5, 2.5)
    )
  ) %>% 
  tab_header(
    title = "Round 1 QB Trade Ups Since 2002 Ranked by Lopsidedness",
    subtitle = "Value Given Up Relative to Value Acquired (100% = Even Trade)"
  ) %>% 
  opt_table_font(font = google_font("Roboto")) %>% 
  cols_label(
    season = "Season",
    trade_up = "Picked By",
    trade_down = "Traded With",
    picks_up = "Received",
    picks_down = "Sent",
    johnson = "JJ",
    stuart = "AV",
    otc = "OTC"
  ) %>% 
  tab_source_note(
    "Trade data from @LeeSharpeNFL and PFR, AV chart from Chase Stuart, OTC chart from Jason Fitzgerald and Brad Spielberger"
  )

# output trades
print(featured_trades)
gtsave(featured_trades,"featured_trades.png")
