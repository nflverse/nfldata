get_draft_pick_data <- function()
{
  # scrape tankathon for draft pick info
  teams <- silent_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/teams.csv") %>% 
    filter(season == max(season)) %>% 
    select(team,short_location)
  tank_raw <- silent_readLines("http://www.tankathon.com/nfl/full_draft")
  tank_text <- tank_raw[4]
  tank_text <- gsub('.*<h1 class="page-title">Full \\d+ NFL Draft Order</h1>','',tank_text)
  tank_text <- gsub('<div class="footer-wrapper">.*','',tank_text)
  tank_text <- gsub('> <','><',tank_text)
  tank_text <- gsub('<div class="round-title">','\n<div class="round-title">',tank_text)
  tank_text <- gsub('<table class="full-draft">','\n<table class="full-draft">',tank_text)
  tank_text <- gsub('</tr><tr>','\n',tank_text)
  tank_text <- gsub("<[^>]+>","%",tank_text)
  tank_text <- gsub("%+","%",tank_text)
  tank_text <- gsub("%\n%","\n",tank_text)
  tank_text <- gsub("^\n","",tank_text)
  tank_split <- str_split(tank_text,"\n")[[1]]
  tank_split <- gsub("^%","",tank_split)
  tank_split <- gsub("%$","",tank_split)
  tank_split <- gsub("\\*","",tank_split)
  round_starts <- which(!is.na(str_match(tank_split,".* Round$")))
  picks <- as_tibble(str_split_fixed(tank_split[-round_starts],"%",4)) %>% 
    mutate(V1=as.integer(V1)) %>% 
    select(-V2) %>% 
    rename(pick=V1,short_location=V3,original=V4) %>% 
    filter(original != " Forfeited") %>% 
    inner_join(teams,by="short_location") %>% 
    mutate(original=ifelse(original == "",team,original)) %>% 
    select(pick,team)
  picks$pick <- 1:nrow(picks)
  return(picks)
}

get_salary_cap_data <- function()
{
  teams <- silent_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/teams.csv") %>% 
    filter(season == max(season)) %>% 
    select(team,nickname)
  otc_raw <- silent_readLines("https://overthecap.com/salary-cap-space/")
  otc_text <- otc_raw[50]
  otc_text <- gsub('.*<tbody>',"<tbody>",otc_text)
  otc_text <- gsub('</tr></thead><tbody><tr class="sortable">',"\n",otc_text)
  otc_text <- gsub('</tr><tr class="sortable">',"\n",otc_text)
  otc_text <- gsub("<[^>]+>","%",otc_text)
  otc_text <- gsub("%+","%",otc_text)
  otc_text <- gsub(",","",otc_text)
  otc_text <- gsub("\\$","",otc_text)
  otc_text <- gsub("\\(","-",otc_text)
  otc_text <- gsub(")","",otc_text)
  otc_split <- str_split(otc_text,"\n")[[1]][1:32]
  otc_split <- gsub("^%","",otc_split)
  otc_split <- gsub("%$","",otc_split)
  cap <- as_tibble(str_split_fixed(otc_split,"%",6)) %>% 
    mutate(V2=as.numeric(V2),V3=as.numeric(V3),V4=as.numeric(V4),
           V5=as.numeric(V5),V6=as.numeric(V6)) %>% 
    rename(nickname=V1,cap_space_actual=V2,cap_space=V3,
           num_players=V4,active_cap_spend=V5,dead_money=V6)
  return(cap)
}