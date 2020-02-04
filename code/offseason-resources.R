source("~/nfl/nfl.R")
library(tidyverse)
library(glue)

# season
season <- 2020

# round 1 (based on finish 
rd <- query(glue(
  "SELECT (CASE WHEN tm.new IS NULL THEN d.team ELSE tm.new END) AS team,
           d.wins, d.playoff, 1 AS round, d.draft_order AS pick
   FROM divisions d
   LEFT OUTER JOIN team_moves tm
      ON d.season+1 = tm.season
      AND d.team = tm.old
   WHERE d.season = {season-1}")) %>% 
  arrange(pick)

# add additional rounds
picks <- rd
for (r in 2:7)
{
  # in each round we cycle teams at equivalent playoff outcomes and win counts
  rd <- rd %>% 
    group_by(playoff,wins) %>% 
    mutate(round=r,pick=lag(pick,default=max(pick))) %>% 
    ungroup() %>% 
    arrange(pick)
  picks <- bind_rows(picks,rd)
}

# query trades
trades <- query(glue(
  "SELECT round, from, to FROM draft_trades WHERE season = {season}"))

# process trades
picks <- picks %>% 
  left_join(trades,by=c("round"="round","team"="from")) %>% 
  mutate(team=ifelse(!is.na(to),to,team)) %>% 
  filter(team != "---") %>%   # remove forfeited draft picks
  select(round,pick,team)
  
# add compensatory picks
comp_picks <- query(glue(
  "SELECT round, from, to FROM draft_comp_picks WHERE season = {season}"))

# order compensatory picks correctly
picks <- picks %>% 
  bind_rows(comp_picks) %>% 
  arrange(round,pick)

# change pick numbering
picks$pick <- 1:nrow(picks)

# add in draft values
picks <- picks %>% 
  left_join(query("SELECT pick, stuart AS draft_value FROM draft_values"),by="pick")

# cap space
cap <- read_csv("otc_cap_space.csv")

# all offseason data
offseason <- picks %>% 
  group_by(team) %>% 
  summarize(draft_value=sum(draft_value)) %>% 
  ungroup() %>% 
  left_join(cap,by="team") %>% 
  apply_colors_and_logos()

# plot
p <- ggplot(offseason,aes(x=draft_value,y=cap_space)) +
  theme_minimal() +
  scale_y_continuous(breaks=seq(0,10e7,2e7),
                     labels=c("$0M","$20M","$40M","$60M","$80M","$100M")) +
  geom_vline(xintercept=mean(offseason$draft_value),linetype="dashed") +
  geom_hline(yintercept=mean(offseason$cap_space),linetype="dashed") +
  geom_image(aes(image=team_logo),size=0.03,asp=1.5) +
  labs(title="NFL Team 2020 Offseason Resources",
       subtitle="Analysis by @LeeSharpeNFL",
       x="Draft Pick Value",
       y="Effective Cap Space",
       caption="Dashed line represents the average across NFL teams\nDraft Pick Value from Chase Stuart (@fbgchase) Model\nCompensatory draft pick estimates and Effective Cap Space from Over The Cap")
plot(p)
save_plot("offseason.png")