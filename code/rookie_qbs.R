library(tidyverse)
library(ggplot2)
library(ggimage)
source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays-functions.R")

# open play data
plays <- readRDS("playdata")  # replace with your own file location
plays <- plays %>% 
  fix_team_abbreviations() %>% 
  apply_game_data()

# load draft and logo data
draft_picks <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/draft_picks.csv")
logos <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/logos.csv")

# get rookie QBs from draft pick data
rookie_qbs <- draft_picks %>% 
  filter(position == "QB") %>% 
  mutate(full_name=name) %>%  # renaming name to full_name so the join works well
  select(-name)

# special exception to count 2018 as Patrick Mahomes' "rookie" year
rookie_qbs$season[rookie_qbs$full_name == "Patrick Mahomes"] <- 2018

# reorganize data
rookie_data <- plays %>% # "plays" is where I store nflscrapR data, replace with yours
  filter(!is.na(epa) & play_type %in% c("pass","run")) %>% 
  mutate(name=ifelse(is.na(passer_player_name),rusher_player_name,passer_player_name)) %>% 
  inner_join(rookie_qbs,by=c("season"="season","posteam"="team")) %>% 
  # the below two filter lines are to match up "P.Mahomes" and "Patrick Mahomes"
  # feel free to ask me on Twitter (@LeeSharpeNFL) if you want info on how it works
  # however note that it will not match ALL names in nflscrapR, but it works for these
  filter(substr(name,1,1) == substr(full_name,1,1)) %>% 
  filter(substr(name,str_locate(name,"\\.")+1,100) == 
           substr(full_name,str_locate(full_name," ")+1,100)) %>% 
  inner_join(logos,by=c("posteam"="team")) %>% 
  group_by(season,posteam,name,url) %>% 
  summarize(stddev=sd(epa),epa=mean(epa),count=n()) %>% 
  filter(count >= 100)

# plot
ggplot(rookie_data,aes(x=epa,y=stddev)) +
  theme_minimal() +
  geom_image(aes(image=url), size = 0.05) + 
  geom_text(aes(label=name),nudge_y=-0.02) +
  geom_vline(xintercept=0,linetype="dashed",color="black",size=1) +
  xlab("EPA/Play") +
  ylab("EPA Standard Deviation") +
  labs(title="Rookie QBs 2009-2018: EPA/Play vs. EPA Standard Deviation",
       subtitle="Analysis by @LeeSharpeNFL",
       caption="Minimum 100 Plays\nData from nflscrapR and Pro Football Reference")
ggsave("rookie_qbs.png",last_plot(),dpi=1000)
