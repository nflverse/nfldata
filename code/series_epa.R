source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays.R")

series_data <- plays %>% 
  filter(season == 2018) %>% 
  filter(!is.na(series) & !is.na(series_success)) %>% 
  filter(!is.na(epa) & wp >= 0.2 & wp <= 0.8) %>% 
  distinct(posteam,game_id,series,series_success) %>% 
  group_by(posteam) %>% 
  summarize(series_success_rate=mean(series_success))

epa_data <- plays %>% 
  filter(season == 2018) %>% 
  filter(!is.na(series) & !is.na(series_success)) %>% 
  filter(!is.na(epa) & wp >= 0.2 & wp <= 0.8) %>% 
  group_by(posteam) %>% 
  summarize(mean_epa=mean(epa))

logos <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/logos.csv")

series_epa <- series_data %>% 
  inner_join(epa_data,by=c("posteam"="posteam")) %>% 
  inner_join(logos,by=c("posteam"="team"))

ggplot(series_epa,aes(x=series_success_rate,y=mean_epa)) +
  theme_minimal() +
  geom_image(aes(image = url), size = 0.05) + 
  xlab("Series Success Rate") + 
  ylab("EPA/Play") +
  labs(title="2018: Series Success Rate vs. EPA/Play",
       subtitle="Analysis by @LeeSharpeNFL",
       caption="Data from nflscrapR")
