# Colors in R

I've seen a lot of people working with nflscrapR data in R post on Twitter that their team colors aren't working. They hesitate to post interesting plots because they can't get the colors right. No more! Let me show you how to fix this.

We start off by loading our data and arranging it just how we want. For this example, let's take a look at each NFL team's EPA/play in the 2019 season through Week 4, since that's the present at the time that I'm writing this. After we have the EPA/play value, we'll join to the `team_colors` so we have the color for each team. 

``` r
source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays.R")
team_colors <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/teamcolors.csv")

offense <- plays %>% 
  filter(season == 2019 & week <= 4) %>% 
  filter(!is.na(epa)) %>% 
  filter(pass == 1 | rush == 1) %>% 
  group_by(posteam) %>% 
  summarize(mean_epa=mean(epa)) %>% 
  ungroup() %>% 
  inner_join(team_colors,by=c("posteam"="team")) %>% 
  arrange(mean_epa)
```

When we examine `offense`, we get data that looks like this:

``` r
> offense
# A tibble: 32 x 6
   posteam mean_epa color   color2  color3  color4 
   <chr>      <dbl> <chr>   <chr>   <chr>   <chr>  
 1 MIA     -0.333   #008e97 #f58220 #005778 #008e97
 2 NYJ     -0.284   #203731 #1c2d25 NA      NA     
 3 CIN     -0.153   #000000 #fb4f14 #000000 #d32f1e
 4 WAS     -0.0929  #773141 #ffb612 #000000 #5b2b2f
 5 PIT     -0.0807  #000000 #ffb612 #c60c30 #00539b
 6 CHI     -0.0588  #0b162a #c83803 #0b162a #c83803
 7 CAR     -0.0527  #0085ca #000000 #bfc0bf #0085ca
 8 BUF     -0.0315  #00338d #c60c30 #0c2e82 #d50a0a
 9 CLE     -0.0278  #fb4f14 #22150c #a5acaf #d32f1e
10 ARI     -0.00614 #97233f #000000 #ffb612 #a5acaf
# ... with 22 more rows
```

All right, everything looks good so far. Let's make a bar chart showing this data! I actually think a bar chart is the wrong visualization here, but the point of this is to teach about colors in R, so let's just pretend this is our desired visualization for whatever reason. This is similar many people would go about creating this plot:

``` r
wtf_coloring <- ggplot(offense,aes(x=posteam,y=mean_epa)) +
  theme_minimal() +
  geom_bar(stat="identity",aes(color=color,fill=color)) +
  xlab("") +
  ylab("EPA/Play") +
  labs(title="2019 NFL Offenses: EPA/Play",
       caption="Data from nflscrapR")
plot(wtf_coloring)
```

![THIS IS HORRIBLE, WHAT HAPPENED?!](http://www.habitatring.com/wtf_coloring.png)

## THIS IS HORRIBLE, WHAT HAPPENED?!

R didn't use the colors we wanted at all. Instead it used its own random color selection. We even see a legend where it assigned each of our colors to another color. WHY?!

What's happening here is just because "#97233f" represents a color, R doesn't know that. It's just a series of characters to R. When you say `aes(color=color,fill=color)`, how R intepretes that is that you want R to assign a different color to each different value of the `color` column, and use that to display the bar. But that's not what we want. So how to get the right colors, and clean up the sorting? 

There's one other problem, too. We stuck the `arrange(mean_epa)` when shaping the data because we wanted R to order the bars from worst EPA/play to best so the viewer can easily see how the teams rank. But R didn't do that either, and instead just ordered it alphabetically. We'll solve this problem first, as it will make our other problem easier.

## Factors in R

I know, I know, you just wanted to sort and color your plot right, not learn about a whole new R data type. I promise this one is pretty straightforward. Imagine you're showing a plot by months. You want your months to appear in the order Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec. But just like R didn't understand that "#97233f" is a color, it doesn't understand that "Jan" is a month, either. We need to tell R this information.

Factors are a tool in R to allow you to do just that, define a custom ordering for data that R doesn't inherently know the order for. In this case, we want to do that for the `posteam` column, to let R know they should be ordered in the order they are already stored in `offense`. We can do that with this command:

``` r
offense$posteam <- factor(offense$posteam,levels=offense$posteam)
```

This lets R know how to order the teams, in the order we already placed them when sorting `offense` using the `arrange` command. Now R know the order, great! But how are we telling R which color goes with which team?

## Create a manual color scale!

First, we're going to tell R what colors we want to use. We've already stored them in the `color` column of `offense`. We want this to apply to both the `color` color (the border of the bar) and the `fill` color (the inner main part of the bar), so we'll specify it for both.

Once the color scales are defined, we are instead going to tell R that we are actually determining which color to use based on the value in `posteam` (the abbreviation of the team in question), rather than the `color` column. Note that `posteam` is a factor because of the above command. 

We can do this as follows:

``` r
desired_coloring <- ggplot(offense,aes(x=posteam,y=mean_epa)) +
  theme_minimal() +
  scale_color_manual(values=offense$color) +  
  scale_fill_manual(values=offense$color) +
  geom_bar(stat="identity",aes(color=posteam,fill=posteam),show.legend=FALSE) +
  xlab("") +
  ylab("EPA/Play") +
  labs(title="2019 NFL Offenses: EPA/Play",
       caption="Data from nflscrapR")
plot(desired_coloring)
```

![YAY GOOD COLORS](http://www.habitatring.com/yay_good_colors.png)

There, that's so much better! R knows because of the `scale_color_manual` and `scale_fill_manual` commands that it's supposed to use colors. It will use the first value for the lowest factor value (here, `MIA`), the second for the next, and so on.

There you go! Have fun using team colors. :)
