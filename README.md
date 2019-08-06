# NFL Data for Public Consumption

This is a repository for NFL data for people who want to play with NFL data to have information to look at! My name is Lee Sharpe, and you can find me on Twitter at [@LeeSharpeNFL](https://twitter.com/LeeSharpeNFL). Feel free to reach out if you have questions!

I want to thank [Ben Baldwin](https://twitter.com/benbbaldwin) who has created [a great tutorial](https://gist.github.com/guga31bb/5634562c5a2a7b1e9961ac9b6c568701) for getting started with play-by-play data from nflscrapR. That tutorial was the inspiration for this write-up.

While you can of course use a variety of tools for looking at data, I recommend downloading and installing both R and RStudio. Both are free!
- [Download R](https://cran.cnr.berkeley.edu/)
- [Download RStudio](https://www.rstudio.com/products/rstudio/download/#download)

Once installed, run R Studio and look in the Console tab. This is where you can enter commands and see the results of those commands. if you haven't already, I recommend installing some packages for R (you'll only need to do this once):

``` r
> install.packages("tidyverse")
> install.packages("ggplot2")
```

To download, for example, the standings data into your R enviornment, run the following:

``` r
> library(tidyverse)
> standings <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/standings.csv")
```

You can examine with the head command as in below. The `%>%` operator sends the standings data into the `head()` command, which just shows you the first several rows. This is a good way to get a sense of the structure of the data.

``` r
> standings %>% head()
# A tibble: 6 x 16
  season conf  division  team   wins losses  ties win_rate div_rank scored allowed points   sov   sos  seed playoffs
   <dbl> <chr> <chr>     <chr> <dbl>  <dbl> <dbl>    <dbl>    <dbl>  <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl> <chr>   
1   2002 AFC   AFC East  BUF       8      8     0    0.5          4    379     397    -18 0.352 0.473    NA NA      
2   2002 AFC   AFC East  MIA       9      7     0    0.562        3    378     301     77 0.486 0.508    NA NA      
3   2002 AFC   AFC East  NE        9      7     0    0.562        2    381     346     35 0.451 0.523    NA NA      
4   2002 AFC   AFC East  NYJ       9      7     0    0.562        1    359     336     23 0.5   0.5       4 LostDV  
5   2002 AFC   AFC North BAL       7      9     0    0.438        3    316     354    -38 0.384 0.5      NA NA      
6   2002 AFC   AFC North CIN       2     14     0    0.125        4    279     456   -177 0.406 0.531    NA NA      
```

You can see how this data is structured. Each row corresponds to how well a certain team did in a certain season. This shows a lot of information about the team's performance that year.

For example, you can see the Jets were the 4th seed for the AFC that year, but lost in the divisional round of the playoffs. The other teams have `NA` listed which means "not applicable". Usually you can figure out why something wouldn't apply from context. In this case, it means those teams did not make the playoffs, so there's no seed or playoff result to show.

## Examining Data
#### Example: What playoff seeds have Super Bowl winners had?

Let's say you want to see how many times a team with a given playoff seed as has won the Super Bowl (since realignment in 2002, when this data set begins). This command first takes `standings` and filters it down only to the rows where the team won the Super Bowl that year. Second, it groups these rows by `seed`, so there's one row for each of the six playoff seeds. Finally, we want to count how many rows were collapsed into this each seed's new row, and we'll call that `count`.

``` r
> standings %>% filter(playoffs == "WonSB") %>% group_by(seed) %>% summarize(count=n())
# A tibble: 6 x 2
   seed count
  <dbl> <int>
1     1     7
2     2     4
3     3     1
4     4     2
5     5     1
6     6     2

```

Wow, the team that won the Super Bowl was a 1st or 2nd seed most of the time. Those first round playoff byes sure do appear to be helpful!

## Plotting Data
#### Example: Are teams who score a lot of points more or less likely to be teams with a lot of points allowed?

Let's plot this data. We'll choose points scored to be the x-axis (or horizontal axis) while points allowed is the y-axis (or vertical axis). Then on the plot we can put a dot where each team falls. To help understand this data better, we'll also change the color of the dot based on their playoff outcome.

``` r
library(ggplot2)
ggplot(standings,aes(x=scored,y=allowed)) +
  theme_minimal() +
  geom_point(aes(color=playoffs)) +
  xlab("Points Scored") +
  ylab("Points Allowed") +
  labs(title="Points Scored vs. Points Allowed")
```

After running this command, you should see this in the Plots tab:

![Points Scored vs. Points Allowed](http://www.habitatring.com/scored-vs-allowed.png)

Two things should pop out at you as you look at this data. First: Non-gray dots, indicating a playoff team, are mostly toward the lower-right hand corner of the graph. This indicates that playoff teams tend to be teams that have scored a lot of points (suggesting a good offense), and have allowed their opponents to score relatively few points (suggesting a good defense). This makes sense, of course. You expect teams that are good at both to be in the playoffs!

Second, teams are spread all over this graph. This indicates there's not a strong relationship between how many points team score and how many they allow, so knowing how many they score doesn't really help you know more about how many points they allow. This makes sense. As discussed above, points scored is mostly a reflection of how good the offense is, while points allowed is mostly a reflection of how good the defense is. And we can all think of examples of teams that were good on one side of the ball while being bad at the other.

Let's look back at how we generated this plot and understand the command better:

``` r
ggplot(standings,aes(x=scored,y=allowed)) +
  theme_minimal() +
  geom_point(aes(color=playoffs)) +
  xlab("Points Scored") +
  ylab("Points Allowed") +
  labs(title="Points Scored vs. Points Allowed")
```

The `ggplot()` function is what tells R to create the plot. `standings` is the first argument, telling R what data it should use to make the plot. After that, we tell it which columns withing `standings` should represent the x and y axes. The `aes()` function in the middle is there to allow R to understand within that function, you'll be referring to columns of `standings` by name.

After that we can add additional paramters to our plot. `theme_minimal()` is a good set of defaults for how the title, legend, axes, gridlines, and so forth should appear visually. More advanced options allow you to configure this if you wish. `geom_point()` puts a point for each row in `standings`. Again within `aes()`, we tell R that the color of the dot should be based on the `playoffs` column value for that row. R will assign colors on its own, but more advanced options allow for configuration here. Finally, we can use `xlab()`, `ylab()`, and `labs()` to specify the text labels that we want to appear on this plot to help the viewer understand what the data means.
