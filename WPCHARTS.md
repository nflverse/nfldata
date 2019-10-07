# Creating Animated Annotated Win Probability Chats

- [Initial Stuff To Make This Work](#initial_stuff)
- [Load WP Plot Code](#load_code)
- [Sample Plot](#sample_plot)
- [Interpreting the Plot](#interpret_plot)
- [Choose Your Own Game](#choose_game)
- [Help Understanding the Code](#help_code)

<a name="initial_stuff"/>

## Initial Stuff To Make This Work

You'll need to create a directory or folder within your normal working directory where the GIFs will be stored on your computer. To do that, execute this R command:

``` r
system("mkdir game_wp")
```

You'll need to install various packages if you don't have them installed already. That can be done with this command:

``` r
install.packages(c("nflscrapR","tidyverse","ggplot2","ggimage","glue","animation"))
```

R may prompt you asking for various permissions/confirmations.

<a name="load_code"/>

## Load WP Plot Code

You can download my file here: https://github.com/leesharpe/nfldata/blob/master/code/wpchart.R

This is the preferred approach as you can make changes to it yourself to suit your own needs. However, if you prefer, you can execute the code directly from GitHub here:

``` r
source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/wpchart.R")
```
This will load the code into memory, but also handle downloading the nflscrapR data to your computer if you don't already have it! OK, now let's get it to make a plot!

<a name="sample_plot"/>

## Sample Plot

Let's say you want to see the win probabilty chart of the Packers/Bears Week 12 Thanksgiving game in 2015. Just execute the following command:

``` r
create_wp_plot("2015_12_CHI_GB")
```

This will take about a minute and a half to run, depending on your computer. The output will look like this:

``` r
[1] "2019-09-02 17:40:28: Loading team logos"
Parsed with column specification:
cols(
  team = col_character(),
  url = col_character()
)
[1] "2019-09-02 17:40:28: Processing play data for 2015_12_CHI_GB"
[1] "2019-09-02 17:40:28: Fixing plays with a WPA of NA"
[1] "2019-09-02 17:40:28: Preparing data for plot"
# A tibble: 14 x 11
   play_id   qtr     s    wp      wpa posteam away_score home_score text                        x_text  y_text
     <dbl> <dbl> <dbl> <dbl>    <dbl> <chr>        <dbl>      <dbl> <chr>                        <dbl>   <dbl>
 1      81     1  3588 0.551  0.104   GB               0          0 "E.Lacy Rush\nGB +10%"        3588  0.251 
 2     813     1  3028 0.386  0.107   GB               0          0 "E.Lacy TD\nGB +11%"          3028  0.686 
 3    1469     2  2454 0.330 -0.0629  GB               0          7 "GB FUM LOST\nCHI +6%"          NA NA     
 4    1680     2  2168 0.441  0.0680  CHI              0          7 "Z.Miller TD\nCHI +7%"        2168  0.841 
 5    1716     2  2163 0.504  0.0986  GB               7          7 "J.Janis KR\nGB +10%"         2163  0.204 
 6    1906     2  1923 0.432  0.00731 GB               7          7 "FG GOOD\nGB +1%"               NA NA     
 7    2161     2  1834 0.593  0.0398  CHI              7         10 "J.Langford TD\nCHI +4%"      1834  0.0927
 8    2368     2  1804 0.561  0.0273  GB              14         10 "FG GOOD\nGB +3%"             2604  0.561 
 9    3099     3  1023 0.504  0.119   CHI             14         13 "GB PEN-DPI\nCHI +12%"        1023  0.304 
10    3359     4   738 0.626  0.0273  CHI             14         13 "FG GOOD\nCHI +3%"             738  0.126 
11    3698     4   458 0.681  0.119   CHI             17         13 "M.Mariani Catch\nCHI +12%"    458  0.481 
12    3916     4   203 0.619 -0.218   GB              17         13 "T.Porter INT\nCHI +22%"       203  0.319 
13    4239     4   116 0.694  0.138   GB              17         13 "R.Cobb Catch\nGB +14%"       2316  0.694 
14    4316     4    78 0.648  0.186   GB              17         13 "D.Adams Catch\nGB +19%"        78  0.148 
[1] "2019-09-02 17:40:29: Plotting plays in quarter 1"
geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?
[1] "2019-09-02 17:40:46: Plotting plays in quarter 2"
[1] "2019-09-02 17:41:17: Plotting plays in quarter 4"
[1] "2019-09-02 17:41:41: Plotting frames for pause"
[1] "2019-09-02 17:41:58: Assembling plots into a GIF"
Output at: C:/Users/lsharpe/Documents/game_wp/2015_12_CHI_GB.gif
[1] FALSE
Warning messages:
1: In create_wp_plot("2015_12_CHI_GB") :
  No room for (2015_12_CHI_GB,2454,0.33): GB FUM LOST CHI +6%
2: In create_wp_plot("2015_12_CHI_GB") :
  No room for (2015_12_CHI_GB,1923,0.432): FG GOOD GB +1%
```

The tibble shows a bunch of nflscrapR information. The column `game_seconds_left` is now `s`, and `wp` is skewed to always reprsent the away team. The away team will always appear on top. Anyway, each row is a play my code deemed to be worth highlighting, and the `text` column indicates the label to apply to it. The `x_text` and `y_text` columns represent where the center of the label will be placed on the plot (in the same form as `s` and `wp` respectively).

You'll notice some of these label coordinates have values of NA. This means the label-placing algorithm chose to omit these because there wasn't enough room to display them due to where the WP line was, or it would conflict with other labels displaying plays with larger changes in WP. It also throws warnings up about this, but the plot will still display showing the remaining labels.

The GIF should open automatically, but if not, R tells you the path it's stored on your computer and you can open it from there.

<a name="interpret_plot"/>

## Interpreting the Plot

Our plot looks like this!

![2015_12_CHI_GB WP Chart](http://www.habitatring.com/2015_12_CHI_GB.gif)

The x-axis represents time in the game, as you can see along the bottom. The y-axis represents the probability of each team winning. The closer to the top, the more likely the visiting team (here, the Bears) are to win, and the closer to the bottom, the more likely the home team (here, the Packers) are to win. The 50% line in in bold across the middle.

The red line being drawn represents how the win probability moves across time. At the beginning of the GIF it starts from the kickoff at 50%, and to the right at a continuous pace, but up and down as plays happen. When a significant play occurs, it will be highlighed with a blue dot. Out of the blue dot, a dashed blue line will connect it to a label (appearing simultaneously) explaining what happened in that play and how much in changed win probability.

On the right hand side, you can see each team's logo, and a number representing how many points that team has scored so far at the furthest point in time the red line has reached so far. Once it hits the end, it shows the final score, and will remain paused there for a bit for you to review. Then the GIF will loop.

<a name="choose_game"/>

## Choosing Your Own Game

You can pick any game you want by entering it as the argument to the `create_wp_plot()` function. It will take arguments in one of two forms, either through the `game_id` used by the NFL and nflscrapR, or the more human readable game IDs that I use, stored as `alt_game_id` in the fields [I added to nflscrapR](https://github.com/leesharpe/nfldata/blob/master/UPDATING-NFLSCRAPR.md).

For example, these two commands are equivalent and both bring up the plot for Super Bowl LIII.

``` r
create_wp_plot("2019020300")
create_wp_plot("2019_21_NE_LAR")
```

If you want to find a game, you can filter the `games` tibble this brings into memory to find it. For example, if I knew I was looking for a 2010 Seahawks home game, I can run this command:

``` r
games %>% filter(season == 2010 & home_team == "SEA") %>% select(game_id,alt_game_id)
```

And it will kindly point them out to me, including the home playoff game.

``` r
# A tibble: 9 x 2
  game_id    alt_game_id    
  <chr>      <chr>          
1 2010091211 2010_01_SF_SEA 
2 2010092613 2010_03_SD_SEA 
3 2010102409 2010_07_ARI_SEA
4 2010110708 2010_09_NYG_SEA
5 2010112808 2010_12_KC_SEA 
6 2010120512 2010_13_CAR_SEA
7 2010121910 2010_15_ATL_SEA
8 2011010215 2010_17_STL_SEA
9 2011010801 2010_18_NO_SEA
```

<a name="help_code"/>

## Help Understanding the Code

Sure! The code [can be viewed here](https://github.com/leesharpe/nfldata/blob/master/code/wpchart.R). Here are parts I think people will be interested in. Search for these comments:

- The section marked `# mark labels for relevant plays` identifies which plays get highlighted as well as how they get labeled. They are pulled from various fields in nflscrapR. Also note the `glue` function, which allows you to put in braces variables and functions that R will evaluate and insert the result into that portion of the string.

- The section marked `#determine the location of the label` goes through each label and determines where it should go. It sorts plays by how much WP changes, then scans first vertically and then horizontally to identify a spot free of colliding with the red line and (mostly) free of colliding with another label. If it can't find a good spot, it raises a warning and leaves that label off the plot.

- The section marked `# define function to draw a frame` actually creates the plot for a single frame of the GIF. (The GIF is formed by making a still image of how it would look following each play, and then combining them all together.) It passes in the seconds remaining in the game to know where to be. This is accomplished through the function below it `draw_game`, which in turn is called by the `saveGIF` call at the bottom of the code.
