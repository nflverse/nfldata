# NFL Data for Public Consumption from @LeeSharpeNFL 

This is a repository for NFL data for people who want to play with NFL data to have information to look at! My name is Lee Sharpe, and you can find me on Twitter at [@LeeSharpeNFL](https://twitter.com/LeeSharpeNFL)

Before you begin, if you haven't already, I recommend installing some packages for R (you'll only need to do this once):

``` r
> install.packages("tidyverse")
```

To download, for example, the standings data into R use the following command:

``` r
> library(tidyverse)
> standings <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/standings.csv")
```

You can examine with the head command as in below. The "%>%" operator sends the standings data into the head() command.

``` r
> standings %>% head()

# A tibble: 6 x 13
  season conference division  team   wins losses  ties win_pct div_rank   sov   sos  seed playoffs
   <dbl> <chr>      <chr>     <chr> <dbl>  <dbl> <dbl>   <dbl>    <dbl> <dbl> <dbl> <dbl> <chr>   
1   2002 AFC        AFC East  BUF       8      8     0   0.5          4 0.352 0.473    NA NA      
2   2002 AFC        AFC East  MIA       9      7     0   0.562        3 0.486 0.508    NA NA      
3   2002 AFC        AFC East  NE        9      7     0   0.562        2 0.451 0.523    NA NA      
4   2002 AFC        AFC East  NYJ       9      7     0   0.562        1 0.5   0.5       4 LostDV  
5   2002 AFC        AFC North BAL       7      9     0   0.438        3 0.384 0.5      NA NA      
6   2002 AFC        AFC North CIN       2     14     0   0.125        4 0.406 0.531    NA NA  
```

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
