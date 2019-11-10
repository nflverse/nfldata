# R Code for Updating nflscrapR data

You can download my file here: https://github.com/leesharpe/nfldata/blob/master/code/plays.R

This is the preferred approach as you can make changes to it yourself to suit your own needs. However, if you prefer, you can execute the code directly from GitHub here:

``` r
source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays.R")
```

# What does this code do?

- [Get data for just the games you're missing!](#which_games)
- [Fix team abbreviations](#fix_team_abbreviations)
- [Add in columns about game data](#game_data)
- [Add in columns from Ben Baldwin's excellent nflscrapR tutorial](#apply_baldwin) which you can [read here](https://gist.github.com/guga31bb/5634562c5a2a7b1e9961ac9b6c568701)
- [Add in columns for colors and logos](#apply_colors_logos) **(NEW as of 2019-11-10)**
- [Add in columns for completion probability](#apply_cp) **(NEW as of 2019-11-10)**
  - You must do a one-time install the mgcv package first with command: `install.packages("mgcv")`
- [Add in columns for series data](#apply_series)

<a name="which_games"/>

## Get data for just the games you're missing!

The first time you run this, it will download everything. (This will take a long time.)

But in subsequent executions, it will check which NFL games are complete that you don't have data for, and get data for those games. When binding, it will automatically fix data types of fields that R likes to complain about through a custom function `fix_inconsistent_data_types()` so the binding can happen cleanly.

In either case, the rest of the modifications described below continue to apply.

*Note: If you are trying to use nflscrapR for live in-game data purposes, this won't work for you. This code only downloads data for games that have been completed.*

<a name="fix_team_abbreviations"/>

## Fix team abbreviations

There is inconsistent usage for team abbrevations, this attempts to standardize things through the function `fix_team_abbreviations()`. In particular, it uses JAX to refer to the Jacksonville Jaguars, cleaning up old games that use **JAC**. Additionally, for some reason, the NFL uses **LA** to refer to the Los Angeles Rams, even though there are now two teams that play in Los Angeles. This updates all of the Los Angeles Rams abbreviations to **LAR**. Every nflscrapR column where "team" is somewhere in the name of the column gets updated.

The `fix_team_abbreviations()` function has an optional argument `old_to_new`. When set to **FALSE** (the default), it only does the updates described above. When set to **TRUE**, it goes back and updates older team abbreviations. So when **FALSE**, the San Diego Chargers for example are represented as **SD**. However, sometimes you want to group by franchises across seasons, and want the abbreviations to match for the past. This means the old San Diego Chargers teams will be set to **LAC** instead. You can make this modification easily with the code:

``` r
plays <- plays %>% fix_team_abbreviations(old_to_new=TRUE)
```
<a name="game_data"/>

## Add in columns about game data

If you don't care about these, you can safely ignore them. However, I think most people will find `season` and `week` particularly helpful. I know that I do.

- `alt_game_id`: This is a more human-readable way to refer to a game. It consists of: The season, an underscore, the two-digit week number, an underscore, the away team, an underscore, the home team.
- `season`: The year of the NFL season. This reperesents the whole season, so regular season games that happen in January as well as playoff games will occur in the year after this number.
- `week`: The week of the NFL season the game occurs in. This will be **1**-**17** for the regular season, **18** for wildcard playoff games, **19** for divisional playoff games, **20** for conference championships and **21** for Super Bowls.
- `gameday`: The date on which the game occurred.
- `weekday`: The day of the week on which the game occcured.
- `gametime`: The kickoff time of the game. This is represented in 24-hour time and the Eastern time zone, regardless of what time zone the game was being played in.
- `away_team`: The away team.
- `away_score`: The number of points the away team scored (at end of game). Is NA for games which haven't yet been played.
- `home_team`: The home team. Note that this contains the designated home team for games which no team is playing at home such as Super Bowls or NFL International games.
- `home_score`: The number of points the home team scored (at end of game). Is NA for games which haven't yet been played.
- `location`: Either **Home** if the home team is playing in their home stadium, or **Neutral** if the game is being played at a neutral location. This still shows as **Home** for games between the Giants and Jets even though they share the same home stadium.
- `result`: The number of points the home team scored minus the number of points the visiting team scored (at end of game). Equals **h_score - v_score**. Is NA for games which haven't yet been played. Convenient for evaluating against the spread bets.
- `total`: The sum of each team's score in the game (at end of game). Equals **h_score + v_score**. Is NA for games which haven't yet been played. Convenient for evaluating over/under total bets.
- `gsis`: The id of the game issued by the NFL Game Statistics & Information System.
- `pfr`: The id of the game issued by [Pro Football Reference](https://www.pro-football-reference.com/)
- `pff`: The id of the game issued by [Pro Football Focus](https://www.pff.com/)

<a name="apply_baldwin"/>

## Add in columns from [Ben Baldwin's excellent nflscrapR tutorial](https://gist.github.com/guga31bb/5634562c5a2a7b1e9961ac9b6c568701)

If you don't want to add these columns, you can set the input for this to FALSE at the top of the file. It's done through the function `apply_baldwin_mutations()`.

- `pass`: Does the play description suggest this play was called as a pass?
- `rush`: Does the play description suggest this play was called as a rush?
- `success`: Was EPA for this play greater than 0?
- `player_passer_name`: Fixes the existing nflscrapR column so this is filled in rather than NA on penalty plays. Code for this taken from [Keegan Abdoo](http://twitter.com/KeeganAbdoo)
- `receiver_passer_name`: See above.
- `rusher_passer_name`: See above.
- `name`: Equal to `passer_player_name` unless that is NA, in which case it is equal to `rusher_player_name`
- `yards_gained`: Fixed the existing nflscrapR column so the value is NA for penalties rather than **0**.
- `play`: Is this a "normal" play (including penalties)? Specifically, are both `epa` and `posteam` not NA, and is the `play_type` either **no_play**, **pass**, or **run**?

<a name="apply_colors_logos"/>

## Add team logos and team colors   (NEW as of 2019-11-10)

For making NFL plots, often you want logos and colors. I usually just added them individually, but now I made a function `apply_colors_and_logos()` to just easily add them as follows:

``` r
team_epa <- plays %>%
  filter(season == max(season) & !is.na(epa)) %>%
  group_by(posteam) %>%
  summarize(mean_epa=mean(epa)) %>%
  ungroup() %>%
  apply_colors_and_logos()
```

This will add two columns:
- `use_color`: The hexadecimal color value to use for that team. It will use their primary color unless it is quite dark, in which case it uses their secondary color.
- `logo`: This is a URL that points to a transparent image file of the teams logo. Useful for `geom_image` plots.

The function has an additional optional argument to tell it which column in the exisitng data to use as the team abbreviation to join against. It will default to (in this order): `team`, `posteam`, `defteam`. It will raise an error if you don't specify and none of those columns are present.

<a name="apply_cp"/>

## Add in completion probability   (NEW as of 2019-11-10)

*Note: This requires the Ben Baldwin mutations from above*

This uses a model designed by Ben Baldwin to estimate the completion probability of a pass based on air yards (how many yards from the line of scrimmage the receiver is when the pass arrives) and whether the pass is to the left, middle, or right side of the field. The completion probability is stored in a new column called `cp`. This column will be `NA` for plays where no pass was thrown, when there was no intended receiver (throaways), or if the the number of air yards is -10 or less (very rare).

Completion probability is used in calculating a metric called Completion Percentage Over Expected (CPOE) which is highly stable metric from year-to-year for a given quarterback.  Here's an example of code calculating this which gives you each quarterback's CPOE for the current season.

``` r
plays %>%
  filter(season == max(season) & !is.na(cp)) %>%
  group_by(name) %>%
  summarize(cpoe=100*mean(complete_pass-cp),count=n()) %>%
  ungroup() %>%
  filter(count >= 0.25*max(count)) %>%
  arrange(desc(cpoe))
```

In creating the `cp` column, I used Ben's model trained as follows:
- 2009: This is the earliest season, so no previous training data exists. `cp` is `NA` for all 2009 plays.
- 2010: This is trained from 2009 data.
- 2011: This is trained from 2009 and 2010 data.
- 2012+: Moving forward, each season is trained using the prior three seasons.

<a name="apply_series"/>

## Add in columns for series data

If you don't want to add these columns, you can set the input for this to FALSE at the top of the file. It's done through the function `apply_series_data()`.

This is something I've been working on for a while. (If you discover bugs, please let me know!) Anyway, this code allows you to examine an individual series play makeup, and look at whether it succeeded.

A series is defined as every time the offense receieves a new first down. This can happen because a team gained enough yards in the last play to advance the sticks, a defensive penalty resulted in a first down, a change in possession, or following a kickoff or punt. Much like the nflscrapR `drive` column, my new `series` column starts at **1** for each game, and increments each time there is a new series. Some plays will have NA when they aren't defined as part of a series, such as kickoffs or timeouts.

A series is defined as a success if the team either scores a touchdown, or obtains a new first down in that series (creating another series). The new first down can be obtained through yardage or through defensive penalty, either counts as success. If the series results in a change of possession, a field goal attempt, or a punt, it is considered a failure. The new column `series_success` will report whether the current series ended up succesful or not. This column will be NA when either there isn't a series (so `series` is also NA), or when defining success for a series does not make sense. This occurs when the series contains a quarterback spike or kneel, or when the series is ended by the half/game ending rather than a "clean" ending to the series.

- `series`: What series number is this for this game? (Starts at **1** and increments.) NA for plays not in a series.
- `series_success`: Did this series end in success? **1** when scoring a touchdown, getting enough yards for another first down, or a defensive penalty resulting in a first down. **0** when there is a change in possession, a field goal attempt, or a punt. Is instead NA if the series contains a QB spike or kneel, or if the series ends the half or game and does not have a clean success or failure.

A frequently asked question is why a field goal attempt is counted as a failure. The goal of the series is to either score a touchdown or keep the drive moving so you can score a touchdown on a later series in the drive. A field goal attempt is in this sense a failure. Scoring 3 points is better than scoring 0, but ultimately the goal of a drive is touchdown. This is also why the attempt is a series failure, regardless of whether the field goal attempt results in a score.

When you first execute this, it can take a long time to run. It will report its progress through the vast amount of game data so you know it isn't hung. But after the initial execution, the run time isn't bad when just applying it to new games that have finished since the last execution.

*Note: This is broken for the two games the 2013 Browns hosted in Week 12 and Week 13 due to the `yards_gained` column in nflscrapR not having the necessary data. Both columns just show `NA` for those games.*
