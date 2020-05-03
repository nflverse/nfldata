# Get nflfastR

You only need to do this part once, but you must first install nflfastR, which you can do with the following commands:

``` r
install.packages("devtools")
devtools::install_github("mrcaseb/nflfastR")
```

# Use this R code to update your nflfastR data

You can download my file here: https://github.com/leesharpe/nfldata/blob/master/code/plays.R

This is the preferred approach as you can make changes to it yourself to suit your own needs. However, if you prefer, you can execute the code directly from GitHub here:

``` r
source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays.R")
```

# What does this code do?

- [Get data for just the games you're missing!](#which_games)
- [Apply Ben Baldwin's mutations](#apply_baldwin)
- [Apply Lee Sharpe's mutations](#apply_sharpe)
- [Add in columns for positional data](#positions)
- [Add in columns for colors and logos](#apply_colors_logos)

<a name="which_games"/>

## Get data for just the games you're missing!

The first time you run this, it will download everything. In this case it takes a shortcut and begins with preexisting scraped files hosted by Ben Baldwin rather than actually scrape them all from the NFL which would take longer.

But in subsequent executions, it will check which NFL games are complete that you don't have data for, and get data for those games. When binding, it will automatically fix data types of fields that R likes to complain about through a custom function `fix_inconsistent_data_types()` so the binding can happen cleanly.

In either case, the rest of the modifications described below continue to apply.

*Note: If you are trying to use nflfastR for live in-game data purposes, this code won't work for you. This code only downloads data for games that have been completed.*

<a name="apply_baldwin"/>

## Apply Ben Baldwin mutations

If you don't want to add these columns, you can set the input for this to FALSE at the top of the file.

Ben Baldwin's mutations add the following columns:

- `pass`: Does the play description suggest this play was called as a pass?
- `rush`: Does the play description suggest this play was called as a rush?
- `success`: Was EPA for this play greater than 0?
- `player_passer_name`: Fixes the existing nflscrapR column so this is filled in rather than NA on penalty plays. Code for this taken from [Keegan Abdoo](http://twitter.com/KeeganAbdoo)
- `receiver_passer_name`: See above.
- `rusher_passer_name`: See above.
- `name`: Equal to `passer_player_name` unless that is NA, in which case it is equal to `rusher_player_name`
- `yards_gained`: Fixed the existing nflscrapR column so the value is NA for penalties rather than **0**.
- `play`: Is this a "normal" play (including penalties)? Specifically, are both `epa` and `posteam` not NA, and is the `play_type` either **no_play**, **pass**, or **run**?
- `qb_epa`: This is the epa value to use if focused only on the passer. Usually it's equal to `epa`, but it avoids punishing the passer for plays where the receiver catches the ball but fumbles.

<a name="apply_sharpe"/>

## Apply Lee Sharpe mutations

If you don't want to add these columns, you can set the input for this to FALSE at the top of the file.

Lee Sharpe's mutations will:

- Convert all **JAC** to **JAX** to align usage. The NFL has been inconsistent about this over time.
- Adds in all the columns mentioned [here](https://github.com/leesharpe/nfldata/blob/master/DATASETS.md#games), except for `gametime` as it's redundant with the existing `game_time_eastern`
- Adds the column `special`, which is **1** for extra point attempts, field goal attempts, kickoffs, and punts, and **0** otherwise.
- Change `play_type` to **note** when it's not referring to any kind of play, but rather something like a timeout being called or a notation of the end of a quarter or an injured player leaving or returning to the game. These generally used to be either **NA** or **no_play**. **no_play** is now utilized only for plays where penalties nullified the result of the play. 
- Convert players who are referred to inconsistently. For example, in 2019, Buffalo Bills quarterback Josh Allen was sometimes referred to as **J.Allen** and sometimes referred to as **Jos.Allen**. This standardizes his name to **J.Allen**.

<a name="positions"/>

## Add Position Columns

If you don't want to add these columns, you can set the input for this to FALSE at the top of the file.

nflfastR includes the ability to scrape the NFL's website for roster data. For each column in nflfastR output that references a player, this joins to the roster data to determine the position the player plays, and makes a separate column to notate that information. The added columns are:

- `passer_player_position`
- `receiver_player_position`
- `rusher_player_position`
- `lateral_receiver_player_position`
- `lateral_rusher_player_position`
- `lateral_sack_player_position`
- `interception_player_position`
- `lateral_interception_player_position`
- `punt_returner_player_position`
- `lateral_punt_returner_player_position`
- `kickoff_returner_player_position`
- `lateral_kickoff_returner_player_position`
- `punter_player_position`
- `kicker_player_position`
- `own_kickoff_recovery_player_position`
- `blocked_player_position`
- `tackle_for_loss_1_player_position`
- `tackle_for_loss_2_player_position`
- `qb_hit_1_player_position`
- `qb_hit_2_player_position`
- `forced_fumble_player_1_player_position`
- `forced_fumble_player_2_player_position`
- `solo_tackle_1_player_position`
- `solo_tackle_2_player_position`
- `assist_tackle_1_player_position`
- `assist_tackle_2_player_position`
- `assist_tackle_3_player_position`
- `assist_tackle_4_player_position`
- `pass_defense_1_player_position`
- `pass_defense_2_player_position`
- `fumbled_1_player_position`
- `fumbled_2_player_position`
- `fumble_recovery_1_player_position`
- `fumble_recovery_2_player_position`
- `penalty_player_position`

<a name="apply_colors_logos"/>

## Add team logos and team colors

Note that this is a function available to you, but it's not integrated with the nflfastR output using this code.

For making NFL plots, often you want logos and colors. I usually just added them individually, but now I made a function `apply_colors_and_logos()` to just easily add them as follows:

``` r
team_epa <- pbp %>%
  filter(season == max(season) & !is.na(epa)) %>%
  group_by(posteam) %>%
  summarize(mean_epa=mean(epa)) %>%
  ungroup() %>%
  apply_colors_and_logos()
```

This will add two columns:
- `use_color`: The hexadecimal color value to use for that team. It will use their primary color unless it is quite dark, in which case it uses their secondary color.
- `team_logo`: This is a URL that points to a transparent image file of the teams logo. Useful for `geom_image` plots.

The function has an additional optional argument to tell it which column in the exisitng data to use as the team abbreviation to join against. It will default to (in this order): `team`, `posteam`, `defteam`. It will raise an error if you don't specify and none of those columns are present.

<a name="apply_series"/>

## Add in columns for series data 

_NOTE: This is now part of the default nflfastR column, but I'm leaving the documentation here for posterity._

A series is defined as every time the offense receieves a new first down. This can happen because a team gained enough yards in the last play to advance the sticks, a defensive penalty resulted in a first down, a change in possession, or following a kickoff or punt. Much like the nflscrapR `drive` column, my new `series` column starts at **1** for each game, and increments each time there is a new series. Some plays will have NA when they aren't defined as part of a series, such as kickoffs or timeouts.

A series is defined as a success if the team either scores a touchdown, or obtains a new first down in that series (creating another series). The new first down can be obtained through yardage or through defensive penalty, either counts as success. If the series results in a change of possession, a field goal attempt, or a punt, it is considered a failure. The new column `series_success` will report whether the current series ended up succesful or not. This column will be NA when either there isn't a series (so `series` is also NA), or when defining success for a series does not make sense. This occurs when the series contains a quarterback spike or kneel, or when the series is ended by the half/game ending rather than a "clean" ending to the series.

- `series`: What series number is this for this game? (Starts at **1** and increments.) NA for plays not in a series.
- `series_success`: Did this series end in success? **1** when scoring a touchdown, getting enough yards for another first down, or a defensive penalty resulting in a first down. **0** when there is a change in possession, a field goal attempt, or a punt. Is instead NA if the series contains a QB spike or kneel, or if the series ends the half or game and does not have a clean success or failure.

A frequently asked question is why a field goal attempt is counted as a failure. The goal of the series is to either score a touchdown or keep the drive moving so you can score a touchdown on a later series in the drive. A field goal attempt is in this sense a failure. Scoring 3 points is better than scoring 0, but ultimately the goal of a drive is touchdown. This is also why the attempt is a series failure, regardless of whether the field goal attempt results in a score.

When you first execute this, it can take a long time to run. It will report its progress through the vast amount of game data so you know it isn't hung. But after the initial execution, the run time isn't bad when just applying it to new games that have finished since the last execution.

*Note: This is broken for the two games the 2013 Browns hosted in Week 12 and Week 13 due to the `yards_gained` column in nflscrapR not having the necessary data. Both columns just show `NA` for those games.*
