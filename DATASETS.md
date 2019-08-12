# NFL Data Sets

Here is detailed information on each data set.

- [Draft Picks](#draft_picks)
- [Draft Values](#draft_values)
- [Games](#games)
- [Logos](#logos)
- [Rosters](#rosters)
- [Standings](#standings)

<a name="draft_picks"/>

## Draft Picks

To import and join to nflscrapR (using the Ben Baldwin `name` field):

``` r
draft_picks <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/draft_picks.csv")
# we do a left join here because the names won't always match but don't want to lose any nflscrapR rows
plays <- plays %>%
  left_join(draft_picks,by=c("season"="season","posteam"="team","name"="name")
```

Data begins with the 2000 season, does not include picks from the supplemental draft, and comes from the excellent [Pro Football Reference](https://www.pro-football-reference.com/).

- `season`: The season in which the draft occurred. This is the season after the draft, not the one before it, so this would represent the rookie year for drafted players.
- `team`: The team that drafted the player. This team may have had this pick originally, or traded for it.
- `round`: The round of the draft this pick occurred in.
- `pick`: The number of the pick.
- `full_name`: The name of the selected player.
- `name`: The name of the selected player in the same format as NFL play-by-play data. This will *usually* match nflscrapR name fields, but not for every player.
- `playerid`: The ID of the selected player as used by Pro Football Reference. If `NA`, the player was not assigned an ID by Pro Football Reference, which normally indicates they never played in an NFL game.
- `side`: The side of the ball the player plays on. Can be:
   - For offense, `O`
   - For defense, `D` 
   - For special teams, `S`
   - If `position` is `NA`, this will be also.
- `category`: The category of position the player plays in.
   - For offense, this can be `OL`, `QB`, `RB`, `TE`, or `WR`.
   - For defense, this can be `DB`, `DL`, `ED` (edge), or `LB`. Note that `ED` is only used from 2019 forward.
   - For special teams, this can be `K`, `KR`, `LS`, `P`, or `ST` (generic special teams).
   - If `position` is `NA`, this will be also.
- `position`: The NFL position the selected player plays as reported by Pro Football Reference. If `NA`, Pro Football Reference did not record a position.

<a name="draft_values"/>

## Draft Values

To import and join to draft pick data from above:

``` r
draft_values <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/draft_values.csv")
draft_picks <- draft_picks %>%
  inner_join(draft_values,by=c("pick"="pick")
```
Columns:
- `pick`: The pick number in the NFL draft.
- `stuart`: The value of the pick [according to Football Perspective's Chase Stuart](https://www.footballperspective.com/draft-value-chart/), based on [the AV metric from Pro Football Reference](https://www.pro-football-reference.com/blog/index37a8.html)
- `johnson`: The value of the pick according to the original pick value chart created by then-Cowboys coach Jimmy Johnson
- `hill`: The value of the pick [according to Rich Hill](https://www.patspulpit.com/2018/4/21/17256758/2018-nfl-draft-value-chart-rich-hill)

It's worth noting that the Stuart scale is attempting to measure how teams *should* value draft picks, while the Johnson and Hill scales are attempting to measure how teams *in practice* value draft picks, and that these are different questions. The Hill versions is obviously based on more recent data. Also note the systems are using different numerical scales, so you should only compare values *within* a scale, not compare, say, a Stuart value to a Johnson value (the latter will essentially always be higher).

<a name="games"/>

## Games

To import and join to nflscrapR data:

``` r
games <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv")
plays <- plays %>%
  inner_join(games,by=c("game_id"="game_id","away_team"="away_team","home_team"="home_team")
```

Data begins with the 2006 NFL season. This CSV should be updated within minutes of a game completing. Does not include preseason.

Columns:
- `game_id`: The ID of the game as assigned by the NFL. Note that this value matches the `game_id` field in nflscrapR if you wish to join the data.
- `alt_game_id`: This is a more human-readable ID. It consists of: The season, an underscore, the two-digit week number, an underscore, the away team, an underscore, the home team.
- `season`: The year of the NFL season. This reperesents the whole season, so regular season games that happen in January as well as playoff games will occur in the year after this number.
- `week`: The week of the NFL season the game occurs in. This will be 1-17 for the regular season, 18 for wildcard playoff games, 19 for divisional playoff games, 20 for conference championships and 21 for Super Bowls.
- `gameday`: The date on which the game occurred.
- `weekday`: The day of the week on which the game occcured.
- `gametime`: The kickoff time of the game. This is represented in 24-hour time and the Eastern time zone, regardless of what time zone the game was being played in.
- `away_team`: The away team.
- `away_score`: The number of points the away team scored. Is `NA` for games which haven't yet been played.
- `home_team`: The home team. Note that this contains the designated home team for games which no team is playing at home such as Super Bowls or NFL International games.
- `home_score`: The number of points the home team scored. Is `NA` for games which haven't yet been played.
- `location`: Either `Home` if the home team is playing in their home stadium, or `Neutral` if the game is being played at a neutral location. This still shows as `Home` for games between the Giants and Jets even though they share the same home stadium.
- `result`: The number of points the home team scored minus the number of points the visiting team scored. Equals `h_score - v_score`. Is `NA` for games which haven't yet been played. Convenient for evaluating against the spread bets.
- `total`: The sum of each team's score in the game. Equals `h_score + v_score`. Is `NA` for games which haven't yet been played. Convenient for evaluating over/under total bets.

<a name="logos"/>

## Logos

To import and join to nflscrapR data (for the offense):

``` r
logos <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/logos.csv")
plays <- plays %>%
  inner_join(logos,by=c("posteam"="team")
```

Columns:
- `team`: The team.
- `url`: URL of an image where a transparent team logo is located.

This is based off [a version](https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv) done by [Michael Lopez](https://twitter.com/StatsbyLopez), but includes a manual fix for the Tennessee Titans logo (which had a white rather than transparent background on Wikipedia for some reason) and also supports older team abbreviations (`SD` and `STL`).

<a name="logos"/>

## Rosters

To import and join to nflscrapR (using the Ben Baldwin `name` field):

``` r
logos <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/rosters.csv")
# we do a left join here because the names won't always match but don't want to lose any nflscrapR rows
plays <- plays %>%
  left_join(logos,by=c("season"="season,"posteam"="team","name"="name")
```

Data begins with the 2006 NFL season and comes from [Pro Football Reference](https://www.pro-football-reference.com). 

Columns:
- `season`: The season the player was on the roster.
- `team`: The team whose roster the player was on.
- `full_name`: The name of the selected player.
- `name`: The name of the selected player in the same format as NFL play-by-play data. This will *usually* match nflscrapR name fields, but not for every player.
- `playerid`: The ID of the selected player as used by Pro Football Reference. If `NA`, the player was not assigned an ID by Pro Football Reference, which normally indicates they never played in an NFL game.
- `side`: The side of the ball the player plays on. Can be:
   - For offense, `O`
   - For defense, `D` 
   - For special teams, `S`
   - If `position` is `NA`, this will be also.
- `category`: The category of position the player plays in.
   - For offense, this can be `OL`, `QB`, `RB`, `TE`, or `WR`.
   - For defense, this can be `DB`, `DL`, `ED` (edge), or `LB`. Note that `ED` is only used from 2019 forward.
   - For special teams, this can be `K`, `KR`, `LS`, `P`, or `ST` (generic special teams).
   - If `position` is `NA`, this will be also.
- `position`: The NFL position the selected player plays as reported by Pro Football Reference. If `NA`, Pro Football Reference did not record a position.
- `games`: Number of regular season games the player played in that season.
- `starts`: Number of regular season games the player started that season.
- `years`: Number of prior seasons of NFL experience the player has. Is `0` for rookies.
- `av`: The player's Approximate Value that year, as [defined by the Pro Football Reference metric](https://www.pro-football-reference.com/blog/index37a8.html)

<a name="standings"/>

## Standings

To import and join to nflscrapR data (for the offense):

``` r
standings <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/standings.csv")
plays <- plays %>%
  inner_join(standings,by=c("season"="season","posteam"="team")
```

Data begins with the 2002 NFL season. This CSV should be updated within minutes of a game completing.

Columns:
- `season`: The year of the NFL season. This reperesents the whole season, so regular season games that happen in January as well as playoff games will occur in the year after this number.
- `conf`: The conference the team is in. This will be either `AFC` or `NFC`.
- `division`: The division the team is in. This will be the value of `conf` followed by either `East`, `North`, `South`, or `West`.
- `team`: The team.
- `wins`: The number of games the team won in the regular season.
- `losses`: The number of games the team lost in the regular season.
- `ties`: The number of games the team tied in the regular season.
- `pct`: The win rate of the team in the regular season. Equals `(wins + 0.5 * ties) / (wins + losses + ties)`.
- `div_rank`: This is where this team ranks compared to the other teams in the division based on regular season games only. Will be a number 1-4. If the teams have identical `pct` values, NFL tiebreakers are applied.
- `scored`: The number of points the team has scored in regular season games.
- `allowed`: The number of points the team has allowed to be scored on them in regular season games.
- `net`: Net points scored in regular season games. Equals `scored - allowed`.
- `sov`: As used in NFL tiebreakers, strength of victory, defined as the combined win rates for teams this team has beaten.
- `sos`: As used in NFL tiebreakers, strength of schedule, defined as the combined win rates for teams this team has played.
- `seed`: The seed earned by the team in its conference for playoff games. Is `NA` for teams which do not make the playoffs.
- `playoff`: The outcome of the team's playoff run. Is `NA` for teams which do not make the playoffs, otherwise will be one of `LostWC`, `LostDV`, `LostCC`, `LostSB`, or `WonSB`.
