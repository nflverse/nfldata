# NFL Data Sets

Here is detailed information on each data set.

- [Standings](#standings)  
- [Games](#games)
- [Draft Picks](#draft_picks)
- [Draft Values](#draft_values)

<a name="standings"/>

## Standings

To import:

``` r
> standings <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/standings.csv")
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

<a name="games"/>

## Games

To import:

``` r
> games <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/games.csv")
```

Data begins with the 2006 NFL season. This CSV should be updated within minutes of a game completing. Does not include preseason.

Columns:
- `game_id`: The ID of the game as assigned by the NFL. Note that this value matches the `game_id` field in nflscrapR if you wish to join the data.
- `season`: The year of the NFL season. This reperesents the whole season, so regular season games that happen in January as well as playoff games will occur in the year after this number.
- `week`: The week of the NFL season the game occurs in. This will be 1-17 for the regular season, 18 for wildcard playoff games, 19 for divisional playoff games, 20 for conference championships and 21 for Super Bowls.
- `gameday`: The date on which the game occurred.
- `weekday`: The day of the week on which the game occcured.
- `gametime`: The kickoff time of the game. This is represented in 24-hour time and the Eastern time zone, regardless of what time zone the game was being played in.
- `visitor`: The visiting team.
- `v_score`: The number of points the visiting team scored. Is `NA` for games which haven't yet been played.
- `home`: The home team. Note that this contains the designated home team for games which no team is playing at home such as Super Bowls or NFL International games.
- `h_score`: The number of points the home team scored. Is `NA` for games which haven't yet been played.
- `location`: Either `Home` if the home team is playing in their home stadium, or `Neutral` if the game is being played at a neutral location. This still shows as `Home` for games between the Giants and Jets even though they share the same home stadium.
- `result`: The number of points the home team scored minus the number of points the visiting team scored. Equals `h_score - v_score`. Is `NA` for games which haven't yet been played.
- `total`: The sum of each team's score in the game. Equals `h_score + v_score`. Is `NA` for games which haven't yet been played.

<a name="draft_picks"/>

## Draft Picks

To import:

``` r
> draft_picks <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/draft_picks.csv")
```

Data begins with the 2000 season, does not include picks from the supplemental draft, and comes from the excellent [Pro Football Reference](https://www.pro-football-reference.com/).

- `season`: The season in which the draft occurred. This is the season after the draft, not the one before it, so this would represent the rookie year for drafted players.
- `team`: The team that drafted the player. This team may have had this pick originally, or traded for it.
- `round`: The round of the draft this pick occurred in.
- `pick`: The number of the pick.
- `position`: The NFL position the selected player plays.
- `playerid`: The ID of the selected player as used by Pro Football Reference. If `NA`, the player was not assigned an ID by Pro Football Reference, which normally indicates they never played in an NFL game.
- `name`: The name of the selected player.

<a name="draft_values"/>

## Draft Values

To import:

``` r
> draft_values <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/draft_values.csv")
```
Columns:
- `pick`: The pick number in the NFL draft.
- `stuart`: The value of the pick [according to Football Perspective's Chase Stuart](https://www.footballperspective.com/draft-value-chart/), based on [the AV metric from Pro Football Reference](https://www.pro-football-reference.com/blog/index37a8.html)
- `johnson`: The value of the pick according to the original pick value chart created by then-Cowboys coach Jimmy Johnson
- `hill`: The value of the pick [according to Rich Hill](https://www.patspulpit.com/2018/4/21/17256758/2018-nfl-draft-value-chart-rich-hill)

It's worth noting that the Stuart scale is attempting to measure how teams *should* value draft picks, while the Johnson and Hill scales are attempting to measure how teams *in practice* value draft picks, and that these are different questions. The Hill versions is obviously based on more recent data. Also note the systems are using different numerical scales, so you should only compare values *within* a scale, not compare, say, a Stuart value to a Johnson value (the latter will essentially always be higher).
