# NFL Data for Public Consumption

Here is detailed information on each data set and what each column means:

## standings.csv

Data begins with the 2002 NFL season.

- `season`: The year of the NFL season. This reperesents the whole season, so regular season games that happen in January as well as playoff games will occur in the year after this number.
- `conf`: The conference the team is in. This will be either `AFC` or `NFC`.
- `division`: The division the team is in. This will be the value of `conf` followed by either `East`, `North`, `South`, or `West`.
- `team`: The team.
- `wins`: The number of games the team won in the regular season.
- `losses`: The number of games the team lost in the regular season.
- `ties`: The number of games the team tied in the regular season.
- `pct`: The win rate of the team in the regular season. Equals `(wins + 0.5 * ties) / (wins + losses + ties)`.
- `div_rank`: This is where this team ranks compared to the other teams in the division based on regular season games only. If the teams have identical `pct` values, NFL tiebreakers are applied.
- `scored`: The number of points the team has scored in regular season games.
- `allowed`: The number of points the team has allowed to be scored on them in regular season games.
- `net`: Net points scored in regular season games. Equals `scored - allowed`.
- `sov`: As used in NFL tiebreakers, strength of victory, defined as the combined win rates for teams this team has beaten.
- `sos`: As used in NFL tiebreakers, strength of schedule, defined as the combined win rates for teams this team has played.
- `seed`: The seed earned by the team in its conference for playoff games. Is `NA` for teams which do not make the playoffs.
- `playoff`: The outcome of the team's playoff run. Is `NA` for teams which do not make the playoffs, otherwise will be one of `LostWC`, `LostDV`, `LostCC`, `LostSB`, or `WonSB`.

## games.csv

Data begins with the 2006 NFL season.

- `game_id`: The ID of the game as assigned by the NFL. Note that this value matches the `game_id` field in nflscrapR if you wish to join the data.
- `season`: The year of the NFL season. This reperesents the whole season, so regular season games that happen in January as well as playoff games will occur in the year after this number.
- `week`: The week of the NFL season the game occurs in. This will be 1-17 for the regular season, 18 for wildcard playoff games, 19 for divisional playoff games, 20 for conference championships and 21 for Super Bowls.
- `gameday`: The date on which the game occurred.
- `weekday`: The day of the week on which the game occcured.
- `gametime`: The time of the game. This is represented in 24 hour time and the Eastern time zone, regardless of what time zone the actual game was being played in.
- `visitor`: The visiting team.
- `v_score`: The number of points the visiting team scored. Is `NA` for games which haven't yet been played.
- `home`: The home team. Note that this contains the designated home team for games which no team is playing at home such as Super Bowl games or games played on a neutral site such as an NFL International Game.
- `h_score`: The number of points the home team scored. Is `NA` for games which haven't yet been played.
- `location`: Either `Home` if the home team is playing in their home stadium, or `Neutral` if the game is being played at a neutral location. This still shows as `Home` for games between the Giants and Jets even though they share the same home stadium.
- `result`: The number of points the home team scored minus the number of points the visiting team scored. Equals `h_score - v_score`. Is `NA` for games which haven't yet been played.
- `total`: The sum of each team's score in the game. Equals `h_score + v_score`. Is `NA` for games which haven't yet been played.
