# Draw Schedule Editor for Curling I/O

![Draw Schedule editor for Curling I/O](curlingio-draws.gif?raw=true "Draw Schedule editor for Curling I/O")

## Features:

* Great performance with large numbers of games and draws due to the native `datalist` element.
* Changes are highlighted in yellow / orange.
* Errors are highlighted in red.
* Games removed from the dropdown list once they've been scheduled, and added back to the list when they are removed from the schedule.
* Teams cannot be assigned more than once per draw (a team can't play more than once per draw).
* Labels are validated for uniqueness.
* Start times are validated for uniqueness and presence.
* The ability to discard any changes that haven't been saved.

## Requirements:

1. Accept JSON data from the server representing the current schedule assignments, available games that can be assigned, and the number of sheets.
2. A draw schedule can have a large number of draws, sheets, and games. Editing needs to be performant. Native JS or jQuery w/ standard selects is very slow.
3. When a game is selected it needs to be removed from the pool of available games.
4. When a game is de-selected it needs to be added back to the pool of available games.
5. Validate changes to label, starts at, draw sheet game selection (make sure teams aren't double booked), and attendance.
6. Save the draws back to the server.

## Roadmap

Draw Schedule generation / regeneration based on games, dates, sheets, exclusions, etc.

## Installing Dependencies

```
yarn
```

## Running It

```
yarn start
```

## Source
<https://github.com/pairshaped/curlingio-draws>

## Copyright and License

Draw Schedule Editor
Copyright (C) 2019 Curling I/O

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
