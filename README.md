# Description

This is the draw sheet editor for Curling I/O.

## Requirements:

1. Accept JSON data from the server representing the current schedule assignments, available games that can be assigned, and the number of sheets.
2. A draw schedule can have a large number of draws, sheets, and games. Editing needs to be performant. Native JS or jQuery w/ standard selects is very slow.
3. When a game is selected it needs to be removed from the pool of available games.
4. When a game is de-selected it needs to be added back to the pool of available games.
5. Datalists w/ inputs allow you to enter any value, and it might not be a valid game. Before sending the updated schedule to the server, invalid values needs to be removed.
6. Post JSON data to the server on save.

## Running It

```
yarn
yarn start
```
