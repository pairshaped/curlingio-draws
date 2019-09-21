# Description

This is the draw sheet editor for Curling I/O.

## Requirements:

1. Accept JSON data from the server representing the current schedule assignments, available games that can be assigned, and the number of sheets.
2. A draw schedule can have a large number of draws, sheets, and games. Editing needs to be performant. Native JS or jQuery w/ standard selects is very slow.
3. When a game is selected it needs to be removed from the pool of available games.
4. When a game is de-selected it needs to be added back to the pool of available games.
5. Datalists w/ inputs allow you to enter any value, and it might not be a valid game. Before sending the updated schedule to the server, invalid values needs to be removed.
6. Post JSON data to the server on save.

## Installing Dependencies

```
yarn
```

## Running It

```
yarn start
```

## TODO

- [x] Highlight fields that have changed since last save.
- [ ] Implement a reset to undo changes since last save. (re-fetch JSON)
- [ ] User Elm CSS instead of inline styles.
- [ ] Swap Draw ID for Draw Position / Index. ID will be a reference to the backing server / database.
- [ ] Look into using an Array instead of a List for draws
- [ ] Look into using an Array instead of a List for draw sheets
- [ ] Potential helper method for decoding data in the update
- [ ] Implement save / serilization
- [ ] Hide attendance for events that don't need to track it.
- [ ] Prevent a team from playing more than once in the same draw.
- [ ] Draw Schedule generation / regeneration based on games, dates, sheets, exclusions, etc.
