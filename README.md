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
- [x] Implement a reset to undo changes since last save. (re-fetch JSON)
- [x] Hide attendance for events that aren't tracking it.
- [x] Highlight draw label, starts at, sheet, attendance inputs if they aren't valid.
- [x] Validate draw sheet input is game assigned that would not result it a team playing more than once in the same draw.
- [x] Validate draw starts at isn't blank.
- [x] Validate draw starts at is unique across all draws.
- [x] Validate draw label isn't blank.
- [x] Validate draw label is unique across all draws.
- [x] Validate draw attendance is not a negative number.
- [x] Validate draw attendance is a reasonable number (not too high).
- [ ] Validate draw sheets on change.
- [ ] Get rid of the validate button in favour of real time validation and a disabled save button.
- [ ] Implement save / serilization

## Roadmap Features

- [ ] Draw Schedule generation / regeneration based on games, dates, sheets, exclusions, etc.
