# Draw Schedule Editor for [Curling I/O](https://curling.io)

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

## Roadmap

Draw Schedule generation / regeneration based on games, dates, sheets, exclusions, etc.

## For Contributors

### Installing Dependencies

We use elm and elm-live for development. You can install these via npm.

```
npm install
```

### Running It

Edit dev.html and configure the application's parameters for your environment. Then run it:

```
npm start
```

### Production Deployment

Make sure you have uglify-js installed to compress the production js.
```
npm install -g uglify-js
```

Compile and optimize for production using:

```
./prod.sh
```

## Source
<https://github.com/pairshaped/curlingio-draws>

## Copyright and License

[See LICENSE.md](LICENSE.md)
