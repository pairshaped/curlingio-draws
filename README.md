# Description

This is a playground to compare the performance of:

1. Many inputs sharing the same datalist options.
2. Many selects with embedded (repeated) options.

For both scenarios, the user must not be able to select an option that was already selected in one of the other inputs.

Note: The behaviour of datalists and selects are slightly different.
* With datalists you can search, but also they don't really clear out quite as well (you can to replace the text).
* With datalists we need to guard against random text being input.
* Datalists will show the value when selected instead of the text, therefore we're simply using the display text as the values and mapping them in the Options. This only works if the display text must be unique, otherwise you'll need to append (or prepend) the IDs to the text and match on that. If the IDs themselves are display friendly this would be a non-issue, but often that wouldn't be the case.

## Running It

Just use `elm reactor` then navigate to the two different implementations.

## TODO

Potentially add timed headless browser tests.
