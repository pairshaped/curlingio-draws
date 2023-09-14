Problem: When you change the datalist it doesn't rerender the existing one that is open, since it seems to be a snapshop. This means we need a double click to render the correct available games for the draw.

1. Datalist with a javascript hook that triggers a second click to render it properly. Might be able to just prevent event propogation on the JS / port side to get this working. This could actually work with Browser.Dom.
2. Custom div based dropdowns. Works pretty well at first, but has positioning and sizing woes.
3. Show / hide actual select elements. This could be interesting. When a draw sheet is clicked, the click target is hidden and the select is shown? Might need to then trigger a click on the select to open it up without needing a second user initiated click.
