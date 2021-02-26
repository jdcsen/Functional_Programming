Game Runner
-----------

The `run` game runner takes two programs to run to play against each
other.

GUI
---

Running `gui --interactive` runs a Santorini GUI in a mode that lets
you play the game. Note that the button label for the current player
changes from having an "X" to a checkmark when you can have completed
changes to the board that make a valid turn.

The `gui` program can also read from stdin to display a sequence
boards. If that input is the output of `run`, then the GUI shows a
record of the game. The `gui` program accepts a JSON strings on input
as a description of the board to follow, so sending it input intended
for `check-turn` causes `gui` to show pairs of pre- and post-turn
boards with the pre-turn board description at the bottom left.

You can both send input to `gui` and supply the ` --interactive` flag,
which lets you see a game history but also go back and retry manually
from some game step.

The GUI draws a red box around a player that is at level 3.

Demo Players
------------

The `play-random` and `play-search` players are a little more flexible
than the game protocol requires: they accept, at any time, either a
setup JSON array or a turn JSON board.
