# OTHELLO

This is a Clojure port of Peter Norvig's Othello programme in "Paradigms of Artificial Intelligence"

In addition to porting to Clojure and taking advantage of the lack of a need to copy the board state on each move (immutable data)
it also adds a command line and GUI, using SeeSaw, interface.

The command line interface allows a human to play.

The GUI version at the moment only allows you to watch two AI's face-off.

## Usage

### Command-line:

From the othello.command-line NS, and assuming you have othello.core and othello.strategies loaded
(depending on your repl these may be included as part of evaluating the othello.command-line ns).

To run from the command line simply pass in two strategy functions (defined in othello.strategies) for example:

(othello (maximiser weighted-squares)
         (maximiser weighted-squares))

(othello (alpha-beta-searcher 9 count-difference)
         (alpha-beta-searcher 9 weighted-squares))

To have one, or both, of the players as human replace the ai strategy with the 'human' strategy defined in othello.command-line.

For example for a human to play black against a white random-strategy AI;

(othello human random-strategy)

### GUI

From the othello.gui NS, and assuming you have othello.core, othello.strategies (and othello,.command line: I use some of the print functions in
there for logging) loaded then:

(othello-gui starting-position random-strategy random-strategy)

(othello-gui starting-position (minimax-searcher 3 weighted-squares) (minimax-searcher 3 weighted-squares))

Note that this version does not use a global 'current board' so it has to be passed in. The command line version will be similarly updated shortly.

Passing in a the 'human' strategy will cause prompts to be displayed at the command line, not the GUI. Note that the 'human' strategy is part of the
command-line NS, not the strategy ns and is not by default loaded into the othello.gui NS.

## License

Copyright © 2014 Edward Kenworthy

Distributed under the Eclipse Public License, the same as Clojure.
