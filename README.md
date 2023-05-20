# tetris-clj

Tetris game logic.

I wanted to evaluate how `guardrails` could enhance my workflow on a non-trivial
project with a number of interesting edge cases, so I chose to tackle the internal
logic of a `tetris` game.

A secondary goal was to develop a reusable `tetris.logic` library I could use to
compare and constrast various UI libraries and deployment targets.

The first consumer of `tetris-clj` is [tetris-reagent](https://github.com/codeasone/tetris-reagent).

It provide a basic web-based front-end and adds a basic multi-player option using web-sockets.
