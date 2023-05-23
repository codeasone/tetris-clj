# tetris-clj

[![Test](https://github.com/codeasone/tetris-clj/actions/workflows/pipeline.yml/badge.svg?branch=main)](https://github.com/codeasone/tetris-clj/actions/workflows/pipeline.yml)

Tetris game logic.

I wanted to evaluate how [guardrails](https://github.com/fulcrologic/guardrails)
could enhance my workflow on a non-trivial project with a number of interesting edge
cases, so I chose to implement the logic of a `tetris` game.

A secondary goal was to develop a reusable library that can be used to compare
various React UI libraries and deployment targets (web, mobile, desktop).

## Development

```sh
tetris-clj (main) bb tasks
The following tasks are available:

deps:tree
lint
test:coverage
test:watch
```
