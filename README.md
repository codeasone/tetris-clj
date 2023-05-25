# tetris-clj

[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.codeasone/tetris-clj.svg)](https://clojars.org/org.clojars.codeasone/tetris-clj) [![Test](https://github.com/codeasone/tetris-clj/actions/workflows/pipeline.yml/badge.svg?branch=main)](https://github.com/codeasone/tetris-clj/actions/workflows/pipeline.yml)

Tetris game logic.

## Purpose

The two motivations for creating this library were:

1. Evaluating `fulcro/guardrails` whilst developing something non-trivial - you'll
   notice that all the `tetris.logic` fns are `>defn`'s.

2. Creating a unit-tested and repurposable `tetris.logic` engine that I can use when building fully-fledged game applications using various Clojure-based UI frameworks and deployment targets (e.g. web, desktop, mobile)

# Usage

The main API function is `tetris.logic/handle-events` which takes a `::game-state` and a sequence of `::game-events`, and returns a new `::game-state`.

Optionally a `play-next-tetrimino-fn` can be passed to allow the changes that happen when a new tetrimino enters play to be overridden e.g. to support debouncing strategies or grace-periods specific to the calling application.

An really simple example of usage can be found at: https://github.com/codeasone/tetris-reagent

## Development

```sh
tetris-clj (main) bb tasks
The following tasks are available:

deps:tree
lint
test:coverage
test:watch
```
