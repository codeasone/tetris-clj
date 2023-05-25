# tetris-clj

[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.codeasone/tetris-clj.svg)](https://clojars.org/org.clojars.codeasone/tetris-clj) [![Test](https://github.com/codeasone/tetris-clj/actions/workflows/pipeline.yml/badge.svg?branch=main)](https://github.com/codeasone/tetris-clj/actions/workflows/pipeline.yml)

Tetris game logic.

## Purpose

The two motivations for creating this library were:

1. Evaluating `fulcro/guardrails` whilst developing something non-trivial (all the `tetris.logic` functions use `>defn`)

2. Creating a unit-tested and repurposable `tetris.logic` engine that I can use when building fully-fledged games using various Clojure-based UI frameworks and deployment targets (e.g. web, desktop, mobile)

## API

The main API function is `tetris.logic/handle-events` which takes a `::game-state` and a sequence of `::game-events`, and returns a new `::game-state`.

Optionally a `play-next-tetrimino-fn` can be passed to allow the changes that happen when a new tetrimino enters play to be overridden e.g. to support debouncing strategies or grace-periods specific to a particular embodiment of the game.

## Concepts

### The Grid

The `game-grid` consists of 24 rows and 10 columns. The `visible-grid` is 20 rows indexed as `(row, col)` where row `0` corresponds to the first visible row in the grid.

Four `lead-in` rows exist with row indices `-1..-4`. When a new tetrimino enters play, it resides at a random column offset within the `lead-in` area, then automatically enters the `visible-grid` according to the cadence of the game.

Tetriminos are strictly contained by the `game-grid`. They cannot extend beyond the left and right edges, or travel beyond the `baseline` of the game-grid.

### Cells

The `game-grid` consists of cells that are either empty `0` or contain a number `1..7` which represents the presence of a tetrimino cell at that location.

### Tetriminos

There are seven different `tetrimino-shapes`. Each tetrimino is defined as a vector of its unique rotational variants. E.g.

```clojure
{...

 :tetrimino/T [[[0 3 0]
                [3 3 3]]
               [[3 0]
                [3 3]
                [3 0]]
               [[3 3 3]
                [0 3 0]]
               [[0 3]
                [3 3]
                [0 3]]]
 ...
}
```

The component cells of each tetrimino are given a distinct number. For the case of `:tetrimino/T` that number is 3.

### Events

The only way the `::game-state` changes is in response to `::game-events`.

```clojure
(s/def ::game-event #{::move-left
                      ::move-right
                      ::move-down
                      ::rotate
                      ::drop})
```

### Game Cadence, Scoring, and Levels

Points are earned when rows filled with tetrimino cells are cleared. `100` pts are earned for every line cleared, up to a maximum of four lines and `400` pts.

The game cadence is initially one `::move-down` per `1000ms`, this corresponds to a level `1` of difficulty.

This level increments every time an additional `difficulty-increment-in-pts` are earned, which is currently hard-wired to `1000` pts.

### Peaks

During game play varying numbers of `tetriminos` will have accumulated into the `game-grid`. The `peaks` are 10 row indices corresponding to the 10 columns in the game grid, each representing the row index of the highest cell greater than the current playback `row` occupied by a tetrminino cell.

This information is essential for determining when the cells of tetrimino in play are adjacent to existing tetrimino cells.

Within a virgin `game-grid` that contains no tetriminos, the `peaks` are `[20,20,20,20,20,20,20,20,20,20]`, ensuring that tetriminos never extend beyond the `baseline` of the `game-grid`.

### Game Over

A game is over when the current player position (row) is on the first line of the `visible-grid` or within the `lead-in` area i.e. `(<= row 0)` *and* the current tetrmino piece is adjacent to `peaks` in the grid.

## Development

```sh
tetris-clj (main) bb tasks
The following tasks are available:

deps:tree
lint
test:coverage
test:watch
```

## See it in action...

A simple example of usage can be found at [tetris-reagent](https://github.com/codeasone/tetris-reagent)
