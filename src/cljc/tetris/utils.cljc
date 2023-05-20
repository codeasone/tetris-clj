(ns tetris.utils
  (:require [clojure.spec.alpha :as s]
            [com.fulcrologic.guardrails.core :refer [>defn]]))

(>defn select-from
  [coll indices]
  [(s/coll-of any?) set? => (s/coll-of any?)]
  (into [] (keep-indexed #(when (contains? indices %1) %2) coll)))

(>defn remove-from
  [coll indices]
  [(s/coll-of any?) set? => (s/coll-of any?)]
  (into [] (keep-indexed #(when (not (contains? indices %1)) %2) coll)))
