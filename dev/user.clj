(ns user
  (:require [shadow.cljs.devtools.api :as shadow]
            [shadow.cljs.devtools.server :as server]))

(def cljs-repl
  (fn []
    (server/start!)
    (shadow/watch :app)
    (shadow/nrepl-select :app)))
