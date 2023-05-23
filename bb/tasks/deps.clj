(ns tasks.deps
  (:require [babashka.process :refer [check destroy process]]))

(defn tree []
  (-> (process ["clojure" "-X:deps" "tree" ":aliases" "[:test]"]
               {:shutdown destroy
                :out :inherit
                :err :inherit})
      check))
