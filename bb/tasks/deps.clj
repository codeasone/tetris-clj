(ns tasks.deps
  (:require [babashka.process :refer [check destroy process]]))

(defn tree []
  (-> (process ["clj" "-X:deps" "tree" ":aliases" "[:test]"]
               {:shutdown destroy
                :out :inherit
                :err :inherit})
      check))
