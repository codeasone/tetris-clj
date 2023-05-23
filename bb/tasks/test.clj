(ns tasks.test
  (:require [babashka.process :refer [check destroy process destroy]]))

(defn coverage []
  (-> (process ["clojure" "-M:test"]
               {:shutdown destroy
                :out :inherit})
      check))

(defn watch []
  (-> (process ["clojure" "-M:test" "--focus-meta" ":focus" "--watch"]
               {:shutdown destroy
                :in :inherit
                :out :inherit})
      check))
