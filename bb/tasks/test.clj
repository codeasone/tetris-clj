(ns tasks.test
  (:require [babashka.process :refer [check destroy process destroy]]))

(defn coverage []
  (-> (process ["clj" "-M:test"]
               {:shutdown destroy
                :out :inherit})
      check))

(defn watch []
  (-> (process ["clj" "-M:test" "--watch"]
               {:shutdown destroy
                :out :inherit})
      check))
