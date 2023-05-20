(ns tasks.lint
  (:require [babashka.process :refer [check destroy process]]))

(defn all []
  (-> (process ["clojure" "-M:dev" "-m" "clj-kondo.main" "--lint" "src" "test"]
               {:shutdown destroy
                :out :inherit})
      check))
