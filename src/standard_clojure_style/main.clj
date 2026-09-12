(ns standard-clojure-style.main
  (:require
   [standard-clojure-style.cli :as cli])
  (:gen-class))

(defn -main [& args]
  (let [exit-code (cli/run-cli args)]
    (System/exit exit-code)))
