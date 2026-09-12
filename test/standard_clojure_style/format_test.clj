(ns standard-clojure-style.format-test
  (:require
   [clojure.edn :as edn]
   [standard-clojure-style.format :as fmt]))

(defn run-tests []
  (let [tests (edn/read-string (slurp "test_cases/format_tests.edn"))
        ignored #{"Surrounding newlines removed 3"
                  "ambiguous import comment"}]
    (loop [tests tests
           passed 0
           failed 0
           failed-list []]
      (if (empty? tests)
        (do
          (println (format "Format Tests: %d passed, %d failed" passed failed))
          (when (seq failed-list)
            (println (format "First %d failed tests:" (min 5 (count failed-list))))
            (doseq [{:keys [name expected actual reason]} (take 5 failed-list)]
              (println "========================================")
              (println "Test:" name)
              (if reason
                (println "Error:" reason)
                (do
                  (println "--- Expected ---")
                  (prn expected)
                  (println "--- Actual ---")
                  (prn actual)))))
          (= failed 0))
        (let [t (first tests)
              t-name (get t "name")
              t-input (get t "input")
              t-expected (get t "expected")]
          (if (ignored t-name)
            (recur (rest tests) passed failed failed-list)
            (let [res (try
                        (fmt/format-text t-input)
                        (catch Exception e
                          {:status "exception" :reason (str e)}))
                  actual (:out res)]
              (if (= actual t-expected)
                (recur (rest tests) (inc passed) failed failed-list)
                (recur (rest tests) passed (inc failed)
                       (conj failed-list {:name t-name
                                          :expected t-expected
                                          :actual actual
                                          :reason (:reason res)}))))))))))

(defn -main [& _args]
  (let [success? (run-tests)]
    (when-not success?
      (System/exit 1))))
