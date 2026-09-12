(ns standard-clojure-style.parse-ns-test
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [standard-clojure-style.parse-ns :as ns-parser]
   [standard-clojure-style.parser :as p]))

(defn run-test-case [t]
  (let [t-name (get t "name")
        t-input (get t "input")
        t-expected (get t "expected")]
    (try
      (let [tree (p/parse t-input)
            flat (ns-parser/flatten-tree tree)
            parsed-ns (ns-parser/parse-ns flat)]
        (if (get t-expected "parsingShouldError")
          {:pass? false :name t-name :expected t-expected :actual parsed-ns :reason "Expected exception but succeeded"}
          (if (= parsed-ns t-expected)
            {:pass? true}
            {:pass? false :name t-name :expected t-expected :actual parsed-ns :reason "Output mismatch"})))
      (catch Exception e
        (if (get t-expected "parsingShouldError")
          (let [expected-err (get t-expected "errorMessage")
                actual-err (.getMessage e)]
            (if (and expected-err actual-err (str/includes? actual-err expected-err))
              {:pass? true}
              {:pass? false :name t-name :expected t-expected :actual (.getMessage e) :reason "Error message mismatch"}))
          {:pass? false :name t-name :expected t-expected :actual (.getMessage e) :reason "Unexpected exception"})))))

(defn run-tests []
  (let [tests (edn/read-string (slurp "test_cases/parse_ns_tests.edn"))]
    (loop [tests tests
           passed 0
           failed 0
           failed-list []]
      (if (empty? tests)
        (do
          (println (format "ParseNs Tests: %d passed, %d failed" passed failed))
          (when (seq failed-list)
            (println "Failed tests:")
            (doseq [{:keys [name expected actual reason]} failed-list]
              (println "----" name "----")
              (when reason (println "Reason:" reason))
              (println "Expected:")
              (prn expected)
              (println "Actual:")
              (prn actual))))
        (let [res (run-test-case (first tests))]
          (if (:pass? res)
            (recur (rest tests) (inc passed) failed failed-list)
            (recur (rest tests) passed (inc failed) (conj failed-list res))))))))

(defn -main [& _]
  (run-tests))
