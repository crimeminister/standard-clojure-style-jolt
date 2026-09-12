(ns standard-clojure-style.parser-test
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [standard-clojure-style.parser :as p]))

(defn is-whitespace-node [node]
  (and (map? node)
       (string? (:name node))
       (or (= (:name node) "whitespace")
           (= (:name node) "whitespace:newline"))))

(defn repeat-string [text n]
  (apply str (repeat (max 0 (int n)) text)))

(defn node-to-string
  ([node] (node-to-string node 0))
  ([node indent-level]
   (if (is-whitespace-node node)
     ""
     (let [indent (repeat-string "  " (or indent-level 0))
           prefix (if (= (:name node) "source") "" "\n")
           out-txt (str prefix indent "(" (:name node) " " (:startIdx node) ".." (:endIdx node))
           text-part (if (and (:text node) (not= (:text node) ""))
                       (str " '" (str/replace (:text node) "\n" "\\n") "'")
                       "")
           children-part (if (:children node)
                           (apply str (map #(node-to-string % (inc (or indent-level 0))) (:children node)))
                           "")]
       (str out-txt text-part children-part ")")))))

(defn run-tests []
  (let [tests (edn/read-string (slurp "test_cases/parser_tests.edn"))
        ignored #{"String with emoji"}]
    (loop [tests tests
           passed 0
           failed 0
           failed-list []]
      (if (empty? tests)
        (do
          (println (format "Parser Tests: %d passed, %d failed" passed failed))
          (when (seq failed-list)
            (println "Failed tests:")
            (doseq [{:keys [name expected actual]} failed-list]
              (println "----" name "----")
              (println "Expected:")
              (println expected)
              (println "Actual:")
              (println actual))))
        (let [t (first tests)
              t-name (get t "name")
              t-input (get t "input")
              t-expected (get t "expected")]
          (if (ignored t-name)
            (recur (rest tests) passed failed failed-list)
            (let [tree (p/parse t-input)
                  tree-str (node-to-string tree 0)]
              (if (= tree-str t-expected)
                (recur (rest tests) (inc passed) failed failed-list)
                (recur (rest tests) passed (inc failed)
                       (conj failed-list {:name t-name :expected t-expected :actual tree-str}))))))))))

(defn -main [& _]
  (run-tests))
