(ns standard-clojure-style.parser-scan-test
  "The parser scans its hot terminals by hand instead of matching regexes.
  This checks each scanner against the regex it replaces, on random inputs
  built from the characters those regexes care about."
  (:require
   [standard-clojure-style.parser :as p]))

(def alphabet
  (str "()[]{}\"@~^;`#'\\,: \n\r\t\f" p/whitespace-unicodes "abz09-_./?!<>=+*%&|$"))

;; a fixed-seed LCG, so a failure reproduces
(defn make-rng [seed]
  (let [state (atom seed)]
    (fn [n]
      (swap! state #(mod (+ (* % 6364136223846793005) 1442695040888963407) 9223372036854775807))
      (mod (quot @state 65536) n))))

(defn random-string [rng]
  (apply str (repeatedly (inc (rng 12)) #(.charAt ^String alphabet (rng (count alphabet))))))

(defn regex-end [re ^String s pos]
  (let [m (doto (re-matcher re s) (.region pos (.length s)))]
    (if (.lookingAt m) (+ pos (.length ^String (.group m))) -1)))

(defn scan-end [scan-fn ^String s pos]
  (let [end (scan-fn s pos (.length s))]
    (if (> end pos) end -1)))

(defn literals-end [parser ^String s pos]
  (if-let [node ((:parse parser) s pos)] (:endIdx node) -1))

(def cases
  [["token"
    (re-pattern (str "^(##)?(" p/char-re-str "|" p/token-re-str ")"))
    #(scan-end p/scan-token %1 %2)]
   ["whitespace"
    (re-pattern (str "^[" p/whitespace-chars "]+"))
    #(scan-end p/scan-whitespace %1 %2)]
   ["comment" #"^;[^\n]*" #(scan-end p/scan-comment %1 %2)]
   ["string open" #"^#?\""
    #(literals-end (p/Literals {:strs ["#\"" "\""]}) %1 %2)]
   ["parens open" #"^(#\?@|#\?|#=|#)?\("
    #(literals-end (p/Literals {:strs ["#?@(" "#?(" "#=(" "#(" "("]}) %1 %2)]
   ["meta marker" #"^#?\^"
    #(literals-end (p/Literals {:strs ["#^" "^"]}) %1 %2)]
   ["wrap marker" #"^(@|'|`|~@|~|#')"
    #(literals-end (p/Literals {:strs ["@" "'" "`" "~@" "~" "#'"]}) %1 %2)]])

(defn run-tests []
  (let [rng (make-rng 42)
        inputs (vec (repeatedly 4000 #(random-string rng)))
        failures (for [[case-name re scan] cases
                       s inputs
                       pos (range (count s))
                       :let [expected (regex-end re s pos)
                             actual (scan s pos)]
                       :when (not= expected actual)]
                   {:case case-name :input s :pos pos :expected expected :actual actual})
        failed (vec (take 20 failures))]
    (println (format "Scanner Tests: %d passed, %d failed"
                     (if (seq failed) 0 (count cases))
                     (count (distinct (map :case failed)))))
    (doseq [f failed]
      (prn f))))

(defn -main [& _]
  (run-tests))
