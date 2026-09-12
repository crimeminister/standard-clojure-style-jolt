(ns standard-clojure-style.parser
  (:require
   [jolt.continuations :as c]))

;; -----------------------------------------------------------------------------
;; ID Generator

(def ^:private id-counter (atom 0))

(defn create-id []
  (swap! id-counter inc))

;; -----------------------------------------------------------------------------
;; Node Constructor

(defn make-node
  [{:keys [children endIdx id name startIdx text]}]
  {:id (or id (create-id))
   :startIdx startIdx
   :endIdx endIdx
   :name name
   :text text
   :children children
   :_origColIdx -1
   :_printedColIdx -1
   :_printedLineIdx -1
   :_wasSlurpedUp false})

;; -----------------------------------------------------------------------------
;; Parser Combinators

(defn append-children [children-vec node]
  (cond
    (and (string? (:name node)) (not= (:name node) ""))
    (conj children-vec node)

    (vector? (:children node))
    (reduce append-children children-vec (:children node))

    :else children-vec))

(declare get-parser)

(defn Named [opts]
  (let [p-ref (:parser opts)
        target-name (:name opts)]
    {:name target-name
     :parse
     (fn [txt pos]
       (let [p (get-parser p-ref)
             node ((:parse p) txt pos)]
         (cond
           (nil? node) nil
           (not (string? (:name node)))
           (assoc node :name target-name)
           :else
           (make-node {:children [node]
                       :endIdx (:endIdx node)
                       :name target-name
                       :startIdx (:startIdx node)}))))}))

(defn AnyChar [opts]
  (let [node-name (:name opts)]
    {:name node-name
     :parse
     (fn [^String txt pos]
       (let [txt-len (.length txt)]
         (if (< pos txt-len)
           (make-node {:endIdx (inc pos)
                       :name node-name
                       :startIdx pos
                       :text (subs txt pos (inc pos))})
           nil)))}))

(defn Char [opts]
  (let [node-name (:name opts)
        target-char (str (:char opts))
        target-ch (.charAt target-char 0)]
    {:name node-name
     :isTerminal true
     :char target-char
     :parse
     (fn [^String txt pos]
       (let [txt-len (.length txt)]
         (if (and (< pos txt-len)
                  (= (.charAt txt pos) target-ch))
           (make-node {:endIdx (inc pos)
                       :name node-name
                       :startIdx pos
                       :text target-char})
           nil)))}))

(defn NotChar [opts]
  (let [node-name (:name opts)
        target-char (str (:char opts))
        target-ch (.charAt target-char 0)]
    {:name node-name
     :isTerminal true
     :char target-char
     :parse
     (fn [^String txt pos]
       (let [txt-len (.length txt)]
         (if (< pos txt-len)
           (let [ch (.charAt txt pos)]
             (if (not= ch target-ch)
               (make-node {:endIdx (inc pos)
                           :name node-name
                           :startIdx pos
                           :text (subs txt pos (inc pos))})
               nil))
           nil)))}))

(defn StringParser [opts]
  (let [node-name (:name opts)
        ^String target-str (:str opts)
        target-len (.length target-str)]
    {:name node-name
     :parse
     (fn [^String txt pos]
       (let [txt-len (.length txt)]
         (if (<= (+ pos target-len) txt-len)
           (loop [i 0]
             (if (< i target-len)
               (if (= (.charAt target-str i) (.charAt txt (+ pos i)))
                 (recur (inc i))
                 nil)
               (make-node {:endIdx (+ pos target-len)
                           :name node-name
                           :startIdx pos
                           :text target-str})))
           nil)))}))

(defn Regex [opts]
  (let [node-name (:name opts)
        re (:regex opts)]
    {:name node-name
     :parse
     (fn [^String txt pos]
       (let [txt-len (.length txt)]
         (if (< pos txt-len)
           (let [remaining (- txt-len pos)
                 chunk-len 2048
                 sub (if (<= remaining chunk-len)
                       (subs txt pos)
                       (subs txt pos (+ pos chunk-len)))
                 m (re-find re sub)]
             (if m
               (let [matched-str (if (vector? m) (first m) m)
                     matched-len (.length ^String matched-str)]
                 (if (and (= matched-len chunk-len) (> remaining chunk-len))
                   (let [full-sub (subs txt pos)
                         full-m (re-find re full-sub)]
                     (if full-m
                       (let [f-str (if (vector? full-m) (first full-m) full-m)]
                         (make-node {:endIdx (+ pos (.length ^String f-str))
                                     :name node-name
                                     :startIdx pos
                                     :text f-str}))
                       nil))
                   (make-node {:endIdx (+ pos matched-len)
                               :name node-name
                               :startIdx pos
                               :text matched-str})))
               nil))
           nil)))}))

(defn SeqParser [opts]
  (let [node-name (:name opts)
        parser-refs (:parsers opts)]
    {:name node-name
     :isTerminal false
     :parse
     (fn [txt pos]
       (loop [idx 0
              children []
              end-idx pos]
         (if (< idx (count parser-refs))
           (let [p (get-parser (nth parser-refs idx))
                 node ((:parse p) txt end-idx)]
             (if node
               (recur (inc idx) (append-children children node) (:endIdx node))
               nil))
           (make-node {:children children
                       :endIdx end-idx
                       :name node-name
                       :startIdx pos}))))}))

(defn Choice [opts]
  (let [parser-refs (:parsers opts)]
    {:parse
     (fn [txt pos]
       (c/letcc [escape]
         (loop [idx 0]
           (when (< idx (count parser-refs))
             (let [p (get-parser (nth parser-refs idx))
                   node ((:parse p) txt pos)]
               (if node
                 (escape node)
                 (recur (inc idx))))))))}))

(defn Repeat [opts]
  (let [node-name (:name opts)
        parser-ref (:parser opts)
        min-matches (or (:minMatches opts) 0)]
    {:parse
     (fn [txt pos]
       (let [p (get-parser parser-ref)]
         (loop [end-idx pos
                children []]
           (let [node ((:parse p) txt end-idx)]
             (if node
               (recur (:endIdx node) (append-children children node))
               (if (>= (count children) min-matches)
                 (make-node {:children children
                             :endIdx end-idx
                             :name (when (and (string? node-name) (> end-idx pos)) node-name)
                             :startIdx pos})
                 nil))))))}))

(defn Optional [parser-ref]
  {:parse
   (fn [txt pos]
     (let [p (get-parser parser-ref)
           node ((:parse p) txt pos)]
       (if (and node (string? (:text node)) (not= (:text node) ""))
         node
         (make-node {:startIdx pos :endIdx pos}))))})

;; -----------------------------------------------------------------------------
;; Grammar Definition

(def whitespace-commons " ,\n\r\t\f")
(def whitespace-unicodes "\u000B\u001C\u001D\u001E\u001F\u2028\u2029\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2008\u2009\u200a\u205f\u3000")
(def whitespace-chars (str whitespace-commons whitespace-unicodes))

(def token-head-chars "()\\[\\]{}\"@~^;`#'")
(def token-tail-chars "()\\[\\]{}\"@^;`")

(def token-re-str (str "[^" token-head-chars whitespace-chars "][^" token-tail-chars whitespace-chars "]*"))
(def char-re-str "\\\\[()\\[\\]{}\"@^;`, ]")

(def whitespace-char-set (set whitespace-chars))

(def parsers-registry (atom {}))

(defn get-parser [p]
  (cond
    (map? p) p
    (string? p)
    (or (get @parsers-registry p)
        (throw (Exception. (str "getParser error: could not find parser: " p))))
    (keyword? p)
    (or (get @parsers-registry (name p))
        (throw (Exception. (str "getParser error: could not find parser: " p))))
    :else
    (throw (Exception. (str "getParser error: invalid parser spec: " p)))))

(defn register-parser! [k p]
  (swap! parsers-registry assoc (name k) p)
  p)

(defn init-parsers! []
  (reset! parsers-registry {})

  (register-parser! "string"
    (SeqParser
      {:name "string"
       :parsers [(Regex {:name ".open" :regex #"^#?\""})
                 (Optional (Regex {:name ".body" :regex #"^([^\"\\]+|\\.)+"}))
                 (Optional (Char {:name ".close" :char "\""}))]}))

  (register-parser! "token"
    (Regex {:name "token"
            :regex (re-pattern (str "^(##)?(" char-re-str "|" token-re-str ")"))}))

  (register-parser! "_ws"
    (Regex {:name "whitespace"
            :regex (re-pattern (str "^[" whitespace-chars "]+"))}))

  (register-parser! "comment"
    (Regex {:name "comment"
            :regex #"^;[^\n]*"}))

  (register-parser! "discard"
    (SeqParser
      {:name "discard"
       :parsers [(StringParser {:name "marker" :str "#_"})
                 (Repeat {:parser "_gap"})
                 (Named {:name ".body" :parser "_form"})]}))

  (register-parser! "braces"
    (SeqParser
      {:name "braces"
       :parsers [(Choice
                   {:parsers [(Char {:name ".open" :char "{"})
                              (StringParser {:name ".open" :str "#{"})
                              (StringParser {:name ".open" :str "#::{"})
                              (Regex {:name ".open" :regex #"^#:{1,2}[a-zA-Z][a-zA-Z0-9.-_]*\{"})]})
                 (Repeat
                   {:name ".body"
                    :parser (Choice {:parsers ["_gap" "_form" (NotChar {:name "error" :char "}"})]})})
                 (Optional (Char {:name ".close" :char "}"}))]}))

  (register-parser! "brackets"
    (SeqParser
      {:name "brackets"
       :parsers [(Char {:name ".open" :char "["})
                 (Repeat
                   {:name ".body"
                    :parser (Choice {:parsers ["_gap" "_form" (NotChar {:name "error" :char "]"})]})})
                 (Optional (Char {:name ".close" :char "]"}))]}))

  (register-parser! "parens"
    (SeqParser
      {:name "parens"
       :parsers [(Regex {:name ".open" :regex #"^(#\?@|#\?|#=|#)?\("})
                 (Repeat
                   {:name ".body"
                    :parser (Choice {:parsers ["_gap" "_form" (NotChar {:name "error" :char ")"})]})})
                 (Optional (Char {:name ".close" :char ")"}))]}))

  (register-parser! "_gap"
    {:parse
     (fn [^String txt pos]
       (if (< pos (.length txt))
         (let [ch (.charAt txt pos)]
           (cond
             (contains? whitespace-char-set ch)
             ((:parse (get-parser "_ws")) txt pos)
             (= ch \;)
             ((:parse (get-parser "comment")) txt pos)
             (= ch \#)
             ((:parse (get-parser "discard")) txt pos)
             :else nil))
         nil))})

  (register-parser! "meta"
    (SeqParser
      {:name "meta"
       :parsers [(Repeat
                   {:minMatches 1
                    :parser (SeqParser
                              {:parsers [(Regex {:name ".marker" :regex #"^#?\^"})
                                         (Repeat {:parser "_gap"})
                                         (Named {:name ".meta" :parser "_form"})
                                         (Repeat {:parser "_gap"})]})})
                 (Named {:name ".body" :parser "_form"})]}))

  (register-parser! "wrap"
    (SeqParser
      {:name "wrap"
       :parsers [(Regex {:name ".marker" :regex #"^(@|'|`|~@|~|#')"})
                 (Repeat {:parser "_gap"})
                 (Named {:name ".body" :parser "_form"})]}))

  (register-parser! "tagged"
    (SeqParser
      {:name "tagged"
       :parsers [(Char {:char "#"})
                 (Repeat {:parser "_gap"})
                 (Named {:name ".tag" :parser "token"})
                 (Repeat {:parser "_gap"})
                 (Named {:name ".body" :parser "_form"})]}))

  (let [hash-form (Choice {:parsers ["token" "string" "parens" "braces" "wrap" "meta" "tagged"]})]
    (register-parser! "_hashForm" hash-form)
    (register-parser! "_form"
      {:parse
       (fn [^String txt pos]
         (if (< pos (.length txt))
           (let [ch (.charAt txt pos)]
             (cond
               (= ch \() ((:parse (get-parser "parens")) txt pos)
               (= ch \[) ((:parse (get-parser "brackets")) txt pos)
               (= ch \{) ((:parse (get-parser "braces")) txt pos)
               (= ch \") ((:parse (get-parser "string")) txt pos)
               (or (= ch \@) (= ch \') (= ch \`) (= ch \~))
               ((:parse (get-parser "wrap")) txt pos)
               (= ch \^) ((:parse (get-parser "meta")) txt pos)
               (= ch \#) ((:parse hash-form) txt pos)
               :else ((:parse (get-parser "token")) txt pos)))
           nil))}))

  (register-parser! "source"
    (Repeat
      {:name "source"
       :parser (Choice {:parsers ["_gap" "_form" (AnyChar {:name "error"})]})}))
  nil)

;; Initialize once on load
(init-parsers!)

(defn parse [^String input-txt]
  ((:parse (get-parser "source")) input-txt 0))
