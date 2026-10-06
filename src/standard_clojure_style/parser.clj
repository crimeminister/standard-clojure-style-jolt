(ns standard-clojure-style.parser)

;; -----------------------------------------------------------------------------
;; ID Generator

(def ^:private id-counter (atom 0))

(defn create-id []
  (swap! id-counter inc))

;; -----------------------------------------------------------------------------
;; Node Constructor

(defn make-node [name startIdx endIdx text children]
  {:id (create-id)
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
;; Character Helpers

;; Comparing chars with = or testing a char set with contains? costs tens of
;; times more than a case dispatch, and the scanners do it for every char of the
;; input.

(defmacro char-in?
  "True when ch is one of the chars of chars, a string or the name of a var
  holding one, as a case dispatch."
  [ch chars]
  (let [chars (if (symbol? chars) @(resolve chars) chars)]
    `(case ~ch ~(apply list (distinct chars)) true false)))

(defn starts-at?
  "True when txt holds s at pos."
  [^String txt ^String s pos]
  (let [n (.length s)]
    (and (<= (+ pos n) (.length txt))
         (loop [i 0]
           (or (>= i n)
               (and (identical? (.charAt s i) (.charAt txt (+ pos i)))
                    (recur (inc i))))))))

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

(defn parse-fn
  "The parse function of p-ref, resolved on first use. Grammar rules refer to
  each other by name before they are all registered, so resolution is lazy, and
  cached so the registry is not consulted again on every parse step."
  [p-ref]
  (let [resolved (volatile! nil)]
    (fn [txt pos]
      (let [f (or @resolved (vreset! resolved (:parse (get-parser p-ref))))]
        (f txt pos)))))

(defn Named [opts]
  (let [p (parse-fn (:parser opts))
        target-name (:name opts)]
    {:name target-name
     :parse
     (fn [txt pos]
       (let [node (p txt pos)]
         (cond
           (nil? node) nil
           (not (string? (:name node)))
           (assoc node :name target-name)
           :else
           (make-node target-name (:startIdx node) (:endIdx node) nil [node]))))}))

(defn AnyChar [opts]
  (let [node-name (:name opts)]
    {:name node-name
     :parse
     (fn [^String txt pos]
       (let [txt-len (.length txt)]
         (if (< pos txt-len)
           (make-node node-name pos (inc pos) (subs txt pos (inc pos)) nil)
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
                  (identical? (.charAt txt pos) target-ch))
           (make-node node-name pos (inc pos) target-char nil)
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
             (if-not (identical? ch target-ch)
               (make-node node-name pos (inc pos) (subs txt pos (inc pos)) nil)
               nil))
           nil)))}))

(defn StringParser [opts]
  (let [node-name (:name opts)
        ^String target-str (:str opts)
        target-len (.length target-str)]
    {:name node-name
     :parse
     (fn [^String txt pos]
       (when (starts-at? txt target-str pos)
         (make-node node-name pos (+ pos target-len) target-str nil)))}))

(defn Regex [opts]
  (let [node-name (:name opts)
        re (:regex opts)]
    {:name node-name
     :parse
     (fn [^String txt pos]
       (let [txt-len (.length txt)]
         (when (< pos txt-len)
           ;; match in place: copying the rest of the input to anchor a regex
           ;; at pos costs more than the match itself
           (let [m (doto (re-matcher re txt) (.region pos txt-len))]
             (when (.lookingAt m)
               (let [matched-str (.group m)]
                 (make-node node-name pos (+ pos (.length ^String matched-str)) matched-str nil)))))))}))

;; Scan parsers: hand-written equivalents of the grammar's hot regexes. Each
;; returns the same node the regex would, and each scan-fn returns the end index
;; of a match starting at pos, or -1.

(defn Scan [opts]
  (let [node-name (:name opts)
        scan-fn (:scan opts)]
    {:name node-name
     :parse
     (fn [^String txt pos]
       (let [end-idx (scan-fn txt pos (.length txt))]
         (when (> end-idx pos)
           (make-node node-name pos end-idx (subs txt pos end-idx) nil))))}))

(defn Literals
  "Matches the first of strs found at pos, trying them in order like a regex
  alternation."
  [opts]
  (let [node-name (:name opts)
        strs (:strs opts)]
    {:name node-name
     :parse
     (fn [^String txt pos]
       (loop [strs strs]
         (when-let [^String s (first strs)]
           (if (starts-at? txt s pos)
             (make-node node-name pos (+ pos (.length s)) s nil)
             (recur (next strs))))))}))

(defn SeqParser [opts]
  (let [node-name (:name opts)
        parsers (mapv parse-fn (:parsers opts))
        num-parsers (count parsers)]
    {:name node-name
     :isTerminal false
     :parse
     (fn [txt pos]
       (loop [idx 0
              children []
              end-idx pos]
         (if (< idx num-parsers)
           (let [node ((nth parsers idx) txt end-idx)]
             (if node
               (recur (inc idx) (append-children children node) (:endIdx node))
               nil))
           (make-node node-name pos end-idx nil children))))}))

(defn Choice [opts]
  (let [parsers (mapv parse-fn (:parsers opts))
        num-parsers (count parsers)]
    {:parse
     (fn [txt pos]
       (loop [idx 0]
         (when (< idx num-parsers)
           (or ((nth parsers idx) txt pos)
               (recur (inc idx))))))}))

(defn Repeat [opts]
  (let [node-name (:name opts)
        p (parse-fn (:parser opts))
        min-matches (or (:minMatches opts) 0)]
    {:parse
     (fn [txt pos]
       (loop [end-idx pos
              children []]
         (let [node (p txt end-idx)]
           (if node
             (recur (:endIdx node) (append-children children node))
             (if (>= (count children) min-matches)
               (make-node (when (and (string? node-name) (> end-idx pos)) node-name)
                          pos end-idx nil children)
               nil)))))}))

(defn Optional [parser-ref]
  (let [p (parse-fn parser-ref)]
    {:parse
     (fn [txt pos]
       (let [node (p txt pos)]
         (if (and node (string? (:text node)) (not= (:text node) ""))
           node
           (make-node nil pos pos nil nil))))}))

;; -----------------------------------------------------------------------------
;; Grammar Definition

(def whitespace-commons " ,\n\r\t\f")
(def whitespace-unicodes "\u000B\u001C\u001D\u001E\u001F\u2028\u2029\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2008\u2009\u200a\u205f\u3000")
(def whitespace-chars (str whitespace-commons whitespace-unicodes))

(def token-head-chars "()\\[\\]{}\"@~^;`#'")
(def token-tail-chars "()\\[\\]{}\"@^;`")

(def token-re-str (str "[^" token-head-chars whitespace-chars "][^" token-tail-chars whitespace-chars "]*"))
(def char-re-str "\\\\[()\\[\\]{}\"@^;`, ]")

;; the char classes of token-re-str and char-re-str, unescaped
(def token-head-excluded-chars (str whitespace-chars "()[]{}\"@~^;`#'"))
(def token-tail-excluded-chars (str whitespace-chars "()[]{}\"@^;`"))
(def char-literal-chars "()[]{}\"@^;`, ")

(defn whitespace-char? [ch]
  (char-in? ch whitespace-chars))

(defn token-head-excluded? [ch]
  (char-in? ch token-head-excluded-chars))

(defn token-tail-excluded? [ch]
  (char-in? ch token-tail-excluded-chars))

(defn char-literal-char? [ch]
  (char-in? ch char-literal-chars))

;; the line terminators a regex . does not match
(defn line-terminator? [ch]
  (char-in? ch "\n\r\u0085\u2028\u2029"))

(defn scan-whitespace [^String txt pos len]
  (loop [i pos]
    (if (and (< i len) (whitespace-char? (.charAt txt i)))
      (recur (inc i))
      i)))

(defn scan-comment [^String txt pos len]
  (if (and (< pos len) (identical? (.charAt txt pos) \;))
    (loop [i (inc pos)]
      (if (and (< i len) (not (identical? (.charAt txt i) \newline)))
        (recur (inc i))
        i))
    -1))

(defn- scan-token-body [^String txt pos len]
  (if (< pos len)
    (let [ch (.charAt txt pos)]
      (cond
        (and (identical? ch \\) (< (inc pos) len) (char-literal-char? (.charAt txt (inc pos))))
        (+ pos 2)

        (token-head-excluded? ch)
        -1

        :else
        (loop [i (inc pos)]
          (if (and (< i len) (not (token-tail-excluded? (.charAt txt i))))
            (recur (inc i))
            i))))
    -1))

;; ^(##)?(char-re|token-re): the optional ## is only kept when a body follows it
(defn scan-token [^String txt pos len]
  (let [after-hashes (when (starts-at? txt "##" pos)
                       (scan-token-body txt (+ pos 2) len))]
    (if (and after-hashes (>= after-hashes 0))
      after-hashes
      (scan-token-body txt pos len))))

;; ^([^"\\]+|\\.)+ : its alternatives start with different chars, so the
;; first match found is the regex's
(defn scan-string-body [^String txt pos len]
  (loop [i pos]
    (if (< i len)
      (case (.charAt txt i)
        \" i
        \\ (if (and (< (inc i) len) (not (line-terminator? (.charAt txt (inc i)))))
             (recur (+ i 2))
             i)
        (recur (inc i)))
      i)))

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
       :parsers [(Literals {:name ".open" :strs ["#\"" "\""]})
                 (Optional (Scan {:name ".body" :scan scan-string-body}))
                 (Optional (Char {:name ".close" :char "\""}))]}))

  (register-parser! "token"
    (Scan {:name "token" :scan scan-token}))

  (register-parser! "_ws"
    (Scan {:name "whitespace" :scan scan-whitespace}))

  (register-parser! "comment"
    (Scan {:name "comment" :scan scan-comment}))

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
       :parsers [(Literals {:name ".open" :strs ["#?@(" "#?(" "#=(" "#(" "("]})
                 (Repeat
                   {:name ".body"
                    :parser (Choice {:parsers ["_gap" "_form" (NotChar {:name "error" :char ")"})]})})
                 (Optional (Char {:name ".close" :char ")"}))]}))

  (register-parser! "_gap"
    (let [parse-ws (parse-fn "_ws")
          parse-comment (parse-fn "comment")
          parse-discard (parse-fn "discard")]
      {:parse
       (fn [^String txt pos]
         (if (< pos (.length txt))
           (let [ch (.charAt txt pos)]
             (case ch
               \; (parse-comment txt pos)
               \# (parse-discard txt pos)
               (when (whitespace-char? ch)
                 (parse-ws txt pos))))
           nil))}))

  (register-parser! "meta"
    (SeqParser
      {:name "meta"
       :parsers [(Repeat
                   {:minMatches 1
                    :parser (SeqParser
                              {:parsers [(Literals {:name ".marker" :strs ["#^" "^"]})
                                         (Repeat {:parser "_gap"})
                                         (Named {:name ".meta" :parser "_form"})
                                         (Repeat {:parser "_gap"})]})})
                 (Named {:name ".body" :parser "_form"})]}))

  (register-parser! "wrap"
    (SeqParser
      {:name "wrap"
       :parsers [(Literals {:name ".marker" :strs ["@" "'" "`" "~@" "~" "#'"]})
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

  (let [hash-form (Choice {:parsers ["token" "string" "parens" "braces" "wrap" "meta" "tagged"]})
        parse-parens (parse-fn "parens")
        parse-brackets (parse-fn "brackets")
        parse-braces (parse-fn "braces")
        parse-string (parse-fn "string")
        parse-wrap (parse-fn "wrap")
        parse-meta (parse-fn "meta")
        parse-token (parse-fn "token")
        parse-hash-form (:parse hash-form)]
    (register-parser! "_hashForm" hash-form)
    (register-parser! "_form"
      {:parse
       (fn [^String txt pos]
         (if (< pos (.length txt))
           (case (.charAt txt pos)
             \( (parse-parens txt pos)
             \[ (parse-brackets txt pos)
             \{ (parse-braces txt pos)
             \" (parse-string txt pos)
             (\@ \' \` \~) (parse-wrap txt pos)
             \^ (parse-meta txt pos)
             \# (parse-hash-form txt pos)
             (parse-token txt pos))
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
