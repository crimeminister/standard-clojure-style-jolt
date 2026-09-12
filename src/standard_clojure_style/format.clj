(ns standard-clojure-style.format
  (:require
   [clojure.string :as str]
   [standard-clojure-style.parse-ns :as ns-parser]
   [standard-clojure-style.parser :as parser]))

;; -----------------------------------------------------------------------------
;; Language and String Helpers

(defn crlf-to-lf [^String txt]
  (if txt
    (str/replace txt "\r\n" "\n")
    ""))

(defn repeat-string [^String text n]
  (if (and text (> (int n) 0))
    (let [sb (StringBuilder.)]
      (dotimes [_ n]
        (.append sb text))
      (str sb))
    ""))

(defn is-string-with-chars [s]
  (and (string? s) (not= s "")))

(defn str-has-non-whitespace-chars [^String s]
  (and (string? s) (not= (str/trim s) "")))

(defn is-space-or-comma [ch]
  (or (= ch \space) (= ch \,)))

(defn remove-trailing-whitespace [^String txt]
  (let [len (count txt)]
    (loop [end-idx len]
      (if (and (> end-idx 0) (is-space-or-comma (.charAt txt (dec end-idx))))
        (recur (dec end-idx))
        (subs txt 0 end-idx)))))

(defn remove-leading-whitespace [^String txt]
  (str/trimr (str/replace-first txt #"^[, ]*\n+ *" "")))

(defn num-spaces-after-newline [newline-node]
  (let [t (:text newline-node)
        last-nl (str/last-index-of t "\n")]
    (dec (- (count t) last-nl))))

(defn txt-has-commas-after-newline [^String s]
  (let [last-nl (str/last-index-of s "\n")]
    (if-not last-nl
      false
      (boolean (str/index-of s "," last-nl)))))

(defn has-commas-after-newline [node]
  (and (ns-parser/is-whitespace-node node) (txt-has-commas-after-newline (:text node))))

(defn is-newline-node-with-comma-on-next-line [node]
  (and (ns-parser/is-newline-node node) (txt-has-commas-after-newline (:text node))))

;; -----------------------------------------------------------------------------
;; Namespace Formatter Helpers

(defn print-comments-above [out-txt comments-above indentation-str]
  (if (and (vector? comments-above) (seq comments-above))
    (let [sb (StringBuilder. ^String out-txt)]
      (doseq [c comments-above]
        (.append sb ^String indentation-str)
        (.append sb ^String c)
        (.append sb "\n"))
      (str sb))
    out-txt))

(defn get-platforms-from-array [arr]
  (let [has-default (atom false)
        platforms (atom #{})]
    (doseq [itm arr]
      (when-let [p (get itm "platform")]
        (if (= p ":default")
          (reset! has-default true)
          (swap! platforms conj p))))
    (let [sorted-platforms (sort @platforms)]
      (if @has-default
        (vec (concat sorted-platforms [":default"]))
        (vec sorted-platforms)))))

(defn only-one-require-per-platform [reqs]
  (let [counts (atom {})]
    (loop [idx 0]
      (if (>= idx (count reqs))
        true
        (let [req (nth reqs idx)
              p (get req "platform")]
          (if (and (string? p) (not= p ""))
            (if (get @counts p)
              false
              (do
                (swap! counts assoc p 1)
                (recur (inc idx))))
            (recur (inc idx))))))))

(defn filter-on-platform [arr platform]
  (vec
    (filter (fn [itm]
              (if (= platform false)
                (nil? (get itm "platform"))
                (= (get itm "platform") platform)))
            arr)))

(defn format-renames-list [itms]
  (let [num-itms (count itms)]
    (loop [idx 0
           parts []]
      (if (>= idx num-itms)
        (str/join ", " parts)
        (let [itm (nth itms idx)
              pair-str (str (get itm "fromSymbol") " " (get itm "toSymbol"))]
          (recur (inc idx) (conj parts pair-str)))))))

(defn format-require-line [req initial-indentation metadata-inline]
  (let [out (atom "")
        require-indentation (atom initial-indentation)]
    (swap! out print-comments-above (get req "commentsAbove") initial-indentation)

    (let [metadata (get req "metadata")]
      (when (and (vector? metadata) (seq metadata))
        (swap! out str initial-indentation (str/join " " metadata))
        (if metadata-inline
          (do
            (swap! out str " ")
            (reset! require-indentation ""))
          (swap! out str "\n"))))

    (swap! out str @require-indentation "[" (get req "symbol"))

    (if (is-string-with-chars (get req "as"))
      (swap! out str " :as " (get req "as"))
      (when (is-string-with-chars (get req "asAlias"))
        (swap! out str " :as-alias " (get req "asAlias"))))

    (when (is-string-with-chars (get req "default"))
      (swap! out str " :default " (get req "default")))

    (let [refer (get req "refer")]
      (cond
        (and (vector? refer) (seq refer))
        (let [symbols (mapv #(get % "symbol") refer)]
          (swap! out str " :refer [" (str/join " " symbols) "]"))
        (= refer "all")
        (swap! out str " :refer :all")))

    (let [exclude (get req "exclude")]
      (when (and (vector? exclude) (seq exclude))
        (let [symbols (mapv #(get % "symbol") exclude)]
          (swap! out str " :exclude [" (str/join " " symbols) "]"))))

    (cond
      (= (get req "includeMacros") true)
      (swap! out str " :include-macros true")
      (= (get req "includeMacros") false)
      (swap! out str " :include-macros false"))

    (let [refer-macros (get req "referMacros")]
      (when (and (vector? refer-macros) (seq refer-macros))
        (swap! out str " :refer-macros [" (str/join " " refer-macros) "]")))

    (let [rename (get req "rename")]
      (when (and (vector? rename) (seq rename))
        (swap! out str " :rename {" (format-renames-list rename) "}")))

    (swap! out str "]")
    @out))

(defn get-refer-clojure-keys [refer-clojure]
  (let [keys-vec (atom [])]
    (when refer-clojure
      (when (get refer-clojure "exclude") (swap! keys-vec conj ":exclude"))
      (when (get refer-clojure "only") (swap! keys-vec conj ":only"))
      (when (get refer-clojure "rename") (swap! keys-vec conj ":rename")))
    @keys-vec))

(defn format-keyword-followed-by-list-of-symbols [kwd symbols]
  (str kwd " [" (str/join " " symbols) "]"))

(defn format-refer-clojure-single-keyword [ns exclude-or-only]
  (let [symbols-arr (get-in ns ["referClojure" exclude-or-only])
        kwd (str ":" exclude-or-only)
        platforms (get-platforms-from-array symbols-arr)
        num-platforms (count platforms)
        symbols-for-all (mapv #(get % "symbol") (filter-on-platform symbols-arr false))
        num-symbols-for-all (count symbols-for-all)]
    (cond
      ;; No reader conditionals
      (= num-platforms 0)
      (let [s (atom "\n")]
        (swap! s print-comments-above (get ns "referClojureCommentsAbove") "  ")
        (swap! s str "  (:refer-clojure " (format-keyword-followed-by-list-of-symbols kwd symbols-for-all) ")")
        @s)

      ;; All symbols for a single platform
      (and (= num-platforms 1) (= num-symbols-for-all 0))
      (let [symbols2 (mapv #(get % "symbol") symbols-arr)
            s (atom (str "\n  #?(" (first platforms) "\n"))]
        (swap! s print-comments-above (get ns "referClojureCommentsAbove") "     ")
        (swap! s str "     (:refer-clojure " (format-keyword-followed-by-list-of-symbols kwd symbols2) "))")
        @s)

      ;; All symbols for specific platforms across multiple platforms
      (and (> num-platforms 1) (= num-symbols-for-all 0))
      (let [s (atom "\n")]
        (swap! s print-comments-above (get ns "referClojureCommentsAbove") "  ")
        (swap! s str "  #?(")
        (doseq [i (range num-platforms)]
          (let [p (nth platforms i)
                syms (mapv #(get % "symbol") (filter-on-platform symbols-arr p))]
            (if (= i 0)
              (swap! s str p " (:refer-clojure ")
              (swap! s str "\n     " p " (:refer-clojure "))
            (swap! s str (format-keyword-followed-by-list-of-symbols kwd syms) ")")))
        (swap! s str ")")
        @s)

      ;; Mix of all-platforms and specific platforms
      :else
      (let [s (atom "\n")]
        (swap! s print-comments-above (get ns "referClojureCommentsAbove") "  ")
        (swap! s str "  (:refer-clojure\n    " kwd " [" (str/join " " symbols-for-all))
        (if (= kwd ":exclude")
          (swap! s str "\n" (repeat-string " " 14))
          (swap! s str "\n" (repeat-string " " 11)))
        (swap! s str "#?@(")
        (doseq [i (range num-platforms)]
          (let [p (nth platforms i)
                syms (mapv #(get % "symbol") (filter-on-platform symbols-arr p))]
            (swap! s str (format-keyword-followed-by-list-of-symbols p syms))
            (when (not= (inc i) num-platforms)
              (if (= kwd ":exclude")
                (swap! s str "\n" (repeat-string " " 18))
                (swap! s str "\n" (repeat-string " " 15))))))
        (swap! s str ")])")
        @s))))

(defn format-refer-clojure [ns]
  (let [keys-vec (get-refer-clojure-keys (get ns "referClojure"))
        num-keys (count keys-vec)]
    (cond
      (= num-keys 0) ""
      (and (= num-keys 1) (= (first keys-vec) ":exclude"))
      (format-refer-clojure-single-keyword ns "exclude")

      (and (= num-keys 1) (= (first keys-vec) ":only"))
      (format-refer-clojure-single-keyword ns "only")

      (and (= num-keys 1) (= (first keys-vec) ":rename"))
      (let [renames (get-in ns ["referClojure" "rename"])
            platforms (get-platforms-from-array renames)
            num-platforms (count platforms)
            non-platform (filter-on-platform renames false)
            all-same (and (= (count non-platform) 0) (> (count platforms) 0))]
        (cond
          (= num-platforms 0)
          (let [s (atom "\n")]
            (swap! s print-comments-above (get ns "referClojureCommentsAbove") "  ")
            (swap! s str "  (:refer-clojure :rename {" (format-renames-list renames) "})")
            @s)

          (and (= num-platforms 1) all-same)
          (let [s (atom (str "\n  #?(" (first platforms) "\n"))]
            (swap! s print-comments-above (get ns "referClojureCommentsAbove") "     ")
            (swap! s str "     (:refer-clojure :rename {" (format-renames-list renames) "}))")
            @s)

          :else
          (let [s (atom (str "\n  (:refer-clojure\n    :rename {"
                             (format-renames-list non-platform)
                             "\n             #?@("))]
            (doseq [i (range num-platforms)]
              (let [p (nth platforms i)
                    p-renames (filter-on-platform renames p)]
                (if (= i 0)
                  (swap! s str p " [")
                  (swap! s str "\n                 " p " ["))
                (swap! s str (format-renames-list p-renames) "]")))
            (swap! s str ")})")
            @s)))

      :else
      (let [s (atom "\n  (:refer-clojure")]
        (when-let [exc (get-in ns ["referClojure" "exclude"])]
          (when (seq exc)
            (let [syms (mapv #(get % "symbol") exc)]
              (swap! s str "\n    " (format-keyword-followed-by-list-of-symbols ":exclude" syms)))))
        (when-let [onl (get-in ns ["referClojure" "only"])]
          (when (seq onl)
            (let [syms (mapv #(get % "symbol") onl)]
              (swap! s str "\n    " (format-keyword-followed-by-list-of-symbols ":only" syms)))))
        (when-let [ren (get-in ns ["referClojure" "rename"])]
          (when (seq ren)
            (swap! s str "\n    :rename {" (format-renames-list ren) "}")))
        (swap! s str ")")
        @s))))

(def gen-class-keys
  ["name" "extends" "implements" "init" "constructors" "post-init" "methods"
   "main" "factory" "state" "exposes" "exposes-methods" "prefix" "impl-ns" "load-impl-ns"])

(defn format-ns [ns]
  (let [out (atom (str "(ns " (get ns "nsSymbol")))
        num-require-macros (count (or (get ns "requireMacros") []))
        num-requires (count (or (get ns "requires") []))
        num-imports (count (or (get ns "imports") []))
        comment-outside-ns-form (atom nil)
        has-gen-class (boolean (get ns "genClass"))
        imports-is-last (and (> num-imports 0) (not has-gen-class))
        require-is-last (and (> num-requires 0) (not imports-is-last) (not has-gen-class))
        require-macros-is-last (and (> num-require-macros 0) (= num-requires 0) (= num-imports 0) (not has-gen-class))
        refer-clojure-is-last (and (get ns "referClojure") (= num-require-macros 0) (= num-requires 0) (= num-imports 0) (not has-gen-class))
        trailing-parens-printed (atom false)]

    (when-let [doc (get ns "docstring")]
      (swap! out str "\n  \"" doc "\""))

    (when-let [meta-list (get ns "nsMetadata")]
      (let [cnt (count meta-list)]
        (when (> cnt 0)
          (swap! out str "\n  {")
          (doseq [i (range cnt)]
            (let [m (nth meta-list i)]
              (swap! out str (get m "key") " " (get m "value"))
              (when (not= (inc i) cnt)
                (swap! out str "\n   "))))
          (swap! out str "}"))))

    (when (get ns "referClojure")
      (swap! out str (format-refer-clojure ns))
      (when-let [ca (get ns "referClojureCommentAfter")]
        (if refer-clojure-is-last
          (reset! comment-outside-ns-form ca)
          (swap! out str " " ca))))

    (when (> num-require-macros 0)
      (let [rms (get ns "requireMacros")
            cljs-rms (filter-on-platform rms ":cljs")
            wrap-with-rc (= (count cljs-rms) num-require-macros)
            rm-last-comment (atom nil)
            rm-indent (if wrap-with-rc "      " "   ")]
        (if wrap-with-rc
          (do
            (swap! out str "\n  #?(:cljs\n")
            (swap! out print-comments-above (get ns "requireMacrosCommentsAbove") "     ")
            (swap! out str "     (:require-macros\n"))
          (do
            (swap! out str "\n")
            (swap! out print-comments-above (get ns "requireMacrosCommentsAbove") "  ")
            (swap! out str "  (:require-macros\n")))

        (doseq [i (range num-require-macros)]
          (let [rm (nth rms i)
                is-last (= (inc i) num-require-macros)]
            (swap! out str (format-require-line rm rm-indent false))
            (when (is-string-with-chars (get rm "commentAfter"))
              (if is-last
                (reset! rm-last-comment (get rm "commentAfter"))
                (swap! out str " " (get rm "commentAfter"))))
            (when-not is-last
              (swap! out str "\n"))))

        (cond
          (and (not require-macros-is-last) (not wrap-with-rc))
          (swap! out str ")")
          (and (not require-macros-is-last) wrap-with-rc)
          (swap! out str "))")
          (and require-macros-is-last (not wrap-with-rc))
          (do (swap! out str "))") (reset! trailing-parens-printed true))
          (and require-macros-is-last wrap-with-rc)
          (do (swap! out str ")))") (reset! trailing-parens-printed true)))

        (when (is-string-with-chars @rm-last-comment)
          (swap! out str " " @rm-last-comment))))

    (when (> num-requires 0)
      (let [reqs (get ns "requires")
            close-require-paren-trail (atom ")")
            last-require-has-comment (atom false)
            last-require-comment (atom nil)
            req-platforms (get-platforms-from-array reqs)
            num-platforms (count req-platforms)
            all-under-one-platform (and (= num-platforms 1)
                                        (= num-requires (count (filter-on-platform reqs (first req-platforms)))))
            require-line-indent (if all-under-one-platform "      " "   ")]
        (if all-under-one-platform
          (do
            (swap! out str "\n  #?(" (first req-platforms))
            (when (seq (get ns "requireCommentsAbove"))
              (swap! out str "\n     " (str/join "\n     " (get ns "requireCommentsAbove"))))
            (swap! out str "\n     (:require")
            (when (is-string-with-chars (get ns "requireCommentAfter"))
              (swap! out str " " (get ns "requireCommentAfter")))
            (swap! out str "\n"))
          (do
            (when (seq (get ns "requireCommentsAbove"))
              (swap! out str "\n  " (str/join "\n  " (get ns "requireCommentsAbove"))))
            (swap! out str "\n  (:require\n")))

        (doseq [i (range num-requires)]
          (let [req (nth reqs i)
                is-last (= (inc i) num-requires)]
            (when (or (not (get req "platform")) all-under-one-platform)
              (swap! out str (format-require-line req require-line-indent false))
              (let [ca (get req "commentAfter")]
                (cond
                  (and ca (not is-last))
                  (swap! out str " " ca "\n")

                  (and ca is-last require-is-last (not all-under-one-platform))
                  (do
                    (reset! close-require-paren-trail (str ")) " ca))
                    (reset! trailing-parens-printed true))

                  (and ca is-last all-under-one-platform)
                  (do
                    (reset! last-require-comment ca)
                    (reset! last-require-has-comment true))

                  (and ca is-last)
                  (reset! close-require-paren-trail (str ") " ca))

                  (and (not ca) is-last)
                  (reset! close-require-paren-trail ")")

                  :else
                  (swap! out str "\n"))))))

        (let [require-has-rc (> num-platforms 0)
              use-std-rc (only-one-require-per-platform reqs)]
          (when-not all-under-one-platform
            (if use-std-rc
              (doseq [i (range num-platforms)]
                (let [p (nth req-platforms i)
                      platform-reqs (filter-on-platform reqs p)
                      req (first platform-reqs)]
                  (if (= i 0)
                    (do
                      (swap! out str/trim)
                      (swap! out str "\n   #?(" p " "))
                    (swap! out str "\n      " p " "))
                  (swap! out str (format-require-line req "" true))))
              (doseq [i (range num-platforms)]
                (let [p (nth req-platforms i)
                      platform-reqs (filter-on-platform reqs p)
                      num-filtered (count platform-reqs)
                      print-platform-close (atom true)]
                  (if (= i 0)
                    (do
                      (swap! out str/trim)
                      (swap! out str "\n   #?@(" p "\n       ["))
                    (swap! out str "\n\n       " p "\n       ["))
                  (doseq [j (range num-filtered)]
                    (let [req (nth platform-reqs j)
                          is-last (= (inc j) num-filtered)]
                      (if (> j 0)
                        (swap! out str (format-require-line req "        " false))
                        (swap! out str (format-require-line req "" false)))
                      (let [ca (get req "commentAfter")]
                        (cond
                          (and ca (not is-last))
                          (swap! out str " " ca "\n")

                          (and ca is-last (not= (inc i) num-platforms))
                          (do
                            (swap! out str "] " ca)
                            (reset! print-platform-close false))

                          (and ca is-last (or (= (inc i) num-platforms) require-is-last))
                          (do
                            (reset! last-require-has-comment true)
                            (reset! last-require-comment ca))

                          (and ca is-last)
                          (reset! close-require-paren-trail (str ") " ca))

                          (and (not ca) is-last)
                          (reset! close-require-paren-trail "]")

                          :else
                          (swap! out str "\n")))))
                  (when @print-platform-close
                    (swap! out str "]"))))))

          (cond
            (and (not require-has-rc) (not @last-require-has-comment) (not require-is-last))
            (reset! close-require-paren-trail ")")

            (and (not require-has-rc) @last-require-has-comment (not require-is-last))
            (reset! close-require-paren-trail (str ") " @last-require-comment))

            (and require-has-rc (not @last-require-has-comment) (not require-is-last))
            (reset! close-require-paren-trail "))")

            (and require-has-rc @last-require-has-comment (not require-is-last))
            (reset! close-require-paren-trail (str ")) " @last-require-comment))

            (and require-has-rc (not @last-require-has-comment) require-is-last)
            (do (reset! close-require-paren-trail ")))") (reset! trailing-parens-printed true))

            (and require-has-rc @last-require-has-comment require-is-last)
            (do (reset! close-require-paren-trail (str "))) " @last-require-comment)) (reset! trailing-parens-printed true)))

          (swap! out str/trim)
          (swap! out str @close-require-paren-trail))))

    (when (> num-imports 0)
      (let [imports (get ns "imports")
            non-platform (filter-on-platform imports false)
            num-non-platform (count non-platform)
            import-platforms (get-platforms-from-array imports)
            num-import-platforms (count import-platforms)
            last-import-comment (atom nil)
            is-import-printed (atom false)]
        (doseq [i (range num-non-platform)]
          (when-not @is-import-printed
            (swap! out str "\n  (:import\n")
            (reset! is-import-printed true))
          (let [imp (nth non-platform i)
                is-last (= (inc i) num-non-platform)]
            (swap! out str "   (" (get imp "package"))
            (doseq [c (get imp "classes")]
              (swap! out str " " c))
            (swap! out str ")")
            (when (is-string-with-chars (get imp "commentAfter"))
              (swap! out str " " (get imp "commentAfter")))
            (when-not is-last
              (swap! out str "\n"))))

        (let [import-has-rc (> num-import-platforms 0)
              place-rc-outside (and (= num-import-platforms 1) (= num-non-platform 0))]
          (doseq [i (range num-import-platforms)]
            (let [p (nth import-platforms i)
                  p-imports (filter-on-platform imports p)
                  cnt (count p-imports)]
              (cond
                place-rc-outside
                (do
                  (swap! out str "\n  #?(" p "\n     (:import\n      ")
                  (reset! is-import-printed true))

                (= i 0)
                (do
                  (when-not @is-import-printed
                    (swap! out str "\n  (:import")
                    (reset! is-import-printed true))
                  (swap! out str "\n   #?@(" p "\n       ["))

                :else
                (swap! out str "\n\n       " p "\n       ["))

              (doseq [j (range cnt)]
                (let [imp (nth p-imports j)
                      is-last (= (inc j) cnt)]
                  (swap! out str "(" (get imp "package") " " (str/join " " (get imp "classes")) ")")
                  (if is-last
                    (do
                      (when-not place-rc-outside
                        (swap! out str "]"))
                      (when (is-string-with-chars (get imp "commentAfter"))
                        (reset! last-import-comment (get imp "commentAfter"))))
                    (do
                      (when (is-string-with-chars (get imp "commentAfter"))
                        (swap! out str " " (get imp "commentAfter")))
                      (if place-rc-outside
                        (swap! out str "\n      ")
                        (swap! out str "\n        "))))))))

          (let [close-trail (cond
                              (and imports-is-last import-has-rc) ")))"
                              imports-is-last "))"
                              :else ")")]
            (when imports-is-last
              (reset! trailing-parens-printed true))
            (swap! out str close-trail)
            (when (is-string-with-chars @last-import-comment)
              (swap! out str " " @last-import-comment))))))

    (when has-gen-class
      (let [gen-class (get ns "genClass")
            is-rc (= (get gen-class "platform") ":clj")
            gen-class-indent (if is-rc 5 2)]
        (swap! out str "\n")
        (when is-rc
          (swap! out str "  #?(:clj\n"))
        (let [ind-str (repeat-string " " gen-class-indent)]
          (swap! out print-comments-above (get gen-class "commentsAbove") ind-str)
          (swap! out str ind-str "(:gen-class")
          (let [comment-after (atom nil)]
            (if (get gen-class "isEmpty")
              (when (is-string-with-chars (get gen-class "commentAfter"))
                (reset! comment-after (get gen-class "commentAfter")))
              (do
                (when (is-string-with-chars (get gen-class "commentAfter"))
                  (swap! out str " " (get gen-class "commentAfter")))
                (let [val-indent (inc gen-class-indent)
                      ind-str2 (repeat-string " " val-indent)]
                  (doseq [k gen-class-keys]
                    (when-let [val-obj (get gen-class k)]
                      (when (is-string-with-chars @comment-after)
                        (swap! out str " " @comment-after)
                        (reset! comment-after nil))
                      (swap! out str "\n")
                      (swap! out print-comments-above (or (get val-obj "commentsAbove") (get (meta val-obj) "commentsAbove")) ind-str2)
                      (swap! out str ind-str2 ":" k)

                      (cond
                        (= k "implements")
                        (let [interfaces val-obj
                              num-ifaces (count interfaces)]
                          (doseq [idx (range num-ifaces)]
                            (if (= idx 0)
                              (swap! out str " [" (get (nth interfaces idx) "symbol"))
                              (swap! out str " " (get (nth interfaces idx) "symbol"))))
                          (when (> num-ifaces 0)
                            (swap! out str "]")))

                        (= k "constructors")
                        (let [constructors (get val-obj "value")
                              cnt (count constructors)]
                          (if (= cnt 0)
                            (swap! out str " {}")
                            (let [c-indent (+ val-indent (count k) 3)
                                  c-ind-str (repeat-string " " c-indent)]
                              (doseq [idx (range cnt)]
                                (let [c (nth constructors idx)]
                                  (if (= idx 0)
                                    (swap! out str " {")
                                    (swap! out str "\n" c-ind-str))
                                  (when (get c "metadata")
                                    (swap! out str (get c "metadata") " "))
                                  (swap! out str "[" (str/join " " (get c "parameterTypes")) "] ["
                                         (str/join " " (get c "superParameterTypes")) "]")))
                              (swap! out str "}"))))

                        (= k "methods")
                        (let [methods (get val-obj "value")
                              cnt (count methods)]
                          (if (= cnt 0)
                            (swap! out str " []")
                            (let [m-indent (+ val-indent (count k) 3)
                                  m-ind-str (repeat-string " " m-indent)]
                              (doseq [idx (range cnt)]
                                (let [m (nth methods idx)]
                                  (if (= idx 0)
                                    (swap! out str " [")
                                    (swap! out str "\n" m-ind-str))
                                  (when (get m "metadata")
                                    (swap! out str (get m "metadata") " "))
                                  (swap! out str "[" (get m "name") " ["
                                         (str/join " " (get m "parameterTypes")) "] "
                                         (get m "returnType") "]")))
                              (swap! out str "]"))))

                        (= k "exposes")
                        (let [exposes (get val-obj "value")
                              cnt (count exposes)]
                          (if (= cnt 0)
                            (swap! out str " {}")
                            (let [e-indent (+ val-indent (count k) 3)
                                  e-ind-str (repeat-string " " e-indent)]
                              (doseq [idx (range cnt)]
                                (let [e (nth exposes idx)]
                                  (if (= idx 0)
                                    (swap! out str " {")
                                    (swap! out str "\n" e-ind-str))
                                  (swap! out str (get e "fieldName") " {")
                                  (when (get e "getter")
                                    (swap! out str ":get " (get e "getter")))
                                  (when (get e "setter")
                                    (when (get e "getter")
                                      (swap! out str " "))
                                    (swap! out str ":set " (get e "setter")))
                                  (swap! out str "}")))
                              (swap! out str "}"))))

                        (= k "exposes-methods")
                        (let [em (get val-obj "value")
                              cnt (count em)]
                          (if (= cnt 0)
                            (swap! out str " {}")
                            (let [em-indent (+ val-indent (count k) 3)
                                  em-ind-str (repeat-string " " em-indent)]
                              (doseq [idx (range cnt)]
                                (let [e (nth em idx)]
                                  (if (= idx 0)
                                    (swap! out str " {")
                                    (swap! out str "\n" em-ind-str))
                                  (swap! out str (get e "superMethodName") " " (get e "exposedName"))))
                              (swap! out str "}"))))

                        (get val-obj "metadata")
                        (swap! out str " " (get val-obj "metadata") " " (get val-obj "value"))

                        :else
                        (swap! out str " " (get val-obj "value")))

                      (let [ca (or (get val-obj "commentAfter") (get (meta val-obj) "commentAfter"))]
                        (when (is-string-with-chars ca)
                          (reset! comment-after ca))))))))

            (let [ca @comment-after]
              (cond
                (and (not is-rc) (not ca))
                (do (swap! out str "))") (reset! trailing-parens-printed true))

                (and is-rc (not ca))
                (do (swap! out str ")))") (reset! trailing-parens-printed true))

                (and (not is-rc) ca)
                (do (swap! out str ")) " ca) (reset! trailing-parens-printed true))

                (and is-rc ca)
                (do (swap! out str "))) " ca) (reset! trailing-parens-printed true))))))))

    (when-not @trailing-parens-printed
      (swap! out str ")"))

    (when (is-string-with-chars @comment-outside-ns-form)
      (swap! out str " " @comment-outside-ns-form))

    @out))

;; -----------------------------------------------------------------------------
;; formatNodes & format

(defn format-nodes [nodes-arr parsed-ns]
  (let [num-nodes (count nodes-arr)
        has-parsed-ns-form (not (nil? (get parsed-ns "nsSymbol")))
        ;; wrap each node in an atom so mutable properties can be tracked
        nodes (mapv atom nodes-arr)

        paren-nesting-depth (atom 0)
        idx (atom 0)
        out-txt (atom "")
        output-txt-contains-chars (atom false)
        line-txt (atom "")
        line-idx (atom 0)
        inside-ns-form (atom false)
        line-idx-of-closing-ns-form (atom -1)
        ns-start-string-idx (atom -1)
        ns-end-string-idx (atom -1)
        ignore-nodes-start-id (atom -1)
        ignore-nodes-end-id (atom -1)
        inside-the-ignore-zone (atom false)

        paren-stack (atom [])
        nodes-we-have-printed-on-this-line (atom [])
        col-idx (atom 0)]

    (letfn [(find-next-text-node [i]
              (let [n num-nodes]
                (loop [j i]
                  (when (< j n)
                    (let [node (nth nodes j)]
                      (if (and (string? (:text @node)) (not= (:text @node) ""))
                        node
                        (recur (inc j))))))))

            (find-next-text-node-skipping-meta-atom [i]
              (ns-parser/find-next-text-node-skipping-meta nodes-arr i))]

      (while (< @idx num-nodes)
        (let [node (nth nodes @idx)]
          (when (ns-parser/is-tag-node @node)
            (swap! node assoc :text "#"))
          (let [m @node]
            (when (and (> @ignore-nodes-start-id 0) (= (:id m) @ignore-nodes-start-id))
              (reset! inside-the-ignore-zone true)
              (swap! out-txt str @line-txt)
              (reset! line-txt ""))

            (if @inside-the-ignore-zone
              (do
                (when (and (string? (:text m)) (not= (:text m) ""))
                  (swap! out-txt str (:text m)))
                (when (= (:id m) @ignore-nodes-end-id)
                  (reset! ignore-nodes-start-id -1)
                  (reset! ignore-nodes-end-id -1)
                  (reset! inside-the-ignore-zone false)))

              (do
                (when (= @idx 0)
                  (let [first-m @(first nodes)
                        initial-spaces (if (ns-parser/is-newline-node first-m)
                                         (num-spaces-after-newline first-m)
                                         0)
                        start-i (if (ns-parser/is-newline-node first-m) 1 0)]
                    (loop [i start-i
                           col initial-spaces]
                      (when (< i num-nodes)
                        (let [nd (nth nodes i)
                              nd-m @nd]
                          (when-not (ns-parser/is-newline-node nd-m)
                            (let [txt (:text nd-m)]
                              (cond
                                (and (string? txt) (not= txt ""))
                                (do
                                  (swap! nd assoc :_origColIdx col)
                                  (recur (inc i) (+ col (count txt))))

                                (ns-parser/is-tag-node nd-m)
                                (do
                                  (swap! nd assoc :_origColIdx col)
                                  (recur (inc i) (inc col)))

                                :else
                                (recur (inc i) col)))))))))

                (when (and (= @ns-start-string-idx -1) (= @paren-nesting-depth 1)
                           has-parsed-ns-form (ns-parser/is-ns-node m))
                  (reset! inside-ns-form true)
                  (reset! ns-start-string-idx (count (str @out-txt @line-txt))))

                (let [next-text-node (find-next-text-node (inc @idx))
                      is-last-node (>= (inc @idx) num-nodes)
                      current-node-is-whitespace (ns-parser/is-whitespace-node m)
                      current-node-is-newline (ns-parser/is-newline-node m)
                      skip-printing-this-node (atom false)]

                  ;; :standard-clj/ignore check
                  (when (and (ns-parser/is-standard-clj-ignore-keyword m) (> @idx 1))
                    (let [deref-nodes nodes-arr
                          prev-node1 (ns-parser/find-prev-node-with-text deref-nodes @idx (:id m))
                          prev-node2 (when prev-node1 (ns-parser/find-prev-node-with-text deref-nodes @idx (:id prev-node1)))
                          is-discard-map (and prev-node1 (= (:name prev-node1) ".open") (= (:text prev-node1) "{")
                                              prev-node2 (ns-parser/is-discard-node prev-node2))]
                      (cond
                        (or (ns-parser/is-discard-node prev-node1)
                            (and (ns-parser/is-whitespace-node prev-node1) (ns-parser/is-discard-node prev-node2)))
                        (let [next-ignore-node (ns-parser/find-next-non-whitespace-node deref-nodes (inc @idx))]
                          (if (and (vector? (:children next-ignore-node)) (seq (:children next-ignore-node)))
                            (let [closing-node (last (:children next-ignore-node))]
                              (reset! ignore-nodes-start-id (:id next-ignore-node))
                              (reset! ignore-nodes-end-id (:id closing-node)))
                            (let [next-immediate-node (nth deref-nodes (inc @idx))]
                              (reset! ignore-nodes-start-id (:id next-immediate-node))
                              (reset! ignore-nodes-end-id (:id next-ignore-node)))))

                        is-discard-map
                        (let [opening-brace-node (ns-parser/find-prev-node-with-predicate deref-nodes @idx ns-parser/is-opening-brace-node)
                              closing-brace-node-id (:id (nth (:children opening-brace-node) 2))
                              start-ignore-node (ns-parser/find-next-node-with-predicate-after-specific-node deref-nodes @idx (constantly true) closing-brace-node-id)
                              first-node-inside (ns-parser/find-next-node-with-predicate-after-specific-node deref-nodes @idx (constantly true) (:id start-ignore-node))]
                          (if (and (vector? (:children first-node-inside)) (seq (:children first-node-inside)))
                            (let [closing-node (last (:children first-node-inside))]
                              (reset! ignore-nodes-start-id (:id start-ignore-node))
                              (reset! ignore-nodes-end-id (:id closing-node)))
                            (do
                              (reset! ignore-nodes-start-id (:id start-ignore-node))
                              (reset! ignore-nodes-end-id (:id first-node-inside))))))))

                  (cond
                    (ns-parser/is-paren-opener m)
                    (do
                      (when-let [top (peek @paren-stack)]
                        (when (= @line-idx (:_parenOpenerLineIdx @top))
                          (swap! node assoc :_colIdx @col-idx :_lineIdx @line-idx)
                          (swap! top update :_openingLineNodes conj node)))

                      (swap! paren-nesting-depth inc)

                      (swap! node assoc
                             :_colIdx @col-idx
                             :_nextWithText (when next-text-node @next-text-node)
                             :_nextWithTextSkippingMeta (find-next-text-node-skipping-meta-atom (inc @idx))
                             :_parenOpenerLineIdx @line-idx
                             :_openingLineNodes []
                             :_rule3Active false
                             :_rule3NumSpaces 0
                             :_rule3SearchComplete false)
                      (swap! paren-stack conj node)

                      (when (and next-text-node (ns-parser/is-whitespace-node @next-text-node))
                        (swap! next-text-node assoc :text "")))

                    (ns-parser/is-paren-closer m)
                    (do
                      (swap! paren-nesting-depth dec)
                      (swap! paren-stack pop)
                      (when (and @inside-ns-form (= @paren-nesting-depth 0))
                        (reset! inside-ns-form false)
                        (reset! ns-end-string-idx (count (str @out-txt @line-txt)))
                        (reset! line-idx-of-closing-ns-form @line-idx))))

                  (when-let [top (peek @paren-stack)]
                    (when (ns-parser/node-contains-text m)
                      (when (= @line-idx (:_parenOpenerLineIdx @top))
                        (swap! node assoc :_colIdx @col-idx :_lineIdx @line-idx)
                        (swap! top update :_openingLineNodes conj node))))

                  (when (and current-node-is-whitespace (not current-node-is-newline)
                             next-text-node (ns-parser/is-paren-closer @next-text-node))
                    (reset! skip-printing-this-node true))

                  (when (and current-node-is-whitespace (not current-node-is-newline)
                             next-text-node (ns-parser/is-comment-node @next-text-node))
                    (swap! node update :text str/replace "," ""))

                  ;; Look forward to slurp closing parens
                  (when (and (seq @paren-stack) (not @inside-ns-form))
                    (let [is-comment-followed-by-nl (and (ns-parser/is-comment-node m) next-text-node (ns-parser/is-newline-node @next-text-node))
                          has-commas-after-nl (or (has-commas-after-newline m) (and next-text-node (has-commas-after-newline @next-text-node)))
                          look-forward (cond
                                         has-commas-after-nl false
                                         is-comment-followed-by-nl true
                                         current-node-is-newline true
                                         :else false)]
                      (when look-forward
                        (let [closers (loop [i (inc @idx)
                                             acc []]
                                        (if (>= i num-nodes)
                                          acc
                                          (let [nd (nth nodes i)
                                                nd-m @nd]
                                            (cond
                                              (is-newline-node-with-comma-on-next-line nd-m) acc
                                              (or (ns-parser/is-whitespace-node nd-m)
                                                  (ns-parser/is-paren-closer nd-m)
                                                  (ns-parser/is-comment-node nd-m))
                                              (recur (inc i) (conj acc nd))
                                              :else acc))))
                              last-node-printed (last @nodes-we-have-printed-on-this-line)
                              last-m (when last-node-printed @last-node-printed)
                              trimmed? (atom false)]
                          (when (and last-m (ns-parser/is-whitespace-node last-m))
                            (reset! line-txt (remove-trailing-whitespace @line-txt))
                            (reset! trimmed? true))

                          (doseq [closer-nd closers]
                            (let [c-m @closer-nd]
                              (when (ns-parser/is-paren-closer c-m)
                                (swap! line-txt str (:text c-m))
                                (swap! closer-nd assoc :text "" :_wasSlurpedUp true)
                                (swap! paren-nesting-depth dec)
                                (swap! paren-stack pop))))

                          (when @trimmed?
                            (swap! line-txt str (:text last-m)))))))

                  (when current-node-is-newline
                    ;; Record original col indexes for the next line
                    (let [initial-spaces (num-spaces-after-newline m)
                          start-i (inc @idx)]
                      (loop [i start-i
                             col initial-spaces]
                        (when (< i num-nodes)
                          (let [nd (nth nodes i)
                                nd-m @nd]
                            (when-not (ns-parser/is-newline-node nd-m)
                              (let [txt (:text nd-m)]
                                (cond
                                  (and (string? txt) (not= txt ""))
                                  (do
                                    (swap! nd assoc :_origColIdx col)
                                    (recur (inc i) (+ col (count txt))))

                                  (ns-parser/is-tag-node nd-m)
                                  (do
                                    (swap! nd assoc :_origColIdx col)
                                    (recur (inc i) (inc col)))

                                  :else
                                  (recur (inc i) col))))))))

                    (let [num-spaces-on-next-line (num-spaces-after-newline m)
                          all-next-slurped (loop [i (inc @idx)]
                                             (if (>= i num-nodes)
                                               true
                                               (let [nd-m @(nth nodes i)]
                                                 (cond
                                                   (ns-parser/is-newline-node nd-m) true
                                                   (not (string? (:text nd-m))) (recur (inc i))
                                                   (or (:_wasSlurpedUp nd-m) (ns-parser/is-whitespace-node nd-m)) (recur (inc i))
                                                   :else false))))
                          next-only-comment (let [n1 (when (< (inc @idx) num-nodes) @(nth nodes (inc @idx)))
                                                  n2 (when (< (+ @idx 2) num-nodes) @(nth nodes (+ @idx 2)))]
                                              (cond
                                                (and n1 n2) (and (ns-parser/is-comment-node n1) (ns-parser/is-newline-node n2))
                                                n1 (ns-parser/is-comment-node n1)
                                                :else false))
                          next-comment-col (if next-only-comment num-spaces-on-next-line -1)
                          is-double-newline (str/includes? (:text m) "\n\n")
                          newline-str (atom (if is-double-newline "\n\n" "\n"))]

                      (when @output-txt-contains-chars
                        (let [top (peek @paren-stack)]
                          ;; Rule 3 check
                          (when (and top (not (:_rule3SearchComplete @top)))
                            (let [opening-nodes (:_openingLineNodes @top)
                                  num-op (count opening-nodes)]
                              (when (> num-op 2)
                                (loop [i 1
                                       past-first-ws false]
                                  (when (< i num-op)
                                    (let [op-node (nth opening-nodes i)
                                          op-m @op-node]
                                      (cond
                                        (and past-first-ws (ns-parser/is-node-with-non-blank-text op-m)
                                             (= (:_origColIdx op-m) num-spaces-on-next-line))
                                        (swap! top assoc :_rule3Active true
                                                         :_rule3NumSpaces (or (:_printedColIdx op-m) 0))

                                        (and (not past-first-ws) (ns-parser/is-whitespace-node op-m))
                                        (recur (inc i) true)

                                        :else
                                        (recur (inc i) past-first-ws))))))
                              (swap! top assoc :_rule3SearchComplete true)))

                          ;; Comment vertical alignment check
                          (let [col-idx-comment-align (atom -1)
                                comment-aligned (atom false)]
                            (when (and next-only-comment (not is-double-newline))
                              (let [printed-nodes @nodes-we-have-printed-on-this-line
                                    num-prev (count printed-nodes)]
                                (loop [i 0]
                                  (when (< i num-prev)
                                    (let [prev-node (nth printed-nodes i)
                                          prev-prev (when (> i 0) (nth printed-nodes (dec i)))
                                          prev-m @prev-node
                                          prev-prev-m (when prev-prev @prev-prev)
                                          possible (and (ns-parser/is-node-with-non-blank-text prev-m)
                                                        (or (nil? prev-prev-m) (not (ns-parser/is-paren-opener prev-prev-m))))]
                                      (if (and possible (= next-comment-col (:_origColIdx prev-m)))
                                        (do
                                          (reset! col-idx-comment-align (:_printedColIdx prev-m))
                                          (reset! comment-aligned true))
                                        (recur (inc i))))))))

                            (let [num-spaces
                                  (cond
                                    (and top (:_rule3Active @top))
                                    (:_rule3NumSpaces @top)

                                    (and next-only-comment @comment-aligned)
                                    @col-idx-comment-align

                                    (and next-only-comment (not top))
                                    num-spaces-on-next-line

                                    :else
                                    (if-not top
                                      0
                                      (let [opener @top
                                            next-node (:_nextWithTextSkippingMeta opener)
                                            opener-col (or (:_printedColIdx opener) 0)]
                                        (cond
                                          (ns-parser/is-reader-conditional-opener opener)
                                          (+ opener-col (count (:text opener)))

                                          (and next-node (ns-parser/is-paren-opener next-node))
                                          (cond
                                            (ns-parser/is-map-literal-opener opener) (inc opener-col)
                                            (ns-parser/is-vector-literal-opener opener) (inc opener-col)
                                            (ns-parser/is-single-paren-opener opener) (inc opener-col)
                                            (ns-parser/is-set-literal-opener opener) (+ opener-col 2)
                                            (ns-parser/is-anon-fn-opener opener) (+ opener-col 2)
                                            :else (throw (Exception. "Error inside numSpacesForIndentation")))

                                          (ns-parser/is-map-literal-opener opener) (inc opener-col)
                                          (ns-parser/is-vector-literal-opener opener) (inc opener-col)
                                          (ns-parser/is-anon-fn-opener opener) (+ opener-col 3)
                                          (ns-parser/is-namespaced-map-opener opener) (+ opener-col (count (:text opener)))
                                          :else (+ opener-col 2)))))]

                              (let [indent-str (atom (repeat-string " " num-spaces))]
                                (when all-next-slurped
                                  (reset! newline-str "")
                                  (reset! indent-str ""))

                                (when (ns-parser/is-comma-node m)
                                  (let [trail (str/trimr (remove-leading-whitespace (:text m)))]
                                    (swap! indent-str str trail)))

                                (when (str-has-non-whitespace-chars @line-txt)
                                  (swap! out-txt str @line-txt))
                                (swap! out-txt str @newline-str)

                                (reset! line-txt @indent-str)
                                (reset! nodes-we-have-printed-on-this-line [])
                                (reset! col-idx (count @indent-str))
                                (swap! line-idx inc)
                                (when is-double-newline
                                  (swap! line-idx inc)))))))

                      (reset! skip-printing-this-node true)))

                  (when (and (ns-parser/node-contains-text @node) (not @skip-printing-this-node))
                    (let [next-txt-m (when next-text-node @next-text-node)
                          is-tok-fol-by-op (and (ns-parser/is-token-node @node) next-txt-m (ns-parser/is-paren-opener next-txt-m))
                          is-closer-fol-by-txt (and (ns-parser/is-paren-closer @node) next-txt-m
                                                    (or (ns-parser/is-token-node next-txt-m)
                                                        (ns-parser/is-paren-opener next-txt-m)))
                          add-space (or is-tok-fol-by-op is-closer-fol-by-txt)
                          node-txt (atom (:text @node))]

                      (when (ns-parser/is-comment-node @node)
                        (when (ns-parser/comment-needs-space-inside @node-txt)
                          (swap! node-txt str/replace-first #"^(;+)([^ ])" "$1 $2"))
                        (when (ns-parser/comment-needs-space-before @line-txt @node-txt)
                          (swap! node-txt #(str " " %))))

                      (cond
                        (and current-node-is-whitespace (or is-last-node (not @output-txt-contains-chars)))
                        (reset! skip-printing-this-node true)

                        (and (ns-parser/is-comment-node @node)
                             (= (get parsed-ns "commentOutsideNsForm") (:text @node))
                             (= @line-idx @line-idx-of-closing-ns-form))
                        (reset! skip-printing-this-node true)

                        (and current-node-is-whitespace (= @line-idx @line-idx-of-closing-ns-form))
                        (reset! skip-printing-this-node true))

                      (when-not @skip-printing-this-node
                        (let [line-len-before (count @line-txt)]
                          (swap! line-txt str @node-txt)
                          (when (not= @line-txt "")
                            (reset! output-txt-contains-chars true))

                          (swap! node assoc :_printedColIdx line-len-before
                                            :_printedLineIdx @line-idx)
                          (swap! nodes-we-have-printed-on-this-line conj node)))

                      (when add-space
                        (swap! line-txt str " "))

                      (swap! col-idx + (count @node-txt)))))))

            (swap! idx inc))))

      (when (not= @line-txt "")
        (swap! out-txt str @line-txt))

      ;; Replace ns form with formatted version
      (when (> @ns-start-string-idx 0)
        (let [head-str (subs @out-txt 0 (dec @ns-start-string-idx))
              ns-str (try
                       (format-ns parsed-ns)
                       (catch Exception e
                         (throw e)))
              tail-str (if (> @ns-end-string-idx 0)
                         (subs @out-txt (inc @ns-end-string-idx))
                         "")]
          (reset! out-txt (str head-str ns-str tail-str))))

      {:status "success"
       :out (str/trim @out-txt)})))

(defn format-text [^String input-txt]
  (let [cleaned-txt (crlf-to-lf input-txt)
        tree (parser/parse cleaned-txt)
        nodes-arr (ns-parser/flatten-tree tree)
        ignore-file? (ns-parser/look-for-ignore-file nodes-arr)]
    (if ignore-file?
      {:fileWasIgnored true
       :status "success"
       :out input-txt}
      (let [parsed-ns (try
                        (ns-parser/parse-ns nodes-arr)
                        (catch Exception e
                          {:status "error"
                           :reason (.getMessage e)}))]
        (if (= (:status parsed-ns) "error")
          parsed-ns
          (try
            (format-nodes nodes-arr parsed-ns)
            (catch Exception e
              {:status "error"
               :reason (.getMessage e)})))))))
