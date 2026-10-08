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

(defn sb-has-non-whitespace-chars [^StringBuilder sb]
  (let [n (.length sb)]
    (loop [i 0]
      (and (< i n)
           (or (not (Character/isWhitespace (.charAt sb i)))
               (recur (inc i)))))))

(defn is-space-or-comma [ch]
  (or (= ch \space) (= ch \,)))

(defn remove-trailing-whitespace! [^StringBuilder sb]
  (loop [end-idx (.length sb)]
    (if (and (> end-idx 0) (is-space-or-comma (.charAt sb (dec end-idx))))
      (recur (dec end-idx))
      (.setLength sb end-idx))))

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
        platforms (loop [itms arr
                         p-set (transient #{})]
                    (if (seq itms)
                      (let [itm (first itms)
                            p (get itm "platform")]
                        (if p
                          (if (= p ":default")
                            (do (reset! has-default true)
                                (recur (rest itms) p-set))
                            (recur (rest itms) (conj! p-set p)))
                          (recur (rest itms) p-set)))
                      (persistent! p-set)))]
    (let [sorted-platforms (sort platforms)]
      (if @has-default
        (vec (concat sorted-platforms [":default"]))
        (vec sorted-platforms)))))

(defn only-one-require-per-platform [reqs]
  (loop [idx 0
         counts (transient #{})]
    (if (>= idx (count reqs))
      true
      (let [req (nth reqs idx)
            p (get req "platform")]
        (if (and (string? p) (not= p ""))
          (if (contains? counts p)
            false
            (recur (inc idx) (conj! counts p)))
          (recur (inc idx) counts))))))

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
           parts (transient [])]
      (if (>= idx num-itms)
        (str/join ", " (persistent! parts))
        (let [itm (nth itms idx)
              pair-str (str (get itm "fromSymbol") " " (get itm "toSymbol"))]
          (recur (inc idx) (conj! parts pair-str)))))))

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
  (if-not refer-clojure
    []
    (let [keys-t (transient [])
          keys-t (if (get refer-clojure "exclude") (conj! keys-t ":exclude") keys-t)
          keys-t (if (get refer-clojure "only") (conj! keys-t ":only") keys-t)
          keys-t (if (get refer-clojure "rename") (conj! keys-t ":rename") keys-t)]
      (persistent! keys-t))))

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

;; Node kinds, as bits. The format loop asks the same few questions of every
;; node many times over; node-kind answers them once per node (and again when
;; the loop rewrites a node's text), and kind? tests one bit.

(defmacro ^:private kind-bit [k]
  (case k
    :whitespace 1
    :newline 2
    :comment 4
    :token 8
    :tag 16
    :paren-opener 32
    :paren-closer 64
    :text 128
    :non-blank-text 256
    :commas-after-newline 512
    :newline-with-comma 1024
    :comma 2048
    :ns 4096
    :ignore-keyword 8192))

(defmacro ^:private kind? [kinds i k]
  `(not (zero? (bit-and (aget ~kinds ~i) (kind-bit ~k)))))

;; node-kind answers what the ns-parser predicates would, but reads the node's
;; :name and :text once: asked through the predicates, each question re-reads
;; them, and the predicates re-ask each other.
(defn node-kind [m]
  (let [txt (:text m)
        has-text (and (string? txt) (not= txt ""))
        k (if has-text
            (cond-> (kind-bit :text)
              (not (identical? (.charAt ^String txt 0) \space)) (bit-or (kind-bit :non-blank-text)))
            0)]
    (case (:name m)
      "whitespace"
      (if (string? txt)
        (cond-> (bit-or k (kind-bit :whitespace))
          (str/includes? txt "\n") (bit-or (kind-bit :newline))
          ;; only a newline can have commas after it
          (txt-has-commas-after-newline txt) (bit-or (kind-bit :commas-after-newline)
                                                     (kind-bit :newline-with-comma))
          (str/includes? txt ",") (bit-or (kind-bit :comma)))
        (bit-or k (kind-bit :whitespace)))

      "comment" (bit-or k (kind-bit :comment))

      "token"
      (case txt
        "ns" (bit-or k (kind-bit :token) (kind-bit :ns))
        ":standard-clj/ignore" (bit-or k (kind-bit :token) (kind-bit :ignore-keyword))
        (bit-or k (kind-bit :token)))

      ".tag" (bit-or k (kind-bit :tag))

      ".open"
      (cond-> k
        (ns-parser/is-paren-opener m) (bit-or (kind-bit :paren-opener)))

      ".close"
      (case txt
        (")" "]" "}") (bit-or k (kind-bit :paren-closer))
        k)

      k)))

(defn format-nodes [nodes-arr parsed-ns]
  (let [num-nodes (count nodes-arr)
        has-parsed-ns-form (not (nil? (get parsed-ns "nsSymbol")))

        ;; Per-node state, indexed by node position. `nodes` holds each node's
        ;; current map: formatting rewrites some nodes' :text, and the
        ;; ns-parser predicates read it. The bookkeeping upstream keeps on the
        ;; node objects themselves lives in the arrays beside it.
        ^objects nodes (object-array nodes-arr)
        ^longs kinds (let [a (long-array num-nodes)]
                       (dotimes [i num-nodes]
                         (aset a i (long (node-kind (aget nodes i)))))
                       a)
        ^longs orig-col-idx (long-array num-nodes -1)
        ^longs printed-col-idx (long-array num-nodes -1)
        ^objects was-slurped-up (object-array num-nodes)
        ;; paren openers only
        ^longs paren-opener-line-idx (long-array num-nodes -1)
        ^objects opening-line-nodes (object-array num-nodes)
        ;; filled in on first use: most openers never start a line inside them
        ^objects next-with-text-skipping-meta (object-array num-nodes)
        ^objects rule3-active (object-array num-nodes)
        ^longs rule3-num-spaces (long-array num-nodes)
        ^objects rule3-search-complete (object-array num-nodes)

        paren-nesting-depth (volatile! 0)
        idx (volatile! 0)
        out-sb (StringBuilder.)
        output-txt-contains-chars (volatile! false)
        line-sb (StringBuilder.)
        line-idx (volatile! 0)
        inside-ns-form (volatile! false)
        line-idx-of-closing-ns-form (volatile! -1)
        ns-start-string-idx (volatile! -1)
        ns-end-string-idx (volatile! -1)
        ignore-nodes-start-id (volatile! -1)
        ignore-nodes-end-id (volatile! -1)
        inside-the-ignore-zone (volatile! false)

        paren-stack (volatile! [])
        nodes-we-have-printed-on-this-line (volatile! [])]

    (letfn [(node-at [i] (aget nodes i))

            (next-text-skipping-meta [opener-i]
              (let [cached (aget next-with-text-skipping-meta opener-i)]
                (if (some? cached)
                  (when-not (identical? cached ::none) cached)
                  (let [found (ns-parser/find-next-text-node-skipping-meta nodes-arr (inc opener-i))]
                    (aset next-with-text-skipping-meta opener-i (if (some? found) found ::none))
                    found))))

            (set-text! [i txt]
              (let [m (assoc (aget nodes i) :text txt)]
                (aset nodes i m)
                (aset kinds i (node-kind m))))

            (find-next-text-node [i]
              (loop [j i]
                (when (< j num-nodes)
                  (let [txt (:text (aget nodes j))]
                    (if (and (string? txt) (not= txt ""))
                      j
                      (recur (inc j)))))))

            ;; record original col indexes for the line that starts at start-i
            (record-orig-col-idxs! [start-i initial-spaces]
              (loop [i start-i
                     col initial-spaces]
                (when (< i num-nodes)
                  (let [nd-m (aget nodes i)]
                    (when-not (kind? kinds i :newline)
                      (let [txt (:text nd-m)]
                        (cond
                          (and (string? txt) (not= txt ""))
                          (do
                            (aset orig-col-idx i (long col))
                            (recur (inc i) (+ col (count txt))))

                          (kind? kinds i :tag)
                          (do
                            (aset orig-col-idx i (long col))
                            (recur (inc i) (inc col)))

                          :else
                          (recur (inc i) col))))))))]

      (while (< @idx num-nodes)
        (let [i @idx]
          (when (kind? kinds i :tag)
            (set-text! i "#"))
          (let [m (node-at i)]
            (when (and (> @ignore-nodes-start-id 0) (= (:id m) @ignore-nodes-start-id))
              (vreset! inside-the-ignore-zone true)
              (.append out-sb line-sb)
              (.setLength line-sb 0))

            (if @inside-the-ignore-zone
              (do
                (when (and (string? (:text m)) (not= (:text m) ""))
                  (.append out-sb ^String (:text m)))
                (when (= (:id m) @ignore-nodes-end-id)
                  (vreset! ignore-nodes-start-id -1)
                  (vreset! ignore-nodes-end-id -1)
                  (vreset! inside-the-ignore-zone false)))

              (do
                (when (= i 0)
                  (let [first-m (node-at 0)]
                    (if (kind? kinds 0 :newline)
                      (record-orig-col-idxs! 1 (num-spaces-after-newline first-m))
                      (record-orig-col-idxs! 0 0))))

                (when (and (= @ns-start-string-idx -1) (= @paren-nesting-depth 1)
                           has-parsed-ns-form (kind? kinds i :ns))
                  (vreset! inside-ns-form true)
                  (vreset! ns-start-string-idx (+ (.length out-sb) (.length line-sb))))

                (let [next-text-idx (find-next-text-node (inc i))
                      is-last-node (>= (inc i) num-nodes)
                      current-node-is-whitespace (kind? kinds i :whitespace)
                      current-node-is-newline (kind? kinds i :newline)
                      skip-printing-this-node (volatile! false)]

                  ;; :standard-clj/ignore check
                  (when (and (kind? kinds i :ignore-keyword) (> i 1))
                    (let [deref-nodes nodes-arr
                          prev-node1 (ns-parser/find-prev-node-with-text deref-nodes i (:id m))
                          prev-node2 (when prev-node1 (ns-parser/find-prev-node-with-text deref-nodes i (:id prev-node1)))
                          is-discard-map (and prev-node1 (= (:name prev-node1) ".open") (= (:text prev-node1) "{")
                                              prev-node2 (ns-parser/is-discard-node prev-node2))]
                      (cond
                        (or (ns-parser/is-discard-node prev-node1)
                            (and (ns-parser/is-whitespace-node prev-node1) (ns-parser/is-discard-node prev-node2)))
                        (let [next-ignore-node (ns-parser/find-next-non-whitespace-node deref-nodes (inc i))]
                          (if (and (vector? (:children next-ignore-node)) (seq (:children next-ignore-node)))
                            (let [closing-node (last (:children next-ignore-node))]
                              (vreset! ignore-nodes-start-id (:id next-ignore-node))
                              (vreset! ignore-nodes-end-id (:id closing-node)))
                            (let [next-immediate-node (nth deref-nodes (inc i))]
                              (vreset! ignore-nodes-start-id (:id next-immediate-node))
                              (vreset! ignore-nodes-end-id (:id next-ignore-node)))))

                        is-discard-map
                        (let [opening-brace-node (ns-parser/find-prev-node-with-predicate deref-nodes i ns-parser/is-opening-brace-node)
                              closing-brace-node-id (:id (nth (:children opening-brace-node) 2))
                              start-ignore-node (ns-parser/find-next-node-with-predicate-after-specific-node deref-nodes i (constantly true) closing-brace-node-id)
                              first-node-inside (ns-parser/find-next-node-with-predicate-after-specific-node deref-nodes i (constantly true) (:id start-ignore-node))]
                          (if (and (vector? (:children first-node-inside)) (seq (:children first-node-inside)))
                            (let [closing-node (last (:children first-node-inside))]
                              (vreset! ignore-nodes-start-id (:id start-ignore-node))
                              (vreset! ignore-nodes-end-id (:id closing-node)))
                            (do
                              (vreset! ignore-nodes-start-id (:id start-ignore-node))
                              (vreset! ignore-nodes-end-id (:id first-node-inside))))))))

                  (cond
                    (kind? kinds i :paren-opener)
                    (do
                      (when-let [top (peek @paren-stack)]
                        (when (= @line-idx (aget paren-opener-line-idx top))
                          (aset opening-line-nodes top (conj (aget opening-line-nodes top) i))))

                      (vswap! paren-nesting-depth inc)

                      (aset paren-opener-line-idx i (long @line-idx))
                      (aset opening-line-nodes i [])
                      (aset rule3-active i false)
                      (aset rule3-num-spaces i 0)
                      (aset rule3-search-complete i false)
                      (vswap! paren-stack conj i)

                      (when (and next-text-idx (kind? kinds next-text-idx :whitespace))
                        (set-text! next-text-idx "")))

                    (kind? kinds i :paren-closer)
                    (do
                      (vswap! paren-nesting-depth dec)
                      (vswap! paren-stack pop)
                      (when (and @inside-ns-form (= @paren-nesting-depth 0))
                        (vreset! inside-ns-form false)
                        (vreset! ns-end-string-idx (+ (.length out-sb) (.length line-sb)))
                        (vreset! line-idx-of-closing-ns-form @line-idx))))

                  (when-let [top (peek @paren-stack)]
                    (when (kind? kinds i :text)
                      (when (= @line-idx (aget paren-opener-line-idx top))
                        (aset opening-line-nodes top (conj (aget opening-line-nodes top) i)))))

                  (when (and current-node-is-whitespace (not current-node-is-newline)
                             next-text-idx (kind? kinds next-text-idx :paren-closer))
                    (vreset! skip-printing-this-node true))

                  (when (and current-node-is-whitespace (not current-node-is-newline)
                             next-text-idx (kind? kinds next-text-idx :comment))
                    (set-text! i (str/replace (:text (node-at i)) "," "")))

                  ;; Look forward to slurp closing parens
                  (when (and (seq @paren-stack) (not @inside-ns-form))
                    (let [is-comment-followed-by-nl (and (kind? kinds i :comment) next-text-idx (kind? kinds next-text-idx :newline))
                          has-commas-after-nl (or (kind? kinds i :commas-after-newline)
                                                  (and next-text-idx (kind? kinds next-text-idx :commas-after-newline)))
                          look-forward (cond
                                         has-commas-after-nl false
                                         is-comment-followed-by-nl true
                                         current-node-is-newline true
                                         :else false)]
                      (when look-forward
                        (let [closers (loop [j (inc i)
                                             acc []]
                                        (if (>= j num-nodes)
                                          acc
                                          (cond
                                            (kind? kinds j :newline-with-comma) acc
                                            (or (kind? kinds j :whitespace)
                                                (kind? kinds j :paren-closer)
                                                (kind? kinds j :comment))
                                            (recur (inc j) (conj acc j))
                                            :else acc)))
                              last-node-printed (peek @nodes-we-have-printed-on-this-line)
                              last-m (when last-node-printed (node-at last-node-printed))
                              trimmed? (and last-node-printed (kind? kinds last-node-printed :whitespace))]
                          (when trimmed?
                            (remove-trailing-whitespace! line-sb))

                          (doseq [closer-idx closers]
                            (let [c-m (node-at closer-idx)]
                              (when (kind? kinds closer-idx :paren-closer)
                                (.append line-sb ^String (:text c-m))
                                (set-text! closer-idx "")
                                (aset was-slurped-up closer-idx true)
                                (vswap! paren-nesting-depth dec)
                                (vswap! paren-stack pop))))

                          (when trimmed?
                            (.append line-sb ^String (:text last-m)))))))

                  (when current-node-is-newline
                    (record-orig-col-idxs! (inc i) (num-spaces-after-newline m))

                    (let [num-spaces-on-next-line (num-spaces-after-newline m)
                          all-next-slurped (loop [j (inc i)]
                                             (if (>= j num-nodes)
                                               true
                                               (cond
                                                 (kind? kinds j :newline) true
                                                 (not (string? (:text (aget nodes j)))) (recur (inc j))
                                                 (or (aget was-slurped-up j) (kind? kinds j :whitespace)) (recur (inc j))
                                                 :else false)))
                          next-only-comment (cond
                                              (< (+ i 2) num-nodes) (and (kind? kinds (inc i) :comment) (kind? kinds (+ i 2) :newline))
                                              (< (inc i) num-nodes) (kind? kinds (inc i) :comment)
                                              :else false)
                          next-comment-col (if next-only-comment num-spaces-on-next-line -1)
                          is-double-newline (str/includes? (:text m) "\n\n")]

                      (when @output-txt-contains-chars
                        (let [top (peek @paren-stack)]
                          ;; Rule 3 check
                          (when (and top (not (aget rule3-search-complete top)))
                            (let [op-nodes (aget opening-line-nodes top)
                                  num-op (count op-nodes)]
                              (when (> num-op 2)
                                (loop [k 1
                                       past-first-ws false]
                                  (when (< k num-op)
                                    (let [op-idx (nth op-nodes k)]
                                      (cond
                                        (and past-first-ws (kind? kinds op-idx :non-blank-text)
                                             (= (aget orig-col-idx op-idx) num-spaces-on-next-line))
                                        (do
                                          (aset rule3-active top true)
                                          (aset rule3-num-spaces top (aget printed-col-idx op-idx)))

                                        (and (not past-first-ws) (kind? kinds op-idx :whitespace))
                                        (recur (inc k) true)

                                        :else
                                        (recur (inc k) past-first-ws))))))
                              (aset rule3-search-complete top true)))

                          ;; Comment vertical alignment check
                          (let [col-idx-comment-align
                                (when (and next-only-comment (not is-double-newline))
                                  (let [printed-nodes @nodes-we-have-printed-on-this-line
                                        num-prev (count printed-nodes)]
                                    (loop [k 0]
                                      (when (< k num-prev)
                                        (let [prev-idx (nth printed-nodes k)
                                              possible (and (kind? kinds prev-idx :non-blank-text)
                                                            (or (= k 0)
                                                                (not (kind? kinds (nth printed-nodes (dec k)) :paren-opener))))]
                                          (if (and possible (= next-comment-col (aget orig-col-idx prev-idx)))
                                            (aget printed-col-idx prev-idx)
                                            (recur (inc k))))))))

                                num-spaces
                                (cond
                                  (and top (aget rule3-active top))
                                  (aget rule3-num-spaces top)

                                  (and next-only-comment col-idx-comment-align)
                                  col-idx-comment-align

                                  (and next-only-comment (not top))
                                  num-spaces-on-next-line

                                  :else
                                  (if-not top
                                    0
                                    (let [opener (node-at top)
                                          next-node (next-text-skipping-meta top)
                                          opener-col (aget printed-col-idx top)]
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
                                        :else (+ opener-col 2)))))

                                indent-str (cond-> (if all-next-slurped "" (repeat-string " " num-spaces))
                                             (kind? kinds i :comma)
                                             (str (str/trimr (remove-leading-whitespace (:text m)))))
                                newline-str (cond
                                              all-next-slurped ""
                                              is-double-newline "\n\n"
                                              :else "\n")]

                            (when (sb-has-non-whitespace-chars line-sb)
                              (.append out-sb line-sb))
                            (.append out-sb ^String newline-str)

                            (.setLength line-sb 0)
                            (.append line-sb ^String indent-str)
                            (vreset! nodes-we-have-printed-on-this-line [])
                            (vswap! line-idx inc)
                            (when is-double-newline
                              (vswap! line-idx inc)))))

                      (vreset! skip-printing-this-node true)))

                  (let [node-m (node-at i)]
                    (when (and (kind? kinds i :text) (not @skip-printing-this-node))
                      (let [is-tok-fol-by-op (and (kind? kinds i :token) next-text-idx (kind? kinds next-text-idx :paren-opener))
                            is-closer-fol-by-txt (and (kind? kinds i :paren-closer) next-text-idx
                                                      (or (kind? kinds next-text-idx :token)
                                                          (kind? kinds next-text-idx :paren-opener)))
                            add-space (or is-tok-fol-by-op is-closer-fol-by-txt)
                            node-txt (if (kind? kinds i :comment)
                                       (cond-> (:text node-m)
                                         (ns-parser/comment-needs-space-inside (:text node-m))
                                         (str/replace-first #"^(;+)([^ ])" "$1 $2")

                                         true
                                         (as-> t (if (ns-parser/comment-needs-space-before (str line-sb) t)
                                                   (str " " t)
                                                   t)))
                                       (:text node-m))]

                        (cond
                          (and current-node-is-whitespace (or is-last-node (not @output-txt-contains-chars)))
                          (vreset! skip-printing-this-node true)

                          (and (kind? kinds i :comment)
                               (= (get parsed-ns "commentOutsideNsForm") (:text node-m))
                               (= @line-idx @line-idx-of-closing-ns-form))
                          (vreset! skip-printing-this-node true)

                          (and current-node-is-whitespace (= @line-idx @line-idx-of-closing-ns-form))
                          (vreset! skip-printing-this-node true))

                        (when-not @skip-printing-this-node
                          (let [line-len-before (.length line-sb)]
                            (.append line-sb ^String node-txt)
                            (when (pos? (.length line-sb))
                              (vreset! output-txt-contains-chars true))

                            (aset printed-col-idx i (long line-len-before))
                            (vswap! nodes-we-have-printed-on-this-line conj i)))

                        (when add-space
                          (.append line-sb " ")))))))))

          (vswap! idx inc)))

      (when (pos? (.length line-sb))
        (.append out-sb line-sb))

      ;; Replace ns form with formatted version
      (let [out-txt (str out-sb)
            out-txt (if (> @ns-start-string-idx 0)
                      (str (subs out-txt 0 (dec @ns-start-string-idx))
                           (format-ns parsed-ns)
                           (if (> @ns-end-string-idx 0)
                             (subs out-txt (inc @ns-end-string-idx))
                             ""))
                      out-txt)]
        {:status "success"
         :out (str/trim out-txt)}))))

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
