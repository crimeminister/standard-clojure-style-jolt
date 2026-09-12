(ns standard-clojure-style.parse-ns
  (:require
   [clojure.string :as str]
   [standard-clojure-style.parser :as parser]))

;; -----------------------------------------------------------------------------
;; Node Predicates and Helpers

(defn node-contains-text [node]
  (and (map? node) (string? (:text node)) (not= (:text node) "")))

(defn is-node-with-non-blank-text [node]
  (and (node-contains-text node) (not= (first (:text node)) \space)))

(defn is-ns-node [node]
  (and (= (:name node) "token") (= (:text node) "ns")))

(defn is-use-node [node]
  (and (map? node) (string? (:text node)) (or (= (:text node) ":use") (= (:text node) "use"))))

(defn is-require-node [node]
  (and (map? node) (string? (:text node)) (or (= (:text node) ":require") (= (:text node) "require"))))

(defn is-require-macros-keyword [node]
  (and (map? node) (string? (:text node)) (= (:text node) ":require-macros")))

(defn is-refer-clojure-node [node]
  (and (map? node) (string? (:text node)) (or (= (:text node) ":refer-clojure") (= (:text node) "refer-clojure"))))

(defn is-exclude-keyword [node]
  (and (map? node) (string? (:text node)) (= (:text node) ":exclude")))

(defn is-only-keyword [node]
  (and (map? node) (string? (:text node)) (= (:text node) ":only")))

(defn is-rename-keyword [node]
  (and (map? node) (string? (:text node)) (= (:text node) ":rename")))

(defn is-as-keyword [node]
  (and (map? node) (string? (:text node)) (= (:text node) ":as")))

(defn is-as-alias-keyword [node]
  (and (map? node) (string? (:text node)) (= (:text node) ":as-alias")))

(defn is-refer-keyword [node]
  (and (map? node) (string? (:text node)) (= (:text node) ":refer")))

(defn is-default-keyword [node]
  (and (map? node) (string? (:text node)) (= (:text node) ":default")))

(defn is-refer-macros-keyword [node]
  (and (map? node) (string? (:text node)) (= (:text node) ":refer-macros")))

(defn is-include-macros-node [node]
  (and (map? node) (string? (:text node)) (= (:text node) ":include-macros")))

(defn is-boolean-node [node]
  (and (map? node) (string? (:text node)) (or (= (:text node) "true") (= (:text node) "false"))))

(defn is-all-node [node]
  (and (map? node) (string? (:text node)) (= (:text node) ":all")))

(defn is-keyword-node [node]
  (and (map? node) (string? (:text node)) (str/starts-with? (:text node) ":")))

(defn is-import-node [node]
  (and (map? node) (string? (:text node)) (or (= (:text node) ":import") (= (:text node) "import"))))

(defn is-newline-node [n]
  (and (map? n) (= (:name n) "whitespace") (string? (:text n)) (str/includes? (:text n) "\n")))

(defn is-whitespace-node [n]
  (and (map? n) (or (= (:name n) "whitespace") (is-newline-node n))))

(defn is-comma-node [n]
  (and (map? n) (= (:name n) "whitespace") (string? (:text n)) (str/includes? (:text n) ",")))

(defn is-namespaced-map-opener [n]
  (and (map? n) (= (:name n) ".open") (string? (:text n)) (str/starts-with? (:text n) "#:") (str/ends-with? (:text n) "{")))

(def paren-openers-set #{"(" "[" "{" "#{" "#(" "#?(" "#?@("})

(defn is-paren-opener [n]
  (and (map? n) (= (:name n) ".open") (or (contains? paren-openers-set (:text n)) (is-namespaced-map-opener n))))

(defn is-paren-closer [n]
  (and (map? n) (= (:name n) ".close") (or (= (:text n) ")") (= (:text n) "]") (= (:text n) "}"))))

(defn is-token-node [n]
  (and (map? n) (= (:name n) "token")))

(defn is-tag-node [n]
  (and (map? n) (= (:name n) ".tag")))

(defn is-string-node [n]
  (and (map? n) (= (:name n) "string") (vector? (:children n)) (= (count (:children n)) 3) (= (:name (nth (:children n) 1)) ".body")))

(defn get-text-from-string-node [n]
  (:text (nth (:children n) 1)))

(defn is-comment-node [n]
  (and (map? n) (= (:name n) "comment")))

(defn is-reader-comment-node [n]
  (and (map? n) (= (:name n) "discard")))

(defn is-discard-node [n]
  (and (map? n) (= (:name n) "marker") (= (:text n) "#_")))

(defn is-standard-clj-ignore-keyword [n]
  (and (map? n) (= (:name n) "token") (= (:text n) ":standard-clj/ignore")))

(defn is-standard-clj-ignore-file-keyword [n]
  (and (map? n) (= (:name n) "token") (= (:text n) ":standard-clj/ignore-file")))

(defn node-contains-text-and-not-whitespace [n]
  (and (node-contains-text n) (not (is-whitespace-node n))))

(defn is-map-literal-opener [n]
  (and (map? n) (= (:name n) ".open") (= (:text n) "{")))

(defn is-vector-literal-opener [n]
  (and (map? n) (= (:name n) ".open") (= (:text n) "[")))

(defn is-single-paren-opener [n]
  (and (map? n) (= (:name n) ".open") (= (:text n) "(")))

(defn is-anon-fn-opener [n]
  (and (map? n) (= (:name n) ".open") (= (:text n) "#(")))

(defn is-set-literal-opener [n]
  (and (map? n) (= (:name n) ".open") (= (:text n) "#{")))

(defn is-reader-conditional-opener [n]
  (and (map? n) (= (:name n) ".open") (or (= (:text n) "#?(") (= (:text n) "#?@("))))

(defn is-opening-brace-node [n]
  (and (map? n) (= (:name n) "braces") (vector? (:children n)) (= (count (:children n)) 3)
       (= (:name (nth (:children n) 2)) ".close") (= (:text (nth (:children n) 2)) "}")))

(defn is-map-literal-node [n]
  (if (or (not (map? n)) (not= (:name n) "braces") (not (vector? (:children n))))
    false
    (let [c (:children n)
          cnt (count c)]
      (and (>= cnt 2)
           (is-map-literal-opener (first c))
           (= (:name (peek c)) ".close")
           (= (:text (peek c)) "}")))))

(defn is-vector-literal-node [n]
  (if (or (not (map? n)) (not= (:name n) "brackets") (not (vector? (:children n))))
    false
    (let [c (:children n)
          cnt (count c)]
      (and (>= cnt 2)
           (is-vector-literal-opener (first c))
           (= (:name (peek c)) ".close")
           (= (:text (peek c)) "]")))))

(defn is-meta-marker [n]
  (and (map? n) (= (:name n) ".marker") (or (= (:text n) "^") (= (:text n) "#^"))))

(defn is-string-opener [n]
  (and (map? n) (= (:name n) ".open") (or (= (:text n) "\"") (= (:text n) "#?\""))))

(defn is-string-closer [n]
  (and (map? n) (= (:name n) ".close") (= (:text n) "\"")))

(defn comment-needs-space-before [line-txt node-txt]
  (and (str/starts-with? node-txt ";")
       (not= line-txt "")
       (not (str/ends-with? line-txt " "))
       (not (str/ends-with? line-txt "("))
       (not (str/ends-with? line-txt "["))
       (not (str/ends-with? line-txt "{"))))

(defn comment-needs-space-inside [comment-txt]
  (and (not (re-find #"^;+ " comment-txt))
       (not (re-find #"^;+$" comment-txt))))

(defn is-gen-class-node [node]
  (and (map? node) (string? (:text node)) (= (:text node) ":gen-class")))

(def gen-class-keywords-set
  #{":name" ":extends" ":implements" ":init" ":constructors" ":post-init" ":methods"
    ":main" ":factory" ":state" ":exposes" ":exposes-methods" ":prefix" ":impl-ns" ":load-impl-ns"})

(defn is-gen-class-keyword [node]
  (and (map? node) (string? (:text node)) (contains? gen-class-keywords-set (:text node))))

(defn is-gen-class-name-key [key-txt]
  (contains? #{"name" "extends" "init" "post-init" "factory" "state" "impl-ns"} key-txt))

(defn is-gen-class-boolean-key [key-txt]
  (contains? #{"main" "load-impl-ns"} key-txt))

;; -----------------------------------------------------------------------------
;; Tree Traversal and Extraction

(defn recurse-all-children [node f]
  (f node)
  (when-let [children (:children node)]
    (doseq [c children]
      (recurse-all-children c f))))

(defn flatten-tree [tree]
  (let [nodes (atom [])]
    (recurse-all-children tree #(swap! nodes conj %))
    @nodes))

(defn get-text-from-root-node [root-node]
  (let [sb (StringBuilder.)]
    (recurse-all-children
      root-node
      (fn [n]
        (when (= (:name n) ".tag")
          (.append sb "#"))
        (when (and (string? (:text n)) (not= (:text n) ""))
          (.append sb ^String (:text n)))))
    (str sb)))

(defn get-metadata-strings-from-meta-node [meta-node]
  (let [metadata (atom [])
        marker (atom nil)]
    (doseq [child-node (:children meta-node)]
      (cond
        (is-meta-marker child-node)
        (reset! marker (:text child-node))

        (and @marker (= (:name child-node) ".meta"))
        (do
          (swap! metadata conj (str @marker (get-text-from-root-node child-node)))
          (reset! marker nil))))
    @metadata))

(defn get-body-node-from-meta-node [meta-node]
  (some #(when (= (:name %) ".body") %) (:children meta-node)))

(defn get-last-child-node-with-text [root-node]
  (let [last-node (atom nil)]
    (recurse-all-children
      root-node
      (fn [n]
        (when (and (string? (:text n)) (not= (:text n) ""))
          (reset! last-node n))))
    @last-node))

(defn get-direct-form-nodes-from-collection [collection-node]
  (let [children (:children collection-node)]
    (if (and (= (count children) 3) (= (:name (nth children 1)) ".body"))
      (let [body-children (:children (nth children 1))]
        (vec (filter #(and (not (is-whitespace-node %))
                           (not (is-comment-node %)))
                     body-children)))
      [])))

(defn get-gen-class-symbol-text [node]
  (cond
    (nil? node) nil
    (is-token-node node) (:text node)
    (is-string-node node) (str "\"" (get-text-from-string-node node) "\"")
    (= (:name node) "meta") (get-text-from-root-node node)
    :else nil))

(defn split-gen-class-metadata-form [node]
  (if (or (nil? node) (not= (:name node) "meta") (not (vector? (:children node))))
    {:bodyNode node :metadata nil}
    (let [children (:children node)
          n (count children)]
      (loop [i 0
             meta-str ""
             body-node nil]
        (if (>= i n)
          {:bodyNode body-node :metadata (when (seq meta-str) (str/trimr meta-str))}
          (let [c (nth children i)]
            (if (= (:name c) ".body")
              (let [b (when (= (count (:children c)) 1)
                        (first (:children c)))]
                {:bodyNode b :metadata (when (seq meta-str) (str/trimr meta-str))})
              (recur (inc i) (str meta-str (get-text-from-root-node c)) body-node))))))))

(defn parse-gen-class-type-vector [vector-node error-msg]
  (let [type-nodes (get-direct-form-nodes-from-collection vector-node)]
    (mapv (fn [node]
            (or (get-gen-class-symbol-text node)
                (throw (Exception. error-msg))))
          type-nodes)))

(defn parse-gen-class-parameter-types [vector-node]
  (parse-gen-class-type-vector vector-node ":gen-class :methods parameter types must be symbols inside a vector."))

(defn get-gen-class-map-pairs [map-node error-msg]
  (let [form-nodes (get-direct-form-nodes-from-collection map-node)]
    (when (odd? (count form-nodes))
      (throw (Exception. error-msg)))
    (vec (partition 2 form-nodes))))

(defn parse-gen-class-constructors [map-node]
  (let [err-msg ":gen-class :constructors must be a map of parameter and super-parameter vectors."
        pairs (get-gen-class-map-pairs map-node ":gen-class :constructors must contain parameter and super-parameter vector pairs.")]
    (mapv (fn [[param-node super-param-node]]
            (let [param-form (split-gen-class-metadata-form param-node)
                  param-body (:bodyNode param-form)]
              (when (or (not (is-vector-literal-node param-body))
                        (not (is-vector-literal-node super-param-node)))
                (throw (Exception. err-msg)))
              (let [m (cond-> {}
                        (and (string? (:metadata param-form)) (not= (:metadata param-form) ""))
                        (assoc "metadata" (:metadata param-form)))]
                (assoc m
                       "parameterTypes" (parse-gen-class-type-vector param-body err-msg)
                       "superParameterTypes" (parse-gen-class-type-vector super-param-node err-msg)))))
          pairs)))

(defn parse-gen-class-methods [vector-node]
  (let [method-nodes (get-direct-form-nodes-from-collection vector-node)]
    (mapv (fn [m-node]
            (let [method-form (split-gen-class-metadata-form m-node)
                  method-node (:bodyNode method-form)]
              (when (not (is-vector-literal-node method-node))
                (throw (Exception. ":gen-class :methods must be a vector of method vectors.")))
              (let [form-nodes (get-direct-form-nodes-from-collection method-node)]
                (when (not= (count form-nodes) 3)
                  (throw (Exception. ":gen-class :methods entries must contain a name, parameter-types vector, and return type.")))
                (let [m-name (get-gen-class-symbol-text (nth form-nodes 0))
                      param-node (nth form-nodes 1)
                      ret-type (get-gen-class-symbol-text (nth form-nodes 2))]
                  (when-not m-name
                    (throw (Exception. ":gen-class :methods method names must be symbols.")))
                  (when-not (is-vector-literal-node param-node)
                    (throw (Exception. ":gen-class :methods parameter types must be provided in a vector.")))
                  (when-not ret-type
                    (throw (Exception. ":gen-class :methods return types must be symbols.")))
                  (cond-> {}
                    (and (string? (:metadata method-form)) (not= (:metadata method-form) ""))
                    (assoc "metadata" (:metadata method-form))
                    true (assoc "name" m-name
                                "parameterTypes" (parse-gen-class-parameter-types param-node)
                                "returnType" ret-type))))))
          method-nodes)))

(defn parse-gen-class-exposes [map-node]
  (let [pairs (get-gen-class-map-pairs map-node ":gen-class :exposes must contain protected-field and accessor-map pairs.")]
    (mapv (fn [[field-node accessors-node]]
            (let [field-name (get-gen-class-symbol-text field-node)]
              (when-not field-name
                (throw (Exception. ":gen-class :exposes protected field names must be symbols.")))
              (when-not (is-map-literal-node accessors-node)
                (throw (Exception. ":gen-class :exposes field values must be maps containing :get and/or :set.")))
              (let [accessor-pairs (get-gen-class-map-pairs accessors-node ":gen-class :exposes accessor maps must contain keyword and method-name pairs.")
                    seen (atom #{})
                    expose (atom {"fieldName" field-name})]
                (doseq [[k-node v-node] accessor-pairs]
                  (let [k (when (is-token-node k-node) (:text k-node))
                        v (get-gen-class-symbol-text v-node)]
                    (when (and (not= k ":get") (not= k ":set"))
                      (throw (Exception. ":gen-class :exposes accessor keys must be :get or :set.")))
                    (when (contains? @seen k)
                      (throw (Exception. ":gen-class :exposes accessor maps cannot repeat :get or :set.")))
                    (when-not v
                      (throw (Exception. ":gen-class :exposes accessor method names must be symbols.")))
                    (swap! seen conj k)
                    (if (= k ":get")
                      (swap! expose assoc "getter" v)
                      (swap! expose assoc "setter" v))))
                @expose)))
          pairs)))

(defn parse-gen-class-exposes-methods [map-node]
  (let [pairs (get-gen-class-map-pairs map-node ":gen-class :exposes-methods must contain superclass-method and exposed-name pairs.")]
    (mapv (fn [[super-node exposed-node]]
            (let [super-method (get-gen-class-symbol-text super-node)
                  exposed (get-gen-class-symbol-text exposed-node)]
              (when (or (not super-method) (not exposed))
                (throw (Exception. ":gen-class :exposes-methods names must be symbols.")))
              {"superMethodName" super-method
               "exposedName" exposed}))
          pairs)))

;; -----------------------------------------------------------------------------
;; Node Search Utilities

(defn find-next-node-with-text [all-nodes idx]
  (let [n (count all-nodes)]
    (loop [i idx]
      (when (< i n)
        (let [node (nth all-nodes i)]
          (if (and (string? (:text node)) (not= (:text node) ""))
            node
            (recur (inc i))))))))

(defn find-next-non-whitespace-node [all-nodes idx]
  (let [n (count all-nodes)]
    (loop [i idx]
      (when (< i n)
        (let [node (nth all-nodes i)]
          (if (not (is-whitespace-node node))
            node
            (recur (inc i))))))))

(defn find-prev-node-with-text [all-nodes start-idx starting-node-id]
  (let [n (count all-nodes)]
    (loop [i start-idx
           before? false]
      (when (>= i 0)
        (let [node (nth all-nodes i)]
          (if-not before?
            (recur (dec i) (= (:id node) starting-node-id))
            (if (node-contains-text node)
              node
              (recur (dec i) true))))))))

(defn find-next-node-with-predicate-after-specific-node [all-nodes start-idx pred specific-node-id]
  (let [n (count all-nodes)]
    (loop [i start-idx
           after? false]
      (when (< i n)
        (let [node (nth all-nodes i)]
          (if-not after?
            (recur (inc i) (= (:id node) specific-node-id))
            (if (pred node)
              node
              (recur (inc i) true))))))))

(defn find-prev-node-with-predicate [all-nodes start-idx pred]
  (loop [i start-idx]
    (when (>= i 0)
      (let [node (nth all-nodes i)]
        (if (pred node)
          node
          (recur (dec i)))))))

(defn find-next-token-inside-require-form [all-nodes idx]
  (let [n (count all-nodes)]
    (loop [i idx]
      (when (< i n)
        (let [node (nth all-nodes i)]
          (cond
            (is-paren-closer node) nil
            (and (is-token-node node) (not= (:text node) "")) node
            :else (recur (inc i))))))))

(defn find-next-text-node-skipping-meta [nodes-arr start-idx]
  (let [max-idx (count nodes-arr)]
    (loop [idx start-idx
           paren-depth 0
           string-depth 0
           forms-to-skip 0]
      (if (>= idx max-idx)
        nil
        (let [node (nth nodes-arr idx)]
          (if (and (node-contains-text node)
                   (not (is-whitespace-node node))
                   (not (is-comment-node node)))
            (cond
              (is-paren-opener node)
              (if (= string-depth 0)
                (if (and (> forms-to-skip 0) (= paren-depth 0))
                  (recur (inc idx) 1 string-depth forms-to-skip)
                  (if (> paren-depth 0)
                    (recur (inc idx) (inc paren-depth) string-depth forms-to-skip)
                    node))
                (recur (inc idx) paren-depth string-depth forms-to-skip))

              (is-paren-closer node)
              (if (= string-depth 0)
                (if (> paren-depth 0)
                  (let [new-depth (dec paren-depth)]
                    (if (= new-depth 0)
                      (recur (inc idx) new-depth string-depth (dec forms-to-skip))
                      (recur (inc idx) new-depth string-depth forms-to-skip)))
                  node)
                (recur (inc idx) paren-depth string-depth forms-to-skip))

              (is-string-opener node)
              (if (and (> forms-to-skip 0) (= paren-depth 0) (= string-depth 0))
                (recur (inc idx) paren-depth 1 forms-to-skip)
                (if (or (> paren-depth 0) (> string-depth 0))
                  (recur (inc idx) paren-depth (inc string-depth) forms-to-skip)
                  node))

              (is-string-closer node)
              (if (> string-depth 0)
                (let [new-depth (dec string-depth)]
                  (if (and (= new-depth 0) (= paren-depth 0))
                    (recur (inc idx) paren-depth new-depth (dec forms-to-skip))
                    (recur (inc idx) paren-depth new-depth forms-to-skip)))
                node)

              :else
              (cond
                (or (> paren-depth 0) (> string-depth 0))
                (recur (inc idx) paren-depth string-depth forms-to-skip)

                (or (is-meta-marker node) (is-discard-node node))
                (recur (inc idx) paren-depth string-depth (inc forms-to-skip))

                (> forms-to-skip 0)
                (recur (inc idx) paren-depth string-depth (dec forms-to-skip))

                :else node))
            (recur (inc idx) paren-depth string-depth forms-to-skip)))))))

;; -----------------------------------------------------------------------------
;; Namespace Comparisons and Sorting

(defn compare-symbols-then-platform [itm-a itm-b]
  (let [sa (get itm-a "symbol")
        sb (get itm-b "symbol")
        c (compare sa sb)]
    (if (not= c 0)
      c
      (let [pa (get itm-a "platform")
            pb (get itm-b "platform")]
        (cond
          (and pa (not pb)) 1
          (and pb (not pa)) -1
          (and pa pb) (compare pa pb)
          :else 0)))))

(defn compare-from-symbol [itm-a itm-b]
  (compare (get itm-a "fromSymbol") (get itm-b "fromSymbol")))

(defn compare-imports [import-a import-b]
  (compare (get import-a "package") (get import-b "package")))

(defn looks-like-a-java-classname [^String s]
  (if (and s (> (count s) 0))
    (let [first-char (subs s 0 1)]
      (= (str/upper-case first-char) first-char))
    false))

(defn parse-java-package-with-class [^String s]
  (let [chunks (str/split s #"\.")
        last-itm (last chunks)]
    (if (looks-like-a-java-classname last-itm)
      (let [package-chunks (butlast chunks)
            package-name (str/join "." package-chunks)]
        {"package" package-name
         "className" last-itm})
      {"package" s
       "className" nil})))

(defn sort-ns-result [result prefix-list-comments]
  (let [res (atom result)]
    (when-let [exc (get-in @res ["referClojure" "exclude"])]
      (when (vector? exc)
        (swap! res assoc-in ["referClojure" "exclude"] (vec (sort compare-symbols-then-platform exc)))))

    (when-let [onl (get-in @res ["referClojure" "only"])]
      (when (vector? onl)
        (swap! res assoc-in ["referClojure" "only"] (vec (sort compare-symbols-then-platform onl)))))

    (when-let [ren (get-in @res ["referClojure" "rename"])]
      (when (vector? ren)
        (swap! res assoc-in ["referClojure" "rename"] (vec (sort compare-from-symbol ren)))))

    (when-let [impl (get-in @res ["genClass" "implements"])]
      (when (vector? impl)
        (let [m (meta impl)]
          (swap! res assoc-in ["genClass" "implements"] (with-meta (vec (sort compare-symbols-then-platform impl)) m)))))

    (when-let [rm (get @res "requireMacros")]
      (when (vector? rm)
        (let [sorted-rm (mapv (fn [m]
                                (if-let [r (get m "refer")]
                                  (if (vector? r)
                                    (assoc m "refer" (vec (sort compare-symbols-then-platform r)))
                                    m)
                                  m))
                              (sort compare-symbols-then-platform rm))]
          (swap! res assoc "requireMacros" sorted-rm))))

    (when-let [reqs (get @res "requires")]
      (when (vector? reqs)
        (let [sorted-reqs (sort compare-symbols-then-platform reqs)
              plc (atom prefix-list-comments)
              processed-reqs
              (mapv (fn [req]
                      (let [req-atom (atom req)]
                        (when-let [plid (get @req-atom "prefixListId")]
                          (when-let [c (get @plc plid)]
                            (when-let [ca (get c "commentsAbove")]
                              (swap! req-atom assoc "commentsAbove" ca))
                            (when-let [cafter (get c "commentAfter")]
                              (swap! req-atom assoc "commentAfter" cafter))
                            (swap! plc dissoc plid))
                          (swap! req-atom dissoc "prefixListId"))
                        (when-let [r (get @req-atom "refer")]
                          (when (vector? r)
                            (swap! req-atom assoc "refer" (vec (sort compare-symbols-then-platform r)))))
                        (when-let [e (get @req-atom "exclude")]
                          (when (vector? e)
                            (swap! req-atom assoc "exclude" (vec (sort compare-symbols-then-platform e)))))
                        (when-let [ren (get @req-atom "rename")]
                          (when (vector? ren)
                            (swap! req-atom assoc "rename" (vec (sort compare-from-symbol ren)))))
                        @req-atom))
                    sorted-reqs)]
          (swap! res assoc "requires" processed-reqs))))

    (when-let [imports-obj (get @res "importsObj")]
      (let [imports-list
            (mapv (fn [[pkg-name obj]]
                    (let [classes (get obj "classes")
                          non-nil-classes (vec (remove nil? classes))
                          sorted-classes (vec (sort non-nil-classes))
                          m (cond-> {"package" pkg-name
                                     "classes" sorted-classes}
                              (get obj "commentsAbove") (assoc "commentsAbove" (get obj "commentsAbove"))
                              (get obj "commentAfter") (assoc "commentAfter" (get obj "commentAfter"))
                              (get obj "platform") (assoc "platform" (get obj "platform")))]
                      m))
                  imports-obj)]
        (swap! res dissoc "importsObj")
        (swap! res assoc "imports" (vec (sort compare-imports imports-list)))))

    (when-let [meta-list (get @res "nsMetadata")]
      (when (> (count meta-list) 1)
        (let [meta-map (atom {})
              meta-keys (atom [])]
          (doseq [itm meta-list]
            (swap! meta-map assoc (get itm "key") (get itm "value"))
            (swap! meta-keys conj (get itm "key")))
          (let [new-meta (atom [])
                seen (atom #{})]
            (doseq [k (reverse @meta-keys)]
              (when-not (@seen k)
                (swap! seen conj k)
                (swap! new-meta conj {"key" k "value" (get @meta-map k)})))
            (swap! res assoc "nsMetadata" (vec (reverse @new-meta)))))))

    @res))

(defn look-for-ignore-file [nodes-arr]
  (let [n (count nodes-arr)]
    (loop [i 0]
      (if (>= i n)
        false
        (let [node (nth nodes-arr i)]
          (cond
            (is-discard-node node)
            (let [next1 (find-next-node-with-predicate-after-specific-node nodes-arr i node-contains-text-and-not-whitespace (:id node))]
              (cond
                (is-standard-clj-ignore-file-keyword next1) true
                (= (:text next1) "{")
                (let [next2 (find-next-node-with-predicate-after-specific-node nodes-arr i node-contains-text-and-not-whitespace (:id next1))]
                  (if (is-standard-clj-ignore-file-keyword next2)
                    (let [next3 (find-next-node-with-predicate-after-specific-node nodes-arr i node-contains-text-and-not-whitespace (:id next2))]
                      (if (and (= (:name next3) "token") (= (:text next3) "true"))
                        true
                        (recur (inc i))))
                    (recur (inc i))))
                :else (recur (inc i))))

            (is-ns-node node) false
            :else (recur (inc i))))))))

;; -----------------------------------------------------------------------------
;; parseNs

(defn parse-ns [nodes-arr]
  (let [num-nodes (count nodes-arr)
        result (atom {"nsSymbol" nil})
        prefix-list-comments (atom {})

        ;; State variables
        continue-parsing-ns-form (atom true)
        ns-form-ends-line-idx (atom -1)
        paren-nesting-depth (atom 0)
        line-no (atom 0)
        paren-stack (atom [])
        inside-ns-form (atom false)
        inside-refer-clojure-form (atom false)
        refer-clojure-paren-nesting-depth (atom -1)
        inside-require-form (atom false)
        require-form-paren-nesting-depth (atom -1)
        require-form-line-no (atom -1)
        inside-import-form (atom false)
        import-form-line-no (atom -1)
        next-text-node-is-ns-symbol (atom false)
        inside-import-package-list (atom false)
        collect-refer-clojure-exclude-symbols (atom false)
        collect-refer-clojure-only-symbols (atom false)
        collect-refer-clojure-rename-symbols (atom false)
        collect-require-exclude-symbols (atom false)
        require-exclude-symbol-paren-depth (atom -1)
        renames-tmp (atom [])
        import-package-list-first-token (atom nil)
        ns-node-idx (atom -1)
        ns-symbol-idx (atom -1)
        beyond-ns-metadata (atom false)
        inside-ns-metadata-hash-map (atom false)
        inside-ns-metadata-shorthand (atom false)
        next-token-node-is-metadata-true-key (atom false)
        next-text-node-is-metadata-key (atom false)
        metadata-value-node-id (atom -1)
        tmp-metadata-key (atom "")
        refer-clojure-node-idx (atom -1)
        require-node-idx (atom -1)
        refer-idx (atom -1)
        refer-paren-nesting-depth (atom -1)
        import-node-idx (atom -1)
        import-node-paren-nesting-depth (atom -1)
        active-require-idx (atom -1)
        require-symbol-idx (atom -1)
        next-token-is-as-symbol (atom false)
        single-line-comments (atom [])
        active-import-package-name (atom nil)
        prev-node-is-newline (atom false)
        line-of-last-comment-recording (atom -1)
        inside-prefix-list (atom false)
        prefix-list-paren-nesting-depth (atom -1)
        prefix-list-prefix (atom nil)
        prefix-list-line-no (atom -1)
        current-prefix-list-id (atom nil)
        inside-reader-conditional (atom false)
        current-reader-conditional-platform (atom nil)
        reader-conditional-paren-nesting-depth (atom -1)
        inside-require-list (atom false)
        require-list-paren-nesting-depth (atom -1)
        refer-macros-idx (atom -1)
        refer-macros-paren-nesting-depth (atom -1)
        inside-include-macros (atom false)
        active-require-macros-idx (atom -1)
        inside-require-macros-form (atom false)
        require-macros-node-idx (atom -1)
        require-macros-line-no (atom -1)
        require-macros-paren-nesting-depth (atom -1)
        require-macros-refer-node-idx (atom -1)
        require-macros-as-node-idx (atom -1)
        require-macros-rename-idx (atom -1)
        gen-class-node-idx (atom -1)
        inside-gen-class (atom false)
        gen-class-line-no (atom -1)
        gen-class-toggle (atom 0)
        gen-class-key-str (atom nil)
        gen-class-value-line-no (atom -1)
        inside-reader-comment (atom false)
        id-of-last-node-inside-reader-comment (atom -1)
        rename-idx (atom -1)
        rename-paren-nesting-depth (atom -1)
        skip-nodes-until-we-reach-this-id (atom -1)
        section-to-attach-eol-comments-to (atom nil)
        next-token-is-require-default-symbol (atom false)
        num-symbols-inside-list (atom 0)
        inside-gen-class-implements (atom false)
        gen-class-implements-paren-depth (atom -1)
        gen-class-value-last-node-id (atom -1)
        pending-require-metadata (atom [])
        pending-reader-comment-line-no (atom -1)

        idx (atom 0)]

    (while @continue-parsing-ns-form
      (let [node (nth nodes-arr @idx)
            current-node-is-newline (is-newline-node node)
            is-token-node2 (is-token-node node)
            is-text-node (node-contains-text node)
            node-has-non-blank-text (is-node-with-non-blank-text node)]

        (when (and (>= @paren-nesting-depth 1) is-token-node2 node-has-non-blank-text)
          (swap! num-symbols-inside-list inc))

        (cond
          (and (= @paren-nesting-depth 1) (is-ns-node node) (= @num-symbols-inside-list 1))
          (do
            (reset! inside-ns-form true)
            (reset! next-text-node-is-ns-symbol true)
            (reset! ns-node-idx @idx))

          (and @inside-ns-form (is-refer-clojure-node node))
          (do
            (reset! inside-refer-clojure-form true)
            (reset! refer-clojure-paren-nesting-depth @paren-nesting-depth)
            (reset! section-to-attach-eol-comments-to "refer-clojure")
            (reset! refer-clojure-node-idx @idx)
            (reset! beyond-ns-metadata true))

          (and @inside-ns-form (is-require-node node))
          (do
            (reset! inside-require-form true)
            (reset! require-form-paren-nesting-depth @paren-nesting-depth)
            (reset! require-form-line-no @line-no)
            (reset! require-node-idx @idx)
            (reset! beyond-ns-metadata true)
            (reset! section-to-attach-eol-comments-to "require"))

          (and @inside-ns-form (is-import-node node))
          (do
            (reset! inside-import-form true)
            (reset! import-form-line-no @line-no)
            (reset! import-node-idx @idx)
            (reset! import-node-paren-nesting-depth @paren-nesting-depth)
            (reset! beyond-ns-metadata true)
            (reset! section-to-attach-eol-comments-to "import"))

          (and @inside-ns-form (is-require-macros-keyword node))
          (do
            (reset! inside-require-macros-form true)
            (reset! require-macros-node-idx @idx)
            (reset! require-macros-line-no @line-no)
            (reset! require-macros-paren-nesting-depth @paren-nesting-depth)
            (reset! beyond-ns-metadata true)
            (reset! section-to-attach-eol-comments-to "require-macros"))

          (and @inside-ns-form (is-gen-class-node node))
          (do
            (reset! inside-gen-class true)
            (reset! gen-class-node-idx @idx)
            (reset! beyond-ns-metadata true)
            (reset! section-to-attach-eol-comments-to "gen-class")))

        (if (is-paren-opener node)
          (do
            (swap! paren-nesting-depth inc)
            (swap! paren-stack conj node)
            (reset! num-symbols-inside-list 0)
            (cond
              (and @inside-ns-form (is-reader-conditional-opener node))
              (do
                (reset! inside-reader-conditional true)
                (reset! current-reader-conditional-platform nil)
                (reset! reader-conditional-paren-nesting-depth @paren-nesting-depth))

              (and @inside-require-form (= @require-list-paren-nesting-depth -1))
              (do
                (reset! inside-require-list true)
                (reset! require-list-paren-nesting-depth @paren-nesting-depth))

              (and @inside-import-form (> @paren-nesting-depth @import-node-paren-nesting-depth))
              (reset! inside-import-package-list true)

              (and @inside-gen-class (= @gen-class-toggle 1) (= @gen-class-key-str "implements"))
              (do
                (reset! inside-gen-class-implements true)
                (reset! gen-class-implements-paren-depth @paren-nesting-depth)
                (when-not (vector? (get-in @result ["genClass" "implements"]))
                  (let [existing (get-in @result ["genClass" "implements"])
                        c-above (when (map? existing) (get existing "commentsAbove"))]
                    (swap! result assoc-in ["genClass" "implements"] (with-meta [] (when c-above {"commentsAbove" c-above}))))))))

          (when (is-paren-closer node)
            (swap! paren-nesting-depth dec)
            (swap! paren-stack pop)

            (when (and @inside-ns-form (= @paren-nesting-depth 0))
              (reset! inside-ns-form false)
              (reset! ns-form-ends-line-idx @line-no))

            (when @inside-import-package-list
              (reset! inside-import-package-list false)
              (reset! import-package-list-first-token nil))

            (when (and @inside-require-form (< @paren-nesting-depth @require-form-paren-nesting-depth))
              (reset! inside-require-form false)
              (reset! require-form-paren-nesting-depth -1))

            (when (and @inside-refer-clojure-form (< @paren-nesting-depth @refer-clojure-paren-nesting-depth))
              (reset! inside-refer-clojure-form false)
              (reset! refer-clojure-node-idx -1))

            (when (and @inside-refer-clojure-form (<= @paren-nesting-depth @refer-clojure-paren-nesting-depth))
              (reset! collect-refer-clojure-exclude-symbols false)
              (reset! collect-refer-clojure-only-symbols false)
              (reset! collect-refer-clojure-rename-symbols false))

            (when (and (> @refer-idx 0) (<= @paren-nesting-depth @refer-paren-nesting-depth))
              (reset! refer-idx -1)
              (reset! refer-paren-nesting-depth -1))

            (when (and (> @rename-idx 0) (<= @paren-nesting-depth @rename-paren-nesting-depth))
              (reset! rename-idx -1)
              (reset! rename-paren-nesting-depth -1))

            (when (and @inside-require-list (< @paren-nesting-depth @require-list-paren-nesting-depth))
              (reset! inside-require-list false)
              (reset! require-list-paren-nesting-depth -1)
              (reset! next-token-is-require-default-symbol false))

            (when (and @inside-require-form (> @require-symbol-idx 0))
              (reset! require-symbol-idx -1))

            (when (and @inside-require-form @inside-prefix-list (not= @prefix-list-paren-nesting-depth -1) (= @paren-nesting-depth (dec @prefix-list-paren-nesting-depth)))
              (reset! inside-prefix-list false)
              (reset! prefix-list-prefix nil)
              (reset! prefix-list-paren-nesting-depth -1))

            (when (and @inside-reader-conditional (= @paren-nesting-depth (dec @reader-conditional-paren-nesting-depth)))
              (reset! inside-reader-conditional false)
              (reset! current-reader-conditional-platform nil)
              (reset! reader-conditional-paren-nesting-depth -1))

            (when (and (> @idx @refer-macros-idx) (<= @paren-nesting-depth @refer-macros-paren-nesting-depth))
              (reset! refer-macros-idx -1)
              (reset! refer-macros-paren-nesting-depth -1))

            (when (and @inside-import-form (< @paren-nesting-depth @import-node-paren-nesting-depth))
              (reset! inside-import-form false)
              (reset! import-node-idx -1)
              (reset! import-node-paren-nesting-depth -1))

            (when (and @inside-require-macros-form (< @paren-nesting-depth @require-macros-paren-nesting-depth))
              (reset! inside-require-macros-form false)
              (reset! require-macros-paren-nesting-depth -1)
              (reset! require-macros-node-idx -1)
              (reset! require-macros-as-node-idx -1))

            (when (and @collect-require-exclude-symbols (< @paren-nesting-depth @require-exclude-symbol-paren-depth))
              (reset! collect-require-exclude-symbols false)
              (reset! require-exclude-symbol-paren-depth -1))

            (when (and @inside-gen-class-implements (< @paren-nesting-depth @gen-class-implements-paren-depth))
              (reset! inside-gen-class-implements false)
              (reset! gen-class-implements-paren-depth -1)
              (reset! gen-class-toggle 0)
              (reset! gen-class-value-line-no @line-no))

            (reset! require-macros-refer-node-idx -1)
            (reset! require-macros-rename-idx -1)))

        (let [is-comment-node2 (is-comment-node node)
              is-reader-comment-node2 (is-reader-comment-node node)
              reader-comment-merge-window-is-open
              (and @inside-require-form
                   (= @pending-reader-comment-line-no @line-no)
                   (> (count @single-line-comments) 0))]

          (when is-reader-comment-node2
            (reset! inside-reader-comment true)
            (let [last-child (get-last-child-node-with-text node)]
              (reset! id-of-last-node-inside-reader-comment (:id last-child))))

          (cond
            (> @skip-nodes-until-we-reach-this-id 0)
            (when (= (:id node) @skip-nodes-until-we-reach-this-id)
              (reset! skip-nodes-until-we-reach-this-id -1)
              (when (= (:id node) @gen-class-value-last-node-id)
                (reset! gen-class-value-line-no @line-no)
                (reset! gen-class-value-last-node-id -1)))

            (and @inside-require-form (= @require-symbol-idx -1) (= (:name node) "meta"))
            (let [metadata (get-metadata-strings-from-meta-node node)]
              (doseq [m metadata]
                (swap! pending-require-metadata conj m))
              (when-let [body-node (get-body-node-from-meta-node node)]
                (reset! skip-nodes-until-we-reach-this-id (:id body-node))))

            @inside-ns-metadata-shorthand
            (cond
              (and (= (:name node) ".marker") (= (:text node) "^"))
              (reset! next-token-node-is-metadata-true-key true)

              (and @next-token-node-is-metadata-true-key is-token-node2)
              (do
                (when-not (get @result "nsMetadata")
                  (swap! result assoc "nsMetadata" []))
                (swap! result update "nsMetadata" conj {"key" (:text node) "value" "true"})
                (reset! next-token-node-is-metadata-true-key false)
                (reset! inside-ns-metadata-shorthand false)))

            @inside-ns-metadata-hash-map
            (cond
              (and @next-text-node-is-metadata-key (= (:name node) ".close") (= (:text node) "}"))
              (reset! inside-ns-metadata-hash-map false)

              (and (not @next-text-node-is-metadata-key) (= (:name node) ".open") (= (:text node) "{"))
              (reset! next-text-node-is-metadata-key true)

              (and @next-text-node-is-metadata-key is-token-node2)
              (do
                (when-not (get @result "nsMetadata")
                  (swap! result assoc "nsMetadata" []))
                (reset! tmp-metadata-key (:text node))
                (reset! next-text-node-is-metadata-key false)
                (let [next-non-ws (find-next-non-whitespace-node nodes-arr (inc @idx))]
                  (reset! metadata-value-node-id (:id next-non-ws))))

              (= (:id node) @metadata-value-node-id)
              (do
                (swap! result update "nsMetadata" conj {"key" @tmp-metadata-key "value" (get-text-from-root-node node)})
                (reset! tmp-metadata-key "")
                (reset! next-text-node-is-metadata-key true)
                (reset! metadata-value-node-id -1)
                (loop [cand node]
                  (when (and cand (vector? (:children cand)) (seq (:children cand)))
                    (let [last-c (last (:children cand))]
                      (reset! skip-nodes-until-we-reach-this-id (:id last-c))
                      (recur last-c))))))

            (and (not @inside-ns-metadata-hash-map) (not @inside-ns-metadata-shorthand)
                 @inside-ns-form (< @ns-symbol-idx 0) (= (:name node) "meta"))
            (let [marker-node (find-next-node-with-text nodes-arr (inc @idx))]
              (when (= (:text marker-node) "^")
                (let [node-after (find-next-node-with-text nodes-arr (+ @idx 2))]
                  (cond
                    (and node-after (= (:text node-after) "{"))
                    (reset! inside-ns-metadata-hash-map true)

                    (and node-after (is-token-node node-after))
                    (reset! inside-ns-metadata-shorthand true)))))

            (and @inside-ns-form (> @idx @ns-node-idx) (>= @paren-nesting-depth 1)
                 (not @beyond-ns-metadata) (not @inside-reader-comment)
                 (not @inside-ns-metadata-shorthand) (not @inside-ns-metadata-hash-map)
                 (= (:name node) ".open") (= (:text node) "{"))
            (do
              (reset! inside-ns-metadata-hash-map true)
              (reset! next-text-node-is-metadata-key true))

            (and (> @idx @ns-node-idx) @next-text-node-is-ns-symbol is-token-node2 is-text-node)
            (do
              (swap! result assoc "nsSymbol" (:text node))
              (reset! ns-symbol-idx @idx)
              (reset! next-text-node-is-ns-symbol false))

            (and @inside-reader-conditional (= @paren-nesting-depth @reader-conditional-paren-nesting-depth) (is-keyword-node node))
            (reset! current-reader-conditional-platform (:text node))

            (and @inside-ns-form (> @idx @ns-node-idx) @prev-node-is-newline is-comment-node2)
            (swap! single-line-comments conj (:text node))

            (and @inside-require-form (< @active-require-idx 0) (= @require-form-line-no @line-no)
                 (= @require-symbol-idx -1) (= @refer-idx -1) (= @rename-idx -1) is-reader-comment-node2)
            (do
              (swap! single-line-comments conj (get-text-from-root-node node))
              (reset! pending-reader-comment-line-no @line-no))

            (and reader-comment-merge-window-is-open is-comment-node2)
            (let [last-idx (dec (count @single-line-comments))]
              (swap! single-line-comments update last-idx #(str % " " (:text node)))
              (reset! pending-reader-comment-line-no -1))

            (and @inside-ns-form (> @idx @ns-node-idx) @prev-node-is-newline is-reader-comment-node2)
            (do
              (swap! single-line-comments conj (get-text-from-root-node node))
              (reset! pending-reader-comment-line-no @line-no))

            (and (> @idx @ns-node-idx) (not @prev-node-is-newline) (or is-comment-node2 is-reader-comment-node2))
            (let [comment-at-end (if is-comment-node2 (:text node) (get-text-from-root-node node))]
              (cond
                (= @prefix-list-line-no @line-no)
                (do
                  (when-not (get @prefix-list-comments @current-prefix-list-id)
                    (swap! prefix-list-comments assoc @current-prefix-list-id {}))
                  (swap! prefix-list-comments assoc-in [@current-prefix-list-id "commentAfter"] comment-at-end)
                  (reset! line-of-last-comment-recording @line-no))

                (and (= @require-form-line-no @line-no) (< @active-require-idx 0))
                (do
                  (swap! result assoc "requireCommentAfter" comment-at-end)
                  (reset! line-of-last-comment-recording @line-no))

                (and (= @require-form-line-no @line-no) (>= @active-require-idx 0))
                (do
                  (swap! result assoc-in ["requires" @active-require-idx "commentAfter"] comment-at-end)
                  (reset! line-of-last-comment-recording @line-no))

                (and (= @section-to-attach-eol-comments-to "refer-clojure") (get @result "referClojure"))
                (do
                  (swap! result assoc "referClojureCommentAfter" comment-at-end)
                  (reset! line-of-last-comment-recording @line-no))

                (and (= @import-form-line-no @line-no) (not (get @result "importsObj")))
                (do
                  (swap! result assoc "importCommentAfter" comment-at-end)
                  (reset! line-of-last-comment-recording @line-no))

                (= @import-form-line-no @line-no)
                (do
                  (swap! result assoc-in ["importsObj" @active-import-package-name "commentAfter"] comment-at-end)
                  (reset! line-of-last-comment-recording @line-no))

                (= @require-macros-line-no @line-no)
                (do
                  (swap! result assoc-in ["requireMacros" @active-require-macros-idx "commentAfter"] comment-at-end)
                  (reset! line-of-last-comment-recording @line-no))

                (= @gen-class-line-no @line-no)
                (do
                  (swap! result assoc-in ["genClass" "commentAfter"] comment-at-end)
                  (reset! line-of-last-comment-recording @line-no))

                (= @gen-class-value-line-no @line-no)
                (do
                  (if (vector? (get-in @result ["genClass" @gen-class-key-str]))
                    (swap! result update-in ["genClass" @gen-class-key-str] vary-meta assoc "commentAfter" comment-at-end)
                    (swap! result assoc-in ["genClass" @gen-class-key-str "commentAfter"] comment-at-end))
                  (reset! line-of-last-comment-recording @line-no)))

              (when (and (not @inside-ns-form) (= @line-no @line-of-last-comment-recording))
                (swap! result assoc "commentOutsideNsForm" comment-at-end)))

            @inside-reader-comment
            (when (= (:id node) @id-of-last-node-inside-reader-comment)
              (reset! inside-reader-comment false)
              (reset! id-of-last-node-inside-reader-comment -1))

            (and @inside-require-form (= @idx @require-node-idx) (seq @single-line-comments))
            (do
              (swap! result assoc "requireCommentsAbove" @single-line-comments)
              (reset! single-line-comments []))

            (and @inside-import-form (= @idx @import-node-idx) (seq @single-line-comments))
            (do
              (swap! result assoc "importCommentsAbove" @single-line-comments)
              (reset! single-line-comments []))

            (and @inside-refer-clojure-form (= @idx @refer-clojure-node-idx) (seq @single-line-comments))
            (do
              (swap! result assoc "referClojureCommentsAbove" @single-line-comments)
              (reset! single-line-comments []))

            (and @inside-ns-form (> @idx @ns-node-idx) (= @paren-nesting-depth 1)
                 (not @beyond-ns-metadata) (not @inside-ns-metadata-shorthand)
                 (not @inside-ns-metadata-hash-map) (is-string-node node))
            (swap! result assoc "docstring" (get-text-from-string-node node))

            (and @inside-refer-clojure-form (> @idx @refer-clojure-node-idx) (is-exclude-keyword node))
            (do
              (when-not (get @result "referClojure")
                (swap! result assoc "referClojure" {}))
              (when-not (vector? (get-in @result ["referClojure" "exclude"]))
                (swap! result assoc-in ["referClojure" "exclude"] []))
              (reset! collect-refer-clojure-exclude-symbols true))

            (and (> @idx (inc @refer-clojure-node-idx)) @collect-refer-clojure-exclude-symbols
                 (>= @paren-nesting-depth 3) is-token-node2 is-text-node
                 (get @result "referClojure") (vector? (get-in @result ["referClojure" "exclude"])))
            (let [sym-obj (cond-> {"symbol" (:text node)}
                            (and @inside-reader-conditional @current-reader-conditional-platform)
                            (assoc "platform" @current-reader-conditional-platform))]
              (swap! result update-in ["referClojure" "exclude"] conj sym-obj))

            (and @inside-refer-clojure-form (> @idx @refer-clojure-node-idx) (is-only-keyword node))
            (do
              (when-not (get @result "referClojure")
                (swap! result assoc "referClojure" {}))
              (swap! result assoc-in ["referClojure" "only"] [])
              (reset! collect-refer-clojure-only-symbols true))

            (and (> @idx (inc @refer-clojure-node-idx)) @collect-refer-clojure-only-symbols
                 (>= @paren-nesting-depth 3) is-token-node2 is-text-node
                 (get @result "referClojure") (vector? (get-in @result ["referClojure" "only"])))
            (let [sym-obj (cond-> {"symbol" (:text node)}
                            (and @inside-reader-conditional @current-reader-conditional-platform)
                            (assoc "platform" @current-reader-conditional-platform))]
              (swap! result update-in ["referClojure" "only"] conj sym-obj))

            (and @inside-refer-clojure-form (> @idx @refer-clojure-node-idx) (is-rename-keyword node))
            (do
              (when-not (get @result "referClojure")
                (swap! result assoc "referClojure" {}))
              (swap! result assoc-in ["referClojure" "rename"] [])
              (reset! collect-refer-clojure-rename-symbols true))

            (and (> @idx (inc @refer-clojure-node-idx)) @collect-refer-clojure-rename-symbols
                 (>= @paren-nesting-depth 3) is-token-node2 is-text-node
                 (get @result "referClojure") (vector? (get-in @result ["referClojure" "rename"])))
            (do
              (swap! renames-tmp conj (:text node))
              (when (= (count @renames-tmp) 2)
                (let [itm (cond-> {"fromSymbol" (first @renames-tmp)
                                   "toSymbol" (second @renames-tmp)}
                            (and @inside-reader-conditional @current-reader-conditional-platform)
                            (assoc "platform" @current-reader-conditional-platform))]
                  (swap! result update-in ["referClojure" "rename"] conj itm)
                  (reset! renames-tmp []))))

            (and (> @idx @require-node-idx) @inside-require-form is-token-node2 (is-as-keyword node))
            (reset! next-token-is-as-symbol true)

            (and (> @idx @require-node-idx) @inside-require-form @next-token-is-as-symbol is-token-node2 is-text-node)
            (do
              (reset! next-token-is-as-symbol false)
              (swap! result assoc-in ["requires" @active-require-idx "as"] (:text node)))

            (and @inside-require-macros-form (not= @require-macros-refer-node-idx -1)
                 (> @idx @require-macros-refer-node-idx) is-token-node2 is-text-node)
            (do
              (when-not (vector? (get-in @result ["requireMacros" @active-require-macros-idx "refer"]))
                (swap! result assoc-in ["requireMacros" @active-require-macros-idx "refer"] []))
              (let [refer-obj (cond-> {"symbol" (:text node)}
                                (and @inside-reader-conditional @current-reader-conditional-platform)
                                (assoc "platform" @current-reader-conditional-platform))]
                (swap! result update-in ["requireMacros" @active-require-macros-idx "refer"] conj refer-obj)))

            (and @inside-require-macros-form (not= @require-macros-as-node-idx -1)
                 (> @idx @require-macros-as-node-idx) is-token-node2 is-text-node)
            (do
              (swap! result assoc-in ["requireMacros" @active-require-macros-idx "as"] (:text node))
              (reset! require-macros-as-node-idx -1))

            (and @inside-require-macros-form (not= @require-macros-rename-idx -1)
                 (> @idx @require-macros-rename-idx) is-token-node2 is-text-node)
            (do
              (when-not (vector? (get-in @result ["requireMacros" @active-require-macros-idx "rename"]))
                (swap! result assoc-in ["requireMacros" @active-require-macros-idx "rename"] []))
              (swap! renames-tmp conj (:text node))
              (when (= (count @renames-tmp) 2)
                (let [itm (cond-> {"fromSymbol" (first @renames-tmp)
                                   "toSymbol" (second @renames-tmp)}
                            (and @inside-reader-conditional @current-reader-conditional-platform)
                            (assoc "platform" @current-reader-conditional-platform))]
                  (swap! result update-in ["requireMacros" @active-require-macros-idx "rename"] conj itm)
                  (reset! renames-tmp []))))

            (and @inside-require-macros-form (> @idx @require-macros-node-idx) (is-refer-keyword node))
            (reset! require-macros-refer-node-idx @idx)

            (and @inside-require-macros-form (> @idx @require-macros-node-idx) (is-as-keyword node))
            (reset! require-macros-as-node-idx @idx)

            (and @inside-require-macros-form (> @idx @require-macros-node-idx) (is-rename-keyword node))
            (do
              (reset! require-macros-rename-idx @idx)
              (reset! renames-tmp []))

            (and @inside-require-macros-form (> @idx @require-macros-node-idx) is-token-node2 is-text-node)
            (do
              (when-not (get @result "requireMacros")
                (swap! result assoc "requireMacros" [])
                (when (seq @single-line-comments)
                  (swap! result assoc "requireMacrosCommentsAbove" @single-line-comments)
                  (reset! single-line-comments [])))
              (let [req-obj (cond-> {"symbol" (:text node)}
                              (seq @single-line-comments)
                              (assoc "commentsAbove" @single-line-comments)
                              (and @inside-reader-conditional @current-reader-conditional-platform)
                              (assoc "platform" @current-reader-conditional-platform))]
                (reset! single-line-comments [])
                (swap! result update "requireMacros" conj req-obj)
                (swap! active-require-macros-idx inc)
                (reset! require-macros-line-no @line-no)))

            (and (> @idx @require-node-idx) @inside-require-form is-token-node2 (is-include-macros-node node))
            (reset! inside-include-macros true)

            (and @inside-include-macros is-token-node2 (is-boolean-node node))
            (do
              (swap! result assoc-in ["requires" @active-require-idx "includeMacros"] (= (:text node) "true"))
              (reset! inside-include-macros false))

            (and (> @idx @require-node-idx) @inside-require-form is-token-node2 (is-refer-macros-keyword node))
            (do
              (reset! refer-macros-idx @idx)
              (reset! refer-macros-paren-nesting-depth @paren-nesting-depth))

            (and (> @idx @refer-macros-idx) @inside-require-form
                 (= @paren-nesting-depth (inc @refer-macros-paren-nesting-depth))
                 is-token-node2 is-text-node)
            (do
              (when-not (vector? (get-in @result ["requires" @active-require-idx "referMacros"]))
                (swap! result assoc-in ["requires" @active-require-idx "referMacros"] []))
              (swap! result update-in ["requires" @active-require-idx "referMacros"] conj (:text node)))

            (and (> @idx @require-node-idx) @inside-require-form is-token-node2 (is-refer-keyword node))
            (do
              (reset! refer-idx @idx)
              (reset! refer-paren-nesting-depth @paren-nesting-depth))

            (and (> @idx @require-node-idx) @inside-require-form is-token-node2 (is-default-keyword node))
            (reset! next-token-is-require-default-symbol true)

            (and (> @idx @require-node-idx) @inside-require-form is-token-node2
                 @collect-require-exclude-symbols (> @paren-nesting-depth @require-exclude-symbol-paren-depth))
            (swap! result update-in ["requires" @active-require-idx "exclude"] conj {"symbol" (:text node)})

            (and (> @idx @require-node-idx) @inside-require-form is-token-node2 (is-exclude-keyword node))
            (do
              (swap! result assoc-in ["requires" @active-require-idx "exclude"] [])
              (reset! collect-require-exclude-symbols true)
              (reset! require-exclude-symbol-paren-depth @paren-nesting-depth))

            (and (> @idx @require-node-idx) @inside-require-form is-token-node2 (is-as-alias-keyword node))
            (let [next-sym (find-next-token-inside-require-form nodes-arr (inc @idx))]
              (swap! result assoc-in ["requires" @active-require-idx "asAlias"] (:text next-sym)))

            (and (> @idx @refer-idx) @inside-require-form is-token-node2 (is-all-node node))
            (swap! result assoc-in ["requires" @active-require-idx "refer"] "all")

            (and (> @idx @refer-idx) @inside-require-form is-token-node2 @next-token-is-require-default-symbol)
            (do
              (swap! result assoc-in ["requires" @active-require-idx "default"] (:text node))
              (reset! next-token-is-require-default-symbol false))

            (and @inside-require-form @inside-require-list (= @rename-idx -1) (is-rename-keyword node))
            (do
              (reset! rename-idx @idx)
              (reset! rename-paren-nesting-depth @paren-nesting-depth)
              (reset! renames-tmp []))

            (and @inside-require-form @inside-require-list (> @rename-idx 0) (> @idx @rename-idx)
                 (> @paren-nesting-depth @rename-paren-nesting-depth) is-token-node2 is-text-node)
            (do
              (swap! renames-tmp conj (:text node))
              (when (= (count @renames-tmp) 2)
                (let [itm (cond-> {"fromSymbol" (first @renames-tmp)
                                   "toSymbol" (second @renames-tmp)}
                            (and @inside-reader-conditional @current-reader-conditional-platform)
                            (assoc "platform" @current-reader-conditional-platform))]
                  (when-not (vector? (get-in @result ["requires" @active-require-idx "rename"]))
                    (swap! result assoc-in ["requires" @active-require-idx "rename"] []))
                  (swap! result update-in ["requires" @active-require-idx "rename"] conj itm)
                  (reset! renames-tmp []))))

            (and (> @idx @refer-idx) @inside-require-form (not= @refer-paren-nesting-depth -1)
                 (> @paren-nesting-depth @refer-paren-nesting-depth) is-token-node2 is-text-node)
            (do
              (when-not (vector? (get-in @result ["requires" @active-require-idx "refer"]))
                (swap! result assoc-in ["requires" @active-require-idx "refer"] []))
              (swap! result update-in ["requires" @active-require-idx "refer"] conj {"symbol" (:text node)}))

            (and @inside-require-form (not @inside-require-list) (> @idx @require-node-idx)
                 is-token-node2 is-text-node (= @require-symbol-idx -1) (not (is-keyword-node node)))
            (do
              (when-not (vector? (get @result "requires"))
                (swap! result assoc "requires" []))
              (let [req-obj (cond-> {"symbol" (:text node)}
                              (seq @pending-require-metadata)
                              (assoc "metadata" @pending-require-metadata)
                              (seq @single-line-comments)
                              (assoc "commentsAbove" @single-line-comments)
                              (and @inside-reader-conditional @current-reader-conditional-platform)
                              (assoc "platform" @current-reader-conditional-platform))]
                (reset! pending-require-metadata [])
                (reset! single-line-comments [])
                (swap! result update "requires" conj req-obj)
                (swap! active-require-idx inc)
                (reset! require-form-line-no @line-no)))

            (and @inside-prefix-list is-token-node2 is-text-node)
            (do
              (when-not (vector? (get @result "requires"))
                (swap! result assoc "requires" []))
              (let [ns-str (str @prefix-list-prefix "." (:text node))
                    req-obj (cond-> {"symbol" ns-str
                                     "prefixListId" @current-prefix-list-id}
                              (seq @pending-require-metadata)
                              (assoc "metadata" @pending-require-metadata))]
                (reset! pending-require-metadata [])
                (swap! result update "requires" conj req-obj)
                (swap! active-require-idx inc)
                (reset! require-symbol-idx @idx)
                (reset! require-form-line-no @line-no)))

            (and @inside-require-form @inside-require-list (> @idx @require-node-idx)
                 (= @refer-idx -1) (= @rename-idx -1) is-token-node2 is-text-node
                 (= @require-symbol-idx -1) (not (is-keyword-node node)))
            (do
              (when-not (vector? (get @result "requires"))
                (swap! result assoc "requires" []))
              (let [next-tok (find-next-token-inside-require-form nodes-arr (inc @idx))
                    is-prefix-list (and next-tok (not (is-keyword-node next-tok)))]
                (if is-prefix-list
                  (let [pl-id (parser/create-id)]
                    (reset! inside-prefix-list true)
                    (reset! prefix-list-paren-nesting-depth @paren-nesting-depth)
                    (reset! prefix-list-line-no @line-no)
                    (reset! prefix-list-prefix (:text node))
                    (reset! current-prefix-list-id pl-id)
                    (when (seq @single-line-comments)
                      (swap! prefix-list-comments assoc pl-id {"commentsAbove" @single-line-comments})
                      (reset! single-line-comments [])))
                  (let [req-obj (cond-> {"symbol" (:text node)}
                                  (seq @pending-require-metadata)
                                  (assoc "metadata" @pending-require-metadata)
                                  (seq @single-line-comments)
                                  (assoc "commentsAbove" @single-line-comments)
                                  (and @inside-reader-conditional @current-reader-conditional-platform)
                                  (assoc "platform" @current-reader-conditional-platform))]
                    (reset! pending-require-metadata [])
                    (reset! single-line-comments [])
                    (swap! result update "requires" conj req-obj)
                    (swap! active-require-idx inc)
                    (reset! require-symbol-idx @idx)
                    (reset! require-form-line-no @line-no)
                    (reset! inside-prefix-list false)
                    (reset! prefix-list-line-no -1)))))

            (and @inside-require-form @inside-require-list (> @idx @require-node-idx) (is-string-node node))
            (do
              (when-not (vector? (get @result "requires"))
                (swap! result assoc "requires" []))
              (let [req-obj (cond-> {"symbol" (str "\"" (get-text-from-string-node node) "\"")
                                     "symbolIsString" true}
                              (seq @pending-require-metadata)
                              (assoc "metadata" @pending-require-metadata)
                              (seq @single-line-comments)
                              (assoc "commentsAbove" @single-line-comments)
                              (and @inside-reader-conditional @current-reader-conditional-platform)
                              (assoc "platform" @current-reader-conditional-platform))]
                (reset! pending-require-metadata [])
                (reset! single-line-comments [])
                (swap! result update "requires" conj req-obj)
                (swap! active-require-idx inc)
                (reset! require-form-line-no @line-no)))

            (and @inside-import-form (> @idx @import-node-idx) (not @inside-import-package-list) is-token-node2 is-text-node)
            (do
              (when-not (get @result "importsObj")
                (swap! result assoc "importsObj" {}))
              (let [pkg-parsed (parse-java-package-with-class (:text node))
                    pkg-name (get pkg-parsed "package")
                    class-name (get pkg-parsed "className")]
                (when-not (get-in @result ["importsObj" pkg-name])
                  (swap! result assoc-in ["importsObj" pkg-name] {"classes" []}))
                (swap! result update-in ["importsObj" pkg-name "classes"] conj class-name)
                (reset! active-import-package-name pkg-name)
                (reset! import-form-line-no @line-no)
                (when (seq @single-line-comments)
                  (swap! result assoc-in ["importsObj" pkg-name "commentsAbove"] @single-line-comments)
                  (reset! single-line-comments []))
                (when (and @inside-reader-conditional @current-reader-conditional-platform)
                  (swap! result assoc-in ["importsObj" pkg-name "platform"] @current-reader-conditional-platform))))

            (and @inside-import-package-list is-token-node2 is-text-node)
            (if-not @import-package-list-first-token
              (let [pkg-name (:text node)]
                (reset! import-package-list-first-token pkg-name)
                (reset! active-import-package-name pkg-name)
                (reset! import-form-line-no @line-no)
                (when-not (get @result "importsObj")
                  (swap! result assoc "importsObj" {}))
                (when-not (get-in @result ["importsObj" pkg-name])
                  (swap! result assoc-in ["importsObj" pkg-name] {"classes" []}))
                (when (seq @single-line-comments)
                  (swap! result assoc-in ["importsObj" pkg-name "commentsAbove"] @single-line-comments)
                  (reset! single-line-comments []))
                (when (and @inside-reader-conditional @current-reader-conditional-platform)
                  (swap! result assoc-in ["importsObj" pkg-name "platform"] @current-reader-conditional-platform)))
              (swap! result update-in ["importsObj" @import-package-list-first-token "classes"] conj (:text node)))

            (and @inside-gen-class @inside-gen-class-implements (> @idx @gen-class-node-idx)
                 (= @gen-class-toggle 1) (= @gen-class-key-str "implements")
                 (or is-token-node2 (is-string-node node) (= (:name node) "meta")))
            (let [symbol (get-gen-class-symbol-text node)]
              (when-not symbol
                (throw (Exception. ":gen-class :implements values must be symbols or strings.")))
              (let [sym-obj (cond-> {"symbol" symbol}
                              (and @inside-reader-conditional @current-reader-conditional-platform)
                              (assoc "platform" @current-reader-conditional-platform))]
                (swap! result update-in ["genClass" "implements"] conj sym-obj)
                (when-not is-token-node2
                  (let [last-node (get-last-child-node-with-text node)]
                    (reset! skip-nodes-until-we-reach-this-id (:id last-node))))))

            (and @inside-gen-class (= @idx @gen-class-node-idx))
            (do
              (swap! result assoc "genClass" {"isEmpty" true})
              (when (and @inside-reader-conditional @current-reader-conditional-platform)
                (swap! result assoc-in ["genClass" "platform"] @current-reader-conditional-platform))
              (when (seq @single-line-comments)
                (swap! result assoc-in ["genClass" "commentsAbove"] @single-line-comments)
                (reset! single-line-comments []))
              (reset! gen-class-line-no @line-no))

            (and @inside-gen-class (> @idx @gen-class-node-idx) is-text-node
                 (= @gen-class-toggle 0) (is-gen-class-keyword node))
            (do
              (swap! result assoc-in ["genClass" "isEmpty"] false)
              (let [k-str (subs (:text node) 1)]
                (reset! gen-class-key-str k-str)
                (swap! result assoc-in ["genClass" k-str] {})
                (when (seq @single-line-comments)
                  (swap! result assoc-in ["genClass" k-str "commentsAbove"] @single-line-comments)
                  (reset! single-line-comments []))
                (reset! gen-class-toggle 1)))

            (and @inside-gen-class (> @idx @gen-class-node-idx) (= @gen-class-toggle 1)
                 (= @gen-class-key-str "prefix") (is-string-node node))
            (do
              (swap! result assoc-in ["genClass" "prefix" "value"] (get-gen-class-symbol-text node))
              (reset! gen-class-toggle 0)
              (let [last-node (get-last-child-node-with-text node)]
                (reset! skip-nodes-until-we-reach-this-id (:id last-node))
                (reset! gen-class-value-last-node-id (:id last-node))))

            (and @inside-gen-class (> @idx @gen-class-node-idx) (= @gen-class-toggle 1)
                 (is-gen-class-name-key @gen-class-key-str) (= (:name node) "meta"))
            (let [val-form (split-gen-class-metadata-form node)
                  val (get-gen-class-symbol-text (:bodyNode val-form))]
              (when-not val
                (throw (Exception. (str ":gen-class :" @gen-class-key-str " must be a symbol or string."))))
              (when (and (string? (:metadata val-form)) (not= (:metadata val-form) ""))
                (swap! result assoc-in ["genClass" @gen-class-key-str "metadata"] (:metadata val-form)))
              (swap! result assoc-in ["genClass" @gen-class-key-str "value"] val)
              (reset! gen-class-toggle 0)
              (let [last-node (get-last-child-node-with-text node)]
                (reset! skip-nodes-until-we-reach-this-id (:id last-node))
                (reset! gen-class-value-last-node-id (:id last-node))))

            (and @inside-gen-class (> @idx @gen-class-node-idx) (= @gen-class-toggle 1)
                 (is-gen-class-name-key @gen-class-key-str) (or is-token-node2 (is-string-node node)))
            (do
              (swap! result assoc-in ["genClass" @gen-class-key-str "value"] (get-gen-class-symbol-text node))
              (reset! gen-class-toggle 0)
              (if (is-string-node node)
                (let [last-node (get-last-child-node-with-text node)]
                  (reset! skip-nodes-until-we-reach-this-id (:id last-node))
                  (reset! gen-class-value-last-node-id (:id last-node)))
                (reset! gen-class-value-line-no @line-no)))

            (and @inside-gen-class (> @idx @gen-class-node-idx) (= @gen-class-toggle 1)
                 (= @gen-class-key-str "constructors") (is-map-literal-node node))
            (do
              (swap! result assoc-in ["genClass" "constructors" "value"] (parse-gen-class-constructors node))
              (reset! gen-class-toggle 0)
              (let [last-node (get-last-child-node-with-text node)]
                (reset! skip-nodes-until-we-reach-this-id (:id last-node))
                (reset! gen-class-value-last-node-id (:id last-node))))

            (and @inside-gen-class (> @idx @gen-class-node-idx) (= @gen-class-toggle 1)
                 (= @gen-class-key-str "methods") (is-vector-literal-node node))
            (do
              (swap! result assoc-in ["genClass" "methods" "value"] (parse-gen-class-methods node))
              (reset! gen-class-toggle 0)
              (let [last-node (get-last-child-node-with-text node)]
                (reset! skip-nodes-until-we-reach-this-id (:id last-node))
                (reset! gen-class-value-last-node-id (:id last-node))))

            (and @inside-gen-class (> @idx @gen-class-node-idx) (= @gen-class-toggle 1)
                 (= @gen-class-key-str "methods") (node-contains-text-and-not-whitespace node))
            (throw (Exception. ":gen-class :methods must be a vector of method vectors."))

            (and @inside-gen-class (> @idx @gen-class-node-idx) (= @gen-class-toggle 1)
                 (= @gen-class-key-str "exposes") (is-map-literal-node node))
            (do
              (swap! result assoc-in ["genClass" "exposes" "value"] (parse-gen-class-exposes node))
              (reset! gen-class-toggle 0)
              (let [last-node (get-last-child-node-with-text node)]
                (reset! skip-nodes-until-we-reach-this-id (:id last-node))
                (reset! gen-class-value-last-node-id (:id last-node))))

            (and @inside-gen-class (> @idx @gen-class-node-idx) (= @gen-class-toggle 1)
                 (= @gen-class-key-str "exposes") (node-contains-text-and-not-whitespace node))
            (throw (Exception. ":gen-class :exposes must be a map of protected-field names to accessor maps."))

            (and @inside-gen-class (> @idx @gen-class-node-idx) (= @gen-class-toggle 1)
                 (= @gen-class-key-str "exposes-methods") (is-map-literal-node node))
            (do
              (swap! result assoc-in ["genClass" "exposes-methods" "value"] (parse-gen-class-exposes-methods node))
              (reset! gen-class-toggle 0)
              (let [last-node (get-last-child-node-with-text node)]
                (reset! skip-nodes-until-we-reach-this-id (:id last-node))
                (reset! gen-class-value-last-node-id (:id last-node))))

            (and @inside-gen-class (> @idx @gen-class-node-idx) (= @gen-class-toggle 1)
                 (= @gen-class-key-str "exposes-methods") (node-contains-text-and-not-whitespace node))
            (throw (Exception. ":gen-class :exposes-methods must be a map of superclass-method names to exposed names."))

            (and @inside-gen-class (> @idx @gen-class-node-idx) is-text-node is-token-node2
                 (= @gen-class-toggle 1) (is-gen-class-boolean-key @gen-class-key-str))
            (do
              (if (= (:text node) "true")
                (swap! result assoc-in ["genClass" @gen-class-key-str "value"] true)
                (when (= (:text node) "false")
                  (swap! result assoc-in ["genClass" @gen-class-key-str "value"] false)))
              (reset! gen-class-toggle 0)
              (reset! gen-class-value-line-no @line-no))

            (and @inside-ns-form is-token-node2 (>= @paren-nesting-depth 1) (is-use-node node))
            (throw (Exception. "Standard Clojure Style does not support :use inside of the ns form. Please refactor with :require as appropriate."))))

        (when current-node-is-newline
          (swap! line-no inc))
        (reset! prev-node-is-newline current-node-is-newline)

        (swap! idx inc)

        (cond
          (>= @idx num-nodes)
          (reset! continue-parsing-ns-form false)

          (and (> @ns-node-idx 0) (not @inside-ns-form) (>= @line-no (+ @ns-form-ends-line-idx 2)))
          (reset! continue-parsing-ns-form false))))

    (sort-ns-result @result @prefix-list-comments)))
