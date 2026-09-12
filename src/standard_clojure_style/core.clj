(ns standard-clojure-style.core
  (:refer-clojure :exclude [format])
  (:require
   [standard-clojure-style.format :as fmt]
   [standard-clojure-style.parse-ns :as ns-p]
   [standard-clojure-style.parser :as parser]))

(defn format [txt]
  (fmt/format-text txt))

(defn parse [txt]
  (parser/parse txt))

(defn parse-ns [txt-or-nodes]
  (if (string? txt-or-nodes)
    (ns-p/parse-ns (ns-p/flatten-tree (parser/parse txt-or-nodes)))
    (ns-p/parse-ns txt-or-nodes)))
