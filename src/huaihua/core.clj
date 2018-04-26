(ns huaihua.core
  (:require [clojure.core.match :refer [match]]
            [instaparse.core :as insta])
  (:refer-clojure :exclude [compile]))

(defn- error! [msg & [map]]
  (let [map (or map {})]
    (throw (ex-info msg map))))

(def ^:private gramma
  "TEXT = ( TAG | NORMAL_STR )*
  TAG = OPEN_TOKEN, TEXT, CLOSE_TOKEN
  NORMAL_STR = #\"[^{{}}]*\"
  OPEN_TOKEN = <'{{'>,SYMBOL, <'}}'>
  CLOSE_TOKEN = <'{{/'> SYMBOL <'}}'>
  SYMBOL = #\"[a-zA-Z][a-zA-Z0-9-]+[a-zA-Z0-9]\" ")

(def ^:private parse (insta/parser gramma :start :TEXT))

(def ^:private root-label :root)

(defn- ast [ep]
  (letfn [(get-ast [ep]
            (match [ep]
              [[:TEXT & rest-text]]
              (mapv get-ast rest-text)

              [[:TAG
                [:OPEN_TOKEN [:SYMBOL open-symbol]]
                [:TEXT & tag-text]
                [:CLOSE_TOKEN [:SYMBOL close-symbol]]]]
              {:label (keyword open-symbol)
               :children (mapv get-ast tag-text)}

              [([:NORMAL_STR str] :seq)]
              str

              :else
              (throw (RuntimeException. "Unexpected error when parse"))))]
    {:label root-label
     :children (get-ast ep)}))

(defn template [s]
  (-> (parse s) (ast)))

(defn get-snippet [snippet label]
  (letfn [(find-node [struct]
            (cond
              (map? struct)
              (if (= (:label struct) label)
                struct
                (some find-node (:children struct)))

              (vector? struct)
              (some find-node struct)

              (string? struct)
              nil

              :else (error! "Unknown Struct: " {:struct struct})))]
    (if-let [node (find-node snippet)]
      node
      (error! "Cannot find this node."))))

(defn transform [struct & [label-transforms]]
  {:pre [(map? label-transforms)]}
  (letfn [(do-transform [struct]
            (cond
              (map? struct)
              (let [{:keys [label children]} struct
                    s (->> (map do-transform children)
                           (apply str))
                    v (get label-transforms label ::not-found)]
                (if (= v ::not-found)
                  s
                  (cond
                    (fn? v) (v s)
                    (number? v) (str v)
                    (string? v) v
                    (keyword? v) (name v)
                    :else (error! "Should pass fn, string, number, keyword."))))

              (vector? struct)
              (->> (map do-transform struct)
                   (apply str))

              (string? struct)
              struct

              :else
              (error! (format "Unknown struct." {:struct struct}))))]
    (do-transform struct)))
