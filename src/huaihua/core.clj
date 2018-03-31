(ns huaihua.core
  (:require [clojure.core.match :refer [match]]
            [instaparse.core :as insta])
  (:refer-clojure :exclude [compile]))

(defn- error! [msg & [map]]
  (let [map (or map {})]
    (throw (ex-info msg map))))

(defn- assert! [pred & [msg map]]
  (let [msg (or msg "Assert failed.")]
    (when-not pred (error! msg map))))

(def ^:private gramma
  "TEXT = ( TAG | NORMAL_STR )*
  TAG = OPEN_TOKEN, TEXT, CLOSE_TOKEN
  NORMAL_STR = #\"[^{{}}]*\"
  OPEN_TOKEN = <'{{'>,SYMBOL, <'}}'>
  CLOSE_TOKEN = <'{{/'> SYMBOL <'}}'>
  SYMBOL = #\"[a-zA-Z][a-zA-Z0-9-]+[a-zA-Z0-9]\" ")

(def ^:private parse (insta/parser gramma :start :TEXT))

(defn- struct1 [ep]
  (match [ep]
    [[:TEXT & rest-text]]
    (mapv struct1 rest-text)

    [[:TAG
      [:OPEN_TOKEN [:SYMBOL open-symbol]]
      [:TEXT & tag-text]
      [:CLOSE_TOKEN [:SYMBOL close-symbol]]]]
    (->> (mapv struct1 tag-text) (into [open-symbol]))

    [([:NORMAL_STR str] :seq)]
    str

    :else
    (throw (RuntimeException. "Unexpected error when parse"))))

(defn- compile* [input]
  (cond
    (vector? input)
    (let [[first & rest] input]
      (assert (string? first))
      {:name (keyword first)
       :children (mapv compile* rest)})

    (string? input)
    input

    :else
    (throw
      (IllegalArgumentException.
        (format "Argument should vector or string: %s" input)))))

(def ^:private root-name "root")

(defn- compile [struct1]
  (-> (into [root-name] struct1)
      (compile*)))

(defn get-snippet
  ([s]
   (-> (parse s)
       (struct1)
       (compile)))
  ([s node-key]
   (letfn [(find-node [struct]
             (cond
               (map? struct)
               (if (= (:name struct) node-key)
                 struct
                 (some find-node (:children struct)))

               (vector? struct)
               (some find-node struct)

               (string? struct)
               nil

               :else (error! "Unknown Struct: " {:struct struct})))]
     (if-let [node (find-node (get-snippet s))]
       node
       (error! "Cannot find this node.")))))

(defn emit [struct]
  (cond
    (map? struct)
    (apply str (mapv emit (:children struct)))

    (vector? struct)
    (apply str (map emit (:children struct)))

    (string? struct)
    struct

    :else (error! "Unknown Struct: " {:struct struct})))

(defn transform [struct & {:as key-transforms}]
  (letfn [(do-transform [struct]
            (cond
              (map? struct)
              (let [{:keys [name children]} struct
                    s (->> (map do-transform children)
                           (apply str))
                    f (get key-transforms name)]
                (if f (do (assert! (fn? f)) (f s)) s))

              (vector? struct)
              (->> (map do-transform struct)
                   (apply str))

              (string? struct)
              struct))]
    (do-transform struct)))
