(ns huaihua.core-test
  (:require [clojure.test :refer :all])
  (:require [huaihua.core :as h]))

(deftest test-get-snippet
  (testing "testing get snippet"
    (let [s "text1{{ layer1 }}text2{{ layer2 }}text3{{ / layer2 }}text4{{/layer1}}text6"
          tpl (h/template s)]
      (= {:name :root,
          :children
          ["text1"
           {:name :layer1,
            :children ["text2"
                       {:name :layer2,
                        :children ["text3"]}
                       "text4"]}
           "text6"]}
         tpl)))
  (testing "testing get snippet node"
    (let [tpl (h/template "text1{{layer1}}text2{{layer2}}text3{{/layer2}}text4{{/layer1}}text6")
          snippet (h/get-snippet tpl :layer1)]
      (= {:name :layer1,
          :children ["text2"
                     {:name :layer2,
                      :children ["text3"]}
                     "text4"]}
         snippet))

    (let [tpl (h/template "text1{{layer1}}text2{{/layer1}}{{layer2}}text3{{/layer2}}text4")]
      (= {:name :layer2, :children ["text3"]}
         (h/get-snippet tpl :layer2)))))

(deftest test-core-function
  (testing "testing basic"
    (let [s "text1{{layer1}}text2{{layer2}}text3{{/layer2}}text4{{/layer1}}text6"
          tpl (h/template s)]
      (is (= "text1[I'm the replaced text]text6"
             (h/transform tpl {:layer1 (fn [_] "[I'm the replaced text]")})))

      (is (= "text1text2 [ I update layer2 ]text4[ add some thing after updated layer 2 ]text6"
            (h/transform tpl
              {:layer1 (fn [x] (str x "[ add some thing after updated layer 2 ]"))
               :layer2 (fn [_] " [ I update layer2 ]")}))))))

(deftest test-transform-many-times
  (testing "testing one params occur in many places"
    (let [s "text1{{layer1}}text2{{/layer1}}text3{{layer1}}text4{{/layer1}}text6"
          tpl (h/template s)]
      (= "text1 [ updated layer1 ]text3 [ updated layer1 ]text6"
         (h/transform tpl
           {:layer1 (fn [_] " [ updated layer1 ]")})))))


(comment
  (def s (slurp (io/resource "example.txt")))
  (def snippet (get-snippet s))
  (transform snippet
             :title "程序员"
             :introduce (fn [x] (str x "在倍洽开心的写bug,"))))