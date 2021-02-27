(ns challenge20191125.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn StrToNumVec [s]
  (map #(Integer/parseInt %)
       (split s #", *")))

(defn FindIntersection [a]
  (defn f [a]
    (let [[a1 a2] a]
      (loop [[this1 & next1 :as all1] (StrToNumVec a1)
             [this2 & next2 :as all2] (StrToNumVec a2)
             result '[]]
        (cond (or (= this1 nil) (= this2 nil))
              result
              (< this1 this2)
              (recur next1 all2 result)
              (> this1 this2)
              (recur all1 next2 result)
              :else
              (recur next1 next2 (conj result this1))))))
  (let [result (f a)]
    (if (= result '[])
      "false"
      (str/join ", " result))))
