(ns challenge20191202.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn check-equivalent-keypresses [v]
  (defn parse-string [s]
    (vec (str/split s #",")))

  (defn apply-keypresses [v s]
    (if (= s "-B")
      (vec (drop-last v))
      (conj v s)))

  (let [[s1 s2] v
        v1 (str/split s1 #",")
        v2 (str/split s2 #",")]
    (= (reduce apply-keypresses '[] v1)
       (reduce apply-keypresses '[] v2))))

#(loop [v % r '()]
   (if (= nil v)
     r
     (let [[v1 & v2] v]
       (recur v2 (conj r v1)))))

#(reduce conj %)

(fn [s]
  (reduce [] conj s))
