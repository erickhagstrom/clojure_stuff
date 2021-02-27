(ns sicp.ch0103)
(require '[sicp.ch0102 :as ch0102])

(defn sum-orig [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum-orig term (next a) next b))))

(defn sum [term a0 next b]
  (loop [a a0 running-sum 0]
    (if (> a b)
      running-sum
      (recur (next a) (+ running-sum (term a)))))
  )

(defn inc [n]
  (+ n 1))

(defn cube [x]
  (* x x x))

(defn sum-cubes [a b]
  (sum cube a inc b))

(defn identity [x] x)

(defn sum-integers [a b]
  (sum identity a inc b))

(defn pi-sum [a b]
  (defn pi-term [x]
    (/ 1.0 (* x (+ x 2))))
  (defn pi-next [x]
    (+ x 4))
  (sum pi-term a pi-next b))

(defn integral [f a b dx]
  (defn add-dx [x]
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; Exercise 1.29
(defn simpson [f a b n0]
  (defn y [k h]
    (f (+ a (* k h))))
  (let [n (if (= 0 (ch0102/remainder n0 2)) n0 (+ n0 1))
        h (/ (- b a) n)]
    (loop [k 0 sum 0]
      (if (= k n)
        (/ (* h sum) 3)
        (recur (+ k 1)
               (+ sum
                  (* (y k h)
                     (cond (= k 0) 1
                           (= k n) 1
                           (= (ch0102/remainder k 2) 0) 2
                           :else 4))))))))

;; Exercise 1.30
(defn sumi [term a next b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))
()

;; Exercise 1.31 a
(defn product [term a0 next b]
  (loop [a a0 running-prod 1]
    (if (> a b)
      running-prod
      (recur (next a) (* running-prod (term a))))))

(defn factorial [n]
  (product identity 1 inc n))

(defn pi-a [n]
  (defn pi-term [n]
    (let [a (* n 2)]
      (let [b (+ a 2)
            c (+ a 1)]
        (/ (* a b) (* c c))))
    )
  (loop [i 1 prod 4.0]
    (if (> i n) prod
        (recur (+ i 1) (* (pi-term i) prod)))))
