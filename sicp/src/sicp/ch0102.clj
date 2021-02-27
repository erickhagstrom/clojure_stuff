(ns sicp.ch0102)
;; 1.2.1 Linear Recursion and Iteration

(defn factorial [n]
  (if (= n 0)
    1
    (* n (factorial (- n 1))))
  )

;; Exercise 1.10
(defn A [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (A (- x 1)
                   (A x (- y 1)))))

;; Exercise 1.11
(defn f1-11 [x y z]
  (+ x (* 2 y) (* 3 z)))

(defn f1-11a [n]
  (if (< n 3)
    n
    (let [x (f1-11a (- n 1)) y (f1-11a (- n 2)) z (f1-11a (- n 3))]
      (f1-11 x y z))))

(defn f1-11b [n]
  (let [[x1 y1 z1]
        (if (< n 3)
          [n 0 0]
          (loop [x 0 y 0 z 0 m 0]
            (if (= m n)
              [x y z]
              (let [a (if (< m 3)
                        m
                        (f1-11 x y z))
                    b x
                    c y
                    d (+ m 1)]
                (recur a b c d))
              )
            )
          )]
    (f1-11 x1 y1 z1)))

;; Exercise 1.12
(defn pascal-elem [i j]
  (cond (< i j) 0
        (< i 1) 0
        (= i 0) 1
        (or (= j 1) (= i j)) 1
        :else (+ (pascal-elem (- i 1) (- j 1))
                 (pascal-elem (- i 1) j))))

;; Exercise 1.15
(defn abs [x]
  (if (< x 0)
    (* -1 x)
    x))

(defn cube [x]
  (* x x x))

(defn p [x]
  (- (* 3 x) (* 4 (cube x))))

(defn sine [angle]
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

;; 1.2.4 Exponentiation
(defn square [x]
  (* x x))

(defn remainder [x y]
  (let [x-by-y (quot x y)]
    (- x (* x-by-y y))))

(defn fast-expt [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt (bigint b) (/ n 2)))
        :else (* b (fast-expt b (- n 1)))))

;; Exercise 1.16
(defn fast-expt-i [b0 n0]
  (loop [a 1N b (bigint b0) n (bigint n0)]
    (if (= n 0N)
      a
      (let [e (even? n)]
        (recur
         (if e
           a
           (* a b))
         (if e
           (* b b)
           b)
         (if e
           (/ n 2)
           (- n 1)))))))

;; 1.2.5 Greatest Common Divisors

;; 1.2.6 Example: Testing for Primality
(defn divides? [a b]
  (= (remainder b a) 0))

(defn smallest-divisor [n1]
  (loop [n n1 test-divisor 2]
    (cond (> (square test-divisor) n) n
          (divides? test-divisor n) test-divisor
          :else (recur n (+ test-divisor 1)))))

(defn prime? [n]
  (= n (smallest-divisor n)))

;; compute the exponential of a number modulo another number
;; this approach does not allow for tail call recursion,
;; so use of the Clojure loop/recur construct is not possible
(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (remainder (square (expmod base (/ exp 2) m)) m)
        :else (remainder (* base (expmod base (- exp 1) m)) m)))

(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (+ 1 (rand-int (- n 1)))))

(defn fast-prime? [n times]
  (cond (= times 0) true
        (fermat-test n) (fast-prime? n (- times 1))
        :else false))

;; Exercise 1.27
(defn carmichael [n]
  (loop [i (- n 1)]
    (if (= i 0)
      true
      (if (= (expmod i n n) 0)
        false
        (recur (- i 1))))))
