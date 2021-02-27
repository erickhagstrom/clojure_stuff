(defn square [x] (* x x))

(defn sum-of-squares
  [x y]
  (+ (square x) (square y)))

(defn f [a]
  (sum-of-squares (+ a 1) (* a 2 )))

(defn abs [x]
  (cond
    (> x 0) x
    (= x 0) 0
    (< x 0) (- x)))

(defn sum-of-squares-of-larger-two
  [x y z]
  (cond
    (and (< z x) (< z y)) (sum-of-squares x y)
    (and (< y x) (< y z)) (sum-of-squares x z)
    (and (< x y) (< x z)) (sum-of-squares y z)))

(defn a-plus-abs-b
  [a b]
  ((if (> b 0) + -) a b))

(defn p [] '(p))

(defn test [x y]
  (if (= x 0)
    0
    y))

;; 1.1.7 Example: Square Roots by Newton's Method
(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn sqrt [x]
  (loop [guess 1.0 n x]
    (if (good-enough? guess n)
      guess
      (recur (improve guess n) n)))
  )

;; Exercise 1.6
(defn new-if [predicate then-clause else-clause]
  (cond predicate then-clause
        :else else-clause))

;; The following fails to compile. The recur is no longer in a tail position.
'("(defn new-sqrt [x]
    (loop [guess 1.0 n x]
      (new-if (good-enough? guess n)
              guess
              (recur (improve guess n) n))))")

;; 1.1.8 Procedures as Black-Box Abstractions
(defn sqrt [x]
  (defn good-enough? [guess]
    (< (abs (- (square guess) x)) 0.001))
  (defn improve [guess]
    (average guess (/ x guess)))
  (loop [guess 1.0]
    (if (good-enough? guess)
      guess
      (recur (improve guess)))))
