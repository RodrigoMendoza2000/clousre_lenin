(ns activities.actividad-5-1)

; Rodrigo Alfredo Mendoza España A01720627
; Sergio Manuel González Vargas A01745446

(defn sign [n]
  (cond
    (= n 0) 0
    (< n 0) -1
    (> n 0) 1) )

(println(sign -5))
(println(sign 10))
(println(sign 0))


(defn roots [a b c]
  (/ (- (Math/sqrt (- (* b b) (* 4 a c))) b) (* 2 a)))

(println(roots 2 4 2))
(println(roots 1 0 0))
(println(roots 4 5 1))

(defn bmi [weight height]
  (def bmi-index (/ weight (* height height)))
  (cond
    (< bmi-index 20) "underweight"
    (and (< bmi-index 25) (> bmi-index 20)) "normal"
    (and (< bmi-index 30) (> bmi-index 25)) "obese1"
    (and (< bmi-index 40) (> bmi-index 30)) "obese2"
    (> bmi-index 40) "obese3"))

(println(bmi 45 1.7))
(println(bmi 55 1.5))
(println(bmi 76 1.7))
(println(bmi 81 1.6))
(println(bmi 120 1.6))

(defn factorial [x]
  (loop [n x resultado 1]
    (if (= n 0)
      resultado
      (recur (- n 1) (* n resultado)))))

(println(factorial 0))
(println(factorial 5))
(println(factorial 20))

(defn fib [n]
  (if (<= n -1)
    n
    (loop [contador n primero 0 segundo 1]
      (if (= contador 0)
        primero
        (recur (- contador 1) segundo (+ primero segundo))))))

(println(fib 6))
(println(map fib (range 10) ))
(println(fib 42))

(defn pow [a, b]
  (loop [numeroPotencia b resultado 1]
    (if (= numeroPotencia 0)
      resultado
      (recur (- numeroPotencia 1) (* resultado a)))))

(println(pow 5 0))
(println(pow -5 3))
(println(pow 15 12))