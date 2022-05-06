(ns clousre-lenin.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

;Define una función average que recibe dos argumentos y regrese el promedio
(defn average
  [x y]
  (/ (+ x y)2))

;convertir de farenheight a celcius
(defn fahrenheit-to-celsius
  [f]
  (/ (* (- f 32) 5) 9))

;imprima mensaje con argumentos
(defn long-greeting
  [nombre edad carrera]
  (println "Nombre:" nombre "\nEdad:" edad "\nCarrera:" carrera))

;Actividad 5.1
(defn negative [x]
  (cond
    (= x 0) 0
    (< x 0) -1
    (> x 0) 1) )

(defn roots [a b c]
  (/ (- (Math/sqrt (- (* b b) (* 4 a c))) b) (* 2 a)))


(defn bmi [weight height]
  (def bmi-index (/ weight (* height height)))
  (cond
    (< bmi-index 20) "underweight"
    (and (< bmi-index 25) (> bmi-index 20)) "normal"
    (and (< bmi-index 30) (> bmi-index 25)) "obese1"
    (and (< bmi-index 40) (> bmi-index 30)) "obese2"
    (> bmi-index 40) "obese3"))

(defn fact [x]
  (loop [n x resultado 1]
    (if (= n 0)
      resultado
        (recur (- n 1) (* n resultado)))))

(defn fib [n]
  (if (<= n -1)
    n
      (loop [contador n primero 0 segundo 1]
        (if (= contador 0)
          primero
            (recur (- contador 1) segundo (+ primero segundo))))))


(defn pow [a, b]
  (loop [numeroPotencia b resultado 1]
    (if (= numeroPotencia 0)
      resultado
      (recur (- numeroPotencia 1) (* resultado a)))))

;Actividad 5.2

;1
;Con conj agrega el elemento n a la lista y sortea todos los elementos
(defn insert
  [n lst]
  (sort (conj lst n)))


;2
;Ingresa a listaNueva una lista vacia y a listaVieja el parametros
;Si listaVieja está vacia regresa listaNueva
;Utiliza recursión para ingresar a listaNueva los elementos de listaVieja y los sortea con insert
(defn insertion-sort
  [lst]
  (loop [listaNueva '() listaVieja lst]
    (if (empty? listaVieja)
      listaNueva
      (recur (insert (peek listaVieja) listaNueva) (pop listaVieja)))))


;3
;Si n es mayor a 0, concatenar las dos listas creadas con split-at dependiendo del modulo de n con 7
(defn rotate-left
  [n lst]
  (if (= n 0)
    lst
    (if (> n 0)
      (let [[principio final] (split-at (mod n 7) lst)]
        (concat final principio)))))


;4
;Si b es igual a 0, regresa a
;Si b no es igual a 0, aplica la recursión de b con el remainer de a con b
(defn gcd
  [a b]
  (if (= b 0)
    a
    (recur b (rem a b))))

;5
;Utilizando partition-by que hace que la lista se separe cada vez que hay un valor nuevo
;Utilizando map para agarrar todas las listas separadas, agarramos el primer valor
(defn compress
  [lst]
  (if (empty? lst)
    ()
    (map first (partition-by identity lst))))

;6
;Crea una nueva función que toma los parametros 'x' y 'y' y regresa la funcion con los parametros en orden inverso
(defn args-swap
  [f]
  (fn [x y] (f y x)))

;7
;Regresa la función de la derivada aplicando la función correspondiente
(defn deriv
  [f h]
  (fn [x]
    (/ (- (f (+ x h)) (f x)) h)))

;8
;Si n es cero, regresa 0
; Si n no es cero regresa método de newton aplicando la función correspondiente
(defn newton
  [f n]
  (if (= n 0)
    0
    (let [x-n-1 (newton f (dec n)) f' (deriv f 0.0001)]
      (- x-n-1 (/ (f x-n-1) (f' x-n-1))))))