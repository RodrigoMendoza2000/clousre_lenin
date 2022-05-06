(ns activities.actividad-5-2)

; Rodrigo Alfredo Mendoza España A01720627
; Sergio Manuel González Vargas A01745446


;Actividad 5.2
;(println)
;1
;Con conj agrega el elemento n a la lista y sortea todos los elementos
(defn insert
  [n lst]
  (sort (conj lst n)))

(println "EJERCICIO 1")
(println(insert 14 '()))

(println(insert 4 '(5 6 7 8)))

(println(insert 5 '(1 3 6 7 9 16)))

(println(insert 10 '(1 5 6)))

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

(println "EJERCICIO 2")
(println(insertion-sort '()))
(println(insertion-sort '(4 3 6 8 3 0 9 1 7) ))
(println(insertion-sort '(1 2 3 4 5 6)))
(println(insertion-sort '(5 5 5 1 5 5 5)))

;3
;Si n es mayor a 0, concatenar las dos listas creadas con split-at dependiendo del modulo de n con 7
(defn rotate-left
  [n lst]
  (if (= n 0)
    lst
    (if (> n 0)
      (let [[principio final] (split-at (mod n 7) lst)]
        (concat final principio)))))

(println "EJERCICIO 3")
(println(rotate-left 5 '()) )
(println(rotate-left 0 '(a b c d e f g)))
(println(rotate-left 1 '(a b c d e f g)))
(println(rotate-left 3 '(a b c d e f g)))
(println(rotate-left 8 '(a b c d e f g)))
(println(rotate-left 45 '(a b c d e f g)) )

;4
;Si b es igual a 0, regresa a
;Si b no es igual a 0, aplica la recursión de b con el remainer de a con b
(defn gcd
  [a b]
  (if (= b 0)
    a
    (recur b (rem a b))))

(println "EJERCICIO 4")
(println(gcd 13 7919) )
(println(gcd 20 16) )
(println(gcd 54 24))
(println(gcd 6307 1995))
(println(gcd 48 180))
(println(gcd 42 56))
;5
;Utilizando partition-by que hace que la lista se separe cada vez que hay un valor nuevo
;Utilizando map para agarrar todas las listas separadas, agarramos el primer valor
(defn compress
  [lst]
  (if (empty? lst)
    ()
    (map first (partition-by identity lst))))

(println "EJERCICIO 5")
(println(compress '()))
(println(compress '(a b c d)))
(println(compress '(a a a a b c c a a d e e e e)))
(println(compress '(a a a a a a a a a a)))
;6
;Crea una nueva función que toma los parametros 'x' y 'y' y regresa la funcion con los parametros en orden inverso
(defn args-swap
  [f]
  (fn [x y] (f y x)))

(println "EJERCICIO 6")
(println((args-swap list) 1 2) )
(println((args-swap /) 8 2) )
(println((args-swap cons) '(1 2 3) '(4 5 6)))
(println((args-swap map) '(-1 1 2 5 10) /))
;7
;Regresa la función de la derivada aplicando la función correspondiente
(defn deriv
  [f h]
  (fn [x]
    (/ (- (f (+ x h)) (f x)) h)))

(println "EJERCICIO 7")
(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(println(df 5))
(println(ddf 5))
(println(dddf 5))
;8
;Si n es cero, regresa 0
; Si n no es cero regresa método de newton aplicando la función correspondiente
(defn newton
  [f n]
  (if (= n 0)
    0
    (let [x-n-1 (newton f (dec n)) f' (deriv f 0.0001)]
      (- x-n-1 (/ (f x-n-1) (f' x-n-1))))))

(println "EJERCICIO 8")
(defn f1 [x] (- x 10))
(defn f2 [x] (+ (* 4 x) 2))
(defn f3 [x] (+ (* x x x) 1))
(defn f4 [x] (+(Math/cos x) (* 0.5 x)))

(println(newton f1 1) )
(println(newton f2 1))
(println(newton f3 50))
(println(newton f4 5))