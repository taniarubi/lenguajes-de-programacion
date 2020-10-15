#lang plai
                                                                                
;; Ejercicio 1.
;; Una función que reciba un número n, un número r, y devuelva el conjunto de
;; los primeros r múltiplos de n (empezando por n). 
;; multiplos: number number -> (listof number)
(define (multiplos n r)
  (map (λ (x) (* x n))
       (cdr (range (add1 r)))))

;; Ejercicio 2.
;; El predicado divisor? que reciba un número n y un número m y devuelva
;; verdadero si m divide n, falso en otro caso. Si m es cero, lanza un
;; error.
;; divisor?: number number -> boolean
(define (divisor? m n)
  (if (zero? m)
      (error 'divisor? "El cero no divide a ningún número")
      (and (= (modulo n m) 0))))

;; Ejercicio 3.
;; Función que recibe un número n como parámetro y devuelve el conjunto de
;; divisores de n.
;; divisores: number -> (listof number)
(define (divisores n)
  (filter (λ (x) (divisor? x n))
          (cdr (range (add1 n)))))

;; Ejercicio 4.
;; Un predicado que reciba un elemento e y una lista l y decida si e pertenece
;; a l.
;; pertenece?: a (listof a) -> boolean
(define (pertenece? e l)
  (ormap (λ (x) (equal? x e))
         l))

;; Ejercicio 5.
;; Una función que reciba una lista l con elementos y devuelva una lista de
;; elementos sin repetir de la lista original
;; eliminaRepetidos: (listof a) -> (listof a)
(define (eliminaRepetidos l)
  (foldr (lambda (x y) (cons x (filter (lambda (z) (not (equal? x z))) y)))
         '()
         l))

;; --- Definimos el tipo de dato Punto ---
;;(define-type Punto
;;  [Punto (x number?) (y number?)])

;; Ejercicio 6.
;; Una función que reciba dos puntos p = (x1, y1) y q = (x2, y2); y calcule el
;; punto medio entre p y q. Si alguno de los dos argumentos no es un punto,
;; regresa un error.
;; punto-medio: Punto Punto -> number
;; (define (punto-medio p q) ...)

;; Ejercicio 7.
;; Una función que reciba dos puntos p = (x1, y1) y q = (x2, y2); y calcule la
;; distancia entre p y q. Si alguno de los dos argumentos no es un punto, arroja
;; error.
;; distancia: Punto Punto -> number
;; (define (distancia p q)...)

;; Ejercicio 8.
;; Define un tipo de dato abstracto para crear figuras geométricas.
;;(define-type Figura
;;  [Circulo ]
;;  [Triangulo]
;;  [Cuadrado]
;;  [Rectangulo])

;; Ejercicio 9.
;; Una función que reciba una figura y calcule el perímetro de la misma.
;; perimetro: Figura -> number
;; (define (perimetro fig) ...)

;; Ejercicio 10.
;; Una función que reciba una figura y calcule el área de la misma.
;; area: Figura -> number
;; (define (area fig) ... )

;; ---------- Punto Extra ------------
;; Ejercicio 11
;; Una función que reciba una lista l como parámetro y devuelva el elemento con
;; mayor número de repeticiones en la lista l. Si hay dos o más elementos
;; repetidos el mismo número de veces, regresa el primero de éstos en aparecer
;; de izquierda a derecha en la lista original. Si la lista es vacía, lanza un
;; error.
;; masRepetido (listof a) -> a
;; (define (masRepetido l)...)