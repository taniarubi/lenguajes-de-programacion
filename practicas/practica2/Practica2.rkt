#lang plai
                                                                                
;; Ejercicio 1.
;; Función que reciba un número n, un número r, y devuelva el conjunto de
;; los primeros r múltiplos de n (empezando por n). 
;; multiplos: number number -> (listof number)
(define (multiplos n r)
  (map (λ (x) (* x n))
       (cdr (range (add1 r)))))

;; Ejercicio 2.
;; Predicado que nos dice si un número m es divisor de otro número n.
;; Si el parámetro recibido es cero, se devuelve un error.
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
;; Predicado que reciba un elemento e y una lista l y decida si e pertenece a l.
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

;; Estructura que nos permite modelar puntos en el plano.
;; Sirve para modelar figuras geométricas.
(struct Punto (x y) #:inspector #f)

;; Ejercicio 6.
;; Una función que reciba dos puntos p = (x1, y1) y q = (x2, y2); y calcule el
;; punto medio entre p y q. Si alguno de los dos argumentos no es un punto,
;; regresa un error.
;; punto-medio: Punto Punto -> number
(define (punto-medio p q)
  (if (or (not (Punto? p) ) (not (Punto? q) ))
      (error "Necesitas meter dos puntos")
      (let ([x1 (Punto-x p)]
            [y1 (Punto-y p)]
            [x2 (Punto-x q)]
            [y2 (Punto-y q)])
       (Punto (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)))
      ))

;; Ejercicio 7.
;; Una función que reciba dos puntos p = (x1, y1) y q = (x2, y2); y calcule la
;; distancia entre p y q. Si alguno de los dos argumentos no es un punto, arroja
;; error.
;; distancia: Punto Punto -> number
(define (distancia p q)
  (if (or (not (Punto? p) ) (not (Punto? q) ))
      (error "Necesitas meter dos puntos")
      (let ([x1 (Punto-x p)]
            [y1 (Punto-y p)]
            [x2 (Punto-x q)]
            [y2 (Punto-y q)])
       (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))
      ))

;; Ejercicio 8.
;; Define un tipo de dato abstracto para crear figuras geométricas.
(define-type Figura
  [Circulo (centro  Punto?) (radio  number?)]
  [Triangulo (a  Punto?) (b  Punto?) (c  Punto?)]
  [Cuadrado (EsI  Punto?) (Ll  number?)]
  [Rectangulo (EsI  Punto?) (base  number?) (altura  number?)])

;; Ejercicio 9.
;; Una función que reciba una figura y calcule el perímetro de la misma.
;; perimetro: Figura -> number
(define (perimetro fig)
  (type-case Figura fig
    [Circulo (centro radio) (* (radio) (pi) 2)]
    [Triangulo (a b c) (+ (distancia a b) (distancia b c) (distancia a c))]
    [Cuadrado (EsI Ll) (* (Ll) 4)]
    [Rectangulo (EsI base altura) (+ (* (base) 2) (* (altura) 2))]))

;; Ejercicio 10.
;; Una función que reciba una figura y calcule el área de la misma.
;; area: Figura -> number
(define (area fig)
  (type-case Figura fig
    [Circulo (centro radio) (* pi (expt (radio) 2))]
    [Triangulo (a b c) (let ([d (distancia a b)]
                             [e (distancia b c)]
                             [f (distancia a c)])
                       (sqrt (* (/ (+ d e f) 2)
                           (- (/ (+ d e f) 2) d)
                           (- (/ (+ d e f) 2) e)
                           (- (/ (+ d e f) 2) f))))]
    [Cuadrado (EsI Ll) (* (Ll) (Ll))]
    [Rectangulo (EsI base altura) (* (base) (altura))]))

;; ---------- Punto Extra ------------
;; Ejercicio 11
;; Una función que reciba una lista l como parámetro y devuelva el elemento con
;; mayor número de repeticiones en la lista l. Si hay dos o más elementos
;; repetidos el mismo número de veces, regresa el primero de éstos en aparecer
;; de izquierda a derecha en la lista original. Si la lista es vacía, lanza un
;; error.
;; masRepetido (listof a) -> a

;(masRepetido (list 1 2 3 3 6 6 7 7))
;(masRepetido (list 1 2 3 4))
;(masRepetido (list 1 2 3 4 5 6 3))