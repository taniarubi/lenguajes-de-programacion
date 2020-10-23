#lang plai
(require "./Practica2.rkt")

(test (multiplos 5 20) '(5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100))
(test (multiplos 67 10) '(67 134 201 268 335 402 469 536 603 670))
(test (multiplos 348 4) '(348 696 1044 1392))

(test (divisor? 73 4) #f)
(test (divisor? 73 14454) #t)
(test (divisor? 16 16384) #t)

(test (divisores 81) '(1 3 9 27 81))
(test (divisores 67) '(1 67))
(test (divisores 199) '(1 199))
(test (divisores 196) '(1 2 4 7 14 28 49 98 196))

(test (pertenece? 1 (list 'a "hola" 3 4 (- 2 1))) #t)
(test (pertenece? 1 (list 'a "hola" 3 4 - 2 1)) #t)
(test (pertenece? 1 (list 'a "hola" 3 4 - 2 "string" 'number)) #f)

(test (eliminaRepetidos (list 'a 8 "hola" 3 4 2 "string" 'a)) '(a 8 "hola" 3 4 2 "string"))
(test (eliminaRepetidos (list 1 1 1 2 3 5 6 7 8 1 4 5 8 10)) '(1 2 3 5 6 7 8 4 10))
(test (eliminaRepetidos (list 1 8 3 4 2)) '(1 8 3 4 2))

(test (punto-medio (Punto 2 2) (Punto 2 8)) (Punto 2 5))
(test (punto-medio (Punto 2 2) (Punto 2 2)) (Punto 2 2))
(test (punto-medio (Punto 3 1) (Punto 44.0 18.0)) (Punto 23.5 9.5))

(test (distancia (Punto 44.0 18.0) (Punto 23.5 9.5)) 22.192341021172147)
(test (distancia (Punto 44.0 18.0) (Punto 3 1)) 44.384682042344295)
(test (distancia (Punto 23.5 9.5) (Punto 3 1)) 22.192341021172147)

(define c1 (Circulo (Punto 2 5) 19))
(define c2 (Circulo (Punto 2 4.3) 10.4))
(define c3 (Circulo (Punto 3 6) 18))
(test c1 (Circulo (Punto 2 5) 19))
(test c2 (Circulo (Punto 2 4.3) 10.4))
(test c3 (Circulo (Punto 3 6) 18))

(define t1 (Triangulo (Punto 2 5) (Punto 12.3 3) (Punto 4.5 6.1)))
(define t2 (Triangulo (Punto 2 4.3) (Punto 10.4 5.4) (Punto 1.1 2.6)))
(define t3 (Triangulo (Punto 3 6) (Punto 3 5.7) (Punto 10 15.6)))
(test t1 (Triangulo (Punto 2 5) (Punto 12.3 3) (Punto 4.5 6.1)))
(test t2 (Triangulo (Punto 2 4.3) (Punto 10.4 5.4) (Punto 1.1 2.6)))
(test t3 (Triangulo (Punto 3 6) (Punto 3 5.7) (Punto 10 15.6)))

(test (perimetro c1) 119.38052083641213)
(test (perimetro c2) 65.3451271946677)
(test (perimetro c3) 113.09733552923255)

(test (perimetro t1) 21.61712806993984)
(test (perimetro t2) 20.107619314780955)
(test (perimetro t3) 24.30584543124163)

(test (area c1) 1134.1149479459152)
(test (area c2) 339.79466141227203)
(test (area c3) 1017.8760197630929)

(test (area t1) 8.164999999999996)
(test (area t2) 6.644999999999983)
(test (area t3) 1.0499999999999896)

