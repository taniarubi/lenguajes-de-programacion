#lang plai
(require (file "./Practica2.rkt"))

;; Test para la función multiplos.
(test (multiplos 73 8) (list 73 146 219 292 365 438 511 584))
(test (multiplos 20 10) (list 20 40 60 80 100 120 140 160 180 200))
(test (multiplos 7 9) (list 7 14 21 28 35 42 49 56 63))
(test (multiplos 12 3) (list 12 24 36))

;; Test para el predicado divisor?
(test (divisor? 0 3) "divisor?: El cero no divide a ningún número")
(test (divisor? 5 30) #t)
(test (divisor? 2 9) #f)
(test (divisor? 3 25) #f)
(test (divisor? 2 8) #t)

;; Test para la función divisores.
(test (divisores 135) (list 1 3 5 9 15 27 45 135))
(test (divisores 2) (list 1 2))
(test (divisores 25) (list 1 5 25))
(test (divisores 100) (list 1 2 4 5 10 20 25 50 100))

;; Test para el predicado pertenece.
(test (pertenece? 3 (list 4 7 8 9 32 4 1 3)) #t)
(test (pertenece? 2 (list 1 2 3 4)) #t)
(test (pertenece? 9 (list 1 2 3 4)) #f)
(test (pertenece? 'a (list 'z 'v 'k 's)) #f)

;; Test para la función eliminaRepetidos.
(test (eliminaRepetidos (list 1 2 3 5 6 7 8 4 4 1 6 7 8)) (list 1 2 3 5 6 7 8 4))
(test (eliminaRepetidos (list 4 3 5 6 4 4 8 9 6 3 5)) (list 4 3 5 6 8 9))
(test (eliminaRepetidos (list 5 5 5 5 5 5 5 5)) (list 5))
(test (eliminaRepetidos (list 7 6 3 4 7 8 9 7 7 5 2 3)) (list 7 6 3 4 8 9 5 2))

;; Test para la función punto-medio.
;;(test (punto-medio (Punto 2 2) (Punto 2 8)) (Punto 2 5))
;;(test (punto-medio (Punto 1 3) (3 1)) (Punto 2 2))
;;(test (punto-medio (Punto 8 9) (Punto 4 5)) (Punto 6 7))
;;(test (punto-medio (Punto 14 1) (Punto 3 6)) (Punto 17/2 7/2))

;; Test para la función distancia.
;;(test () )
;;(test () )
;;(test () )
;;(test () )

;; Test para la función área.
;; (test (area (Triangulo (Punto 2 2) (Punto 3 6) (Punto 1 9)))

;; ---- Punto extra ----
;; Test para la función masRepetido.
;(test (masRepetido (list 1 2 3 3 6 6 7 7)) 3)
;(test (masRepetido (list 1 2 3 4)) 1)
;(test (masRepetido (list 1 2 3 4 5 6 3)) 3)
;(test (masRepetido (list 1 1 2 2 3 4 1)) 1)
