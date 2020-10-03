#lang plai
(require (file "./Practica1.rkt"))


;Tests del predicado esPar?
(test (esPar? 0) #t)
(test (esPar? -2) #t)
(test (esPar? 1) #f)

;Tests de la función menores
(test (menores 10) (list 0 1 2 3 4 5 6 7 8 9 10))
(test (menores 1) (list 0 1))
(test (menores 0) (list 0))
(test (menores -10) empty)

;Tests de la función pares
(test (pares 10) (list 0 2 4 6 8 10))
(test (pares 0) (list 0))

;Tests de la función suma-cuadrados
(test (suma-cuadrados 4) 30)
(test (suma-cuadrados 10) 385)

;Tests de la función suma-cuadrados recursiva
(test (suma-cuadradosR 4) 30)
(test (suma-cuadradosR 10) 385)

;Tests de la función raicesReales?
(test (raicesReales? 1 2 3) #f)
(test (raicesReales? 5 2 3) #f)
(test (raicesReales? 1 5 3) #t)

;Tests de la función general1
(test/exn (general1 1 2 3) "general1: El polonomio 1x² + 2x + 3 no tiene raíces reales")
(test (general1 1 5 3) -0.6972243622680054)

;Tests de la función general2
(test/exn (general2 1 2 3) "general2: El polonomio 1x² + 2x + 3 no tiene raíces reales")
(test (general2 1 5 3) -4.302775637731995)

;Tests de la función reversa-lista
(test (reversa-lista (list 1 2 3 4)) '(4 3 2 1))
(test (reversa-lista (list )) '())

;Tests de la función palindromo
(test (palindromo? '(1 "hola" 1)) #t)
(test (palindromo? '(1 "hola" "hola" 1)) #t)

