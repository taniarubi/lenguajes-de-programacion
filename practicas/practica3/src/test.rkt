#lang plai

(require (file "./parser.rkt"))
(require (file "./interp.rkt"))

(define (prueba e)
  (interp (parse e)))

(test (prueba 'foo) "Error: variable libre")
(test (prueba '1234) 1234)
(test (prueba '{+ 1 2 3}) 6)
(test (prueba '{- 1 2 3}) -4)
(test (prueba '{* 1 2 -3}) -6)
(test (prueba '{* 1 2 -3 0}) 0)
(test (prueba '{/ 1 2}) (/ 1 2))
(test (prueba '{modulo 3 2}) 1)
(test (prueba '{sub1 3}) 2)
(test (prueba '{sub1 (add1 (expt 3 3))}) 27)
(test (prueba 'a) "Error: variable libre")
(test (prueba '{with {[x 2] [y 3]} {+ x 3 y}}) 8)
(test (prueba'{with* {{a 2} {b {+ a a}}} b}) 4)
(test (prueba'{with {{a 2} {b {+ a a}}} b}) "Error: variable libre")




