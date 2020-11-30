#lang plai

(define (foo n)
  (if (< n 10)
      n
      (+ (modulo n 10) (foo (quotient n 10)))))

(foo 1729)
(foo 51)
(foo 963852741)
(foo 82)

(define (suma-digitos n)
  (suma-digitos-tail n 0))

(define (suma-digitos-tail n acc)
  (if (= n 0)
      acc
      (suma-digitos-tail (quotient n 10) (+ acc (modulo n 10)))))

(suma-digitos 1729)
(suma-digitos 51)
(suma-digitos 963852741)
(suma-digitos 82)

