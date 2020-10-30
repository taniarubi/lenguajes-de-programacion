#lang plai

(define (agregaN e n l)
  (cond
    [(< n 0) error 'agregaN "El índice debe ser un entero positivo."]
    [(= n 0) (cons e l)]
    [else (cons (car l) (agregaN e (- n 1) (cdr l)))]))

(agregaN 2 7 (list 1 2 3 4 5 6 7 8))
(agregaN 0 2 (list 5 4 3 14 1))