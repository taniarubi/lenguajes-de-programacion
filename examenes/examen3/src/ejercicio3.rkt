#lang plai

(define (filter-neg l)
  (filter-neg/k l (lambda (x) x)))

(define (filter-neg/k l k)
  (cond
    [(empty? l) (k '())]
    [else
     (if (< (first l) 0)
         (filter-neg/k (rest l)
                       (lambda (v) (k (cons (first l) v))))
         (filter-neg/k (rest l) k))]))

(filter-neg '(0 1 -1 0 -4 1 -2))