#lang plai

;; Regresa la reversa de una lista.
(define (mi-reversa xs)
  (if (empty? xs)
      '()
      (append (mi-reversa (cdr xs)) (list (car xs)))))

;; Concatena dos listas.
(define (mi-append xs ys)
  (if (empty? xs)
      ys
      (cons (car xs) (mi-append (cdr xs) ys))))