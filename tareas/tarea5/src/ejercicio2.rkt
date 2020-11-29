#lang plai

(define (goo l)
  (if (empty? l)
      empty
      (append (car l) (goo (cdr l)))))

(goo '((1 2 3) (1 2)))
(goo '((1 2 3) (4 5 6) (7 8 9)))

(define (concatena-listas-de-lista l)
  (concatena-listas-de-lista-tail l '()))

(define (concatena-listas-de-lista-tail l acc)
  (if (empty? l)
      acc
      (concatena-listas-de-lista-tail (cdr l) (append acc (car l)))))

(concatena-listas-de-lista '((1 2 3) (1 2)))
(concatena-listas-de-lista '((1 2 3) (4 5 6) (7 8 9)))