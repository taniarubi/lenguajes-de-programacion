#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta WAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> WAE
;; parse: s-expression -> WAE
(define (parse sexp)
  (cond
    ;; Identificadores.
    [(symbol? sexp) (id sexp)]
    ;; Números.
    [(number? sexp) (num sexp)] 
    [(list? sexp)
       (case (first sexp)
         ;; Operadores binarios.
         [(+ - * / modulo expt)
          (if (equal? (length sexp) 3)
              (op (elige-operador (first sexp)) (map parse (cdr sexp)))
              (error 'parse "El operador es binario."))]
         ;; Operadores unarios.
         [(add1 sub1)
          (if (equal? (length sexp) 2)
              (op (elige-operador (first sexp)) (map parse (cdr sexp)))
              (error 'parse "El operador es unario."))]
         ;; Asignaciones locales.
         [(with) (with (parse (second sexp)) (parse (third sexp)))]
         [(with*) (with (parse (second sexp)) (parse (third sexp)))])]
    [else error 'parse "Expresión inválida."]))

;; Función auxiliar. Regresa el operador que le corresponde a la expresión.
;; elige-operador: symbol -> procedure
(define (elige-operador sexp)
  (case sexp
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(modulo) modulo]
    [(expt) expt]
    [(add1) add1]
    [(sub1) sub1]))

(parse '(+ 2 3))
(parse '(- 9 4))
(parse '(sub1 2))
;;(parse '(+ 2))
;;(parse '(add1 3 6))
(parse 4)
(parse 'foo)
(parse '(with x 2 5))