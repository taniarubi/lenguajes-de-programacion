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
         ;; Operadores n-narios.
         [(+ - * /)
          (parse-op sexp)]
         ;; Operadores binarios.
         [(modulo expt)
          (if (equal? (length sexp) 3)
              (parse-op sexp)
              (error 'parse "El operador es binario."))]
         ;; Operadores unarios.
         [(add1 sub1)
          (if (equal? (length sexp) 2)
              (parse-op sexp)
              (error 'parse "El operador es unario."))]
         ;; Asignaciones locales.
         [(with) (with (parse-binding sexp) (parse (third sexp)))]
         [(with*) (with* (parse-binding sexp) (parse (third sexp)))])]
    [else error 'parse "La expresión no se encuentra dentro de la " +
                       "gramática WAE."]))

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

;; Aplica la función parse a una expresión que inicia con un operador.
;; parse-op: s-expression -> WAE
(define (parse-op sexp)
  (op (elige-operador (first sexp)) (map parse (cdr sexp))))

;; Aplica la función parse a la lista de bindings de una expresión with.
;; parse-with: s-expression -> WAE
(define (parse-binding sexp)
  (if (equal? (hayDuplicados (encuentraID (second sexp))) #t)
      (error 'parse "Hay id's repetidos.")
      (map (λ (b) (binding (car b) (parse (cadr b))))
           (second sexp))))

;; Nos dice si hay elementos duplicados en una lista.
(define (hayDuplicados lista)
  (cond
    [(empty? lista) #f]
    [(member (first lista) (cdr lista)) #t]
    [else (hayDuplicados (cdr lista))]))

;; Regresa todos los binding-id de una expresión with.
(define (encuentraID lista)
  (if (empty? lista)
      '()
      (cons (car (first lista)) (encuentraID (cdr lista)))))
