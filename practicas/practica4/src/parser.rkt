#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> CFWAE
;; parse: s-expression -> CFWAE
(define (parse sexp)
  (cond
    ;; Identificadores.
    [(symbol? sexp) (id sexp)]
    ;; Números.
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (first sexp)
       ;; Condicionales if0.
       [(if0)
        (if0 (parse (second sexp))
             (parse (third sexp))
             (parse (fourth sexp)))]
       ;; Operadores n-arios.
       [(+ - * /) (parse-op sexp)]
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
       ;; Asignaciones with.
       [(with) (with (parse-bindings (second sexp)) (parse (third sexp)))]
       ;; Asignaciones with*.
       [(with*) (with* (parse-bindings (second sexp)) (parse (third sexp)))]
       ;; Funciones.
       [(fun) (fun (second sexp) (parse (third sexp)))]
       ;; Aplicación de funciones.
       [(app) (app (parse (second sexp)) (map parse (third sexp)))]
       [else error 'parse "La expresión no pertenece a la gramática CFWAE."])]))

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
  (op (elige-operador (car sexp)) (map parse (cdr sexp))))

;; Nos dice si hay elementos duplicados en una lista.
(define (hay-duplicados? l)
  (cond
    [(empty? l) #f]
    [(member (car l) (cdr l)) #t]
    [else (hay-duplicados? (cdr l))]))

;; Regresa una lista con todos los binding-id's dentro de la lista de bindings
;; de una expresión with.
(define (get-ids lb)
  (if (empty? lb)
      '()
      (cons (caar lb) (get-ids (cdr lb)))))

;; Aplica la función parse a una lista de bindings.
(define (parse-bindings lb)
  (if (hay-duplicados? (get-ids lb))
      (error 'parse "Hay id's repetidos en los bindings.")
      (map (λ (b) (binding (car b) (parse (cadr b)))) lb)))
      