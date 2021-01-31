#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWBAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> SCFWBAE
;; parse: s-expression -> SCFWBAE
(define (parse sexp)
  (match sexp
    ;; Identificadores.
    [(? symbol?) (parse-id sexp)]
    ;; Números.
    [(? number?) (numS sexp)]
    ;; Booleanos.
    [(? boolean?) (boolS sexp)]
    ;; Condicionales ifs.
    [(list 'if condition then else)
     (if (equal? (length sexp) 4)
         (parse-if condition then else)
         (error 'parser "Condicional incompleto."))]
    ;; Condicionales cond.
    [(cons 'cond cases) (condS (parse-cond cases))]
    ;; With.
    [(list 'with bindings body) 
     (withS (parse-bindings bindings) (parse body))]
    ;; With*
    [(list 'with* bindings body) 
     (withS* (parse-bindings bindings) (parse body))]
    ;; Funciones.
    [(list 'fun params body) (parse-fun params body)]
    [(list (list 'fun params body) args) 
     (appS (parse-fun params body) (map parse args))]
    [(list 'f args) (appS (parse 'f) (map parse args))]
    ;; Aplicación de función.
    [(list 'app fun args) (appS (parse fun) (map parse args))]
    ;; Operaciones.
    [(cons x xs)
     (case x
       [(+ - * / = < <= > >= or and) (parse-op (cons x xs))]
       [(modulo expt)
        (if (equal? (length sexp) 3)
            (parse-op (cons x xs))
            (error 'parse "El operador es binario."))]
       [(add1 sub1 not zero?)
        (if (equal? (length sexp) 2)
            (parse-op (cons x xs))
            (error 'parse "El operador es unario."))])]))

;; Aplica la función parse a un id.
(define (parse-id sexp)
  (cond
    [(equal? sexp 'true) (boolS #t)]
    [(equal? sexp 'false) (boolS #f)]
    [else (idS sexp)]))

;; Aplica la función parse a una expresión if.
(define (parse-if condition then else)
  (iFS (parse condition) (parse then) (parse else)))

;; Regresa el operador que le corresponde a la expresión.
(define (elige-operador sexp)
  (case sexp
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(=) =]
    [(<) <]
    [(<=) <=]
    [(>) >]
    [(>=) >=]
    [(modulo) modulo]
    [(expt) expt]
    [(add1) add1]
    [(sub1) sub1]
    [(not) not]
    [(and) (λ args (foldr (λ (x y) (and x y)) #t args))]
    [(or) (λ args (foldr (λ (x y) (or x y)) #f args))]
    [(zero?) zero?]))

;; Aplica la función parse a una expresión que inicia con un operador.
(define (parse-op sexp)
  (opS (elige-operador (car sexp)) (map parse (cdr sexp))))

;; Aplica la función parse a una lista de bindings.
(define (parse-bindings lb)
  (if (hay-duplicados? (get-ids lb))
      (error (~a "parser: parámetro definido dos veces: " (car get-ids)))
      (map (λ (b) (binding (car b) (parse (cadr b)))) lb)))

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

;; Aplica la función parse a una expresión fun.
(define (parse-fun params body)
  (if (hay-duplicados? params)
      (error (~a "parser: parámetro definido dos veces: " (car params)))
      (funS params (parse body))))

;; Toma una lista de parejas de condiciones y genera la sintáxis abstracta
;; de una condicional en CFWBAE
;; parse-cond: A -> SCFWBAE
;; parse-cond: s-expression -> SCFWBAE
(define (parse-cond cond-expr)
  (parse-conditions-tail cond-expr '()))

;; Función auxiliar para parse-conditions
(define (parse-conditions-tail conds acc)
  (match conds
    [(cons (list 'else expr) '())
     (append acc (list (else-cond (parse expr))))]
    [(cons (list test-expr expr) cs)
     (parse-conditions-tail
      cs
      (append acc (list (condition (parse test-expr) (parse expr)))))]))
