#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Recibe una expresión (expr) del lenguaje WAE,
;; un id (sub-id) y otra expresión (value).
;; Sustituye el valor sub-id por value, en expr.
;; subst: WAE symbol WAE -> WAE
(define (subst expr sub-id value)
  (match expr
    [(id i) (if (symbol=? i sub-id)
                value
                expr)]
    [(num n) expr]
    [(op f sexp) (op f (map (λ (x) (subst x sub-id value)) sexp))]
    [(with bindings body) (if (member sub-id (encuentra-ids bindings))
                              (with (subst-b sub-id value bindings)
                                    body)
                              (with (subst-b sub-id value bindings)
                                    (subst body sub-id value)))]
    [(with* bindings body) (if (member sub-id (encuentra-ids bindings))
                              (with* (subst-b sub-id value bindings)
                                    body)
                              (with* (subst-b sub-id value bindings)
                                    (subst body sub-id value)))]))

;; Regresa los id's de una lista de bindings.
(define (encuentra-ids expr)
  (map get-id expr))

;; Regresa el id de un binding.
(define (get-id expr)
  (match expr
    [(binding id value) id]))

;; Ejecuta la función subst en una lista de bindings.
(define (subst-b sub-id value bindings)
  (map (λ (b) (subst-binding b sub-id value)) bindings))

;; Ejecuta la función subst en un binding.
(define (subst-binding b sub-id v)
  (match b
    [(binding id value) (binding id (subst value sub-id v))]))
                                      
;; Toma un árbol de sintáxis abstracta del lenguaje WAE
;; y lo interpreta, devolviendo el valor numérico correspondiente
;; interp: WAE -> number
(define (interp expr)
  (match expr
    [(id i) (error 'interp "Variable libre.")]
    [(num n) n]
    [(op f args) (apply f (map (λ (v) (interp v)) args))]
    [(with bindings body)
     (interp (subst-b2 (interp-b bindings) body))]
    [(with* bindings body) (subst-b2 (interp bindings) (interp body))]))

;; Aplica la función subst a una lista de bindings.
(define (subst-b2 lista body)
  (if (empty? lista)
      body
      (subst-binding2 (car lista) (subst-b2 (cdr lista) body))))

;; Aplica la función subst en un binding.
(define (subst-binding2 expr body)
  (match expr
    [(binding id value) (subst body id value)]))

;; Aplica la función interp a una lista de bindings.
(define (interp-b lista)
  (if (empty? lista)
      '()
      (cons (interp-binding (car lista)) (interp-b (cdr list)))))

;; Aplica la función interp a un binding.
(define (interp-binding expr)
  (match expr
    [(binding id value) (binding id (num (interp value)))]))
