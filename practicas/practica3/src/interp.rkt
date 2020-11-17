#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Recibe una expresión (expr) del lenguaje WAE,
;; un id (sub-id) y otra expresión (value).
;; Sustituye el valor sub-id por value, en expr.
;; subst: WAE symbol WAE -> WAE
(define (subst expr sub-id value)
  (match expr
    [(id i) (if (symbol=? i sub-id) value expr)]
    [(num n) expr]
    [(op f sexp) (op f (map (λ (x) (subst x sub-id value)) sexp))]
    [(with bindings body)
     (if (and (member sub-id (bindings-id bindings))
              (λ (x y) (symbol=? x y)))
         (with (map (λ (b) (subst-value-binding b sub-id value)) bindings)
               body)
         (with (map (λ (b) (subst-value-binding b sub-id value)) bindings)
               (subst body sub-id value)))]
    [(with* bindings body)
     (if (and (member sub-id (bindings-id bindings))
              (λ (x y) (symbol=? x y)))
         (with* (map (λ (b) (subst-binding b sub-id value)) bindings)
               body)
         (with* (map (λ (b) (subst-binding b sub-id value)) bindings)
               (subst body sub-id value)))]))
                                      
;; Toma un árbol de sintáxis abstracta del lenguaje WAE
;; y lo interpreta, devolviendo el valor numérico correspondiente
;; interp: WAE -> number
(define (interp expr)
  (match expr
    [(id i) (error 'interp "Variable libre.")]
    [(num n) n]
    [(op f args) (apply f (map (λ (v) (interp v)) args))]
    [(with bindings body)
     (interp (subst-bindings (interp-bindings bindings) body))]
    [(with* bindings body) (interp (subst-bindings bindings body))]))

;; Función para obtener todos los id's.
(define (bindings-id expr)
  (map binding-id expr))

;; Regresa el id de un binding.
(define (binding-id expr)
  (match expr
    [(binding id value) id]))

;; Sustitución de un valor.
(define (subst-value-binding bind sub-id val)
  (type-case Binding bind
    [binding (id value) (binding id (subst value sub-id val))]))

;; Sustitución para un binding.
(define (subst-binding expr body)
  (match expr
    [(binding id value) (subst body id value)]))

;; Aplica la sustitución a una lista de bindings
(define (subst-bindings lista body)
  (if (empty? lista)
      body
      (subst-binding (car lista) (subst-bindings (cdr lista) body))))
    
;; Interpretación de un binding.
(define (interp-bindings bind-list)
  (cond
    [(empty? bind-list) '()]
    [else
     (cons
      (type-case Binding (car bind-list)
        [binding (id value) (binding id (num (interp value)))])
      (interp-bindings (cdr bind-list)))]))
