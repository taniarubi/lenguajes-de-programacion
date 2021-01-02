#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Recibe una expresión (expr) del lenguaje WAE,
;; un id (sub-id) y otra expresión (value).
;; Sustituye el valor sub-id por value, en expr.
;; subst: WAE symbol WAE -> WAE
(define (subst expr sub-id value)
  (type-case WAE expr
    [id (i) (if (symbol=? i sub-id)
                value
                expr)]
    [num (n) expr]
    [op (f args) (op f (map (λ (x) (subst x sub-id value)) args))]
    [with (bindings body)
          (if (member sub-id (get-ids2 bindings))
              (with (subst-b sub-id value bindings)
                    body)
              (with (subst-b sub-id value bindings)
                    (subst body sub-id value)))]
    [with* (bindings body) (subst-with* expr sub-id value)]))

;; Regresa los id's de una lista de bindings.
(define (get-ids2 expr)
  (map get-id expr))

;; Regresa el id de un binding.
(define (get-id expr)
  (type-case Binding expr
    [binding (id value) id]))

;; Aplica la función subst a una lista de bindings.
(define (subst-b sub-id value bindings)
  (map (λ (b) (subst-binding b sub-id value)) bindings))

;; Aplica la función subst a un binding.
(define (subst-binding b sub-id v)
  (type-case Binding b
    [binding (id value) (binding id (subst value sub-id v))]))

;; Aplica la función subst en una expresión with*.
(define (subst-with* expr sub-id val)
  (let ([final-body (with*-body expr)])
    (match (with*-bindings expr)
      ;; Caso base.
      [(cons (binding id value) '()) 
       (let ([bindings-act (list (binding id (subst value sub-id val)))])
         (with* bindings-act (if (symbol=? id sub-id)
                                 final-body
                                 (subst final-body sub-id val))))]
      ;; Paso recursivo.
      [(cons (binding id value) cdr-bindings) 
       (if (symbol=? id sub-id)
           (with* (cons (binding id (subst value sub-id val)) cdr-bindings)
                  final-body)
           (let ([rec (subst-with* (with* cdr-bindings final-body)
                                   sub-id val)])
                (with* (cons (binding id (subst value sub-id val))
                             (with*-bindings rec))
                       (with*-body rec))))])))

;; Toma un árbol de sintáxis abstracta del lenguaje WAE
;; y lo interpreta, devolviendo el valor numérico correspondiente
;; interp: WAE -> number
(define (interp expr)
  (match expr
    [(id i) (error 'interp "Variable libre.")]
    [(num n) n]
    [(op f args) (apply f (map interp args))]
    [(with bindings body)
     (interp (foldr (lambda (x body) (subst body
                                            (binding-id x)
                                            (num (interp (binding-value x)))))
                    body bindings))]
    [(with* (cons (binding id value) cdr-bindings) body)
     (if (empty? cdr-bindings)
         (interp (subst body id value))
         (interp (subst (with* cdr-bindings body)
                        id
                        (num (interp value)))))]))
