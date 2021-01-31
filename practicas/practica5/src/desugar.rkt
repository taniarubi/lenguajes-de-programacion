#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Función que toma una expresión con azúcar sintáctica
;; SCFWBAE y elimina el azúcar sintáctica, tansformándola
;; en una expresión del tipo CFWBAE; formando el árbol de
;; sintáxis abstracta correspondiente a la expresión recibida.
;; desugar SCFWBAE-> CFWBAE
(define (desugar sexpr)
  (match sexpr
    [(idS i) (id i)]
    [(numS n) (num n)] 
    [(boolS b) (bool b)] 
    [(iFS condition then else) (desugar-if condition then else)]
    [(opS f args) (op f (map desugar args))]
    [(condS cases) (desugar-cases cases)]
    [(withS bindings body) (desugar-with bindings body)]
    [(withS* bindings body) (desugar-with* bindings body)]
    [(funS params body) (desugar-funS params body)]
    [(appS (idS 'f) args) (app (id 'f) (map desugar args))]
    [(appS fun args) (desugar-appS fun args)]))

;; Aplica la función desugar a una expresión if.
(define (desugar-if condition then else)
  (iF (desugar condition)
      (desugar then)
      (desugar else)))

;; Aplica la función desugar a una expresión cond.
(define (desugar-cases cases)
  (type-case Condition (car cases)
    [condition (test-expr then-expr)
               (iF (desugar test-expr)
                   (desugar then-expr)
                   (desugar-cases (cdr cases)))]
    [else-cond (else-expr)
               (desugar else-expr)]))

;; Aplica la función desugar a una expresión with.
(define (desugar-with bindings body)
  (app (fun (with-ids bindings) (desugar body))
                (map desugar (with-values bindings))))

;; Regresa los ids de una lista de bindings.
(define (with-ids bindings)
  (map (λ (b) (binding-id b)) bindings))

;; Regresa los valores de una lista de bindings.
(define (with-values bindings)
  (map (λ (b) (binding-value b)) bindings))

;; Aplica la función desugar a una expresión with*.
(define (desugar-with* bindings body)
  (desugar (withS (list (car bindings))
                  (if (empty? (cdr bindings))
                      body
                      (withS* (cdr bindings) body)))))

;; Aplica la función desugar a una expresión funS.
(define (desugar-funS params body)
  (fun params (desugar body)))

;; Aplica la función desugar a una expresión appS.
(define (desugar-appS fun args)
  (if (empty? (cdr args))
      (app (desugar fun) (desugar (car args)))
      (desugar (appS (appS fun (list (car args))) (cdr args)))))
