#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWBAE-Value
;; (define (lookup name ds)
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error (~a "lookup: Variable libre: " name))]
    [aSub (bound-name value rest-ds)
          (if (symbol=? bound-name name)
              value
              (lookup name rest-ds))]))

;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: CFWBAE DefrdSub-> CFWBAE-Value
(define (interp expr ds)
  (type-case CFWBAE expr
    ;; Identificadores.
    [id (i) (lookup i ds)]
    ;; Números.
    [num (n) (numV n)]
    ;; Booleanos.
    [bool (b) (boolV b)]
    ;; Condicionales if.
    [iF (condition then else) (interp-if condition then else ds)]
    ;; Operadores.
    [op (f args) (interp-op f args ds)]
    ;; Funciones.
    [fun (params body) (closure params body ds)]
    ;; Aplicaciones de función.
    [app (fun args) (interp-app fun args ds)]))

;; Aplica la función interp a una expresión if.
(define (interp-if condition then else ds)
  (if (boolV-b (interp condition ds))
      (interp then ds)
      (interp else ds)))

;; Aplica la función interp a una expresión operador.
(define (interp-op f args ds)
  (let* ([args-val (interp-args args ds)]
         [op-val (apply f args-val)])
    (match op-val
      [(? boolean?) (boolV op-val)]
      [(? number?) (numV op-val)])))

;; Aplica la función interp a cada elemento de la lista op.
(define (interp-args args ds)
  (map (λ (e) (match (interp e ds)
                [(boolV b) b]
                [(numV n) n]))
       args))

;; Aplica la función interp a una expresión app.
(define (interp-app fun args ds)
  (let* ([fun-val (interp fun ds)]
               [params (closure-param fun-val)]
               [init-env (closure-env fun-val)]
               [argumentos (map (λ (a) (interp a init-env)) args)]
               [fenv (final-env params args init-env)])
           (interp (closure-body fun-val) fenv)))

(define (final-env params args env)
  (cond
    [(empty? params) env]
    [else
     (aSub (car params) (car args) (final-env (cdr params) (cdr args) env))]))

;----------------
;;(interp (desugar (parse '{fun {x} {+ x x}})))