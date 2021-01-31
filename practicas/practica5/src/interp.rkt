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
    [mtSub () (error (~a "lookup: Hay un identificador libre: " name))]
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
         [local-ds (closure-env fun-val)])
    (interp (closure-body fun-val)
            (add-func-bindings (closure-param fun-val)
                               args
                               local-ds
                               ds))))

(define (add-func-bindings param-lis arg-lis ds original-ds)
  (cond
    [(and (null? param-lis) (null? arg-lis)) ds]
    [(equal? (length param-lis) (length arg-lis))
     (add-func-bindings (rest param-lis)
                        (rest arg-lis)
                        (aSub (first param-lis)
                              (interp (first arg-lis) original-ds)
                              ds)
                        original-ds)]
    [else (error ' interp "argument list (~a) does not have required number of elements (~a)" (length param-lis) (length arg-lis))]))
