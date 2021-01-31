#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWAE
;; (define (lookup name ds)
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "Variable libre.")]
    [aSub (bound-name value rest-ds)
          (if (symbol=? bound-name name)
              value
              (lookup name rest-ds))]))

;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: CFWAE DefrdSub-> number
(define (interp expr ds)
    (match expr
      ;; Identificadores.
      [(id i) (lookup i ds)]
      ;; Números.
      [(num n) (numV n)]
      ;; Condicionales if.
      [(if0 condition then else)
       (if (equal? 0 (numV-n (errorIf0 (interp condition ds))))
           (interp then ds)
           (interp else ds))]
      ;; Operadores. 
      [(op f args)
       (numV (apply f (map (λ (e) (numV-n (interp e ds))) args)))]
      ;; Funciones.
      [(fun params body) (closure params body ds)]
      ;; Aplicaciones de función.
      [(app f args-list)
       (let* ([fun-val (interp f ds)]
              [local-ds (closure-env fun-val)])
         (interp (closure-body fun-val)
                 (add-func-bindings (closure-param fun-val)
                                    args-list
                                    local-ds
                                    ds)))]))

;; Regresa un error en caso de ser necesario en una expresión if.
(define (errorIf0 expr)
  (if (numV? expr)
      expr
      (error "interp: Símbolo no esperado la condicional de if0, no es un número")))

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
