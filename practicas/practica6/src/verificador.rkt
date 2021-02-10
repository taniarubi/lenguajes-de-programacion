#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Busca el identificador "name" en el contexto "context" regresándo el
;; valor correspondiente o informando un error si no lo encuentra.
(define (lookup-context name context)
  (type-case Type-Context context
    [phi () (error (~a "lookup: Hay un identificador libre: " name))]
    [gamma (id tipo rest)
           (if (symbol=? name id)
               tipo
               (lookup-context name rest))]))

;; Toma un Árbol de sintaxis abstracta CFWBAE y obtiene el tipo
;; de la expresión mínima.
;; typeof CFWBAE -> Type-Context -> Type
;; (define (typeof expr context)
(define (typeof expr context)
  (match expr
    ;; Identificadores
    [(idS i) (lookup-context i context)]
    ;; Números.
    [(numS n) (numberT)]
    ;; Booleanos.
    [(boolS b) (booleanT)]
    ;; Condicionales.
    [(iFS condition then else) (typeof-if condition then else context)]
    ;; Operadores.
    [(opS f args) (typeof-op f args context)]
    [(condS cases) 9]
    [(withS bindings body) 10]
    [(withS* bindings body) 11]
    [(funS params rType body) 12]
    [(appS fun args) 13]))
  
(define (prueba exp)
  (typeof (parse exp) (phi)))

;; Aplica la función typeof a una expresión op.
(define (typeof-op f args context)
  (cond
    [(member f (list + - * / modulo expt add1 sub1))
     (check-args1 args context)]
    [(member f (list = < <= > >= zero?)) (check-args2 args context)]
    [(member f (list and-aux or-aux not)) (check-args3 args context)]))    

;; Revisa los argumentos de un operador {+ - * / modulo expt add1 sub1} cuyos
;; tipos son numberT.
(define (check-args1 args context)
  (cond
    [(empty? args) (numberT)]
    [(if (numberT? (typeof (car args) context))
         (check-args1 (cdr args) context)
         (error (~a "typeof: Error in parameter " (car args) "\nExpected type: "
                    "(numberT)\nGiven type: " (typeof (car args) context))))]))

;; Revisa los argumentos de un operador {= < <= > >= zero?} cuyos tipos son
;; booleanT.
(define (check-args2 args context)
  (cond
    [(empty? args) (booleanT)]
    [(if (numberT? (typeof (car args) context))
         (check-args2 (cdr args) context)
         (error (~a "typeof: Error in parameter " (car args) "\nExpected type: "
                    "(booleanT)\nGiven type: " (typeof (car args) context))))]))

;; Revisa los argumentos de un operador {and or not} cuyos tipos son booleanT.
(define (check-args3 args context)
  (cond
    [(empty? args) (booleanT)]
    [(if (booleanT? (typeof (car args) context))
         (check-args3 (cdr args) context)
         (error (~a "typeof: Error in parameter " (car args) "\nExpected type: "
                    "(booleanT)\nGiven type: " (typeof (car args) context))))]))

;; Aplica la función typeof a una expresión if.
(define (typeof-if condition then else context)
  (let ([condition-type (typeof condition context)]
        [then-type (typeof then context)]
        [else-type (typeof else context)])
    (cond
      [(booleanT? condition-type)
       (if (equal? then-type else-type)
           then-type
           (error "typeof: Type error\nconditionals must have same type in then-expr and else-expr"))]
      [else
       (error "if: Type error\nConditional's test-expr type must be a boolean\nGiven: " condition-type)])))
