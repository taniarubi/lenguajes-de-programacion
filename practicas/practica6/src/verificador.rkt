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
    ;; Condicionales if.
    [(iFS condition then else)
     (typeof-iFS condition then else context)]
    ;; Operadores.
    [(opS f args) (typeof-opS f args context)]
    ;; Condicionales.
    [(condS cases) (typeof-condS cases context)]
    ;; With.
    [(withS bindings body) (typeof-with bindings body context)]
    ;; With*
    [(withS* bindings body) (typeof-with bindings body context)]
    ;; Funciones.
    [(funS params rType body)5]
    ;; Aplicación de funciones.
    [(appS fun args) 13]))
  
(define (prueba exp)
  (typeof (parse exp) (phi)))

;; Aplica la función typeof a una expresión if.
(define (typeof-iFS condition then else context)
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

;; Aplica la función typeof a una expresión op.
(define (typeof-opS f args context)
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

;; Aplica la función typeof a una expresión cond.
(define (typeof-condS cases context)
  (let ([tipo empty])
    (if (andmap (λ (c) (type-case Condition c
                         [condition (test-expr then-expr)
                                    (let ([test-type (typeof test-expr context)]
                                          [then-type (typeof then-expr context)])
                                      (cond
                                        [(booleanT? test-type)
                                         (or (list (equal? tipo then-type)
                                                   (equal? tipo empty)))
                                         (set! tipo then-type)]
                                        [else (error "cond: Type error\nConditional's test-expr type must be a boolean\nGiven: " test-type)]))]
                         [else-cond (else-expr)
                                    (let ([else-type (typeof else-expr context)])
                                      (if (equal? tipo else-type)
                                          #t
                                          (error "typeof: Type error\nconditionals must have same type in then-expr and else-expr")))]))
                cases)
        tipo
        (error "typeof: Type error\nconditionals"))))

;; Aplica la función typeof a una expresión with.
(define (typeof-with bindings body context)
  (typeof body (typeof-bindings bindings context)))

;; Define un contexto para una expresión with a partir de sus bindings.
(define (typeof-bindings bindings context)
  (cond
    [(empty? bindings) context]
    [else
     (type-case Binding (car bindings)
       [binding (id tipo value)
                (let ([value-type (typeof value context)])
                  (if (equal? tipo value-type)
                      (typeof-bindings (cdr bindings)
                                       (gamma id tipo context))
                      (error "typeof: Type error\nBindings must have same type in tipo and value")))])]))
