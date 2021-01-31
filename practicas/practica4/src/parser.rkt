#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> CFWAE
;; parse: s-expression -> CFWAE
(define (parse sexp)
  (match sexp
    ;; Identificadores.
    [(? symbol?) (id sexp)]
    ;; Números.
    [(? number?) (num sexp)]
    ;; Condicionales if.
    [(list 'if0 condicion then else)
     (if0 (parse condicion) (parse then) (parse else))]
    ;; With.
    [(list 'with bindings body)
     (get-fun (get-id bindings) body (get-value bindings))]
    ;; With*
    [(list 'with* bindings body)
     (to-fun (get-id bindings) (get-value bindings) body)]
    ;; Funciones.
    [(list (list 'fun params body) args)
     (get-fun params body args)]
    [(list 'fun (list args ...) body)
     (fun (check-id args) (parse body))]
    ;; Aplicaciones de funciones.
    [(list func-name (list args ...))
     (app (parse func-name) (map parse args))]
    ;; Operadores.
    [(cons x xs)
     (case x
       ;; Operadores n-arios.
       [(+ - * /) (parse-op (cons x xs))]
       ;; Operadores binarios.
       [(modulo expt)
        (if (equal? (length (cons x xs)) 3)
            (parse-op (cons x xs))
            (error 'parse "El operador es binario."))]
       ;; Operadores unarios.
       [(add1 sub1)
        (if (equal? (length (cons x xs)) 2)
            (parse-op (cons x xs))
            (error 'parse "El operador es unario."))])]))

;; Nos dice si hay elementos duplicados en una lista.
(define (hay-duplicados? l)
  (cond
    [(empty? l) #f]
    [(member (car l) (cdr l)) #t]
    [else (hay-duplicados? (cdr l))]))

;; Regresa los id's de una lista de bindings.
(define (get-id bindings)
  (if (empty? bindings)
      '()
      (append (list (caar bindings)) (get-id (cdr bindings)))))

;; Aplica la función parse a una expresión with.
(define (get-fun args body value)
  (if (equal? (length args) (length value))
      (app (fun (check-id args) (parse body)) (map parse value))
      (error "parser: La cardinalidad de los argumentos difiere de la aridad de la función")))

;; Revisa los id's de una función.
(define (check-id ids)
  (cond
    [(empty? ids) ids]
    [(hay-duplicados? ids)
     (error (~a "parser: parámetro definido dos veces: " (car ids)))]
    [else (append (list (car ids)) (check-id (cdr ids)))]))

;; Regresa los valores de una lista de bindings.
(define (get-value bindings)
  (if (empty? bindings)
      '()
      (append (cdar bindings) (get-value (cdr bindings)))))

;; Aplica la función parse a una expresión with*.
(define (to-fun id value body)
  (if (empty? id)
      (parse body)
      (app (fun (check-id (list (car id)))
                (to-fun (cdr id) (cdr value) body))
           (list (parse (car value))))))

;; Aplica la función parse a una expresión que inicia con un operador.
;; parse-op: s-expression -> WAE
(define (parse-op sexp)
  (op (elige-operador (car sexp)) (map parse (cdr sexp))))

;; Función auxiliar. Regresa el operador que le corresponde a la expresión.
;; elige-operador: symbol -> procedure
(define (elige-operador sexp)
  (case sexp
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(modulo) modulo]
    [(expt) expt]
    [(add1) add1]
    [(sub1) sub1]))
