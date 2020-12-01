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
  (cond
    ;; Identificadores.
    [(symbol? sexp) (id sexp)]
    ;; Números.
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (first sexp)
       ;; Condicionales if0.
       [(if0)
        (if0 (parse (second sexp))
             (parse (third sexp))
             (parse (fourth sexp)))]
       ;; Operadores n-arios.
       [(+ - * /) (parse-op sexp)]
       ;; Operadores binarios.
       [(modulo expt)
        (if (equal? (length sexp) 3)
            (parse-op sexp)
            (error 'parse "El operador es binario."))]
       ;; Operadores unarios.
       [(add1 sub1)
        (if (equal? (length sexp) 2)
            (parse-op sexp)
            (error 'parse "El operador es unario."))]
       ;; Asignaciones with.
       [(with) (with (parse-bindings (second sexp)) (parse (third sexp)))]
       ;; Asignaciones with*.
       [(with*) (with* (parse-bindings (second sexp)) (parse (third sexp)))]
       ;; Funciones.
       [(fun) (fun (second sexp) (parse (third sexp)))]
       ;; Aplicación de funciones.
       [(app) (app (parse (second sexp)) (map parse (third sexp)))])]))

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

;; Aplica la función parse a una expresión que inicia con un operador.
;; parse-op: s-expression -> WAE
(define (parse-op sexp)
  (op (elige-operador (first sexp)) (map parse (cdr sexp))))

;; Nos dice si hay elementos duplicados en una lista.
(define (hay-duplicados? l)
  (cond
    [(empty? l) #f]
    [(member (first l) (cdr l)) #t]
    [else (hay-duplicados? (cdr l))]))

;; Regresa una lista con todos los binding-id's dentro de la lista de bindings
;; de una expresión with.
(define (get-ids lb)
  (if (empty? lb)
      '()
      (cons (caar lb) (get-ids (cdr lb)))))

;; Aplica la función parse a una lista de bindings.
(define (parse-bindings lb)
  (if (hay-duplicados? (get-ids lb))
      (error 'parse "Hay id's repetidos en los bindings.")
      (map (λ (b) (binding (car b) (parse (cadr b)))) lb)))

;; Test
(parse 'foo)
(parse 'baz)

(parse 4)
(parse -666)

(parse '{if0 {modulo a b} a b})
(parse '{if0 {- c d} {+ c d} {* c d}})

(parse '{+ 1 2 3 4})
(parse '{/ 1 1 1 1})
(parse '{modulo 50 10})
(parse '{add1 1})
(parse '{+ 1 {- 5 {* 5 {sub1 6}}}})

(parse '{with {{a 666}} {+ 666 666}})
(parse '{with {{a 666}} {with {{b 0}} {+ a b}}})
(parse '{with {{a 666} {b 0} {c 1}} {+ a b c}})

(parse '{with* {{a 0} {b a}} {+ b b}})
(parse '{with* {{a 0} {b 1}} {+ a b}})
(parse '{with* {{a 0} {b 1} {c 2}} {+ a b c}})

(parse '{fun {x} {+ x 2}})
(parse '{fun {z} {with {{a 666} {b 0} {c 1}} {+ a b c}}})

(parse '{app {fun {a b} {+ a b}} {2 3}})