#lang plai

(define-type FWAE
  [idS (i symbol?)]
  [numS (n number?)]
  [addS (izq FWAE?) (der FWAE?)]
  [subS (izq FWAE?) (der FWAE?)]
  [withS (id symbol?) (value FWAE?) (body FWAE?)]
  [funS (param symbol?) (body FWAE?)]
  [appS (fun-expr FWAE?) (arg FWAE?)])

;; Sintaxis abstracta.
(define-type FAE
  [id (i symbol?)]
  [num (n number?)]
  [add (izq FAE?) (der FAE?)]
  [sub (izq FAE?) (der FAE?)]
  [fun (param symbol?) (body FAE?)]
  [app (fun-expr FAE?) (arg FAE?)])


;; Función que quitar el azúcar sintáctica de una expresión FWAE.
;; desugar: FWAE -> FAE
(define (desugar expr)
  (type-case FWAE expr
    [idS (i) (id i)]
    [numS (n) (num n)]
    [addS (lhs rhs) (add (desugar lhs) (desugar rhs))]
    [subS (lhs rhs) (sub (desugar lhs) (desugar rhs))]
    [withS (id value body) (app (fun id (desugar body)) (desugar value))]
    [funS (param body) (fun param (desugar body))]
    [appS (fun-expr arg) (app (desugar fun-expr) (desugar arg))]))

(desugar (withS 'w (subS (idS 'u) (numS 8)) (withS 'v (numS 5) (addS (idS 'w) (addS (idS 'y) (idS 'x))))))
(desugar (withS 'a (numS 2) (addS (idS 'a) (idS 'a))))