#lang plai

;; Definición del tipo Binding
(define-type Binding
  [binding (id symbol?) (value WAE?)])

;; Definición del tipo WAEs
(define-type WAE
  [id    (i symbol?)]
  [num   (n number?)]
  [op    (f procedure?) (args (listof WAE?))]
  [with  (bindings (listof binding?)) (body WAE?)]
  [with* (bindings (listof binding?)) (body WAE?)])