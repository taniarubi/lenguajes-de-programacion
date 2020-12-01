#lang plai
;; Definición del tipo Binding
(define-type Binding
  [binding (id symbol?) (value CFWAE?)])

;; Definición del tipo CFWAE
(define-type CFWAE
  [id    (i symbol?)]
  [num   (n number?)]
  [if0 (condicion CFWAE?) (then CFWAE?) (else CFWAE?)]
  [op    (f procedure?) (args (listof CFWAE?))]
  [with  (bindings (listof binding?)) (body CFWAE?)]
  [with* (bindings (listof binding?)) (body CFWAE?)]
  [fun (params (listof symbol?)) (body CFWAE?)]
  [app (fun CFWAE?) (args (listof CFWAE?))])

;;Data-type que representa la sintaxis abstracta de CFWAE-Value
(define-type CFWAE-Value
  [closure (param (listof symbol?)) (body CFWAE?) (env DefrdSub?)]
  [numV(n number?)])

;; Data-type que representa un caché de sustituciones
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value CFWAE?) (ds DefrdSub?)])