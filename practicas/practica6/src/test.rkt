#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
;;(require (file "./desugar.rkt"))
(require (file "./verificador.rkt"))


;; Función auxiliar para pruebas, llama a parse y a typeof
;; con el contexto de tipos vacío.
;; test: SCFWBAE -> Type
(define (prueba exp)
  (typeof (parse exp) (phi)))

(display "PRUEBAS TYPEOF\n______________________________________________________________________________________________________________________________\n\n")

(test (prueba '#t) (booleanT))

(test (prueba '2) (numberT))

(test (prueba '{+ 1 2}) (numberT))

(test/exn (prueba '{+ 1 #f}) "typeof: Error in parameter (boolS #f)\nExpected type: (numberT)\nGiven type: (booleanT)")

(test/exn (prueba '{and 1 #f}) "typeof: Error in parameter (numS 1)\nExpected type: (booleanT)\nGiven type: (numberT)")
                  
(test (prueba '{if #t 2 3}) (numberT))

(test (prueba '{with {{x : number 2} {y : boolean #t} {z : number 1}} {if y x z}}) (numberT))

(test/exn (prueba '{if #t 2 #t}) "typeof: Type error\nconditionals must have same type in then-expr and else-expr")

(test/exn (prueba '{cond {#t 2} {#f 3} {else #t}}) "typeof: Type error\nconditionals must have same type in then-expr and else-expr")

(test/exn (prueba '{if 3 2 #t}) "if: Type error\nConditional's test-expr type must be a boolean\nGiven: (numberT)")

(test (prueba '{fun {{x : number} {y : boolean}} : (number boolean -> number) {if y x 0}}) (funT (list (numberT) (booleanT) (numberT))))

(test/exn (prueba '{fun {{x : number} {y : number}} : (number number -> number) {if y x 0}}) "if: Type error\nConditional's type must be a boolean\nGiven: (numberT)")

(test (prueba '{{fun {{x : number} {y : boolean}} : (number boolean -> number) {if y x 0}} {2 #t}}) (numberT))

(test/exn (prueba '{{fun {{x : number} {y : boolean}} : (number boolean -> number) {if y x 0}} {2 3}})
          "app: Type error:\nParameter's type doesn't match expected types\nGiven: (numberT)\nExpected: (booleanT)")

