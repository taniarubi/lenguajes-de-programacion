#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))
(require (file "./interp.rkt"))

;; Función auxiliar para pruebas, llama a parse y a interp
;; con el caché vacío.
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; test: A -> CFWBAE
(define (prueba exp)
  (interp (desugar (parse exp)) (mtSub)))

#| Pruebas de parse|#
(printf "___________________________________________________________________________________________________________________________\nInician pruebas de la función parse\n")

(test/exn (prueba '{fun {x y x} {+ x {+ y z}}})
      "parser: parámetro definido dos veces: x")

(test (parse '{{fun {x y} {+ x y}} {10 8}})
      (appS (funS '(x y) (opS + (list (idS 'x) (idS 'y)))) (list (numS 10) (numS 8))))

(test (parse '{with {{x 1} {y x} {z 3}} x})
      (withS (list (binding 'x (numS 1)) (binding 'y (idS 'x)) (binding 'z (numS 3))) (idS 'x)))

(test (parse '{with {{x 1}
                     {y 2}}
                    {with* {{z 3}
                           {w z}}
                          {with {{f {fun {x} x}}}
                                {f {w}}}}})
      (withS (list (binding 'x (numS 1)) (binding 'y (numS 2)))
             (withS* (list (binding 'z (numS 3)) (binding 'w (idS 'z)))
                    (withS (list (binding 'f (funS '(x) (idS 'x)))) (appS (idS 'f) (list (idS 'w)))))))

(test (parse '{{fun {x y} {+ {* x y} x}} {3 4}})
      (appS (funS '(x y) (opS + (list (opS * (list (idS 'x) (idS 'y))) (idS 'x)))) (list (numS 3) (numS 4))))

(test (parse '{with* {{x 1} {y x} {z 3}} x})
      (withS* (list (binding 'x (numS 1)) (binding 'y (idS 'x)) (binding 'z (numS 3))) (idS 'x)))

(test/exn (parse '{if (= 2 4) 5}) "parser: Falta la else-expresion")

(test (parse '{if (= 2 4) 5 4})
      (iFS (opS = (list (numS 2) (numS 4))) (numS 5) (numS 4)))

(test/exn (prueba '{cond {(= 2 4) 5} {#t 6}}) "parser: Falta la else-expresion")

(test (parse '{cond {(= 2 4) 5} {#t 6} {else 7}})
      (condS (list (condition (opS = (list (numS 2) (numS 4))) (numS 5)) (condition (boolS #t) (numS 6)) (else-cond (numS 7)))))

(test (parse '{cond {(= 2 4) 5} {#t 6} {(<= 3 3) 7} {#f 8} {else 9}})
      (condS
       (list
        (condition (opS = (list (numS 2) (numS 4))) (numS 5))
        (condition (boolS #t) (numS 6))
        (condition (opS <= (list (numS 3) (numS 3))) (numS 7))
        (condition (boolS #f) (numS 8))
        (else-cond (numS 9)))))
      
(printf "___________________________________________________________________________________________________________________________\nInician pruebas de la función desugar\n")
#| Pruebas de desugar|#

(test (desugar (parse '{cond {#t 1} {#f 2} {else 3}}))
      (iF (bool #t) (num 1) (iF (bool #f) (num 2) (num 3))))

(test (desugar (parse '{cond {(= 2 4) 5} {#t 6} {(<= 3 3) 7} {#f 8} {else 9}}))
      (iF (op = (list (num 2) (num 4))) (num 5) (iF (bool #t) (num 6) (iF (op <= (list (num 3) (num 3))) (num 7) (iF (bool #f) (num 8) (num 9))))))

(test
 (desugar
  (parse
   '{with {{x 1}
           {y 2}}
          {with* {{z 3}
                  {w z}}
                 {with {{f {fun {x} x}}}
                       {f {w}}}}}))
 (app (fun '(x y)
           (app (fun '(z)
                     (app (fun '(w)
                               (app (fun '(f)
                                         (app (id 'f)
                                              (list (id 'w))))
                                    (list (fun '(x) (id 'x)))))
                          (list (id 'z))))
                (list (num 3))))
      (list (num 1) (num 2))))

(printf "___________________________________________________________________________________________________________________________\nInician pruebas de la función interp\n")
#| Pruebas de interp|#

(test (prueba '3) (numV 3))

(test (prueba #t) (boolV #t))

(test/exn (prueba 'x) "lookup: Variable libre: x")

(test (prueba '{if {< 1 1} 5 6}) (numV 6))

(test (prueba '{if {< 1 2} 5 6}) (numV 5)) 

(test/exn (prueba '{if {fun {x} {+ x 1}} 5 6}) "interp: Símbolo no esperado la condicional de if, no es un booleano")


(test (prueba'{cond {(= 2 2) 5} {#t 6} {(<= 3 3) 7} {#f 8} {else 9}}) (numV 5))

(test (prueba'{cond {(= 2 4) 5} {#t 6} {(<= 3 3) 7} {#f 8} {else 9}}) (numV 6))

(test (prueba'{cond {(= 2 4) 5} {#f 6} {(<= 3 3) 7} {#f 8} {else 9}}) (numV 7))

(test (prueba'{cond {(= 2 4) 5} {#f 6} {(< 3 3) 7} {#t 8} {else 9}}) (numV 8))

(test (prueba'{cond {(= 2 4) 5} {#f 6} {(< 3 3) 7} {#f 8} {else 9}}) (numV 9))


(test (prueba '{with {{x 5} {y 1}} {+ x y}}) (numV 6))

(test/exn (prueba '{with {{x 5} {y {+ x 1}}} {+ x y}}) "lookup: Hay un identificador libre: x")

(test (prueba '{with {{f {fun {x} {+ x x}}}} {f {3}}}) (numV 6)) 

(test/exn (prueba '{with {{x 3} {f {fun {a} {+ x a}}}}
                      {f {0}}}) "lookup: Hay un identificador libre: x")


(test (prueba '{with* {{x 5} {y 1}} {+ x y}}) (numV 6)) 

(test (prueba '{with* {{x 5} {y {+ x 1}}} {+ x y}}) (numV 11))

(test (prueba '{with* {{x 3}}
                      {with* {{f {fun {y} {+ x y}}}}
                             {with* {{x 4}}
                                    {f {1}}}}}) (numV 4)) 
(test (prueba '{with* {{x 1} {y 2} {z 3}}
                      {fun {x y z} {+ x {+ y z}}}})
      (closure '(x y z) (op + (list (id 'x) (op + (list (id 'y) (id 'z))))) (aSub 'z (numV 3) (aSub 'y (numV 2) (aSub 'x (numV 1) (mtSub))))))

(test (prueba '{with* {{x 3}
                       {f {fun {a} {+ x a}}}}
                      {f {0}}}) (numV 3))

(test (prueba '{{fun {x y} {+ x y}} {10 3}}) (numV 13)) 


(test (prueba '{with {{x 1}
                {y 2}}
               {with* {{z 3}
                       {w z}}
                      {with {{f {fun {x} x}}}
                            {f {w}}}}})
      (numV 3))

(test (prueba '{with* {{x 5}
                       {w {+ x 1}}
                       {z {with {{x 10}
                                 {f {fun {a} {+ x a}}}}
                                {f {10}}}}}
                      {+ x z}}) (numV 20))
