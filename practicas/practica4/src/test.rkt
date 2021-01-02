#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./interp.rkt"))
(define (prueba sexp)
  (interp (parse sexp) (mtSub)))

#| Pruebas de parse|#
(printf "___________________________________________________________________________________________________________________________\nInician pruebas de la función parse\n")

(test/exn (prueba '{fun {x y x} {+ x {+ y z}}})
      "parser: parámetro definido dos veces: x")

(test (parse '{{fun {x y} {+ x y}} {10 8}})
      (app (fun '(x y) (op + (list (id 'x) (id 'y)))) (list (num 10) (num 8))))

(test (parse '{with {{x 1} {y x} {z 3}} x})
      (app (fun '(x y z) (id 'x)) (list (num 1) (id 'x) (num 3))))

(test (parse '{with {{x 1}
                     {y 2}}
                    {with {{z 4}
                           {w 5}}
                          {with {{f {fun {x} x}}}
                                {f {3}}}}})
      (app (fun '(x y)
                (app (fun '(z w)
                          (app (fun '(f)
                                    (app (id 'f)
                                         (list (num 3))))
                               (list (fun '(x) (id 'x)))))
                     (list (num 4) (num 5))))
           (list (num 1) (num 2))))

(test (parse '{{fun {x y} {+ {* x y} x}} {3 4}})
      (app (fun '(x y) (op + (list (op * (list (id 'x) (id 'y))) (id 'x)))) (list (num 3) (num 4))))


(printf "___________________________________________________________________________________________________________________________\nInician pruebas de la función interp\n")
#| Pruebas de interp
de if0 |#
(test (prueba '{if0 {- 1 1} 5 6}) (numV 5))

(test (prueba '{if0 {+ 1 2} 5 6}) (numV 6)) 

(test/exn (prueba '{if0 {fun {x} {+ x 1}} 5 6}) "interp: Símbolo no esperado la condicional de if0, no es un número")

;Pruebas de función
(test/exn (prueba '{fun {x y x} {+ x {+ y z}}})
      "parser: parámetro definido dos veces: x")

(test (prueba '{fun {x y z} {+ x {+ y z}}})
      (closure '(x y z) (op + (list (id 'x) (op + (list (id 'y) (id 'z))))) (mtSub)))

(test/exn (prueba '{{fun {x y} {+ x y}} {10}})
      "parser: La cardinalidad de los argumentos difiere de la aridad de la función")

;Pruebas de aplicación de función
(test (prueba '{{fun {x y} {+ x y}} {10 3}}) (numV 13))

(test (prueba '{{fun {x y} {+ {* x y} x}} {3 4}}) (numV 15))

;Pruebas de with
(test (prueba '{with {{x 5} {y 1}} {+ x y}}) (numV 6))

(test/exn (prueba '{with {{x 5} {y {+ x 1}}} {+ x y}}) "lookup: Hay un identificador libre: x")

(test (prueba '{with {{f {fun {x} {+ x x}}}} {f {3}}}) (numV 6))

(test/exn (prueba '{with {{x 3} {f {fun {a} {+ x a}}}}
                      {f {0}}}) "lookup: Hay un identificador libre: x")

;Pruebas de with*
(test (prueba '{with* {{x 5}
                       {w {+ x 1}}
                       {z {with {{x 10}
                                 {f {fun {a} {+ x a}}}}
                                {f {10}}}}}
                      {+ x z}}) (numV 20))

(test (prueba '{with* {{x 3}}
                      {with* {{f {fun {y} {+ x y}}}}
                             {with* {{x 4}}
                                    {f {1}}}}}) (numV 4))

(test (prueba '{with* {{x 5} {y 1}} {+ x y}}) (numV 6))

(test (prueba '{with* {{x 5} {y {+ x 1}}} {+ x y}}) (numV 11))

(test (prueba '{with* {{x 1} {y 2} {z 3}} {fun {x y z} {+ x {+ y z}}}})
      (closure '(x y z) (op + (list (id 'x) (op + (list (id 'y) (id 'z))))) (aSub 'z (numV 3) (aSub 'y (numV 2) (aSub 'x (numV 1) (mtSub))))))

(test/exn (prueba '(with* ([y {+ x x}] [x 1]) y)) "lookup: variable libre: 'x")

(test (prueba '(with* ([x 2] [y {+ x x}] [x 1]) (+ 0 y))) (numV 4))

(test (prueba '(with* ([x 2] [y {+ x x}] [x 1]) (+ x y))) (numV 5))