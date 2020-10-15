#lang plai

(define-type WAE
  [id (i symbol?)]
  [num (n number?)]
  [add (izq WAE?) (der WAE?)]
  [sub (izq WAE?) (der WAE?)]
  [with (id symbol?) (value WAE?) (body WAE?)])

;; De-ligado
(define (de-ligado expr)
  (type-case WAE expr
    [id (i) empty]
    [num (n) empty]
    [add (izq der) (append (de-ligado izq) (de-ligado der))]
    [sub (izq der) (append (de-ligado izq) (de-ligado der))]
    [with (id value body)
          (append (list id) (de-ligado value) (de-ligado body))]))

;;(de-ligado (with 'a (num 2) (add (id 'a) (id 'a))))
;;(de-ligado (with 'w (sub (id 'u) (num 8)) (with 'v (num 5) (add (id 'w) (add (id 'y) (id 'x))))))

;; Ligadas
(define (ligadas expr)
  (type-case WAE expr
    [id (i) '()]
    [num (n) '()]
    [add (lhs rhs) (append (ligadas lhs) (ligadas rhs))]
    [sub (lhs rhs) (append (ligadas lhs) (ligadas rhs))]
    [with (id value body)
          (append (ligadas-aux value (list id)) (ligadas-aux body (list id)))]))

;; Función contiene.
(define (contiene? e l)
  (match l
    ['() #f]
    [(cons x xs) (or (equal? e x) (contiene? e xs))]))

;; Función ligadas-aux.
(define (ligadas-aux expr lst)
  (type-case WAE expr
    [id (i) (if (contiene? i lst)
                (list i)
                '())]
    [num (n) '()]
    [add (lhs rhs) (append (ligadas-aux lhs lst) (ligadas-aux rhs lst))]
    [sub (lhs rhs) (append (ligadas-aux lhs lst) (ligadas-aux rhs lst))]
    [with (id value body)
          (append (ligadas-aux value (append lst (list id)))
                  (ligadas-aux body (append lst (list id))))]))

;;(ligadas (with 'a (num 2) (add (id 'a) (id 'a))))
;;(ligadas (with 'b (id 'a) (id 'a)))
;;(ligadas (add (sub (id 'a) (num 1835)) (id 'b)))
;;(ligadas (sub (id 'b) (with 'b (num 8) (add (id 'b) (id 'c)))s))
;;(ligadas (with 'a (num 2) (with 'a (num 3) (add (id 'a) (id 'a)))))
;;(ligadas (with 'w (sub (id 'u) (num 8)) (with 'v (num 5) (add (id 'w) (add (id 'y) (id 'x))))))

;; Función libres
(define (libres expr)
  (type-case WAE expr
    [id (i) (list i)]
    [num (n) '()]
    [add (lhs rhs) (append (libres lhs) (libres rhs))]
    [sub (lhs rhs) (append (libres lhs) (libres rhs))]
    [with (id value body)
          (append (libres-aux value (list id)) (libres-aux body (list id)))]))

;; Función libres-aux.
(define (libres-aux expr lst)
  (type-case WAE expr
    [id (i) (if (not(contiene? i lst))
                (list i)
                '())]
    [num (n) '()]
    [add (lhs rhs) (append (libres-aux lhs lst) (libres-aux rhs lst))]
    [sub (lhs rhs) (append (libres-aux lhs lst) (libres-aux rhs lst))]
    [with (id value body)
          (append (libres-aux value (append lst (list id)))
                  (libres-aux body (append lst (list id))))]))

;;(libres (with 'w (sub (id 'u) (num 8)) (with 'v (num 5) (add (id 'w) (add (id 'y) (id 'x))))))
;;(libres (id 'n))

;; Test general.
;;(de-ligado (with 'a (num 2) (add (id 'a) (id 'b))))
;;(ligadas (with 'a (num 2) (add (id 'a) (id 'b))))
;;(libres (with 'a (num 2) (add (id 'a) (id 'b))))

;(de-ligado (id 'foo))
;(ligadas (id 'foo))
;(libres (id 'foo))

;(de-ligado  (with 'a (num 2) (add (id 'a) (id 'a))))
;(ligadas  (with 'a (num 2) (add (id 'a) (id 'a))))
;(libres  (with 'a (num 2) (add (id 'a) (id 'a))))

;(de-ligado (with 'a (num 3)(with 'b (num 4) (add (id 'a) (id 'b)))))
;(ligadas (with 'a (num 3)(with 'b (num 4) (add (id 'a) (id 'b)))))
;(libres (with 'a (num 3)(with 'b (num 4) (add (id 'a) (id 'b)))))

;;(de-ligado (num 1729))
;;(ligadas (num 1729))
;;(libres (num 1729))

;(de-ligado (add (id 'a) (num 1729)))
;(ligadas (add (id 'a) (num 1729)))
;(libres (add (id 'a) (num 1729)))

;(de-ligado (add (sub (id 'a) (num 1835)) (id 'b)))
;(ligadas (add (sub (id 'a) (num 1835)) (id 'b)))
;(libres (add (sub (id 'a) (num 1835)) (id 'b)))

;(de-ligado (with 'a (id 'a) (id 'a)))
;(ligadas (with 'a (id 'a) (id 'a)))
;(libres (with 'a (id 'a) (id 'a)))

;;(de-ligado (with 'b (id 'a) (id 'a)))
;;(ligadas (with 'b (id 'a) (id 'a)))
;;(libres (with 'b (id 'a) (id 'a)))
