#lang plai
(define div
  (lambda (n m)
    (if (= n 0)
        1
        (+ 1 (div (- n m) m)))))

(div 4 2)
(div 8 2)
(div 9 3)

(define division-tail
  (local ((define sos
            (lambda (n m acc)
              (if (= n 0)
                  acc
                  (sos (- n m) m (+ 1 acc))))))
    (lambda (n m)
    (sos n m 1))))

(define division
  division-tail)

(division 4 2)
(division 8 2)
(division 9 3)