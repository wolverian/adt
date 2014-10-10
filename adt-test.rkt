#lang typed/racket

(require typed/rackunit
         "adt.rkt")

(deftype Expr
  (num [n : Number])
  (plus [l : Expr] [r : Expr]))

(define input (plus (num 2) (num 3)))

(check-equal? (match/type Expr input [(num n) n] [(plus l r) 42]) 42)
(check-equal? (match/type Expr (plus (num 2) (num 3)) [x x]) input)
(check-exn exn:fail?
           (lambda () (expand-once '(match/type Expr input [(plus _ (num n)) n]))))
