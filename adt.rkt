#lang typed/racket

(require (for-syntax (only-in racket first)))

(provide deftype match/type)

(define-for-syntax parts (make-hash))

(define-syntax (match/type stx)
  (syntax-case stx ()
    [(_ type-name expr (pattern body) ...)
     (begin
       (let* ([patterns (syntax->datum #'(pattern ...))]
              [has-wildcard (ormap symbol? patterns)]
              [matched-conses (map first (filter list? patterns))]
              [defined-conses (hash-ref parts (syntax->datum #'type-name) '())])
         (unless (or has-wildcard
                     (equal? matched-conses defined-conses))
           (error 'match/type "non-exhaustive match")))
       #'(match expr (pattern body) ...))]))

(define-syntax (deftype stx)
  (syntax-case stx ()
    [(_ type-name (cons-name fields ...) ...)
     (begin
       (hash-set! parts (syntax->datum #'type-name) (syntax->datum #'(cons-name ...)))
       #'(begin
           (define-type type-name (U cons-name ...))
           (struct cons-name (fields ...) #:transparent) ...))]))
