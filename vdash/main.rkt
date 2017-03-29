#lang racket/base

(module+ test
  (require racket
           rackunit
           (for-syntax "vdash.rkt"
                       syntax/parse))

  ;; PEANO ARITHMETIC

  (begin-for-syntax
    (define-relation-keys
      #:in (P+ P* toInt)
      #:out (=>))
    )


  (define-syntax zer
    (judgement-parser
     [(zer) P+ n
      ---------
      #:then => n]

     [(zer) P* n
      ---------
      #:then => (zer)]

     [(zer) toInt ()
      -----------
      #:then => 0]
     ))


  (define-syntax suc
    (judgement-parser
     [(suc n) P+ m
      [#:if n P+ m => s]
      -----------
      #:then => (suc s)]

     [(suc n) P* m
      [#:if n P* m => p]
      [#:if m P+ p => s]
      ------------
      #:then => s]

     [(suc n) toInt ()
      [#:if n toInt() => k]
      #:with r (add1 (syntax-e #'k))
      -------------
      #:then => r]
     ))

  (define-syntax p+
    (judgement-parser
     [(_ x y) [#:if x P+ y => z]
      ------------
      z]))

  (define-syntax p*
    (judgement-parser
     [(_ x y) [#:if x P* y => z]
      ------------
      z]))

  (begin-for-syntax
    (define-fallback-judgements
      #:datum-literals (suc zer)
      [n [#:if n toInt () => k]
       --------
       'k]))

  (check-equal? (zer)             0)
  (check-equal? (suc (zer))       1)
  (check-equal? (suc (suc (zer))) 2)

  (check-equal? (p+ (zer) (zer))               0)
  (check-equal? (p+ (zer) (suc (zer)))         1)
  (check-equal? (p+ (suc (zer)) (suc (zer)))   2)
  (check-equal? (p+ (suc (suc (zer))) (zer))   2)

  (check-equal? (p* (zer) (zer))                         0)
  (check-equal? (p* (zer) (suc (zer)))                   0)
  (check-equal? (p* (suc (zer)) (zer))                   0)
  (check-equal? (p* (suc (suc (zer))) (suc (zer)))       2)
  (check-equal? (p* (suc (suc (zer))) (suc (suc (zer)))) 4)
  (check-equal? (p* (suc (suc (suc (zer)))) (suc (suc (zer)))) 6)
  (check-equal? (p* (suc (suc (zer))) (suc (suc (suc (zer))))) 6)

  (begin-for-syntax
    (define-relation-keys
      #:in (FOO)
      #:out (BAR)))

  (define-syntax woa
    (judgement-parser
     [(_) FOO x
      --------
      #:then BAR x]))

  ;; TODO: fix infinite-loop bug when you try to expand (woa)
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
