#lang racket/base

(module+ test
  (require racket
           rackunit
           (for-syntax "vdash.rkt"
                       syntax/parse))

  ;; PEANO ARITHMETIC

  (begin-for-syntax
    (define-relation-keys
      #:in (P+ P*)
      #:out (=>))
    (define-literal-set peano
      #:datum-literals (TRUE FALSE zer suc)
      ())
    )

  (define-syntax zer
    (judgement-parser
     #:literal-sets (peano)
     ;;;;;;;;;;;
     [(zer) P+ n
      ---------
      [⊢ => n]]
     ;;;;;;;;;;;
     [(zer) P* n
      ---------
      [⊢ => (zer)]]
     ))

  (define-syntax suc
    (judgement-parser
     #:literal-sets (peano)
     ;;;;;;;;;;;;
     [(suc n) P+ m
      [⊢ n P+ m => s]
      -----------
      [⊢ => (suc s)]]
     ;;;;;;;;;;;;
     [(suc n) P* m
      [⊢ n P* m => p]
      [⊢ m P+ p => s]
      ------------
      [⊢ => s]]))


  (define-syntax p+
    (judgement-parser
     #:literal-sets (peano)
     [(_ x y)
      [⊢ x P+ y => z]
      ------------
      [≻ 'z]]))

  (define-syntax p*
    (judgement-parser
     #:literal-sets (peano)
     [(_ x y)
      [⊢ x P* y => z]
      ------------
      [≻ 'z]]))

  (check-equal? (p+ (zer) (zer))               '(zer))
  (check-equal? (p+ (zer) (suc (zer)))         '(suc (zer)))
  (check-equal? (p+ (suc (zer)) (suc (zer)))   '(suc (suc (zer))))
  (check-equal? (p+ (suc (suc (zer))) (zer))   '(suc (suc (zer))))

  (check-equal? (p* (zer) (zer))               '(zer))
  (check-equal? (p* (zer) (suc (zer)))         '(zer))
  (check-equal? (p* (suc (zer)) (zer))         '(zer))
  (check-equal? (p* (suc (suc (zer))) (suc (zer)))       '(suc (suc (zer))))
  (check-equal? (p* (suc (suc (zer))) (suc (suc (zer)))) '(suc (suc (suc (suc (zer))))))

  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
