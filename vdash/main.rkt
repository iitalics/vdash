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
     ;;;;;;;;;;;
     [(zer) toInt ()
      -----------
      [⊢ => 0]]
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
      [⊢ => s]]
     ;;;;;;;;;;;;
     [(suc n) toInt ()
      [⊢ n toInt() => k]
      #:with r (add1 (syntax-e #'k))
      -------------
      [⊢ => r]]
     ))

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

  (define-syntax nat->int
    (judgement-parser
     [(_ n)
      [⊢ n toInt () => k]
      ------------
      [≻ 'k]]))

  (check-equal? (p+ (zer) (zer))               '(zer))
  (check-equal? (p+ (zer) (suc (zer)))         '(suc (zer)))
  (check-equal? (p+ (suc (zer)) (suc (zer)))   '(suc (suc (zer))))
  (check-equal? (p+ (suc (suc (zer))) (zer))   '(suc (suc (zer))))

  (check-equal? (p* (zer) (zer))               '(zer))
  (check-equal? (p* (zer) (suc (zer)))         '(zer))
  (check-equal? (p* (suc (zer)) (zer))         '(zer))
  (check-equal? (p* (suc (suc (zer))) (suc (zer)))       '(suc (suc (zer))))
  (check-equal? (p* (suc (suc (zer))) (suc (suc (zer)))) '(suc (suc (suc (suc (zer))))))

  (check-equal? (nat->int (zer)) 0)
  (check-equal? (nat->int (suc (zer))) 1)
  (check-equal? (nat->int (suc (suc (zer)))) 2)

  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
