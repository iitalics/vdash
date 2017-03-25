#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(module+ test
  (require racket
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
