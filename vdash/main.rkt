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
  ;; Tests to be run with raco test
  (require racket
           (for-syntax "vdash.rkt"
                       syntax/parse))

  (begin-for-syntax
    (define-relation-keys
      #:in (P=)
      #:out (=>))

    (define-literal-set peano
      #:datum-literals (TRUE FALSE zer suc)
      ())
    )


  (define-syntax zer
    (judgement-parser
     #:literal-sets (peano)
     ; 0 = 0
     [(zer) P= (zer)
      --------
      [⊢ => TRUE]]
     ; 0 != anything else
     [(zer) P= _
      ---------
      [⊢ => FALSE]]))

  (define-syntax suc
    (judgement-parser
     #:literal-sets (peano)
     ;;;;;;;;;;
     [(suc n) P= (suc m)
      [⊢ n P= m => TRUE]
      --------
      [⊢ => TRUE]]
     ;;
     [(suc n) P= _
      ---------
      [⊢ => FALSE]]))



  (define-syntax p=?
    (judgement-parser
     #:literal-sets (peano)
     [(_ x y)
      [⊢ x P= y => TRUE]
      ------------
      [≻ #t]]
     [(_ x y)
      [⊢ x P= y => FALSE]
      ------------
      [≻ #f]]))

  (displayln (p=? (zer) (zer)))
  (displayln (p=? (zer) (suc (zer))))
  (displayln (p=? (suc (zer)) (suc (zer))))

  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
