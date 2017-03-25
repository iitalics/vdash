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
      #:in (ADD)
      #:out (RESULT)))

  (define-syntax N
    (judgement-parser
     [(_ x:integer) ADD y
      ------------
      [⊢ RESULT (+ x y)]]))

  (define-syntax Succ
    (judgement-parser
     [(_ e:expr)
      [⊢ e ADD 1 RESULT x]
      ------------
      [≻ (printf "result = ~a\n" x)]]))

  (Succ 2)
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
