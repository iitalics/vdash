#lang racket
(require syntax/parse
         (for-syntax racket
                     "../vdash.rkt"
                     syntax/parse))

(begin-for-syntax
  (define (type=? a b)
    (syntax-parse (list a b)
      [(x:id y:id) (free-identifier=? #'x #'y)]
      [((a ...) (b ...))
       #:when (equal? (length (syntax-e #'(a ...)))
                      (length (syntax-e #'(b ...))))
       (andmap type=?
               (syntax-e #'(a ...))
               (syntax-e #'(b ...)))]
      [(_ _) #f]))

  (require (for-syntax syntax/parse))
  (define-syntax ~→
    (pattern-expander
     (syntax-parser
       [(_ a ...)
        #'((~datum →) a ...)])))

  (define-relation-keys
    #:out (≫ :)))


(provide (rename-out [datum #%datum]))
(define-syntax datum
  (judgement-parser
   [(_ . d:integer)
    ------------
    [⊢ ≫ (#%datum . d) : Int]]
   [(_ . d:str)
    ------------
    [⊢ ≫ (#%datum . d) : Str]]))


(provide (rename-out [app #%app]))
(define-syntax app
  (judgement-parser
   [(_ f e)
    [⊢ f ≫ f- : (~→ τ σ)]
    [⊢ e ≫ e- : τe]
    ------------
    [⊢ ≫ (#%app f- e-) : σ]]))


(provide (rename-out [plus +]))
(define-syntax plus
  (make-set!-transformer
   (lambda (stx)
     (syntax-parse stx
       [(x ...)
        #:with ap (datum->syntax stx '#%app)
        (syntax/loc stx
          (ap x ...))]
       [x:id
        (judgement-parse
         #'x [_
              -------
              [⊢ ≫ +/c : (→ Int (→ Int Int))]])]))))

(define +/c
  (lambda (x) (lambda (y) (+ x y))))


(provide (rename-out [tc/print #%top-interaction]))
(define-syntax tc/print
  (judgement-parser
   [(_ . e)
    [⊢ e ≫ e- : τ]
    ------------
    [≻ (printf "~a : ~a\n"
               e-
               'τ)]]))

(provide (rename-out [tc/print-all #%module-begin]))
(define-syntax tc/print-all
  (syntax-parser
    [(_ e ...)
     #'(#%module-begin
        (tc/print . e) ...)]))
