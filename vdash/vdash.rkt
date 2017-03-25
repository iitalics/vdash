#lang racket
(require syntax/parse
         racket/stxparam
         "stx-props.rkt")
(require (for-syntax racket/base
                     syntax/parse))



(define-syntax-parameter the-stx
  (lambda _
    (error "the-stx used outside of judgement")))

(begin-for-syntax
  (define-syntax-class ----
    (pattern x:id
             #:when (regexp-match
                     #px"-{4,}"
                     (symbol->string (syntax-e #'x)))))

  (define-syntax-class judgement-clause
    #:attributes (stxparse)
    (pattern [pat pre-premise ...
                  :----
                  conclusion ...]
             #:with ((~seq in-keys:id in-pats:expr) ... premises ...) #'(pre-premise ...)
             #:attr stxparse
             #'[pat
                #:when (has-in-keys? '(in-keys ...) the-stx)
                #:with (in-pats ...) (get-in-stx '(in-keys ...) the-stx)
                premises ...
                conclusion ...]))
  )

(define-syntax judgement-parse
  (syntax-parser
    [(_ stx-obj
        jc:judgement-clause ...)
     #'(let ([the-stx-obj stx-obj])
         (syntax-parameterize
             ([the-stx (make-rename-transformer #'the-stx-obj)])
           (syntax-parse the-stx-obj
             jc.stxparse ...)))]))

(define-syntax (judgement-parser stx)
  (syntax-parse stx
    [(_ jc ...)
     (syntax/loc stx
       (lambda (stx-obj)
         (judgement-parse stx-obj jc ...)))]))
