#lang racket
(require syntax/parse)
(require (for-syntax racket/base
                     syntax/parse))

(begin-for-syntax
  (define-syntax-class ----
    (pattern x:id
             #:when (regexp-match
                     #px"-{4,}"
                     (symbol->string (syntax-e #'x)))))

  (define-syntax-class judgement-clause
    #:attributes (stxparse)
    (pattern [patn pre-premise ...
                   :----
                   conclusion ...]
             #:attr stxparse #'[_ #:when #f #'()]))
  )

(define-syntax judgement-parse
  (syntax-parser
    [(_ stx-obj
        jc:judgement-clause ...)
     #'(syntax-parse stx-obj
         jc.stxparse ...)]))

(define-syntax (judgement-parser stx)
  (syntax-parse stx
    [(_ jc ...)
     (syntax/loc stx
       (lambda (stx-obj)
         (judgement-parse stx-obj jc ...)))]))
