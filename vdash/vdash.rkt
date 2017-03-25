#lang racket
(require syntax/parse
         racket/stxparam
         "stx-props.rkt")
(require (for-syntax racket/base
                     (only-in racket/list first filter-not)
                     syntax/parse
                     syntax/id-table))



(define-syntax-parameter the-stx
  (lambda _
    (error "the-stx used outside of judgement")))

(begin-for-syntax
  (define-syntax-class ----
    (pattern x:id
             #:when (regexp-match
                     #px"-{4,}"
                     (symbol->string (syntax-e #'x)))))

  (define delim-directions
    (make-free-id-table))

  (define (delim-direction id)
    (free-id-table-ref delim-directions
                       id
                       (lambda ()
                         (raise-syntax-error #f (format "relation key undefined: ~a"
                                                        (syntax-e id))
                                             id))))

  (define-syntax-class judgement-clause
    #:attributes (stxparse)
    (pattern [pat pre-premise ...
                  :---- ~!
                  conc:conclusion ...]
             ; extract premise
             #:with ((~seq in-keys:id in-pats:expr) ... prem:premise ...) #'(pre-premise ...)

             ; check key direction
             #:with bad-keys (filter-not (lambda (id) (eq? 'in (delim-direction id)))
                                         (syntax->list #'(in-keys ...)))
             #:fail-unless
             (null? (syntax-e #'bad-keys)) (format "relation key is not an output: ~a"
                                                   (first (syntax-e #'bad-keys)))

             ; output a syntax-parse clause
             #:attr stxparse
             #'[pat
                #:when (has-prop-keys? the-stx tg:in '(in-keys ...))
                #:with (in-pats ...) (get-prop-stx the-stx tg:in '(in-keys ...))
                #:with (prem.pat ...) (list prem.expr ...)
                conc.expr ...]))

  (define-syntax-class premise
    #:datum-literals (⊢)
    #:attributes (pat expr)
    (pattern [⊢ targ-expr (~seq i/o-key:id i/o-expr:expr) ...]
             #:with ({in-key in-expr} ...)

             ; find input keys
             (filter (syntax-parser [{k v} (eq? 'in (delim-direction #'k))])
                     (syntax->list #'((i/o-key i/o-expr) ...)))

             ; find output keys
             #:with ({out-key out-pat} ...)
             (filter (syntax-parser [{k v} (eq? 'out (delim-direction #'k))])
                     (syntax->list #'((i/o-key i/o-expr) ...)))

             ; output expression & binding pattern
             #:attr pat #'(out-pat ...)
             #:attr expr #'(relation #'targ-expr
                                     tg:in (list (cons 'in-key #'in-expr) ...)
                                     tg:out (list 'out-key ...))))

  (define-syntax-class conclusion
    #:datum-literals (⊢ ≻)
    #:attributes (expr)
    (pattern (#:error msg fmt ...)
             #:attr expr
             #'(raise-syntax-error #f (format msg fmt ...) the-stx))
    (pattern ([⊢ (~seq out-key:id out-expr:expr) ...])
             ; check key direction
             #:with bad-keys (filter-not (lambda (id) (eq? 'out (delim-direction id)))
                                         (syntax->list #'(out-key ...)))
             #:fail-unless
             (null? (syntax-e #'bad-keys)) (format "relation key is not an output: ~a"
                                                   (first (syntax-e #'bad-keys)))
             ; output a sentinel thing
             #:attr expr
             #'(make-prop-sentinel tg:out
                                   (list (cons 'out-key #'out-expr) ...)))

    (pattern ([≻ new-expr])
             #:attr expr
             #'(copy-prop-keys/stx tg:in #'new-expr the-stx)))

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
