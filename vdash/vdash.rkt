#lang racket
(require syntax/parse
         racket/stxparam
         "stx-props.rkt")
(require (for-syntax (only-in racket/list first filter-not append*)
                     (only-in racket/sequence in-syntax)
                     racket/base
                     racket/syntax
                     syntax/parse
                     syntax/id-table))



(define-syntax-parameter the-stx
  (lambda _
    (error "the-stx used outside of judgement")))

(begin-for-syntax
  (define delim-directions
    (make-free-id-table))

  (define (delim-direction id)
    (free-id-table-ref delim-directions
                       id
                       (lambda ()
                         (raise-syntax-error #f "relation key undefined" id))))

  (define-syntax-class delim-defs
    #:attributes (dir keys after)
    (pattern (#:in ~! (k:id ...) . a)
             #:attr dir #'in
             #:attr keys #'(k ...)
             #:attr after #'a)
    (pattern (#:out ~! (k:id ...) . a)
             #:attr dir #'out
             #:attr keys #'(k ...)
             #:attr after #'a))
  )


(provide define-relation-keys)
(define-syntax (define-relation-keys stx)
  (syntax-parse stx
    [(_) #'(void)]
    [(_ . dd:delim-defs)
     #:with (keys ...) #'dd.keys
     #:do [(for ([key (in-syntax #'(keys ...))])
             (when (free-id-table-ref! delim-directions key #f)
               (raise-syntax-error #f "relation key already defined" key))
             (free-id-table-set! delim-directions key
                                 (syntax-e #'dd.dir)))]
     #'(begin
         (define-syntax (keys stx-obj)
           (raise-syntax-error #f "invalid use of relation key as expression"
                               stx-obj))
         ...
         (define-relation-keys . dd.after))]))

(begin-for-syntax
  ;; #'((a b) (c d) (e f)) -> #'(a b c d e f)
  (define (stx-splice stxl
                      #:src [src stxl])
    (datum->syntax src
                   (append* (map syntax->list
                                 (syntax->list stxl)))))

  (define-syntax-class ----
    (pattern x:id
             #:when (regexp-match
                     #px"-{4,}"
                     (symbol->string (syntax-e #'x)))))

  (define-splicing-syntax-class stxparse-kw
    (pattern (~and kw (~or #:literals
                           #:datum-literals
                           #:literal-sets
                           #:description
                           #:conventions))))

  (define-syntax-class judgement-clause
    #:attributes (stxparse)
    (pattern [pat before-line ...
                  :---- ~!
                  . conc:conclusion]
             ; extract premise
             #:with ((~seq in-keys:id in-pats:expr) ... . prems:premises) #'(before-line ...)

             ; check key direction
             #:with bad-keys (filter-not (lambda (id) (eq? 'in (delim-direction id)))
                                         (syntax->list #'(in-keys ...)))
             #:fail-unless
             (null? (syntax-e #'bad-keys)) (format "relation key is not an input"
                                                   (first (syntax-e #'bad-keys)))

             ; output a syntax-parse clause
             #:with parse-body
             (stx-splice #'([#:when (has-prop-keys? the-stx tg:in '(in-keys ...))]
                            [#:with (in-pats ...) (get-prop-stx the-stx tg:in '(in-keys ...))]
                            prems.stxparse-directives ...
                            [conc.expr]))
             #:attr stxparse #'[pat . parse-body]))

  (define-syntax-class premises
    #:attributes ((stxparse-directives 1))
    ; pattern directive
    (pattern [(~and kw (~or #:with
                            #:fail-when
                            #:fail-unless))
              arg1 arg2
              . after:premises]
             #:with (stxparse-directives ...)
             #'([kw arg1 arg2] after.stxparse-directives ...))
    (pattern [(~and kw (~or #:and
                            #:do
                            #:when))
              arg1
              . after:premises]
             #:with (stxparse-directives ...)
             #'([kw arg1] after.stxparse-directives ...))

    ; normal premise
    (pattern [prem:premise . after:premises]
             #:with (stxparse-directives ...)
             #'(prem.stxparse-directive
                after.stxparse-directives ...))

    ; base case
    (pattern [] #:with (stxparse-directives ...) #'()))

  (define-syntax-class premise
    #:datum-literals (⊢)
    #:attributes (stxparse-directive)
    (pattern [⊢ targ-expr (~seq i/o-key:id i/o-expr:expr) ...]
             #:with ({in-key in-expr} ...)

             ; find input keys
             (filter (syntax-parser [{k v} (eq? 'in (delim-direction #'k))])
                     (syntax->list #'((i/o-key i/o-expr) ...)))

             ; find output keys
             #:with ({out-key out-pat} ...)
             (filter (syntax-parser [{k v} (eq? 'out (delim-direction #'k))])
                     (syntax->list #'((i/o-key i/o-expr) ...)))

             ; output expression & directive
             #:with expr #'(eval-relation #'targ-expr
                                          tg:in (list (cons 'in-key #'in-expr) ...)
                                          tg:out (list 'out-key ...)
                                          '())
             #:attr stxparse-directive
             #'[#:with (out-pat ...) expr]))

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
             (null? (syntax-e #'bad-keys)) (format "relation key is not an output"
                                                   (first (syntax-e #'bad-keys)))
             ; output a sentinel thing
             #:attr expr
             #'(make-prop-sentinel tg:out
                                   (list (cons 'out-key #'out-expr) ...)))

    (pattern ([≻ new-expr])
             #:attr expr
             #'(copy-prop-keys/stx tg:in #'new-expr the-stx))

    (pattern p
             #:post (~fail "not a valid conclusion")
             #:attr expr #'#f))

  )

(provide judgement-parse)
(define-syntax judgement-parse
  (syntax-parser
    [(_ stx-obj
        (~seq spkw:stxparse-kw ~! arg) ...
        . jcs)
     #:and ~!
     #:with (jc:judgement-clause ...) #'jcs
     #:with parse-body (stx-splice #'([spkw.kw arg] ...
                                      [jc.stxparse ...]))
     #'(let ([the-stx-obj stx-obj])
         (syntax-parameterize
             ([the-stx (make-rename-transformer #'the-stx-obj)])
           (syntax-parse the-stx-obj
             . parse-body)))]))

(provide judgement-parser)
(define-syntax (judgement-parser stx)
  (syntax-parse stx
    [(_ jc ...)
     (syntax/loc stx
       (lambda (stx-obj)
         (judgement-parse stx-obj jc ...)))]))



(define (eval-relation stx
                       tg-in in-keys/stx
                       tg-out out-keys
                       ctx)
  (let* ([stx- (set-prop-keys/stx stx tg-in in-keys/stx)]
         [stx/e (local-expand stx- 'expression '())])
    (unless (has-prop-keys? stx/e tg-out out-keys)
      (raise-syntax-error #f (format "incorrect judgement output keys; expected ~s, got ~s"
                                     out-keys
                                     (get-prop-keys stx/e tg-out))
                          stx/e))
    (get-prop-stx stx/e tg-out out-keys)))
