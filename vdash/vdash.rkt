#lang racket
(require syntax/parse
         racket/stxparam
         "stx-props.rkt")
(require (for-syntax (only-in racket/list first filter-not append*)
                     (only-in racket/sequence in-syntax)
                     racket/base
                     racket/syntax
                     syntax/parse
                     syntax/id-table
                     "stx-props.rkt")
         (for-meta 2 racket/base))



(define-syntax-parameter the-stx
  (lambda _
    (error "the-stx used outside of judgement")))

(begin-for-syntax
  ;; relation keys
  (define rel-key-dirs
    (make-free-id-table))

  (define (rel-key-dir id)
    (free-id-table-ref rel-key-dirs id
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
             (when (free-id-table-ref! rel-key-dirs key #f)
               (raise-syntax-error #f "relation key already defined" key))
             (free-id-table-set! rel-key-dirs key
                                 (syntax-e #'dd.dir)))]
     #'(begin
         (define-syntax (keys stx-obj)
           (raise-syntax-error #f "invalid use of relation key as expression"
                               stx-obj))
         ...
         (define-relation-keys . dd.after))]))


(begin-for-syntax
  ;; utilities ;;
  (define (stx-splice stxl
                      #:src [src stxl])
    (datum->syntax src
                   (append* (map syntax->list
                                 (syntax->list stxl)))))

  ;; syntax 'glyphs' ;;
  (define-syntax-class ----
    (pattern x:id
             #:when (regexp-match
                     #px"-{4,}"
                     (symbol->string (syntax-e #'x)))))

  (define-syntax ~⊢/p
    (pattern-expander
     (lambda (s) (syntax-case s ()
              [_ #'(~or (~datum ⊢)
                        #:if)]))))

  (define-syntax ~⊢/c
    (pattern-expander
     (lambda (s) (syntax-case s ()
              [_ #'(~or (~datum ⊢)
                        #:then)]))))


  ;; actual syntax ;;
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
             ; extract keys & premises
             #:with ((~seq in-keys:id in-pats:expr) ... ~! . prems:premises) #'(before-line ...)

             ; check key direction
             #:with bad-keys (filter-not (lambda (id) (eq? 'in (rel-key-dir id)))
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
    ; (2 argument kw)
    (pattern [(~and kw (~or #:with
                            #:fail-when
                            #:fail-unless))
              ~! arg1 arg2
              . after:premises]
             #:with (stxparse-directives ...)
             #'([kw arg1 arg2] after.stxparse-directives ...))
    ; (1 argument kw)
    (pattern [(~and kw (~or #:and
                            #:do
                            #:when))
              ~! arg1
              . after:premises]
             #:with (stxparse-directives ...)
             #'([kw arg1] after.stxparse-directives ...))

    ; normal premise
    (pattern [prem:premise . after:premises]
             #:with (stxparse-directives ...)
             #'([#:with prem.binding prem.expr]
                after.stxparse-directives ...))

    (pattern [prem:premise (~datum ...) . after:premises]
             #:with (stxparse-directives ...)
             #'([#:with prem.binding/ooo prem.expr/ooo]
                after.stxparse-directives ...))

    ; base case
    (pattern [] #:with (stxparse-directives ...) #'()))


  (define-syntax-class premise
    #:attributes (binding binding/ooo expr expr/ooo)
    (pattern [~! (~⊢/p) targ-expr (~seq i/o-key:id i/o-expr:expr) ...]
             #:with ooo #'(... ...)

             ; find input keys
             #:with ({in-key in-expr} ...) (filter (syntax-parser [{k v} (eq? 'in (rel-key-dir #'k))])
                                                   (syntax-e #'({i/o-key i/o-expr} ...)))
             #:with in-list-expr #'(list (cons 'in-key #'in-expr) ...)

             ; find output keys
             #:with ({out-key out-pat} ...) (filter (syntax-parser [{k v} (eq? 'out (rel-key-dir #'k))])
                                                    (syntax->list #'({i/o-key i/o-expr} ...)))
             #:with out-list-expr #'(list 'out-key ...)

             ; output expression & binding
             #:attr expr #'(do-rel/raw (list #'targ-expr)
                                       tg:in in-list-expr
                                       tg:out out-list-expr
                                       '())
             #:attr expr/ooo #'(do-rel/raw (syntax-e #'(targ-expr ooo))
                                           tg:in in-list-expr
                                           tg:out out-list-expr
                                           '())

             #:attr binding #'(_ [(out-pat ...)])
             #:attr binding/ooo #'(_ [(out-pat ...) ooo])
             ))

  (define-syntax-class conclusion
    #:attributes (expr)
    ; raise error as conclusion
    (pattern (#:error msg fmt ...)
             #:attr expr
             #'(raise-syntax-error #f (format msg fmt ...) the-stx))

    ; tagged conclusion
    (pattern ((~⊢/c) (~seq out-key:id out-expr:expr) ...)
             ; check key direction
             #:with bad-keys (filter-not (lambda (id) (eq? 'out (rel-key-dir id)))
                                         (syntax->list #'(out-key ...)))
             #:fail-unless
             (null? (syntax-e #'bad-keys)) (format "relation key is not an output"
                                                   (first (syntax-e #'bad-keys)))
             ; output a sentinel thing
             #:attr expr
             #'(make-prop-sentinel tg:out
                                   (list (cons 'out-key #'out-expr) ...)))

    ; redirect conclusion
    (pattern (new-expr)
             #:attr expr
             #'(copy-prop-keys/stx tg:in #'new-expr the-stx))

    ; (invalid)
    (pattern p
             #:post (~fail "not a valid conclusion")
             #:attr expr #'#f))
  )

(define-syntax judgement-parse/fallback
  (syntax-parser
    [(_ fallback-parser
        stx-obj
        (~seq spkw:stxparse-kw ~! arg) ...
        ~! . jcs)
     #:with (jc:judgement-clause ...) #'jcs
     #:with (parse-body ...) (stx-splice #'([spkw.kw arg] ...
                                            [jc.stxparse ...]))
     #'(let ([the-stx-obj stx-obj])
         (syntax-parameterize ([the-stx (make-rename-transformer #'the-stx-obj)])
           (syntax-parse the-stx-obj
             parse-body ...
             [_ (fallback-parser the-stx-obj)])))]))

(define fallback-parse
  (box (lambda (s)
         (raise-syntax-error #f "no matching clauses for expression"
                             s))))

(provide judgement-parse)
(define-syntax judgement-parse
  (syntax-rules ()
    [(_ stx-obj kw/clause ...)
     (judgement-parse/fallback (unbox fallback-parse)
                               stx-obj
                               kw/clause ...)]))

(provide judgement-parser)
(define-syntax judgement-parser
  (syntax-rules ()
    [(_ kw/clause ...)
     (lambda (stx-obj)
       (judgement-parse stx-obj kw/clause ...))]))

(provide define-fallback-judgements)
(define-syntax define-fallback-judgements
  (syntax-rules ()
    [(_ kw/clause ...)
     (let* ([old-fallback (unbox fallback-parse)]
            [new-fallback (lambda (stx-obj)
                            (judgement-parse/fallback old-fallback stx-obj
                                                      kw/clause ...))])
       (set-box! fallback-parse
                 new-fallback))]))

; (do-rel/raw ...)
;   exprs : (listof syntax?)
;   tg-in : symbol?
;   in-keys/stx : (assoc/c symbol? syntax?)
;   tg-out : symbol?
;   out-keys : (listof symbol?)
;   ctx : any/c
; ->
;   (list/c any/c                      [context bindings]
;           (listof (listof syntax?))  [outputs]
;           )
(define (do-rel/raw exprs
                    tg-in in-keys/stx
                    tg-out out-keys
                    ctx)

  (let ([exprs/expanded (map (lambda (e)
                               (let ([e+tags (set-prop-keys/stx e tg-in in-keys/stx)])
                                 (local-expand e+tags 'expression '())))
                             exprs)])
    (for ([e/e (in-list exprs/expanded)])
      (unless (has-prop-keys? e/e
                              tg-out out-keys)
        (raise-syntax-error #f (format "incorrect judgement output keys; expected ~s, got ~s"
                                       out-keys
                                       (get-prop-keys e/e tg-out))
                            e/e)))

    (let ([outputs
           (map (lambda (e/e) (get-prop-stx e/e tg-out out-keys))
                exprs/expanded)])
      (list #f outputs))))
