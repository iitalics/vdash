#lang racket
(require (for-syntax "../vdash.rkt"
                     syntax/parse
                     racket/list))

(provide (rename-out [top #%top-interaction]
                     [mod #%module-begin]
                     [plus +]
                     [mult *]
                     [d #%datum]))

(begin-for-syntax
  (define-relation-keys
    #:in (<compile>)
    #:out (to ::)))

(define-syntax plus
  (judgement-parser
   [(_ a b ...) <compile> ()
    [#:if a <compile> () to (a-ins ...)]
    [#:if b <compile> () to (b-ins ...)] ...
    #:with (fold ...) (make-list (length (syntax-e #'(b ...))) #'ADD)
    ----------
    #:then to (a-ins ... b-ins ... ... fold ...)]))

(define-syntax mult
  (judgement-parser
   [(_ a b ...) <compile> ()
    [#:if a <compile> () to (a-ins ...)]
    [#:if b <compile> () to (b-ins ...)] ...
    #:with (fold ...) (make-list (length (syntax-e #'(b ...))) #'MUL)
    ----------
    #:then to (a-ins ... b-ins ... ... fold ...)]))

(define-syntax d
  (judgement-parser
   [(_ . n:integer) <compile> ()
    ----------
    #:then to (n)]
   [(_ . d)
    ----------
    #:error (format "unsupported datum ~s" (syntax-e #'d))]))

(define-syntax mod
  (judgement-parser
   [(_ es ...)
    [#:if es <compile> () to e-prgm] ...
    -----------
    (#%module-begin
     (displayln (string-join (map ~a 'e-prgm)))
     ...)]))

(define-syntax top
  (judgement-parser
   [(_ . e)
    [#:if e <compile> () to e-prgm]
    ------------
    (displayln
     (string-join (map ~a 'e-prgm)))]))
