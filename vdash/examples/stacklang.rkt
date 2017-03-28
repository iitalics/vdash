#lang racket
(require (for-syntax "../vdash.rkt"
                     syntax/parse))

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
   [(_ x y) <compile> ()
    [#:if x <compile> () to (x-ins ...)]
    [#:if y <compile> () to (y-ins ...)]
    ----------
    #:then to (x-ins ... y-ins ... ADD)]))

(define-syntax mult
  (judgement-parser
   [(_ x y) <compile> ()
    [#:if x <compile> () to (x-ins ...)]
    [#:if y <compile> () to (y-ins ...)]
    ----------
    #:then to (x-ins ... y-ins ... MUL)]))

(define-syntax d
  (judgement-parser
   [(_ . n:integer) <compile> ()
    ----------
    #:then to (n)]
   [(_ . d)
    ----------
    #:error (format "unsupported datum ~s" (syntax-e #'d))]))

(define-syntax mod
  (syntax-parser
    [(_ . es)
     #'(#%module-begin (void))]))

(define-syntax top
  (judgement-parser
   [(_ . e)
    [#:if e <compile> () to e-prgm]
    ------------
    (displayln
     (string-join (map ~a 'e-prgm)))]))
