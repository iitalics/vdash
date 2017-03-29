#lang racket
(provide (all-defined-out))

(define (plist->alist pl)
  (match pl
    [(list* k v pl-) (cons (cons k v)
                           (plist->alist pl-))]
    [_ '()]))


(define tg:in 'tg-in)
(define tg:out 'tg-out)
(define tg:sent 'tg-sent)

(define (has-prop-keys? stx tg keys)
  (let ([h (syntax-property stx tg)])
    (cond
      [(false? h) (empty? keys)]
      [(empty? keys) (empty? (hash-keys h))]
      [else
       (empty? (set-symmetric-difference (hash-keys h) keys))])))

(define (get-prop-keys stx tg)
  (let ([h (syntax-property stx tg)])
    (if h (hash-keys h) '())))

(define (get-prop-stx stx tg keys)
  (let ([h (syntax-property stx tg)])
    (map (curry hash-ref h) keys)))

(define (set-prop-keys/stx stx tg k/s)
  #;(when (syntax-property stx tg)
    (printf "warning: overwriting keys ~a to ~a for tag: ~a\n"
            (hash-keys (syntax-property stx tg))
            (map car k/s)
            tg))
  (syntax-property stx tg (make-hash k/s)))

(define (set-prop-keys/stx* stx tg . r)
  (set-prop-keys/stx stx tg (plist->alist r)))

(define (copy-prop-keys/stx tg dst-stx src-stx)
  (syntax-property dst-stx tg
                   (syntax-property src-stx tg)))

(require (for-template racket/base))

(define (sentinel)
  (error "Sentinel should not be evaluated; should only be expanded by a premise"))

(define (make-prop-sentinel tg k/s)
  (set-prop-keys/stx (syntax-property #`(#%app #,sentinel)
                                      tg:sent #t)
                     tg k/s))

(define (make-prop-sentinel* tg . r)
  (make-prop-sentinel tg (plist->alist r)))
