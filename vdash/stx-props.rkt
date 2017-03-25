#lang racket
(provide (all-defined-out))

(define (plist->alist pl)
  (match pl
    [(list* k v pl-) (cons (cons k v)
                           (plist->alist pl-))]
    [_ '()]))


(define tg:in 'tg-in)
(define tg:out 'tg-out)


(define (has-in-keys? keys stx)
  (let ([h (syntax-property stx tg:in)])
    (and h
         (empty? (set-symmetric-difference (hash-keys h)
                                           keys)))))

(define (get-in-stx keys stx)
  (let ([h (syntax-property stx tg:in)])
    (map (curry hash-ref h) keys)))

(define (set-in-keys/stx stx k/s)
  (when (syntax-property stx tg:in)
    (printf "warning: overwriting keys: ~a"
            (hash-keys (syntax-property stx tg:in))))
  (syntax-property stx
                   tg:in (make-hash k/s)))

(define (set-in-keys/stx* stx . r)
  (set-in-keys/stx stx (plist->alist r)))
