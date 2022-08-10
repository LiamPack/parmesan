;; the below code stack-overflows at ~150 "many"s in kawa, but somehow
;; chez tail-call optimizes things. very weird.

;; the bare initial pieces of this (return, fail, bind, etc.) referenced
;; from https://gist.github.com/UnkindPartition/3904294 -- 10 years ago!
;; Slick ideas using the "success" input function as the vehicle for
;; composition of parsers
(import (rnrs bytevectors))
(use-modules (srfi srfi-1)
             (srfi srfi-9))

(define (return v) (lambda (s ks kf) (ks v s)))
(define fail (lambda (s ks kf) (kf)))
(define empty/p (return '()))
(define peek1 (lambda (s ks kf)
                (if (null? s)
                    (kf)
                    (ks (car s) s))))

;; >>=
(define (bind a f)
  (lambda (s ks kf)
    (a s
       (lambda (av s1) ((f av) s1 ks kf))
       kf)))

;; >>|
(define (lift a f)
  (bind a (lambda (x) (return (f x)))))

;; <|>
(define (either/p a b)
  (lambda (s ks kf)
    (a s ks
       (lambda () (b s ks kf)))))

;; <*>
(define (and/p a b)
  (lambda (s ks kf)
    (a s
       (lambda (av s1)
         (b s1
            (lambda (bv s2) (ks (cons av bv) s2))
            kf))
       kf)))

(define (one-of/p . as)
  (fold-left either/p fail as))

(define (all-of/p . as)
  (fold-right and/p empty/p as))

(define (many/p p)
  (either/p
   (bind
    p (lambda (pv)
        (lift (many/p p) (lambda (pvs) (cons pv pvs)))))
   empty/p))

(define (psym pred)
  (lambda (s ks kf)
    (if (null? s)
        (kf)
        (if (pred (car s))
            (ks (car s) (cdr s))
            (kf)))))

(define (char/p a) (psym (lambda (c) (eq? a c))))

(define any-char/p
  (lambda (s ks kf)
    (if (null? s)
        (kf)
        (ks (car s) (cdr s)))))

(define (repeat n p)
  (define (helper n1)
    (either/p
     (bind
      p (lambda (pv)
          (if (<= n1 0)
              fail
              (lift (helper (- n1 1))
                    (lambda (pvs) (cons pv pvs))))))
     empty/p))
  (helper n))
(define (take n)
  (lift (repeat n any-char/p) (lambda (x) (apply string x))))
(define take1 (take 1))



(define uint8/p
  (bind take1 (lambda (x)
                (if (< (string-length x) 1)
                    fail
                    (return (bytevector-u8-ref (string->utf8 x) 0))))))
(define uint16/p
  (bind (take 2) (lambda (x)  
                   (if (< (string-length x) 2)
                       fail (return (bytevector-u16-ref (string->utf8 x) 0 (endianness big)))))))
(define uint32/p
  (bind (take 4) (lambda (x)  
                   (if (< (string-length x) 4)
                       fail (return (bytevector-u32-ref (string->utf8 x) 0 (endianness big)))))))
(define uint64/p
  (bind (take 8) (lambda (x)  
                   (if (< (string-length x) 8)
                       fail (return (bytevector-u64-ref (string->utf8 x) 0 (endianness big)))))))


(define int8/p
  (bind take1 (lambda (x)
                (if (< (string-length x) 1)
                    fail
                    (return (bytevector-s8-ref (string->utf8 x) 0))))))
(define int16/p
  (bind (take 2) (lambda (x)  
                   (if (< (string-length x) 2)
                       fail (return (bytevector-s16-ref (string->utf8 x) 0 (endianness big)))))))
(define int32/p
  (bind (take 4) (lambda (x)  
                   (if (< (string-length x) 4)
                       fail (return (bytevector-s32-ref (string->utf8 x) 0 (endianness big)))))))
(define int64/p
  (bind (take 8) (lambda (x)  
                   (if (< (string-length x) 8)
                       fail (return (bytevector-s64-ref (string->utf8 x) 0 (endianness big)))))))


(define float32/p
  (bind (take 4) (lambda (x)
                   (if (< (string-length x) 4)
                       fail
                       (return (bytevector-ieee-single-ref (string->utf8 x) 0 (endianness big)))))))

(define float64/p
  (lift (take 8) (lambda (x)
                   (if (< (string-length x) 8)
                       fail
                       (return (bytevector-ieee-double-ref (string->utf8 x) 0 (endianness big)))))))
