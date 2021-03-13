#lang racket/gui

; inmultim elementele unei liste cu 2

(define (stiva L)
  (if (null? L) 
      '()
      (cons (* 2 (car L)) (stiva (cdr L)))))

(stiva '(1 2 3))

; len -> complexitate temporala O(n)
; null? -> complexitate temporala O(1)

(define (coada L acc)
  (if (null? L)
      (reverse acc)
      (coada (cdr L) (cons (* (car L) 2) acc))))

; append -> complexitate temporala O(lungimea listei 1)
; cons -> complexitate temporala O(1)

(coada '(1 2 3) '())

; recursivitate arborescenta
(define (fib b)
  (cond ((= b 0) 0)
        ((= b 1) 1)
        (else (+ (fib (- b 1)) (fib (- b 2))))))

(fib 5)

