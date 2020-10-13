#lang scheme
(define atom?
 (lambda (x)
    (and (not (pair? x)) (not (null? x)))))  

(define add1
  (lambda (n) (+ n 1)))

(define sub1
  (lambda (n) (- n 1)))

; The o+ function adds two numbers
;
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

; The o- function subtracts one number from the other
;
(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

; The o* function multiplies two numbers
;
(define o*
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (o* n (sub1 m)))))))

; tup add operation
;
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2))
                      (tup+ (cdr tup1) (cdr tup2)))))))

; (tup+ '(3 7) '(4 6 8 1))

; The o/ function computes the integer part of n/m
;
(define o/
  (lambda (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (o/ (o- n m) m))))))

; The o> function compares n with m and returns true if n>m
;
(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
        (o> (sub1 n) (sub1 m))))))

; The o< function compares n with m and returns true if n<m
;
(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
        (o< (sub1 n) (sub1 m))))))

; The o= function compares n with m and returns true if n=m
;
(define o=
  (lambda (n m)
    (cond
      ((o> n m) #f)
      ((o< n m) #f)
      (else #t))))


; remove all numbers in a list
;
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat)
                  (no-nums (cdr lat)))))))

; (no-nums '(a 1 b 2 2 c d))

; count the target element
;
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l))
             (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l))
                (occur* a (cdr l)))))))

;(occur*
;  'banana
;  '((banana)
;    (split ((((banana ice)))
;            (cream (banana))
;            sherbet))
;    (banana)
;    (bread)
;    (banana brandy)))

