#lang scheme

(define atom?
 (lambda (x)
    (and (not (pair? x)) (not (null? x)))))  

; judge if every elem in lat is an atom
;
(define lat?
  (lambda (l)
    (cond
      ((null? l)#t)
      ((atom? (car l))(lat? (cdr l)))
      (else #f))))

; (lat? 'a b c)

; remove the first elem that equals augment a in lat
;
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
        ((eq? (car lat) a) (cdr lat))
        (else (cons (car lat)(rember a
                      (cdr lat)))))))))

;(rember 'a '(b a c d))

; returm a list that consists of the first elem of each list in l
;
(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (car (car l)) (firsts (cdr l)))))))


;(firsts '((apple peach pumpkin)
;          (plum pear cherry)
;          (grape raisin pea)
;          (bean carrot eggplant)))

; insert after the old elem
;
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old)
       (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old
                                        (cdr lat)))))))

;(insertR
;  'topping 'fudge
;  '(ice cream with fudge for dessert)) ;

; insert before the old elem
;
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old)
       (cons new lat))
      (else (cons (car lat) (insertL new old
                                        (cdr lat)))))))

;(insertL
;  'topping 'fudge
;  '(ice cream with fudge for dessert)) ;

; remove all the elem that equals augment a in lat
;
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
        ((eq? (car lat) a) (multirember a (cdr lat)))
        (else (cons (car lat)(multirember a
                      (cdr lat)))))))))

;(multirember 'a '(b a c d a c a))

; insert after the old elem
;
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old)
       (cons old (cons new
                       (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old
                                        (cdr lat)))))))

; rember* function
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
       (else
        (cons (rember* a (car l)) (rember* a (cdr l)))))))

;(rember*
;  'cup
;  '((coffee) cup ((tea) cup) (and (hick)) cup))

; The eqan? function determines whether two arguments are te same
; It uses eq? for atoms and = for numbers
;
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or  (number? a1) (number? a2)) #f)
      (else
        (eq? a1 a2)))))

; the advanced version for eqan?
;
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

(eqlist?
  '(strawberry ice cream)
  '(strawberry ice cream))                  ; #t

(eqlist?
  '(strawberry ice cream)
  '(strawberry cream ice))                  ; #f

(eqlist?
  '(banan ((split)))
  '((banana) split))                        ; #f

(eqlist?
  '(beef ((sausage)) (and (soda)))
  '(beef ((salami)) (and (soda))))          ; #f

(eqlist?
  '(beef ((sausage)) (and (soda)))
  '(beef ((sausage)) (and (soda))))         ; #t

