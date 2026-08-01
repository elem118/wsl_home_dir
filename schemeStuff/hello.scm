(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      (else (or (eq? (car l) a)
                (member? a (cdr l)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) ('()))
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define x '(1 2 3 4 5))
(member? 3 x)
(rember 3 x)
(null? '())

(define firsts
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car (car lat)) (firsts (cdr lat)))))))

(define l '((1 2)
            (3 4)
            (5 6)))
(firsts l)

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat)) (cons old (cons new (cdr lat))))
              (else (cons
                      (car lat)
                      (insertR new old (cdr lat)))))))))

(define n 'e)
(define o 'd)
(define l '(a b c d f g d h))
(insertR n o l)
(multirember o l)
(multiinsertR n o l)
(multiinsertL n o l)
(multisubst n o l)

(define n '1)
(define o '0)
(define o2 '7)
(define l '(1 2 3 4 5 6 7 8))
(insertR n o l)
(insertL n o l)
(subst n o l)
(subst2 n o o2 l)

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat)) (cons new lat))
              (else (cons (car lat)
                          (insertL new old (cdr lat)))))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat)) (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst new old (cdr lat)))))))))

(define subst2
  (lambda (new old1 old2 lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((or (eq? old1 (car lat))
                   (eq? old2 (car lat)))
               (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst2 new old1 old2 (cdr lat)))))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? a (car lat))
               (multirember a (cdr lat)))
              (else (cons (car lat)
                          (multirember a (cdr lat)))))))))
       
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat))
               (cons old (cons new
                               (multiinsertR new old (cdr lat)))))
              (else (cons (car lat)
                          (multiinsertR new old (cdr lat)))))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat)
       '())
      (else (cond
              ((eq? old (car lat))
               (cons new (cons old
                               (multiinsertL new old (cdr lat)))))
              (else (cons (car lat)
                          (multiinsertL new old (cdr lat)))))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat))
               (cons new
                     (multisubst new old (cdr lat))))
              (else (cons (car lat)
                          (multisubst new old (cdr lat)))))))))

; ------------------
; NUMBERS GAME
; ------------------

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define plus
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (add1 (plus a (sub1 b)))))))

(plus 5 10)

(define minus
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (sub1 (minus a (sub1 b)))))))

(minus 2 1)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup)
               (addtup (cdr tup)))))))

(define tup1 '(1 2 3 4 5))
(addtup tup1)

(define into
  (lambda (m n)
    (cond
      ((zero? n) 0))
    (else (plus m
                (into m (sub1 n))))))

(into 2 5)
(into 7 9)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (plus (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))

(define tup2 '(5 5 5 5 5 5 5))
(tup+ tup1 tup2)

;---------- 
; PAREDIT
;---------- 

(hello (from the other) side)
(m n (a b)) (x y)

;-------------------- 
;Back to Numbers
;-------------------- 
;
(> 12 50)
(gt 12 50)

(define gt
  (lambda (m n)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (gt (sub1 m) (sub1 n))))))

(define lt
  (lambda (m n)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (lt (sub1 m) (sub1 n))))))

(< 12 50)
(lt 12 50)
(lt 10 10)

(= 10 10)

(eq 10 10)
(eq 1 2)

(define eq
  (lambda (m n)
    (cond
      ((and (not (gt m n)) (not (lt m n))) #t) 
      (else #f))))

(^ 2 3)
(expt 2 3)

(define ^
  (lambda (m n)
    (cond
      ((zero? n) 1)
      (else (into m
                  (^ m (sub1 n)))))))

(// 4 2)
(// 7 3)
(quotient 7 3)

(define //
  (lambda (n m)
    (cond
      ((lt n m) 0)
      (else (add1 (// (minus n m)
                      m))))))














