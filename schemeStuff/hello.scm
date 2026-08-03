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

(define firsts
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car (car lat)) (firsts (cdr lat)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat)) (cons old (cons new (cdr lat))))
              (else (cons
                      (car lat)
                      (insertR new old (cdr lat)))))))))

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

(define minus
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (sub1 (minus a (sub1 b)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup)
               (addtup (cdr tup)))))))

(define into
  (lambda (m n)
    (cond
      ((zero? n) 0)
      (else (plus m
                  (into m (sub1 n)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (plus (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))


;-------------------- 
;Back to Numbers
;-------------------- 
;
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

(define eq
  (lambda (m n)
    (cond
      ((and (not (gt m n)) (not (lt m n))) #t) 
      (else #f))))

(define ^
  (lambda (m n)
    (cond
      ((zero? n) 1)
      (else (into m
                  (^ m (sub1 n)))))))

(define //
  (lambda (n m)
    (cond
      ((lt n m) 0)
      (else (add1 (// (minus n m)
                      m))))))

(define length_
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((eq n 1)
       (car lat))
      (else (pick (sub1 n)
                  (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n)
                           (cdr lat)))))))
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((not (number? (car lat))) (cons (car lat)
                                       (no-nums (cdr lat))))
      (else (no-nums (cdr lat))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat)
                                 (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (eq a1 a2))
      ((and (not (number? a1)) (not (number? a2)))
       (eq? a1 a2))
      (else #f))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a
              (car lat))
       (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one?
  (lambda (a)
    (eq 1 a)))

(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n)
                           (cdr lat)))))))
      
;----------
; STARS
;----------

(define rember*
  (lambda (a l)
    (cond
       ((null? l) '())
       ((atom? (car l))
        (cond
          ((eq? a (car l))
           (rember* a (cdr l)))
          (else (cons (car l)
                      (rember* a (cdr l))))))
       (else (cons (rember* a (car l))
                   (rember* a (cdr l))))))) 

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) 
       (cond
         ((eq? old (car l))
          (cons (car l)
                (cons new
                      (insertR* new old (cdr l)))))
         (else (cons (car l)
                     (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l))
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (plus (occur* a (car l))
                  (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons new
                (subst* new old (cdr l))))
         (else (cons (car l)
                     (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons new
                (cons old
                      (insertL* new old (cdr l)))))
         (else (cons (car l)
                     (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? a (car l))
          #t)
         (else (member* a (cdr l)))))
      (else (or (member* a (car l))
                (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define l '((potato) (chips ((with) fish) (chips))))
(leftmost l)

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1)
            (null? l2))
       #t)
      ((or (null? l1) (null? l2))
       #f)
      ((and (atom? (car l1))
            (atom? (car l2)))
       (cond
         ((eq? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2)))
         (else #f)))
      ((or (atom? (car l1))
           (atom? (car l2)))
       #f)
      (else (and (eqlist? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))))))

(define l1 '(banana ((split))))
(define l2 '((banana) (split)))
(eqlist? l1 l2)

(define equal??
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1)
          (atom? s2))
       #f)
      (else (eqlist? s1 s2)))))
