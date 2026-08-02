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

(define length_
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define x '(1 2 3 4 5))
(length x)
(length_ x)

(define pick
  (lambda (n lat)
    (cond
      ((eq n 1)
       (car lat))
      (else (pick (sub1 n)
                  (cdr lat))))))

(pick 5 x)
(define y '(lasagna spaghetti ravioli macaroni meatball))
(pick 4 y)
(pick 0 y)

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n)
                           (cdr lat)))))))
(rempick 1 y)
(rempick 3 y)

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((not (number? (car lat))) (cons (car lat)
                                       (no-nums (cdr lat))))
      (else (no-nums (cdr lat))))))

(define z '(5 pears 6 prunes 9 dates))
(no-nums z)
(all-nums z)

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

(define arr1 '(1 2 3 1 3 1 5 7))
(occur 0 arr1)

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

(define l '((coffee) cup ((tea) cup) (and (hick)) cup))
(define a 'cup)
(rember* a l)

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
          
(define new 'roast)
(define old 'chuck)
(define l '((how much (wood))
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood))
(insertR* new old l)
(insertL* new old l)

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

(define a 'banana)
(define l '((banana)
            (split ((((banana ice)))
                    (cream (banana))
                    sherbet))
            (banana)
            (bread)
            (banana brandy)))
(occur* a l)

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

(define new 'orange)
(define old 'banana)
(subst* new old l)
(member* new l)

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
