"Hello World"
;Hello World

(+ 3 4)
;7

(+ 3 4 1 8 5)
;21

5
;5

(- 3 4)
;-1

(* 3 4)
;12

(/ 3 4)
;0

(mod 3 4)
;3

(= 3 4)
;nil

(= 3 3)
;t

(> 3 4)
;nil

(< 3 4)
;t

(length (7 2 4 9))
;error

(length (quote(7 2 4 9)))
;4

(length `(7 2 4 9))
;4

(car `(1 2 3 4))
;1

(cdr `(1 2 3 4))
;2 3 4

(cons 1 `(2 3 4))
;1 2 3 4

(cons 5 `())
;5

(cons 5 nil)
;5

(atom 5)
;t

(atom "Hello World")
;t

(atom `(1 2 3))
;nil

(atom `())
;t

(atom nil)
;t

(listp 5)
;nil

(listp "Hello World")
;nil

(listp `(1 2 3))

;3

(listp `())
;t

(listp nil)
;t

(consp 5)
;nil

(consp "Hello World")
;nil

(consp `())
;nil

(consp nil)
;nil

(defun one () 1)
;one

(one)
;1

(one 5)
;error

(defun two (n) 2)
;two

(two)
;error

(two 5)
;5

(defun twice(n)(* n 2))
;twice

(twice)
;error

(twice 5)
;10

(defun plus(n m)(+ n m))
;plus

(plus)
;error

(plus 5 6)
;11

(cond
 ((> 3 4) 1)
 (nil 2)
 (t 3)
 ((= 5 5) 4))
;3

(defun plusp(n)
  (cond
   ((> n 0) t)
   (t nil)))
;plusp

(plusp 1)
;t

(plusp 2)
;t

(plusp -1)
;nil

(defun factorial(n)
  (cond
   ((= n 0) 1)
   (t (* n (factorial (- n 1))))))
;factorial

(factorial 3)
;6

(factorial 4)
;24

(factorial 5)
;120

(cond
 (0 0)
 (t 1))
;0

(cond
 (1 0)
 (t 1))
;0

(cond
 ("Hello World" 0)
 (t 1))
;0

(cond
 (nil 0)
 (t 1))
;1


;<<-exercise->>
(defun second (list)
  (car (cdr list)))

(second `(1 2 3 4))
;2

(defun third (list)
  (car (cdr (cdr list))))

(third `(1 2 3 4))
;3

(defun remove_2nd (list)
  (cons (car list) (cdr (cdr list))))

(remove_2nd `(1 2 3 4))

(defun remove_3rd (list)
  (cons (car list) (remove_2nd(cdr list))))

;(cons (cons (car list) (car (cdr list))) (cdr(cdr(cdr list))))

(remove_3rd `(1 2 3 4))
;1 2 4

(defun size (list)
  (cond
   ((atom list) 0)
   (t (+ 1 (size (cdr list))))))

(size `(1 2 3 4 5))

(defun at (n list)
  (cond
   ((= n 1) (car list))
   (t (at (- n 1) (cdr list)))))

(at 3 `(1 2 3 4 5))

(defun all_positive (list)
  (cond
   ((atom list) t)
   ((< (car list) 0) nil)
   (t (all_positive (cdr list))) ))

(all_positive `(1 2 -3 4))

(defun has_even (list)
  (cond
   ((atom list) nil)
   ((= (mod (car list) 2) 0 ) t)
   (t (has_even(cdr list)))))

(has_even `(1 3 7 5))

(defun sum_abs (list)
  (cond
   ((atom list) 0)
   ((<0 (car list))(+ sum_abs(cdr list))(cdr list))
   (t (- (sum_abs(cdr list))(car list)))))


(defun twice_all (list)
  (cond
   (atom list) nil)
  (t (cons(* 2 (car list))(twice_all (cdr list))))))

;12/12‰ñ

(defun cdr_of (n list)
  (cond
   ((atom list) nil)
   ((eq (car (car list)) n) (cdr(car list)))
   (t (cdr_of n (cdr list))) ))

(cdr_of 'b '((a b c) (b d e) (c f g)))
;(d e)

(defun conc (list1 list2)
  (cond
   ((atom list1) list2)
   (t (cons (car list1) (conc (cdr list1) list2)))))

(conc '(a b c) '(d e f))
;(a b c d e f)

(defun dsearch0 (goal open connections)
  (cond
   ((atom open) nil)
   ((eq goal (car open)) t)
   (t (dsearch0 goal (conc (cdr_of (car open) connections) (cdr open)) connections))))
	       
(dsearch0 'j '(a) '((a b c) (b d e) (c f g) (d h i) (e j k) (f l m) (g n o)))
;t

(defun includep (item list)
  (cond
   ((atom list) nil)
   ((eq item (car list)) t)
   (t (includep item (cdr list)))))

(includep 'c '(a b c d e))
;t
(includep 'f '(a b c d e))
;nil


(defun common (list1 list2)
  (cond
   ((atom list1) list2)
   ((includep (car list1) list2) (common (cdr list1) list2)) ;‚±‚±‚Å•Ô‚µ‚½‚¢‚Ì‚Ílist2‚Ìæ“ª‚ðœ‚¢‚½‚à‚Ì
   (t (cons (car list1) (common (cdr list1) list2)))))

(common '(d a p m e f g) '(a b c d e f g))
;(p m a b c d e f g)


(defun subtract (list1 list2)
  (cond
   ((atom list1) nil)
   ((includep (car list1) list2) (subtract (cdr list1) list2))
   (t (cons (car list1) (subtract (cdr list1) list2)))))

(subtract '(c a b e d f) '(e c r f))
;(a b d)

(defun intersect (list1 list2)
  (cond
   ((atom list1) nil)
   ((includep (car list1) list2) (cons (car list1) (intersect (cdr list1) list2)))
   (t (intersect (cdr list1) list2))))


(intersect '(a b c d e f g) '(d a p m e f g))
;(a d e f g)


(defun dsearch1 (goal open closed connections)
  (cond
   ((atom open) nil)
   ((eq goal (car open)) t)
   (t (dsearch1 goal (conc (subtract (cdr_of (car open) connections) closed) (cdr open)) (cons (car open) closed) connections))))

(dsearch1 'g '(s) '() '((s a) (a b c d) (b e) (c e h) (d e h) (e a f) (f g) (h f)))
;t

(defun cons_all (list1 list2)
  (cond
   ((atom list1) nil)
   (t (cons (cons (car list1) list2) (cons_all (cdr list1) list2)))))
;

(cons_all '(a b c) '(x y z))
;((a x y z) (b x y z) (c x y z))

(cons_all '(b c d) 'a)
;((b.a) (c.a) (d.a))

   
(defun dsearch2 (goal open closed connections stree)
  (cond
    ((atom open) nil)
  ((eq (car open) goal) stree)
  (t (dsearch2 goal (subtract (conc (cdr_of (car open) connections) closed) (cdr open)) (cons (car open) closed) connections (common stree (cons_all (cdr_of (car open) connections) (car open)))))))

(dsearch2 'g '(s) '() '((s a) (a b c d) (b e) (c e h) (d e h) (e a f) (f g) (h f)) '())
;((a.s) (b.a) (c.a) (d.a) (e.b) (a.e) (f.e) (g.f))

(defun rev (list1 list2)
  (cond
   ((atom (cdr list1)) (cons (car list1) list2))
   (t (rev (cdr list1) (cons (car list1) list2)))))

(rev '(a b c d e) '())
;(e d c b a)

(defun const_route (goal stree route)
  (cond
   ((listp goal) route)
   (t (const_route (cdr_of goal stree) stree (cons goal route)))))

(const_route 'j '((j . e) (k . e) (h . d) (i . d) (d . b) (e . b) (b . a) (c . a)) '())
;(a b e j)

(defun dsearch3 (goal open closed arcs stree)
  (cond
   (const_route goal (dsearch3 goal open closed arcs stree) '())))

(dsearch3 'g '(s) '() '((s a) (a b c d) (b e) (c e h) (d e h) (e a f) (f g) (h f)) '())
;(s a b e f g)

(defun smaller (pivot list)
  (cond
   ((atom  list) list)
   ((>  pivot (car list)) (cons (car list)(smaller pivot (cdr list))))
   (t (smaller  pivot (cdr list)))))

(smaller 5 '(3 7 4 1 5 9 2 8 6))
;(3 4 1 2)

(defun larger (pivot list)
 (cond
   ((atom list) list)
   ((< pivot (car list)) (cons (car list) (larger pivot (cdr list))))
   (t (larger pivot (cdr list)))))

(larger  5 '(3 7 4 1 5 9 2 8 6))
;(7 9 8 6)

(defun qsort (list)
    (cond
     ((atom list) list)
     (t (conc (qsort (smaller (car list) list)) (cons (car list) (qsort (larger (car list) list)))))))

(qsort '(3 7 4 1 5 9 2 8 6))
;(1 2 3 4 5 6 7 8 9)

(defun smaller2 (pivot list costs)
  (cond
   ((atom list) list)
   ((> pivot (cdr_of (car list) costs)) (cons (car list)(smaller2 pivot (cdr list) costs)))
   (t (smaller2 pivot (cdr list) costs))))

(smaller2 5 '(a b c d e f g h i) '((a . 3)(b . 7)(c . 4)(d . 1)(e . 5)(f . 9)(g . 2)(h . 6)(i . 8)))
;(a c d g)


(defun larger2 (pivot list costs)
  (cond
   ((atom list) list)
   ((< pivot (cdr_of (car list) costs)) (cons (car list)(larger2 pivot (cdr list) costs)))
   (t (larger2 pivot (cdr list) costs))))

(larger2 5 '(a b c d e f g h i) '((a . 3)(b . 7)(c . 4)(d . 1)(e . 5)(f . 9)(g . 2)(h . 6)(i . 8)))
;(b f h i)

(defun qsort2 (list costs)
  (cond
   ((atom list) list)
   (t (conc
       (qsort2 (smaller2 (cdr_of (car list) costs) list costs) costs)
       (cons  
	(car list)
	(qsort2 (larger2 (cdr_of (car list) costs) list costs) costs))))))

(qsort2 '(a b c d e f g h i) '((a . 3)(b . 7)(c . 4)(d . 1)(e . 5)(f . 9)(g . 2)(h . 6)(i . 8)))
;(d g a c e h b i f)

(defun bsearch (goal open closed arcs stree costs)
  (cond
   ((atom open) nil)
   ((equal (car open) goal) stree)
   (t (bsearch goal (subtract (conc (qsort2 (cdr_of (car open) arcs) costs)(cdr open)) closed) (cons (car open) closed) arcs (conc (cons_all (subtract (cdr_of (car open) arcs) closed) (car open)) stree) costs))))

(bsearch 'g '(s) '() '((s a)(a b c d)(b e)(c e h)(d e h)(e a f)(f g)(h f)) '() '((s . 5)(a . 3)(b . 6)(c . 3)(d . 15)(e . 8)(f . 5)(g .0)(h . 100)))
