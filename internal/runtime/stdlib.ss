;; Basic building blocks
;; ============================================================================

;; list returns the arguments as they are in a list.
;; Required for backquoting.
(define list
  (lambda args args))

;; append1 concatenates two lists together.
;;
;; time: O(len(s))
;; space: O(len(s))
;; tail-recursive: no
(define append1
  (lambda (s t)
    (if (nil? s) t
      (cons (car s) (append1 (cdr s) t)))))

;; append concatenates an arbitrary number of lists together.
;; Required for backquoting.
;;
;; time: O(n1 + ... + nk), where ni is the length of the i-th list
;; space: O(n1 + ... + nk), where ni is the length of the i-th list
;; tail-recursive: no
(define append
  (lambda (lst . args)
    (if (nil? args) lst
      (append1 lst (apply append args)))))

;; defmacro is a shorthand for (define <name> (macro <params> <body>)).
(define defmacro
  (macro (f v x) `(define ,f (macro ,v ,x))))

;; defun is a shorthand for (define <name> (lambda <params> (begin <body>...))).
(defmacro defun (name params . body)
  `(define ,name (lambda ,params (begin ,@body))))

;; The following let expression:
;;
;;	(let ((k1 v1) (k2 v2) ... (kN vN)) body)
;;
;; is equivalent to the following lambda invocation:
;;
;;	((lambda (k1 k2 ... kN) body) v1 v2 ... vN)
(defmacro let (bindings body)
  (if (nil? bindings)
    `((lambda () ,body))
    `((lambda ,(map car bindings) ,body) 
      ,@(map (lambda (b) (car (cdr b))) bindings))))

;; The following let* expression:
;;
;;	(let* ((k1 v1) (k2 v2) ... (kN vN)) body)
;;
;; is equivalent to the following lambda invocation:
;;
;;	((lambda (k1) ((lambda (k2) ... ((lambda (kN) body) vN) ... v2) v1))
(defmacro let* (bindings body)
  (if (nil? bindings)
    body
    `((lambda (,(car (car bindings))) 
        (let* ,(cdr bindings) ,body))
      ,(car (cdr (car bindings))))))

;; The following letrec expression:
;;
;;	(letrec ((k1 v1) (k2 v2) ... (kN vN)) body)
;;
;; is equivalent to the following lambda invocation:
;;
;;	((lambda (k1 k2 ... kN)
;;	  (begin
;;	    (set! k1 v1)
;;	    (set! k2 v2)
;;	    ...
;;	    (set! kN vN)
;;	    body)) () () ... ())
(defmacro letrec (bindings body)
  (if (nil? bindings)
    `((lambda () ,body))
    `((lambda ,(map car bindings)
        (begin
          ,@(map (lambda (b) `(set! ,(car b) ,(car (cdr b)))) bindings)
          ,body))
      ,@(map (lambda (x) '()) bindings))))

;; when conditionally evaluates its actions without an else clause.
(defmacro when (test . body)
  `(if ,test (begin ,@body) '()))

;; unless is the inverse of when
(defmacro unless (test . body)
  `(if (not ,test) (begin ,@body) '()))

;; -> (thread-first) threads an expression through forms as the first argument.
;; (-> x (f a) (g b)) expands to (g (f x a) b)
(defmacro -> (expr . forms)
  (if (nil? forms)
    expr
    (let ((form (car forms)))
      (if (pair? form)
        `(-> (,(car form) ,expr ,@(cdr form)) ,@(cdr forms))
        `(-> (,form ,expr) ,@(cdr forms))))))

;; ->> (thread-last) threads an expression through forms as the last argument.
;; (->> x (f a) (g b)) expands to (g b (f a x))
(defmacro ->> (expr . forms)
  (if (nil? forms)
    expr
    (let ((form (car forms)))
      (if (pair? form)
        `(->> (,@form ,expr) ,@(cdr forms))
        `(->> (,form ,expr) ,@(cdr forms))))))

;; as-> threads an expression through forms, binding it to a name.
;; (as-> x $ (f $ a) (g b $)) allows to position $ anywhere
(defmacro as-> (expr name . forms)
  (if (nil? forms)
    expr
    `(let ((,name ,expr))
       (as-> ,(car forms) ,name ,@(cdr forms)))))

;; some-> works like the -> macro, but short-circuits on nil.
;; Returns nil if any intermediate result is nil.
(defmacro some-> (expr . forms)
  (if (nil? forms)
    expr
    (let ((tmp (gensym))
          (form (car forms)))
      `(let ((,tmp ,expr))
         (if (nil? ,tmp)
           '()
           ,(if (pair? form)
              `(some-> (,(car form) ,tmp ,@(cdr form)) ,@(cdr forms))
              `(some-> (,form ,tmp) ,@(cdr forms))))))))

;; some->> works like the ->> macro, but short-circuits on nil.
;; Returns nil if any intermediate result is nil.
(defmacro some->> (expr . forms)
  (if (nil? forms)
    expr
    (let ((tmp (gensym))
          (form (car forms)))
      `(let ((,tmp ,expr))
         (if (nil? ,tmp)
           '()
           ,(if (pair? form)
              `(some->> (,@form ,tmp) ,@(cdr forms))
              `(some->> (,form ,tmp) ,@(cdr forms))))))))

;; case provides pattern matching on values.
;;
;; (case expr
;;   ((val1 val2 ...) result)
;;   ((val3 val4 ...) result)
;;   (#t result))  ; default case
(defmacro case (expr . clauses)
  (let ((tmp (gensym)))
    `(let ((,tmp ,expr))
       (cond ,@(map (lambda (clause)
                      (if (eq? (car clause) #t)
                        clause
                        (cons (cons 'or 
                                    (map (lambda (val) 
                                           (list 'equal? tmp (list 'quote val)))
                                         (car clause)))
                              (cdr clause))))
                    clauses)))))

;; not negates its argument: the empty list becomes #t, everything else becomes
;; the empty list.
;;
;; time: O(1)
;; space: O(1)
(define not nil?)

;; equal? tests whether two values are deeply equal.
;;
;; time: O(n)
;; space: O(1)
;; tail-recursive: no
(defun equal? (x y)
  (or (eq? x y)
      (and (pair? x)
           (pair? y)
           (equal? (car x) (car y))
           (equal? (cdr x) (cdr y)))))

;; Higher-order functions
;; ============================================================================

;; id is an identity function.
;;
;; time: O(1)
;; space: O(1)
(defun id (x) x)

;; compose composes two functions.
;;
;; time: O(1)
;; space: O(1)
(defun compose (f g)
  (lambda (x) (f (g x))))

;; curry converts a binary function to a curried form.
;;
;; time: O(1)
;; space: O(1)
(defun curry (f)
  (lambda (x) (lambda (y) (f x y))))

;; partial applies a function to some arguments, returning a new function.
;;
;; time: O(1)
;; space: O(1)
(defun partial (f . args)
  (lambda rest (apply f (append args rest))))

;; List functions
;; ============================================================================

;; foldl folds a list from the left using the given binary operation
;; and an initial value.
;;
;; time: O(n)
;; space: O(1)
;; tail-recursive: yes
(defun foldl (f init lst)
  (if (nil? lst) init
    (foldl f (f (car lst) init) (cdr lst))))

;; map maps a function over a list, producing a list of the results.
;;
;; time: O(n)
;; space: O(n)
;; tail-recursive: yes (implemented using a difference list)
(defun map (f lst)
  ((foldl (lambda (x acc)
            (lambda (tail)
              (acc (cons (f x) tail)))) id lst) '()))

;; filter keeps only those elements of a list satisfying a predicate.
;;
;; time: O(n)
;; space: O(n)
;; tail-recursive: yes (implemented using a difference list)
(defun filter (p lst)
  ((foldl (lambda (x acc)
            (if (p x)
              (lambda (tail)
                (acc (cons x tail)))
              acc)) id lst) '()))

;; length returns the number of elements in a list in O(n) time.
;;
;; time: O(n)
;; space: O(1)
;; tail-recursive: yes
(defun length (lst)
  (foldl (lambda (curr acc) (+ acc 1)) 0 lst))

;; reverse reverses a list.
;;
;; time: O(n)
;; space: O(n)
;; tail-recursive: yes
(defun reverse (lst)
  (foldl cons '() lst))

;; member locates the first element of a list equal to an element,
;; returning the tail of the list beginning with the element (or
;; an empty list if the element is not found).
;;
;; time: O(n)
;; space: O(1)
;; tail-recursive: yes
(defun member (x lst)
  (cond
    ((nil? lst) lst)
    ((equal? x (car lst)) lst)
    (#t (member x (cdr lst)))))

;; nth returns the nth element of a list (0-indexed).
;;
;; time: O(n)
;; space: O(1)
;; tail-recursive: yes
(defun nth (n lst)
  (if (= n 0)
    (car lst)
    (nth (- n 1) (cdr lst))))

;; take returns the first n elements of a list.
;;
;; time: O(n)
;; space: O(n)
;; tail-recursive: no
(defun take (n lst)
  (if (or (= n 0) (nil? lst))
    '()
    (cons (car lst) (take (- n 1) (cdr lst)))))

;; drop returns a list with the first n elements removed.
;;
;; time: O(n)
;; space: O(1)
;; tail-recursive: yes
(defun drop (n lst)
  (if (or (= n 0) (nil? lst))
    lst
    (drop (- n 1) (cdr lst))))

;; any? tests whether at least one element of a list satisfies a predicate.
;;
;; time: O(n)
;; space: O(1)
;; tail-recursive: yes
(defun any? (p lst)
  (cond
    ((nil? lst) '())
    ((p (car lst)) #t)
    (#t (any? p (cdr lst)))))

;; all? tests whether at least one element of a list satisfies a predicate.
;;
;; time: O(n)
;; space: O(1)
;; tail-recursive: yes
(defun all? (p lst)
  (cond
    ((nil? lst) #t)
    ((not (p (car lst))) '())
    (#t (all? p (cdr lst)))))

;; zip combines two lists into a list of pairs.
;;
;; time: O(min(n, m))
;; space: O(min(n, m))
;; tail-recursive: no
(defun zip (lst1 lst2)
  (if (or (nil? lst1) (nil? lst2)) '()
    (cons (list (car lst1) (car lst2))
          (zip (cdr lst1) (cdr lst2)))))

;; range generates a list of integers from start to end (exclusive).
;;
;; time: O(n)
;; space: O(n)
;; tail-recursive: yes
(defun range (start end)
  (letrec ((helper (lambda (i acc)
                     (if (< i start)
                       acc
                       (helper (- i 1) (cons i acc))))))
    (helper (- end 1) '())))

;; assoc looks up a key in an association list.
;; Returns the first pair whose car is equal? to the key.
;;
;; time: O(n)
;; space: O(1)
;; tail-recursive: yes
(defun assoc (key alist)
  (cond
    ((nil? alist) '())
    ((equal? key (car (car alist))) (car alist))
    (#t (assoc key (cdr alist)))))

;; flatten recursively flattens nested lists.
;;
;; time: O(n*d), where d is the maximum level of nesting
;; space: O(n)
;; tail-recursive: no
(defun flatten (lst)
  (cond
    ((nil? lst) '())
    ((pair? lst) (append (flatten (car lst)) (flatten (cdr lst))))
    (#t (list lst))))

;; last returns the last element of a list.
;;
;; time: O(n)
;; space: O(1)
;; tail-recursive: yes
(defun last (lst)
  (if (nil? (cdr lst))
    (car lst)
    (last (cdr lst))))

;; for-each applies a function to each element for side effects.
;;
;; time: O(n)
;; space: O(1)
;; tail-recursive: yes
(defun for-each (f lst)
  (if (nil? lst) '()
    (begin
      (f (car lst))
      (for-each f (cdr lst)))))

;; Mathematical functions
;; ============================================================================

;; min finds the smallest of its arguments, or Inf if no arguments are given.
;;
;; time: O(n)
;; space: O(1)
;; tail-recursive: yes
(defun min args
  (foldl (lambda (curr acc) (if (< curr acc) curr acc)) inf args))

;; max finds the greatest of its arguments, or -Inf if no arguments are given.
;;
;; time: O(n)
;; space: O(1)
;; tail-recursive: yes
(defun max args
  (foldl (lambda (curr acc) (if (> curr acc) curr acc)) -inf args))

;; frac calculates the absolute value of a number.
;;
;; time: O(1)
;; space: O(1)
(defun abs (x)
  (if (< x 0) (- x) x))

;; frac calculates the fractional part of a number.
;;
;; time: O(1)
;; space: O(1)
(defun frac (x)
  (- x (int x)))

;; floor rounds a number down.
;;
;; time: O(1)
;; space: O(1)
(defun floor (x)
  (let ((i (int x)))
    (if (and (< x 0) (not (= x i)))
        (- i 1)
        i)))

;; ceiling rounds a number up.
;;
;; time: O(1)
;; space: O(1)
(defun ceiling (x)
  (let ((i (int x)))
    (if (and (> x 0) (not (= x i)))
        (+ i 1)
        i)))

;; round rounds a number to the closest integer.
;;
;; time: O(1)
;; space: O(1)
(defun round (x)
  (floor (+ x 0.5)))

;; mod calculates the modulo of two numbers.
;;
;; time: O(1)
;; space: O(1)
(defun mod (n m)
  (- n (* m (floor (/ n m)))))

;; rem calculates the remainder of two numbers.
;;
;; time: O(1)
;; space: O(1)
(defun rem (n m)
  (- n (* m (int (/ n m)))))

;; gcd calculates the greatest common divisor of two numbers.
;;
;; time: O(log(min(n, m)))
;; space: O(1)
(defun gcd (n m)
  (if (= m 0) n
    (gcd m (mod n m))))

;; lcm calculates the least common multiple of two numbers.
;;
;; time: O(log(min(n, m)))
;; space: O(1)
(defun lcm (n m)
  (/ (* n m) (gcd n m)))

;; even? tests whether a number is even.
;;
;; time: O(1)
;; space: O(1)
(defun even? (n)
  (= (mod n 2) 0))

;; odd? tests whether a number is odd.
;;
;; time: O(1)
;; space: O(1)
(defun odd? (n)
  (= (mod n 2) 1))
