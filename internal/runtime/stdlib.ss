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
;; time: O(n)
;; space: O(n)
;; tail-recursive: no
(define append
  (lambda (lst . args)
    (if (nil? args) lst
      (append1 lst (apply append args)))))

;; defmacro is a shorthand for (define <name> (macro <params> <body>)).
(define defmacro
  (macro (f v x) `(define ,f (macro ,v ,x))))

;; defun is a shorthand for (define <name> (lambda <params> <body>)).
(defmacro defun (f v x)
  `(define ,f (lambda ,v ,x)))

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

;; id is an identity function
;;
;; time: O(1)
;; space: O(1)
(defun id (x) x)

;; not negates its argument: the empty list becomes #t, everything else becomes
;; the empty list
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
