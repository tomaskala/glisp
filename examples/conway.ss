;; Check whether the value is a list.
(define list?
  (lambda (x)
    (or (nil? x) (pair? x))))

;; Get the length of a list.
(define length
  (lambda (lst)
    (letrec ((length-acc
               (lambda (xs acc)
                 (if (nil? xs) acc
                   (length-acc (cdr xs) (+ acc 1))))))
      (length-acc lst 0))))

;; Map a function over a list.
(define map
  (lambda (f lst)
    (if (nil? lst) ()
      (cons (f (car lst)) (map f (cdr lst))))))

;; Generate a range of integers between the two bounds, inclusive.
(define int-range
  (lambda (bottom top)
    (letrec ((int-range-acc
               (lambda (curr acc)
                 (if (> curr top) acc
                   (int-range-acc (+ curr 1) (cons (+ (- top curr) 1) acc))))))
      (int-range-acc bottom ()))))

;; Retrieve the n-th element from a list, counting from one.
(define nth
  (lambda (lst n)
    (cond ((> n (length lst)) 0)
          ((< n 1) 0)
          ((= n 1) (car lst))
          (#t (nth (cdr lst) (- n 1))))))

;; Get the value of a cell.
(define cell
  (lambda (world x y)
    (let ((row (nth world y)))
      (if (list? row) (nth row x) 0))))

;; Count the number of alive neighbors of a cell.
(define count-neighbors
  (lambda (world x y)
    (+ (cell world (- x 1) (- y 1))
       (cell world x (- y 1))
       (cell world (+ x 1) (- y 1))
       (cell world (- x 1) y)
       (cell world (+ x 1) y)
       (cell world (- x 1) (+ y 1))
       (cell world x (+ y 1))
       (cell world (+ x 1) (+ y 1)))))

;; Get the next value of a cell based on the simulation rules.
(define next-cell
  (lambda (world x y)
    (let ((curr (cell world x y))
          (count (count-neighbors world x y)))
      (cond ((and (= curr 1) (or (< count 2) (> count 3))) 0)
            ((and (= curr 0) (= count 3)) 1)
            (#t curr)))))

;; Get the next value of a row based on the simulation rules.
(define next-row
  (lambda (world y)
    (let ((xs (int-range 1 (length (nth world y)))))
      (map (lambda (x) (next-cell world x y)) xs))))

;; Evolve a world into the next generation.
(define evolve
  (lambda (world)
    (let ((ys (int-range 1 (length world))))
      (map (lambda (y) (next-row world y)) ys))))

;; Simulate the game of life for several steps.
(define simulate
  (lambda (world n)
    (if (= n 0) world
      (simulate (evolve world) (- n 1)))))

;; Pretty-print a simulated world.
(define print-world
  (let* ((prepare-cell (lambda (x) (if (= x 0) '_ '#)))
         (prepare-row (lambda (row) (map prepare-cell row))))
  (lambda (world)
    (if (nil? world) ()
      (begin
        (display (prepare-row (car world)))
        (print-world (cdr world)))))))

(define world
  '((0 0 1 0 0 0 0 0)
    (0 0 0 1 0 0 0 0)
    (0 1 1 1 0 0 0 0)
    (0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0)))

;(define world
;  '((0 1 0)
;    (0 1 0)
;    (0 1 0)))

(print-world world)
(newline)
(print-world (simulate world 1000))
