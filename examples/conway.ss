;; Check whether the value is a list.
(defun list? (x)
  (or (nil? x) (pair? x)))

;; Retrieve the n-th element from a list, counting from one.
(defun nth (lst n)
  (cond ((> n (length lst)) 0)
        ((< n 1) 0)
        ((= n 1) (car lst))
        (#t (nth (cdr lst) (- n 1)))))

;; Get the value of a cell.
(defun cell (world x y)
  (let ((row (nth world y)))
    (if (list? row) (nth row x) 0)))

;; Count the number of alive neighbors of a cell.
(defun count-neighbors (world x y)
  (+ (cell world (- x 1) (- y 1))
     (cell world x (- y 1))
     (cell world (+ x 1) (- y 1))
     (cell world (- x 1) y)
     (cell world (+ x 1) y)
     (cell world (- x 1) (+ y 1))
     (cell world x (+ y 1))
     (cell world (+ x 1) (+ y 1))))

;; Get the next value of a cell based on the simulation rules.
(defun next-cell (world x y)
  (let ((curr (cell world x y))
        (count (count-neighbors world x y)))
    (cond ((and (= curr 1) (or (< count 2) (> count 3))) 0)
          ((and (= curr 0) (= count 3)) 1)
          (#t curr))))

;; Get the next value of a row based on the simulation rules.
(defun next-row (world y)
  (let ((xs (range 1 (+ 1 (length (nth world y))))))
    (map (lambda (x) (next-cell world x y)) xs)))

;; Evolve a world into the next generation.
(defun evolve (world)
  (let ((ys (range 1 (+ 1 (length world)))))
    (map (lambda (y) (next-row world y)) ys)))

;; Simulate the game of life for several steps.
(defun simulate (world n)
  (if (= n 0) world
    (simulate (evolve world) (- n 1))))

;; Pretty-print a simulated world.
(defun print-world (world)
  (let* ((prepare-cell (lambda (x) (if (= x 0) '_ '#)))
         (prepare-row (partial map prepare-cell)))
    (for-each display (map prepare-row world))))

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
