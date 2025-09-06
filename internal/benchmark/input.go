package benchmark

const largeLispProgram = `
; Fibonacci function with memoization
(define memo-fib
  (let ((cache '()))
    (lambda (n)
      (let ((cached (assoc n cache)))
        (if cached
            (cdr cached)
            (let ((result (if (<= n 1)
                             n
                             (+ (memo-fib (- n 1))
                                (memo-fib (- n 2))))))
              (begin
                (set! cache (cons (cons n result) cache))
                result)))))))

; Quick sort implementation
(define quicksort
  (lambda (lst)
    (if (or (null? lst) (null? (cdr lst)))
        lst
        (let ((pivot (car lst))
              (rest (cdr lst)))
          (let ((less (filter (lambda (x) (< x pivot)) rest))
                (greater (filter (lambda (x) (>= x pivot)) rest)))
            (append (quicksort less)
                    (cons pivot (quicksort greater))))))))

; Map function implementation
(define my-map
  (lambda (f xs)
    (if (null? xs)
        '()
        (cons (f (car xs))
              (my-map f (cdr xs))))))

; Factorial with tail recursion
(define factorial
  (begin
    (letrec ((fact-helper (lambda (n acc)
                            (if (= n 0)
                                acc
                                (fact-helper (- n 1) (* n acc))))))
      (lambda (n)
        (fact-helper n 1)))))

; Higher-order function example
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

; Y combinator for recursive functions
(define Y
  (lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (x x))))))

; Complex nested structure with various data types
(define test-data
  '(1 2.5 -3 +4.0 0xFF 0o755 0b1010
    (nested (deeply (very (much so))))
    (+ (* 2 3) (/ 8 4))
    '(quoted (list with) symbols)
    (lambda (x y z) 
      (let ((a (* x x))
            (b (* y y)) 
            (c (* z z)))
        (sqrt (+ a b c))))))

; Function that uses everything
(define kitchen-sink
  (lambda (data)
    (my-map (compose factorial abs)
            (quicksort (filter number? data)))))

; Test the functions
(kitchen-sink '(-3 1 4 -1 5 -9 2 6))
(memo-fib 20)
(factorial 10)
`
