(require-extension srfi-1)

(define fib
  (lambda (x)
    (cond ((= x 0) 0)
          ((= x 1) 1)
          (else (+ (fib (- x 1)) (fib (- x 2)))))))

; (fib 3)

(define quicksort
  (lambda (l c)
    (cond ((null? l) '())
          (else (append (quicksort (filter (lambda (x) (c (car l) x)) (cdr l)) c)
                        (list (car l))
                        (quicksort (filter (lambda (x) (not (c (car l) x))) (cdr l)) c))))))

; (filter (lambda (x) (< x 3)) '(1 2 3 4 5))
; (quicksort '(2 4 1) >)

(define lrange
  (lambda (x)
    (letrec ((f (lambda (x)
                  (cond ((= x 0) '(0))
                        (else (cons x (lrange (- x 1))))))))
      (quicksort (f x) >))))

; (lrange 3)

(define lfib
  (lambda (x)
    (map fib (lrange x))))

; (lfib 10)
