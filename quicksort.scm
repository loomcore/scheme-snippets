(define quicksort
  (lambda (l c)
    (cond ((null? l) '())
          (else (append (quicksort (filter (lambda (x) (c (car l) x)) (cdr l)) c)
                        (list (car l))
                        (quicksort (filter (lambda (x) (not (c (car l) x))) (cdr l)) c))))))

; (filter (lambda (x) (< x 3)) '(1 2 3 4 5))
; (quicksort '(2 4 1) >)
