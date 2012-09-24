(require-extension srfi-1)

(define quicksort
  (lambda (l c)
    (cond ((null? l) '())
          (else (append (quicksort (filter (lambda (x) (c (car l) x)) (cdr l)) c)
                        (list (car l))
                        (quicksort (filter (lambda (x) (not (c (car l) x))) (cdr l)) c))))))

(define lrange
  (lambda (x)
    (letrec ((f (lambda (x)
                  (cond ((= x 0) '(0))
                        (else (cons x (lrange (- x 1))))))))
      (quicksort (f x) >))))

(define alert
  (lambda (r n s)
    (cond ((= 0 (modulo n r)) s)
          (else n))))

; (alert 3 5 "fizz")
; (alert 3 6 "fizz")
; (alert 5 6 "buzz")
; (alert 5 10 "buzz")

(define to-fb
  (lambda (n x y)
    (let ((f (alert x n "fizz"))
          (b (alert y n "buzz")))
      (cond ((and (string? f) (string? b)) (string-append f b))
            ((string? f) f)
            ((string? b) b)
            (else n)))))

; (to-fb 6 3 5)
; (to-fb 10 3 5)
; (to-fb 11 3 5)
; (to-fb 15 3 5)

(define fizzbuzz
  (lambda (n f b)
    (let ((l   (cdr (lrange n)))
          (fun (lambda (x) (to-fb x f b))))
      (map fun l))))

; (fizzbuzz 20 3 5)
; (fizzbuzz 100 7 9)
