; Copyright (C) 2012 P. M. Yeeles
;
; This file is part of scheme-snippets.
;
; scheme-snippets is free software: you can redistribute it and/or
; modify it under the terms of the GNU General Public License as
; published by the Free Software Foundation, either version 3 of the
; License, or (at your option) any later version.
;
; scheme-snippets is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with scheme-snippets.  If not, see
; <http://www.gnu.org/licenses/>.

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
