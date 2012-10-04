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

(define log10
  (lambda (n)
    (floor (/ (log n) (log 10)))))

; ; maybe use this rather than log10 as kids won't have encountered logs yet
; (define max-place
;   (lambda (n)
;     (let loop ((n n)
;                (p 1)
;                (q 2))
;       (cond ((= (modulo n (expt 10 p)) (modulo n (expt 10 q))) p)
;             (else (loop n (+ p 1) (+ q 1)))))))

(define nth
  (lambda (n l)
    (cond ((= n 0) (car l))
          (else (nth (- n 1) (cdr l))))))

(define char->number
  (lambda (c)
    (string->number (string c))))

; horribly hacky way to do it, but maybe demonstrating lateral
; thinking to the kids -- if the maths is tricky, exploit the fact
; that we're on a computer
(define place-value
  (lambda (n p)
    (let ((l (reverse (string->list (number->string n)))))
      (* (char->number (nth p l)) (expt 10 p)))))

(define partition
  (lambda (n)
    (let loop ((out '(+))
               (m   (log10 n)))
      (cond ((= m -1) (reverse out))
            (else (loop (cons (place-value n m) out) (- m 1)))))))

(define expanded-sexp
  (lambda (e)
    (let loop ((in  (cdr e))
               (out (list (car e))))
      (cond ((null? in) (reverse out))
            (else (loop (cdr in) (cons (partition (car in)) out)))))))

(define places-needed
  (lambda (e)
    (+ (apply max (map log10 (cdr e))) 1)))

; TODO: fix +s between translations of sexps, see test below
(define part->string
  (lambda (e)
    (let loop ((l (cdr e))
               (n 0)
               (s ""))
      (cond ((null? l) s)
            ((list? (car l)) (loop (cdr l) (+ n 1) (string-append (part->string (car l)) s)))
            ((odd? n) (loop l (+ n 1) (string-append "+" s)))
            (else (loop (cdr l) (+ n 1) (string-append (number->string (car l)) s)))))))

; (part->string (expanded-sexp '(+ 123 4567)))

(define expanded
  (lambda (e)
    '()))
