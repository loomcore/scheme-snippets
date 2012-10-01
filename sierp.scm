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

(define string-dup
  (lambda (s n)
    (cond ((= n 0) "")
          (else (string-append s (string-dup s (- n 1)))))))

; (string-dup "a" 5)

(define sierpinski-string
  (lambda (n)
    (let ((tri4 "*      ")
          (tri3 "* *    ")
          (tri2 "* * *  ")
          (tri1 "* * * *")
          (nl   "\n"))
      (cond ((= n 0) nl)
            (else (string-append (string-append nl
                                                (string-dup tri1 n)
                                                nl
                                                (string-dup tri2 n)
                                                nl
                                                (string-dup tri3 n)
                                                nl
                                                (string-dup tri4 n))
                                 (sierpinski-string (- n 1))))))))

(define sierpinski
  (lambda (n)
    (print (sierpinski-string n))))

; (sierpinski 1)
; (sierpinski 2)
; (sierpinski 4)
