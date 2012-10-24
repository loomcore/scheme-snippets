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

; TODO: fix wacky behaviour of get-neighbours
;       (try (get-neighbours 1 0 tb) and (get-neighbours 1 0 (iterate tb)) to see
;       that it's weird: former returns '(#f #f #f #f #f), latter returns
;       '(#t #t #t #t #t) :\)

(require-extension srfi-1)

; ugh.  just... ugh.  i'm so sorry.
(define carp
  (lambda (l)
    (cond ((null? l) '())
          (else (car l)))))

; ditto
(define cdrp
  (lambda (l)
    (cond ((null? l) '())
          (else (cdr l)))))

(define rep
  (lambda (x n)
    (cond ((= n 0) '())
          (else (cons x (rep x (- n 1)))))))

(define nth
  (lambda (n l)
    (cond ((= n 0) (carp l))
          (else (nth (- n 1) (cdrp l))))))

(define blank-board
  (lambda (x y)
    (rep (rep #f x) y)))

; define new board with active cells actives.  blank if no actives specified
(define new-board
  (lambda (x y . actives)
    (let ((b (blank-board x y)))
      '())))

(define get-cell
  (lambda (x y b)
    (nth x (nth y b))))

(define top?
  (lambda (x y b)
    (= y 0)))

(define bottom?
  (lambda (x y b)
    (= y (- (length b) 1))))

(define left?
  (lambda (x y b)
    (= x 0)))

(define right?
  (lambda (x y b)
    (= x (- (length (carp b)) 1))))

(define get-neighbours
  (lambda (x y b)
    (cond ((top? x y b) (cond ((left? x y b) (list (get-cell x (+ y 1) b)
                                                   (get-cell (+ x 1) y b)
                                                   (get-cell (+ x 1) (+ y 1) b)))
                              ((right? x y b) (list (get-cell x (+ y 1) b)
                                                    (get-cell (- x 1) y b)
                                                    (get-cell (- x 1) (+ y 1) b)))
                              (else (list (get-cell (- x 1) y b)
                                          (get-cell (+ x 1) y b)
                                          (get-cell (- x 1) (+ y 1) b)
                                          (get-cell x (+ y 1) b)
                                          (get-cell (+ x 1) (+ y 1) b)))))
          ((bottom? x y b) (cond ((left? x y b) (list (get-cell x (- y 1) b)
                                                      (get-cell (+ x 1) y b)
                                                      (get-cell (+ x 1) (- y 1) b)))
                                 ((right? x y b) (list (get-cell x (- y 1) b)
                                                       (get-cell (- x 1) y b)
                                                       (get-cell (- x 1) (- y 1) b)))
                                 (else (list (get-cell (- x 1) (- y 1) b)
                                             (get-cell x (- y 1) b)
                                             (get-cell (+ x 1) (- y 1) b)
                                             (get-cell (- x 1) y b)
                                             (get-cell (+ x 1) y b)))))
          ((left? x y b) (list (get-cell x (- y 1) b)
                               (get-cell x (+ y 1) b)
                               (get-cell (+ x 1) (- y 1) b)
                               (get-cell (+ x 1) y b)
                               (get-cell (+ x 1) (+ y 1) b)))
          ((right? x y b) (list (get-cell x (+ y 1) b)
                                (get-cell x (+ y 1) b)
                                (get-cell (- x 1) (- y 1) b)
                                (get-cell (- x 1) y b)
                                (get-cell (- x 1) (+ y 1) b)))
          (else (list (get-cell (- x 1) (- y 1) b)
                      (get-cell (- x 1) y b)
                      (get-cell (- x 1) (+ y 1) b)
                      (get-cell x (- y 1) b)
                      (get-cell x (+ y 1) b)
                      (get-cell (+ x 1) (- y 1) b)
                      (get-cell (+ x 1) y b)
                      (get-cell (+ x 1) (+ y 1) b))))))

; (get-neighbours 1 0 tb)
; (get-neighbours 1 0 (iterate tb))

(define live-neighbours
  (lambda (x y b)
    (let ((f (lambda (v) (cond (v 1) (else 0)))))
      (apply + (map f (get-neighbours x y b))))))

(define live-or-die
  (lambda (x y b)
    (let ((n (live-neighbours x y b)))
      (cond ((get-cell x y b) (or (= 2 n) (= 3 n)))
            (else (= 3 n))))))

; (define iterate
;   (lambda (b)
;     (let yloop ((y     0)
;                 (board b))
;       (cond ((< y (length b)) (cons (let xloop ((x   0)
;                                                 (row (carp b)))
;                                       (cond ((< x (length (carp b))) (cons (live-or-die x y b) (xloop (+ x 1) (cdrp row)))))) (yloop (+ y 1) (cdrp board))))))))

(define iterate
  (lambda (b)
    (let yl ((y  0)
             (ob b)
             (nb '()))
      (cond ((null? ob) (reverse nb))
            (else (yl (+ y 1)
                      (cdrp ob)
                      (cons (let xl ((x  0)
                                     (or (carp ob))
                                     (nr '()))
                              (cond ((null? or) (reverse nr))
                                    (else (xl (+ x 1)
                                              (cdrp or)
                                              (cons (live-or-die x y b) nr)))))
                            nb)))))))

; (iterate '((#f #t #f) (#f #f #t) (#t #t #t)))
; (iterate '((#f #t #f) (#f #t #f) (#f #t #f)))
; (define tb '((#f #t #f) (#f #t #f) (#f #t #f)))
; (iterate (iterate '((#f #t #f) (#f #t #f) (#f #t #f))))
; (print-board (iterate '((#f #t #f) (#f #t #f) (#f #t #f))))
; (print-board (iterate (iterate '((#f #t #f) (#f #t #f) (#f #t #f)))))
; (print-board (iterate (iterate (iterate '((#f #t #f) (#f #t #f) (#f #t #f))))))

(define row-to-string
  (lambda (r)
    (cond ((null? r) "\n")
          (else (string-append (cond ((carp r) "▓") ; or "#"
                                     (else "░")) ; or "_"
                               (row-to-string (cdrp r)))))))

(define board-to-string
  (lambda (b)
    (cond ((null? b) "")
          (else (string-append (row-to-string (carp b)) (board-to-string (cdrp b)))))))

(define print-board
  (lambda (b)
    (print (string-append "\n" (board-to-string b)))))

; (print (row-to-string '(#f #f #t #t #f)))
; (print (row-to-string '(#t #f #f #f #t)))
; (print-board '((#f #t #f) (#f #f #t) (#t #t #t)))

(define run
  (lambda (b)
    (begin
      (print-board b)
      (run (iterate b)))))

; (run '())
; (run '((#f #t #f) (#f #t #f) (#f #t #f)))
