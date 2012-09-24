(require-extension srfi-1)

(define rep
  (lambda (x n)
    (cond ((= n 0) '())
          (else (cons x (rep x (- n 1)))))))

(define nth
  (lambda (n l)
    (cond ((= n 0) (car l))
          (else (nth (- n 1) (cdr l))))))

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
    (= y (length b))))

(define left?
  (lambda (x y b)
    (= x 0)))

(define right?
  (lambda (x y b)
    (= x (length (car b)))))

(define get-neighbours
  (lambda (x y b)
    (cond ((top? x y b) (cond ((left? x y b) (list (get-cell x (+ y 1) b)
                                                   (get-cell (+ x 1) y b)
                                                   (get-cell (+ x 1) (+ y 1) b)))
                              ((right? x y b) (list (get-cell x (+ y 1) b)
                                                    (get-cell (- x 1) y b)
                                                    (get-cell (- x 1) (+ y 1) b)))
                              (else (list (get-cell (+ x 1) (+ y 1) b)
                                          (get-cell (+ x 1) (+ y 1) b)
                                          (get-cell (+ x 1) (+ y 1) b)
                                          (get-cell (+ x 1) (+ y 1) b)
                                          (get-cell (+ x 1) (+ y 1) b)))))
          ((bottom? x y b) (cond ((left? x y b) (list (get-cell x (- y 1) b)
                                                      (get-cell (+ x 1) y b)
                                                      (get-cell (+ x 1) (- y 1) b)))
                                 ((right? x y b) (list (get-cell x (- y 1) b)
                                                       (get-cell (- x 1) y b)
                                                       (get-cell (- x 1) (- y 1) b)))
                                 (else (list (get-cell (+ x 1) (+ y 1) b)
                                             (get-cell (+ x 1) (+ y 1) b)
                                             (get-cell (+ x 1) (+ y 1) b)
                                             (get-cell (+ x 1) (+ y 1) b)
                                             (get-cell (+ x 1) (+ y 1) b)))))
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

(define live-neighbours
  (lambda (x y b)
    (let ((f (lambda (v) (cond (v 1) (else 0)))))
      (apply + (map f (get-neighbours x y b))))))

(define live-or-die
  (lambda (x y b)
    (let ((n (live-neighbours x y b)))
      (cond ((get-cell x y b) (or (= 2 n) (= 3 n)))
            (else (= 3 n))))))

(define iterate
  (lambda (b)
