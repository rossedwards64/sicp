(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough-square? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough-square? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

;; exercise 1.3
(define (sum-largest-squares x y z)
  (cond ((and (< x y) (< x z)) (apply + (map square `(,y ,z))))
	((and (< y x) (< y z)) (apply + (map square `(,x ,z))))
	(else (apply + (map square `(,x ,y))))))

;; exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4.0 5.0))))) (* 3 (- 6 2) (- 2 7)))
