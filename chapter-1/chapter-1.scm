;; exercise 1.8
(define (cbrt x)
  (cbrt-iter 1.0 x))

(define (cbrt-iter guess x)
  (if (good-enough-cube? guess (improve-guess-cube guess x))
      guess
      (cbrt-iter (improve-guess-cube guess x) x)))

(define (good-enough-cube? guess prev-guess)
  (< (abs (/ (- guess prev-guess) guess)) 0.00000000001))

(define (improve-guess-cube guess x)
  (/ (+ (/ x (pow guess 2)) (* guess 2)) 3.0))

(define (cube x)
  (pow x 3))

;; exercise 1.7
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough-square? guess (improve-guess-square guess x))
      guess
      (sqrt-iter (improve-guess-square guess x) x)))

(define (improve-guess-square guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough-square? guess prev-guess)
  (< (abs (/ (- guess prev-guess) guess)) 0.00000000001))

(define (square x)
  (pow x 2))

;; exercise 1.3
(define (sum-largest-squares x y z)
  (apply + (map square (cond ((and (< x y) (< x z)) `(,y ,z))
			     ((and (< y x) (< y z)) `(,x ,z))
			     (else `(,x ,y))))))

;; exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4.0 5.0))))) (* 3 (- 6 2) (- 2 7)))

(define (pow x exp)
  (cond ((= exp 0) 1)
	((= exp 1) x)
	(else (letrec ((pow-iter (lambda (cur i)
				   (if (not (= i 1))
				       (pow-iter (* cur x) (- i 1))
				       cur))))
		(pow-iter x exp)))))
