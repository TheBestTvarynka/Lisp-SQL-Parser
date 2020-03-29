; only for integers
(defun pow(n s)(cond ((= s 0) 1)
					  (t (* n (pow n (- s 1))))
					  ))
(defun generateSequence(n)(cond ((< n 1) '())
								((= n 1) '(1))
								(t (append
									 (generateSequence (- n 1))
									 (list (* (pow -1 (- n 1)) (pow 2 (- n 1))))))
								))
(write (generateSequence (read)))

