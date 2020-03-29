(defun !(n)(cond ((= n 1) 1)
				 ((= n 0) 1)
				 (t (* n (- n 1)))))

(write (! 8))
