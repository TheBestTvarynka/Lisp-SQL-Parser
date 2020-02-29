
(defun condd(i)(cond ((= i 5) 
							   (write 5)
							   (write "weferf")
							   )
					 (t (condd 5)(write "pasha")(write "pacha"))
					 ))

(condd 6)
