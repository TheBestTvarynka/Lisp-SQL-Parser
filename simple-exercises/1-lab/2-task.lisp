; my inplementation of nth function
(defun mynth(i lst)(cond ((< i 0) nil)
						((null lst) nil)
						((= i 0) (car lst))
						(t (mynth (- i 1) (cdr lst)))
						))
#|
; test mynth
(write (mynth 0 '(1 2 3 4 5 6)))
(write (mynth 1 '(1 2 3 4 5 6)))
(write (mynth 4 '(1 2 3 4 5 6)))
(write (mynth 5 '(1 2 3 4 5 6)))

(write (mynth -1 '(1 2 3 4 5 6)))
(write (mynth 6 '(1 2 3 4 5 6)))
|#
(defun createlist1(l1 l2 l3)
  (list (mynth 5 l1) (mynth 4 l2) (mynth 7 l3))
  )
(defun createlist2(l1 l2 l3)
  (list (nth 5 l1) (nth 4 l2) (nth 7 l3))
  )

(write (createlist1 '(Z X C S A D F) '((R) (30) (3) 23) '(U I 8 9 6 5 4 3 (1 2 3))))
(terpri)
(write (createlist2 '(Z X C S A D F) '((R) (30) (3) 23) '(U I 8 9 6 5 4 3 (1 2 3))))
