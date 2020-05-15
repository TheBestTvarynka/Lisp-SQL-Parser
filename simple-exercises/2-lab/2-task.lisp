;==== set value (val) in list (lst) with index (i) ====
(defun setVal(i val lst)(cond ((= i 0) (cons val (cdr lst)))
							  (t (cons (car lst) (setVal (- i 1) val (cdr lst))))
							  ))
;==== my swap function: swap two elements (i and j) in the list and return new list ====
(defun swap(lst i j)(setVal j (nth i lst) (setVal i (nth j lst) lst)))
;==== sift the element through the heap
(defun siftDown(lst i n)(cond ((>= (* i 2) n) lst)
							  ((= (+ 1 (* 2 i)) n) (cond ((> (nth i lst) (nth (+ 1 (* 2 i)) lst))
														  (swap lst i (+ 1 (* 2 i))))
														 (t lst)))
							  ((and (= (+ 1 (* 2 i)) n)
								    (> (nth i lst) (nth (+ 1 (* 2 i)) lst))
									) (swap lst i (+ 1 (* 2 i))))
							  ((and (>= (nth i lst) (nth (+ 1 (* 2 i)) lst))
									(<= (nth (+ 1 (* 2 i)) lst) (nth (+ 2 (* 2 i)) lst))
									) (siftDown (swap lst i (+ 1 (* 2 i))) (+ 1 (* 2 i)) n))
							  ((and (>= (nth i lst) (nth (+ 2 (* 2 i)) lst))
									(<= (nth (+ 2 (* 2 i)) lst) (nth (+ 1 (* 2 i)) lst))
									) (siftDown (swap lst i (+ 2 (* 2 i))) (+ 2 (* 2 i)) n))
							  (t lst)
							  ))
; ==== make a heap from input list ====
(defun heapify(lst i n)(cond ((< i 0) lst)
						     (t (heapify (siftDown lst i n) (- i 1) n))
						     ))
; ==== sort list in with input heap ====
(defun sortHeap(lst i)(cond ((= i 0) lst)
							(t (sortHeap (siftDown (swap lst 0 i) 0 (- i 1)) (- i 1)))
	         				))
; ==== sort function
(defun heapSort(lst)(sortHeap (heapify lst (- (floor (length lst) 2) 1) (- (length lst) 1))
							  (- (length lst) 1)
							  ))

(defun pow(n s)(cond ((= s 0) 1)
                     (t (* n (pow n (- s 1))))
                     ))
;==== generate ouw sequence ====
(defun generateSequence(n)(cond ((< n 1) '())
                                ((= n 1) '(1))
                                (t (append (generateSequence (- n 1))
                                           (list (* (pow -1 (- n 1)) (pow 2 (- n 1))))))
                                ))
;==== test heapSort
(write (heapSort (generateSequence 7)))
