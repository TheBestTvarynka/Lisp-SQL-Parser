
(defun iterate (fn column index cnt vectors)
  (cond
    ((= index cnt) column)
    (t (let ((row (mapcar (lambda (vec)
                            (aref vec index)
                            )
                          vectors)))
         (vector-push-extend (apply fn row) column)
         (iterate fn column (+ index 1) cnt vectors)
         ))
    )
  )

(defun operator+ (fn1 fn2)
  (let ((col1 (funcall fn1))
		(col2 (funcall fn2)))
	(make-table :columnNames "?column?"
				:data (iterate #'+
							   (make-array 0 :fill-pointer 0)
							   0
							   (min (table-len col1) (table-len col2))
							   (list (table-data col1) (table-data col2)))
				)
	)
  )

(defun operator- (fn1 fn2)
  (let ((col1 (funcall fn1))
		(col2 (funcall fn2)))
	(make-table :columnNames "?column?"
				:data (iterate #'-
							   (make-array 0 :fill-pointer 0)
							   0
							   (min (table-len col1) (table-len col2))
							   (list (table-data col1) (table-data col2)))
				)
	)
  )

(defun operator* (fn1 fn2)
  (let ((col1 (funcall fn1))
		(col2 (funcall fn2)))
	(make-table :columnNames "?column?"
				:data (iterate #'*
							   (make-array 0 :fill-pointer 0)
							   0
							   (min (table-len col1) (table-len col2))
							   (list (table-data col1) (table-data col2)))
				)
	)
  )

(defun operator/ (fn1 fn2)
  (let ((col1 (funcall fn1))
		(col2 (funcall fn2)))
	(make-table :columnNames "?column?"
				:data (iterate #'/
							   (make-array 0 :fill-pointer 0)
							   0
							   (min (table-len col1) (table-len col2))
							   (list (table-data col1) (table-data col2)))
				)
	)
  )

(defun subString (str start n)
  (subseq str start (+ start n))
  )

(defun substr (str from n)
  (let ((colStr (funcall str))
		(colFrom (funcall from))
		(colN (funcall n)))
	(make-table :columnNames "?column?"
				:data (iterate #'subString
							   (make-array 0 :fill-pointer 0)
							   0
							   (table-len colStr)
							   (list (table-data colStr) (table-data colFrom) (table-data colN))))
	)
  )

(defun concatStr (str1 str2)
  (concatenate 'string str1 str2)
  )

(defun concat (str1 str2)
  (let ((colStr1 (funcall str1))
		(colStr2 (funcall str2)))
	(make-table :columnNames "?column?"
				:data (iterate #'concatStr
							   (make-array 0 :fill-pointer 0)
							   0
							   (table-len colStr1)
							   (list (table-data colStr1) (table-data colStr2))))
	)
  )

(defun as (column name)
  (make-table :columnNames (aref (table-data (funcall name)) 0)
			  :data (table-data (funcall column)))
  )

(defun getFunction (fnname)
  (cond
	((string= fnname "+") #'operator+)
	((string= fnname "-") #'operator-)
	((string= fnname "*") #'operator*)
	((string= fnname "/") #'operator/)
	((string= fnname "substr") #'substr)
	((string= fnname "concat") #'concat)
	; add aggregate functions
	((string= fnname "count") #'countRows)
	((string= fnname "max") #'maxRows)
	((string= fnname "avg") #'findAverage)
	((string= fnname "as") #'as)
	(t nil)
	)
  )

(defun getArgumentAmount (fnname)
  (cond
	((string= fnname "+") 2)
	((string= fnname "-") 2)
	((string= fnname "*") 2)
	((string= fnname "/") 2)
	((string= fnname "substr") 3)
	((string= fnname "concat") 2)
	; add aggregate functions
	((string= fnname "count") 1)
	((string= fnname "max") 1)
	((string= fnname "avg") 1)
	((string= fnname "as") 2)
	(t nil)
	)
  )
