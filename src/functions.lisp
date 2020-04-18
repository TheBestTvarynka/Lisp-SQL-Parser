
(defun iterate (fn column index cnt vectors)
  "this function iterates through vectors (list of columns).
  `cnt` - count of all rows. `index` - current index. `column` - the result of iterating.
  takes element with index `index` from each column from vectors and makes a row.
  then executes `fn` functon on this row. result pushes to `column`"
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

(defun operator (operatorFn fn1 fn2)
  "executes `operatorFn` (binary function) on columns `fn1` and `fn2` and return new column with result"
  (let ((col1 (funcall fn1))
		(col2 (funcall fn2)))
	(make-table :columnNames "?column?"
				:data (iterate operatorFn
							   (make-array 0 :fill-pointer 0)
							   0
							   (min (table-len col1) (table-len col2))
							   (list (table-data col1) (table-data col2)))
				)
	)
  )

(defun operator+ (fn1 fn2)
  "`plus` functions"
	(operator #'+ fn1 fn2)
  )

(defun operator- (fn1 fn2)
  "`minus` function"
  (operator #'- fn1 fn2)
  )

(defun operator* (fn1 fn2)
  "`mistiply` function"
  (operator #'* fn1 fn2)
  )

(defun operator/ (fn1 fn2)
  "`devide` function"
  (operator #'/ fn1 fn2)
  )


(defun operator< (fn1 fn2)
  "`less` function"
  (operator #'< fn1 fn2)
  )

(defun operator> (fn1 fn2)
  "`bigger` function"
  (operator #'> fn1 fn2)
  )

(defun isEqual (val1 val2)
  "return function for comparing values corresponds to they type"
  (cond
	((stringp val1) (string= val1 val2))
	(t (= val1 val2))
	)
  )

(defun operator= (fn1 fn2)
  "`equal` function"
  (operator #'isEqual fn1 fn2)
  )

(defun subString (str start n)
  "returns substring of `str` that starts on `start` index and have length `n`"
  (subseq str start (+ start n))
  )

(defun substr (str from n)
  "substring function"
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
  "`concatenate` two strings"
  (concatenate 'string str1 str2)
  )

(defun concat (str1 str2)
  "`concatenate` function"
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
  "`as` function"
  (make-table :columnNames (aref (table-data (funcall name)) 0)
			  :data (table-data (funcall column)))
  )

(defun getFunction (fnname)
  "returns function corresponds to its name"
  (cond
	((string= fnname "+") #'operator+)
	((string= fnname "-") #'operator-)
	((string= fnname "*") #'operator*)
	((string= fnname "/") #'operator/)
	((string= fnname ">") #'operator>)
	((string= fnname "<") #'operator<)
	((string= fnname "=") #'operator=)
	((string= fnname "substr") #'substr)
	((string= fnname "concat") #'concat)
	; add aggregate functions
	((string= fnname "count") #'countRows)
	((string= fnname "max") #'maxRows)
	((string= fnname "avg") #'findAverage)
	((string= fnname "sum") #'countSum)
	((string= fnname "as") #'as)
	(t nil)
	)
  )

(defun getArgumentAmount (fnname)
  "returns number of function arguments corresponds to its name"
  (cond
	((string= fnname "+") 2)
	((string= fnname "-") 2)
	((string= fnname "*") 2)
	((string= fnname "/") 2)
	((string= fnname ">") 2)
	((string= fnname "<") 2)
	((string= fnname "=") 2)
	((string= fnname "substr") 3)
	((string= fnname "concat") 2)
	; add aggregate functions
	((string= fnname "count") 1)
	((string= fnname "max") 1)
	((string= fnname "avg") 1)
	((string= fnname "sum") 1)
	((string= fnname "as") 2)
	(t nil)
	)
  )

(defun findCorrect (&rest lst)
  "lst looks like: #(bool1 val1 bool2 val2 else). Example: #(nil 5 t 3 t 4 nil 2 0).
  this function search first true value of all bool values and returns the next value after found bool.
  if bool with true is not founded then returns the last element of the list."
  (cond
	((= (length lst) 1) (nth 0 lst))
	(t (cond
		 ((nth 0 lst) (nth 1 lst))
		 (t (apply #'findCorrect (cdr (cdr lst))))
		 )
	   )
	)
  )

(defun makeCaseFunction (fns)
  "returns lambda that does `case` operation and returns the result of `case`"
  (let ((data (mapcar (lambda (table)(table-data table)) (mapcar (lambda (fn)(funcall fn)) fns))))
	(lambda ()
      (make-table :columnNames "?column?"
			      :data (iterate #'findCorrect
					  		     (make-array 0 :fill-pointer 0)
							     0
							     (length (nth 0 data))
							     data))
	  )
	)
  )
