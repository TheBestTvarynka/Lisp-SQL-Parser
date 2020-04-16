
(defun createArray (size initialValue)
  "function that create array with length of 'size' where all elements are equals to initialValue"
  (make-array size :initial-element initialValue)
  )

(defun isSegmented (data)
  "check if input data is segmented"
  (and (arrayp (aref data 0)) (not (stringp (aref data 0))))
  )

(defun simpleCount (column)
  "count number of not-null elements in column"
  (reduce (lambda (cnt elem)
			(cond
              ((not elem) cnt)
              (t (+ cnt 1))
              )
            )
           column
           :initial-value 0)
  )

(defun segmentedAggregateFunction (fn column)
  "execute aggregate function 'fn' on each segment of 'column'"
  (reduce (lambda (resColumn elem)
			(vector-push-extend (funcall fn elem) resColumn)
			resColumn
			)
		  column
		  :initial-value (make-array 0 :fill-pointer 0))
  )

(defun countRows (table)
  "count aggregate funciton"
  (setf table (funcall table))
  (make-table :columnNames "?column?"
			  :data (cond
					  ((isSegmented (table-data table)) (segmentedAggregateFunction 'simpleCount (table-data table)))
					  (t (createArray 1 (simpleCount (table-data table))))
					  )
			  )
  )

(defun getComparator (value)
  "returns function for comparing two element that have the same type as value"
  (cond
    ((numberp value) #'<)
    ((stringp value) #'string<)
    (t (lambda (v1 v2)T))
    )
  )

(defun simpleMax (column)
  "finds max element in column"
  (reduce (lambda (maxvalue elem)
	        (cond
		      ((and (not maxvalue) elem) elem)
		      ((not elem) maxvalue)
		      ((funcall (getComparator elem) maxvalue elem) elem)
		      (t maxvalue)
		      )
	        )
		  column
	      :initial-value nil)
  )

(defun maxRows (table)
  "max aggregate funciton"
  (setf table (funcall table))
  (make-table :columnNames "?column?"
			  :data (cond
					  ((isSegmented (table-data table)) (segmentedAggregateFunction #'simpleMax (table-data table)))
					  (t (createArray 1 (simpleMax (table-data table))))
					  )
			  )
  )

(defun simpleAverage (column)
  "determine average value on column"
  (let ((sum 0) (amount 0))
	(setq sum (reduce (lambda (sum elem)
						 (cond
						   ((not elem) sum)
						   (t (setq amount (+ amount 1))
							  (+ sum elem))
						   )
			           )
					 column
					 :initial-value 0))
	(cond
	  ((= amount 0) nil)
	  (t (/ (float sum) amount))
	  )
	)
  )

(defun findAverage (table)
  "this funciton determine average value in given column"
  (setf table (funcall table))
  (make-table :columnNames "?column?"
			  :data (cond
					  ((isSegmented (table-data table)) (segmentedAggregateFunction #'simpleAverage (table-data table)))
					  (t (createArray 1 (simpleAverage (table-data table))))
					  )
			  )
  )

(defun simpleSum (column)
  "sum all elements in column"
  (reduce (lambda (sum elem)
			(cond
			  ((not elem) sum)
			  (t (+ sum elem))
			  )
			)
		  column
		  :initial-value 0)
  )

(defun countSum (table)
  "this funciton determine average value in given column"
  (setf table (funcall table))
  (make-table :columnNames "?column?"
			  :data (cond
					  ((isSegmented (table-data table)) (aggregateSum (table-data table)))
					  (t (createArray 1 (simpleSum (table-data table))))
					  )
			  )
  )

