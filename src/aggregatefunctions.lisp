
(defun createArray (size initialValue)
  (make-array size :initial-element initialValue)
  )

(defun isSegmented (data)
  (vectorp (aref data 0))
  )

(defun simpleCount (column)
  (reduce (lambda (cnt elem)
			(cond
              ((not elem) cnt)
              (t (+ cnt 1))
              )
            )
           column
           :initial-value 0)
  )

(defun aggregateCount (column)
  (reduce (lambda (resColumn elem)
			(vector-push-extend (simpleCount elem) resColumn)
			resColumn
			)
		  column
		  :initial-value (make-array 0 :fill-pointer 0))
  )

(defun countRows (table)
  "count aggregate funciton"
  (setf table (funcall table))
  (make-table :columnNames "?column?"
			  :data (cond ((isSegmented (table-data table)) (aggregateCount (table-data table)))(t (createArray 1 (simpleCount (table-data table))))))
  )

(defun getComparator (value)
  "returns function for comparing two element that have the same type as value"
  (cond
    ((numberp value) #'<)
    ((stringp value) #'string<)
    (t (lambda (v1 v2)T))
    )
  )

(defun maxRows (table)
  "max aggregate funciton"
  (setf table (funcall table))
  (make-table :columnNames "?column?"
			  :data (createArray 1 (reduce (lambda (maxvalue elem)
											  (cond
												((and (not maxvalue) elem) elem)
												((not elem) maxvalue)
												((funcall (getComparator elem) maxvalue elem) elem)
												(t maxvalue)
												)
											  )
											(table-data table)
											:initial-value nil)))
  )

(defun findAverage (table)
  "this funciton determine average value in given column"
  (setf table (funcall table))
  (let ((sum 0) (amount 0))
	(setq sum (reduce (lambda (sum elem)
						 (cond
						   ((not elem) sum)
						   (t (setq amount (+ amount 1))
							  (+ sum elem))
						   )
			           )
					 (table-data table)
					 :initial-value 0))
	(make-table :columnNames "?column?"
				:data (createArray 1 (cond
										((= amount 0) nil)
										(t (/ (float sum) amount))
										)))
	)
  )


