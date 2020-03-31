
(load "importer.lisp")
(load "textprocessing.lisp")

(defun getComparator (value)
  "returns function for comparing in asc order corresponds to value type"
  (cond
        ((numberp value) #'<)
        ((stringp value) #'string<)
        (t (lambda (v1 v2)T))
        )
  )

(defun getComparatorReverse (value)
  "returns function for comparing in desc order corresponds to value type"
  (cond
        ((numberp value) #'>)
        ((stringp value) #'string>)
        (t (lambda (v1 v2)T))
        )
  )

(defun getEqual (value)
  "returns function for checking if two values is equal"
  (cond
        ((numberp value) #'=)
        ((stringp value) #'string=)
        (t (lambda (v1 v2)T))
        )
  )

(defun createComparator (order value)
  (cond
	((not order) (getComparator value))
	((string= order "asc") (getComparator value))
	((string= order "desc") (getComparatorReverse value))
	(t "(lambda (v1 v2)T)")
	)
  )

(defun createHashMap (orderStr table)
  (let ((columns (mapcar (lambda (col)(string-trim " " col)) (split-str orderStr #\,)))
		(indexes (table-columnIndexes table)))
	(setf columns (mapcar (lambda (col)(split-str col #\SPACE)) columns))
	(let ((comparators (make-hash-table :test 'equal)))
	  (mapcar (lambda (section)
				(let ((index (nth 0 (gethash (nth 0 section) indexes))))
				  (setf (gethash index comparators) (createComparator (nth 1 section) (table-value 0 index table)))
				  )
				)
			  columns)
	  comparators
	  )
	)
  )

(defun compareRows (indexes comparators row1 row2)
  (cond
	((= (length indexes) 0) nil)
	(t (let ((elem1 (aref row1 (aref indexes 0)))
			 (elem2 (aref row2 (aref indexes 0))))
		 (cond
		   ((funcall (getEqual elem1) elem1 elem2)
			(compareRows (subseq indexes 1) comparators row1 row2))
		   (t
			 (funcall (gethash (aref indexes 0) comparators) elem1 elem2))
		   )
		 ))
	)
  )

(defun getVectorComparator (comparators)
  "create function (lambda) that compare two vectors by one element with index 'index'"
  (let ((indexes (make-array 0 :fill-pointer 0)))
	(maphash (lambda (key value)(vector-push-extend key indexes)) comparators)
	(lambda (row1 row2)
	  (compareRows indexes comparators row1 row2)
	  )
	)
  )

(defun orderBy (orderStr resultTable)
  "order table by column with index 'index' and order 'order'"
  (setf (table-data resultTable)
		(sort (table-data resultTable)
			  (getVectorComparator (createHashMap orderStr resultTable))))
  resultTable
  )

