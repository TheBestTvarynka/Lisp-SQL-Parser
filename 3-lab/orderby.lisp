
(defun getComparator (value)
  "returns function for comparing two element that have the same type as value"
  (cond
        ((numberp value) #'<)
        ((stringp value) #'string<)
        (t (lambda (v1 v2)T))
        )
  )

(defun orderVectors (index value)
  "create function (lambda) that compare two vectors by one element with index 'index'"
  (let ((comparator (getComparator value)))
	(lambda (vector1 vector2)(funcall comparator (aref vector1 index) (aref vector2 index)))
	)
  )

(defun orderBy (index order resultTable)
  "order table by column with index 'index' and order 'order'"
  (setq order (cond
				((string= order "asc") t)
				(t nil)
				))
  (let ((data (table-data resultTable)))
    (setf data (cond
			     ((not index) data)
			     (t (setq data (sort data (orderVectors index (aref (aref data 0) index))))
		   		    (cond
				      ((not order) (reverse data))
				      (t data)
				      ))
			     ))
	(setf (table-data resultTable) data)
	resultTable
	)
  )

