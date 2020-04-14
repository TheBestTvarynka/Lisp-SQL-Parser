(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(defun getComparator (value)
  "returns function for comparing two element that have the same type as value"
  (cond
	((numberp value) #'<)
	((stringp value) #'string<)
	(t (lambda (v1 v2)T))
	)
  )

(defun getEqual (value)
  "returns function that check if two element are qual. this elements have the same type as value"
  (cond
	((numberp value) #'=)
	((stringp value) #'string=)
	(t (lambda (v1 v2)T))
	)
  )

(defun compareVectors (index vector1 vector2)
  "this function compare vectors"
  (cond
    ((= index (array-total-size vector1)) nil)
    ((funcall (getEqual(aref vector1 index)) (aref vector1 index) (aref vector2 index)
	   )
     (compareVectors (+ index 1) vector1 vector2)
      )
    (t (funcall (getComparator (aref vector1 index)) (aref vector1 index) (aref vector2 index))
      )
    )
  )
(defun compareV (vector1 vector2)
  "run capmpateVectors function"
  (compareVectors 0 vector1 vector2)
  )

(defun sortTable (table)
  "sort table"
  (sort table #'compareV)
  )

(defun delete-nth (index arr)
  "delete element from 'arr' that have index 'index'"
  (delete-if (constantly t) arr :start index :count 1)
  )

(defun selectDistinct (index table)
  "select unique rows from sorted table"
  (cond
    ((= (+ index 1) (simple-table:num-rows table)) table)
    ((equalp (simple-table:get-row index table) (simple-table:get-row (+ index 1) table))
      (selectDistinct index (delete-nth index table))
      )
    (t (selectDistinct (+ index 1) table))
    )
  )

(defun distinct (resultTable)
  "select distinct"
  (setf (table-data resultTable) (selectDistinct 0 (sortTable (table-data resultTable))))
  resultTable
  )

(defun getDistinctFn ()
  "returns lambda that can execute distinct function"
  (lambda (resultTable)
	(distinct resultTable)
	)
  )

