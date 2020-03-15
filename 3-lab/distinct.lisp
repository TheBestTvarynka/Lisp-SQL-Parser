
(defun getComparator (value)
  (cond
	((numberp value) #'<)
	((stringp value) #'string<)
	(t (lambda (v1 v2)T))
	)
  )

(defun getEqual (value)
  (cond
	((numberp value) #'=)
	((stringp value) #'string=)
	(t (lambda (v1 v2)T))
	)
  )

(defun compareVectors (index vector1 vector2)
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
(defun compareV (vector1 vector2)(compareVectors 0 vector1 vector2))

(defun sortTable (table)
  (sort table #'compareV)
  )

(defun delete-nth (index arr)
  (delete-if (constantly t) arr :start index :count 1)
  )

(defun selectDistinct (index table)
  (cond
    ((= (+ index 1) (simple-table:num-rows table)) table)
    ((equalp (simple-table:get-row index table) (simple-table:get-row (+ index 1) table))
      (selectDistinct index (delete-nth index table))
      )
    (t (selectDistinct (+ index 1) table))
    )
  )

(defun distinct (table)
  (selectDistinct 0 (sortTable table))
  )

#||
(require 'asdf)
(load "/home/qkation/Documents/LispFunctionalProgramming/3-lab/cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(defvar table (simple-table:read-csv #P"test.csv" t))
(pprint table)
(terpri)
(pprint (sortTable table))

(terpri)
(pprint (distinct table))
||#

