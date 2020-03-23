
(defun getComparator (value)
  (cond
        ((numberp value) #'<)
        ((stringp value) #'string<)
        (t (lambda (v1 v2)T))
        )
  )

(defun orderVectors (index value)
  (let ((comparator (getComparator value)))
	(lambda (vector1 vector2)(funcall comparator (aref vector1 index) (aref vector2 index)))
	)
  )

(defun orderBy (index order resultTable)
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
#||
(require 'asdf)
(load "/home/qkation/Documents/LispFunctionalProgramming/3-lab/cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(defvar table (simple-table:read-csv #P"datasourse/test.csv" t))
(pprint table)
(terpri)
(pprint (orderBy 2 t table)) 
;||#
