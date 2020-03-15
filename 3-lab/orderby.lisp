
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

(defun orderBy(index order table)
  (setq table (sort table (orderVectors index (aref (aref table 0) index))))
  (cond
	((not order) (reverse table))
	(t table)
	)
  )
#||
(require 'asdf)
(load "/home/qkation/Documents/LispFunctionalProgramming/3-lab/cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(defvar table (simple-table:read-csv #P"test.csv" t))
(pprint table)
(pprint (orderBy 2 t table)) 
||#
