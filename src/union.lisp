
(load "importer.lisp")

(defun unionData (data1 data2)
  (concatenate 'vector data1 data2)
  )

(defun unionTables (fn1 fn2)
  (let ((table1 (funcall fn1))
		(table2 (funcall fn2)))
    (make-table :tableName "Result"
  			    :columnNames (table-columnNames table1)
  			    :columnIndexes (table-columnIndexes table1)
  			    :data (unionData (table-data table1) (table-data table2)))
	)
  )

