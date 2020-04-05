
(load "importer.lisp")
(load "textprocessing.lisp")
; delete it later
(load "print.lisp")

(defun getEqual (value)
  "returns function for checking if two values is equal"
  (cond
        ((numberp value) #'=)
        ((stringp value) #'string=)
        (t (lambda (v1 v2)T))
        )
  )

(defun getJoinTableName (joinStr)
  (string-trim " " (subseq joinStr (+ (search "join" joinStr) 4) (search "on" joinStr)))
  )

(defun getJoinType (joinStr)
  (setf joinStr (string-left-trim " " joinStr))
  (string-right-trim " " (subseq joinStr 0 (search "join" joinStr)))
  )

(defun getJoinColumns (joinStr tableName)
  (let ((params (mapcar (lambda (str)(string-trim " " str))
		                (split-str (subseq joinStr (+ (search "on" joinStr) 3)) #\=))))
	(cond
	  ((starts-with (nth 0 params) tableName) params)
	  (t (reverse params))
	  )
	)
  )

(defun findRow (value index data)
  (find-if (lambda (row)
			 (cond ((funcall (getEqual value) value (aref row index)) row)(t nil))
			)
		   data)
  )

(defun makeEmptyRow (size)
  (make-array size :initial-element nil)
  )

(defun concatenateRowsByIndex (index1 data1 index2 data2)
  (concatenate 'vector data1 (subseq data2 0 index2) (subseq data2 (+ index2 1)))
  )

(defun innerJoin (rowIndex col1 data1 col2 data2)
  (reduce (lambda (resData curRow)
			(let ((newRow (findRow (aref curRow col1) col2 data2)))
			  (cond
				((not newRow) resData)
				(t (vector-push-extend (concatenateRowsByIndex col1 curRow col2 newRow) resData)
				   resData)
				)
			  )
			)
		  data1
		  :initial-value (make-array 0 :fill-pointer 0))
  )

(defun sideJoin (rowIndex col1 data1 col2 data2)
  (reduce (lambda (resData curRow)
			(let ((newRow (findRow (aref curRow col1) col2 data2)))
			  (cond
				((not newRow)
				 (vector-push-extend (concatenateRowsByIndex col1 curRow col2 (makeEmptyRow (length (aref data2 0)))) resData)
				 resData)
				(t
				  (vector-push-extend (concatenateRowsByIndex col1 curRow col2 newRow) resData)
				  resData)
				)
			  )
			)
		  data1
		  :initial-value (make-array 0 :fill-pointer 0))
  )

(defun joinData (joinType col1 data1 col2 data2 table)
  (setf (table-data table) (cond
							 ((string= joinType "left") (sideJoin 0 col1 data1 col2 data2))
							 ((string= joinType "right") (sideJoin 0 col2 data2 col1 data1))
							 ((string= joinType "inner") (innerJoin 0 col1 data1 col2 data2))
							 (t nil)))
  table
  )

(defun addIndexes (table)
  (setf (table-columnIndexes table) (makeIndexHashMap (table-columnNames table)))
  (copy-table table)
  )

(defun joinTables (joinType params table1 table2)
  (let ((col1 (nth 0 (gethash (nth 0 params) (table-columnIndexes table1))))
		(col2 (nth 0 (gethash (nth 1 params) (table-columnIndexes table2)))))
	;(pprint (list col1 col2))
	(let ((resTable (addIndexes (make-table :tableName ""
							                :columnNames (concatenateRowsByIndex col1
																                 (table-columnNames table1)
																                 col2
																                 (table-columnNames table2))))))
	  ;(pprint resTable)
	  ;(terpri)
	  (joinData joinType col1 (table-data table1) col2 (table-data table2) (copy-table resTable))
	  )
	)
  )

(defun concatenateNames (tableName columnNames)
  (reduce (lambda (newNames colName)
			(vector-push-extend (concatenate 'string tableName "." colName) newNames)
			newNames
			)
		  columnNames
		  :initial-value (make-array 0 :fill-pointer 0))
  )

(defun addTablename (table)
  (let ((columnNames (table-columnNames table)))
	(setf (table-columnNames table) (concatenateNames (table-tableName table) columnNames))
	(setf (table-columnIndexes table) (makeIndexHashMap (table-columnNames table)))
	table
	)
  )

(defun join (joinStr table tables)
  ;(pprint joinStr)
  (setf joinStr (string-trim " " joinStr))
  (let ((additionalTable (copy-table (gethash (getJoinTableName joinStr) tables))))
	(joinTables (getJoinType joinStr)
				(getJoinColumns joinStr (table-tableName table))
				(addTablename table)
				(addTablename additionalTable))
	)
  )

#||
(defvar tables (make-hash-table :test 'equal))
(setf (gethash "test" tables) (readTableFromFile "datasource/test.csv"))
(setf (gethash "test2" tables) (readTableFromFile "datasource/test2.csv"))
(printTable (join "  left join test on test.id = test2.id" (gethash "test2" tables)  tables))
;||#
