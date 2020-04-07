
(load "importer.lisp")
(load "textprocessing.lisp")
(load "orderby.lisp")

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

(defun concatenateRows (data1 data2)
  (concatenate 'vector data1 data2)
  )

(defun addEndRow (resData row size)
  (vector-push-extend (concatenateRows (makeEmptyRow (- size (length row))) row) resData)
  resData
  )

(defun addBeginRow (resData row size)
  (vector-push-extend (concatenateRows row (makeEmptyRow (- size (length row)))) resData)
  resData
  )

(defun innerJoin (rowIndex col1 data1 col2 data2)
  (reduce (lambda (resData curRow)
			(let ((newRow (findRow (aref curRow col1) col2 data2)))
			  (cond
				((not newRow) resData)
				(t (vector-push-extend (concatenateRows curRow newRow) resData)
				   resData)
				)
			  )
			)
		  data1
		  :initial-value (make-array 0 :fill-pointer 0))
  )

(defun addNilRows (restData resData size)
  (reduce (lambda (res row)(addBeginRow resData row size))
		  restData
		  :initial-value resData)
  )

(defun deleteFirstValues (value index data)
  (cond
	((= (aref (aref data 0) index) value) (deleteFirstValues value index (subseq data 1)))
	(t data)
	)
  )

(defun joinRowsByValue (col1 row col2 data2 resData)
  (pprint (list "IN joinRowsByValue: " col1 col2))
  (cond
	((= (length data2) 0) resData)
	((= (aref row col1) (aref (aref data2 0) col2))
	 (vector-push-extend (concatenateRows row (aref data2 0)) resData)
	 ;(joinRowsByValue (aref row col1) row col2 (subseq data2 1) resData))
	 (joinRowsByValue col1 row col2 (subseq data2 1) resData))
	(t resData)
	)
  )

(defun sideJoin (col1 data1 col2 data2 resData size)
  (pprint (list col1 col2))
  (cond
	((= (length data1) 0) resData)
	((= (length data2) 0) (addNilRows data1 resData size))
	(t (let ((elem1 (aref (aref data1 0) col1))
			 (elem2 (aref (aref data2 0) col2)))
		 (cond
		   ((> elem1 elem2) (sideJoin col1 data1 col2 (deleteFirstValues elem2 col2 data2) resData size))
		   ((< elem1 elem2) (sideJoin col1 (subseq data1 1) col2 data2 (addBeginRow resData (aref data1 0) size) size))
		   (t (sideJoin col1 (subseq data1 1) col2 data2 (joinRowsByValue col1 (aref data1 0) col2 data2 resData) size))
		   )
		 ))
	)
  )

(defun joinData (joinType col1 data1 col2 data2 table size)
  (setf (table-data table) (cond
							 ((string= joinType "left") (sideJoin col1 data1 col2 data2 (make-array 0 :fill-pointer 0) size))
							 ((string= joinType "right") (sideJoin col2 data2 col1 data1 (make-array 0 :fill-pointer 0) size))
							 ((string= joinType "inner") (innerJoin 0 col1 data1 col2 data2))
							 ((string= joinType "full outer") nil)
							 (t nil)
							 ))
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
							                :columnNames (concatenateRows (table-columnNames table1)
																                 (table-columnNames table2))))))
	  (joinData joinType
				col1
				(table-data (orderBy (nth 0 params) table1))
				col2 (table-data (orderBy (nth 1 params) table2))
				(copy-table resTable)
				(+ (table-column-number table1) (table-column-number table2)))
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
