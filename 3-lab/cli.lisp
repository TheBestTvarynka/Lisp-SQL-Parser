
; load all functionality code
(load "importer.lisp")
(load "distinct.lisp")
(load "where.lisp")
(load "orderby.lisp")
(load "select.lisp")

; tables - hashmap where key is tablename and value is a table
(defvar tables (make-hash-table :test 'equal))
(setf (gethash "map_zal-skl9" tables) (readTableFromFile "datasource/map_zal-skl9.csv"))
(setf (gethash "mp-assistants" tables) (readTableFromFile "datasource/mp-assistants.csv"))
(setf (gethash "mp-posts_full" tables) (readTableFromFile "datasource/mp-posts_full.csv"))
(setf (gethash "mps-declarations_rada" tables) (readTableFromFile "datasource/mps-declarations_rada.json"))
(setf (gethash "test" tables) (readTableFromFile "datasource/test.csv"))
;(setf (gethash "plenary_register_mps-skl9" tables) mps_declarations_rada)

(defun len (value)
  "returns length of value"
  (cond
	((not value) 3)
	((stringp value) (length value))
	((numberp value) (length (write-to-string value)))
	(t 1)
	)
  )

(defun countWidths (table)
  "returns array that contain width of each column of the table"
  (let ((columnsAmount (length (aref table 0))))
    (reduce (lambda (widths row)
  			(map 'vector
  				 (lambda (width elem)
  				   (max width (len elem))
  				   )
  				 widths
  				 row)
  			)
  		  table
  		  :initial-value (make-array columnsAmount :fill-pointer columnsAmount))
	)
  )

(defun getTableName (queryStr)
  "cut table name from query"
  (setq queryStr (string-left-trim " " (subseq queryStr (+ (search "from" queryStr) 4))))
  (let ((spacePosition (position #\SPACE queryStr)))
	(setq spacePosition (cond
						  ((not spacePosition) (length queryStr))
						  (t spacePosition)
						  ))
	(subseq queryStr 0 spacePosition)
	)
  )

(defun cutWhereClause (queryStr)
  "cut where clause from queryStr. example:
  'cond1 and cond2 order by col2 limit 5' -> 'cond1 and cond2'"
  (let ((curCond (getOperator queryStr)))
	(setq queryStr (removeOperator queryStr))
	(setq curCond (concatenate 'string curCond " " (getOperator queryStr)))
	(setq queryStr (removeOperator queryStr))
	(setq curCond (concatenate 'string curCond " " (getOperator queryStr)))
	(setq queryStr (removeOperator queryStr))
	(cond
	  ((string= queryStr "") curCond)
	  ((or (string= (getOperator queryStr) "and") (string= (getOperator queryStr) "or"))
	   (concatenate 'string
					curCond
					" "
					(getOperator queryStr)
					" "
					(cutWhereClause (removeOperator queryStr))
					)
	   )
	  (t curCond)
	  )
	)
  )

(defun getWhere (queryStr)
  "cut where clause from queryStr. example:
  'select col1, * from table where cond1 and cond2 order by col2 limit 5' -> 'cond1 and cond2'"
  (let ((startPosition (search "where" queryStr)))
	(cond
	  ((not startPosition) "")
	  (t (cutWhereClause (subseq queryStr (+ startPosition 5))))
	  )
	)
  )

(defun getOrderBy (queryStr)
  (let ((startPosition (search "order by" queryStr)))
	(cond
	  ((not startPosition) "")
	  (t (getOperator (subseq queryStr (+ startPosition 8))))
	  )
	)
  )

(defun getOrderDirection (queryStr)
  (let ((startPosition (search "order by" queryStr))
		(direction ""))
	(cond
	  ((not startPosition) "")
	  (t (setq direction (getOperator (removeOperator (subseq queryStr (+ startPosition 8)))))
		 (cond
		   ((or (string= direction "asc") (string= direction "desc")) direction)
		   (t "")
		   ))
	  )
	)
  )

(defun ifDistinct (queryStr)
  "check if query contain distinct keyword"
  (cond
	((not (search "distinct" queryStr)) nil)
	(t t)
	)
  )

(defun getColumns(queryStr)
  "cut columns names from query and return tham as list. example:
  'select col1, col2, * from table' -> '(col1 col2 *)"
  (let ((startPosition (search "distinct" queryStr)) (endPosition (search "from" queryStr)))
	(setq startPosition (cond
						  ((not startPosition) (+ (search "select" queryStr) 6))
						  (t (+ startPosition 8))
						  ))
	(setq queryStr (string-trim " " (subseq queryStr startPosition endPosition)))
	(map 'list
	  (lambda (value)(string-trim " " value))
	  (simple-table:split-string #\, queryStr))
	)
  )

(defun getFormatString (width elem)
  "construct control-string for format function"
  (cond
	((stringp elem) (concatenate 'string "~" (write-to-string width) "A"))
	((numberp elem) (concatenate 'string "~" (write-to-string width) "D"))
	(t (concatenate 'string "~" (write-to-string width) "A"))
	)
  )

(defun printRow (widths row)
  "this function just print row"
  (let ((lastWidth (aref widths (- (length widths) 1))) (lastElem (aref row (- (length row) 1))))
    (map 'list
  	     (lambda (width elem)
  		   (princ (format nil (getFormatString width elem) elem))
		   (princ " | ")
  		   )
  		  widths
  		  (delete-if (constantly t) row :count 1 :from-end t))
	(princ (format nil (getFormatString lastWidth lastElem) lastElem))
	)
  )

(defun printTable (resultTable)
  "just print table function in pretty way"
	(let ((widths (countWidths (formatTable resultTable))))
	  (printRow widths (table-columnNames resultTable))
	  (terpri)
	  (princ (format nil "~v@{~A~:*~}" (+ (reduce (lambda (sum width)
										            (+ sum width)
										            )
										          widths) (* (- (length widths) 1) 3)) #\-))
	  (terpri)
	  (map 'list
		   (lambda (row)
		     (printRow widths row)
		     (terpri)
			 )
		   (table-data resultTable))
		)
  )

(defun query (queryStr)
  "this function parse query"
  (let ((columns (getColumns queryStr))
		(tableName (getTableName queryStr))
		(resultTable (make-table))
		(indexes #()))
	; take a table
	(setq resultTable (copy-table (gethash tableName tables)))
	; take a table indexes
	(setq indexes (makeIndexHashMap (table-columnNames resultTable)))
	; where
	(setq resultTable (where (getWhere queryStr) indexes resultTable))
	; order by
	(setq resultTable (orderby (nth 0 (gethash (getOrderBy queryStr) indexes))
							   (getOrderDirection queryStr) resultTable))
	; select columns (or execute aggregate functions)
	(setq resultTable (select columns indexes resultTable))
	; distinct
	(setq resultTable (cond
						((not (ifDistinct queryStr)) resultTable)
						(t (distinct resultTable)
						   )
						))
	; print table
	(printTable resultTable)
	)
  )

(defun loadTable (tableName)
  "load table command: print whole table"
  (query (concatenate 'string "select * from " tableName))
  )

(defun execute (commandQuery)
  "execute entered text"
  (let ((command (parseCommand commandQuery)))
	(cond
	  ((string= command "exit") (exit))
	  ((string= command "query") (query (cutParameter commandQuery)))
	  ((string= command "load") (loadTable (cutParameter commandQuery)))
	  (t (pprint "Error: entered command not fund!!!"))
	  )
	)
  )

(defun run ()
  "run cli"
  (terpri)
  (princ "[user@host ~]$: ")
  (terpri)
  (execute (read-line))
  (run)
 )

(run)

