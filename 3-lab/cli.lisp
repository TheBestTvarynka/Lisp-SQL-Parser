
(require 'asdf)
; load package for .csv and .tsv parsing
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)
; load package for .json parsing
(load "cl-json/cl-json.asd")
(asdf:load-system 'cl-json)

; load all functionality code
(load "convert.lisp")
(load "distinct.lisp")
(load "where.lisp")
(load "orderby.lisp")
(load "select.lisp")

; load parse files and save data to variables
(defvar map_zal (simple-table:read-csv #P"datasource/map_zal-skl9.csv" t))
(defvar mp_assistants (simple-table:read-csv #P"datasource/mp-assistants.csv" t))
(defvar mp_posts (simple-table:read-csv #P"datasource/mp-posts_full.csv"))
;(defvar plenary_register_mps (simple-table:read-tsv #P"datasource/plenary_register_mps-skl9.tsv"))
(defvar mps_declarations_rada(json:decode-json (open "datasource/mps-declarations_rada.json")))
(defvar mps_declarations_rada(json:decode-json (open "datasource/test.json")))
(setf mps_declarations_rada (convertToTable mps_declarations_rada))

; table for testing
(defvar test (simple-table:read-csv #P"datasource/test.csv" t))

; tables - hashmap where key is tablename and value is table
(defvar tables (make-hash-table :test 'equal))
(setf (gethash "map_zal-skl9" tables) map_zal)
(setf (gethash "mp-assistants" tables) mp_assistants)
(setf (gethash "mp-posts_full" tables) mp_posts)
(setf (gethash "mps-declarations_rada" tables) mps_declarations_rada)
(setf (gethash "test" tables) test)
;(setf (gethash "plenary_register_mps-skl9" tables) mps_declarations_rada)

(defun generateSequence (n)
  "generate sequence like '(0 1 2 3 ... )"
  (cond ((< n 0) '())
	((= n 0) '(0))
	(t (append
		 (generateSequence (- n 1))
		 (list n)))
	)
  )

(defun makeIndexes (i row hashTable)
  "make hashMap there key is column name and value is its sequence number.
  like: (col1 -> '(0))
  		(col2 -> '(1)) ..."
  (cond ((< i 0) hashTable)
		(t (setf (gethash (aref row i) hashTable) (list i))
		   (makeIndexes (- i 1) row hashTable)
		  )
		)
  )

(defun makeIndexHashMap (row)
  "make full hashmap with indexes. like:
  (col1 -> '(0))
  ...
  (* -> '(0 1 2 ...))"
  (let ((tmpHashTable (make-hash-table :test 'equal)))
	(setf (gethash "*" tmpHashTable) (generateSequence (- (length row) 1)))
    (makeIndexes (- (length row) 1) row tmpHashTable)
	)
  )

; indexTables - hash table that contain all index hashmaps (index hashmap for each table)
; key is tablename and value is index hashmap
(defvar indexTables (make-hash-table :test 'equal))
(maphash #'(lambda (tableName table)
			 (setf (gethash tableName indexTables)
				   (makeIndexHashMap (simple-table:get-row 0 table))
				   )
			 )
		 tables)

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

; widthTables hash table where key is table name and value is array with width of each column
(defvar widthTables (make-hash-table :test 'equal))
(maphash #'(lambda (tableName table)
			 (setf (gethash tableName widthTables) (countWidths (gethash tableName tables)))
			 )
		 tables)

; delete first row from every table, becouse first row always contain columns names
(maphash #'(lambda (tableName table)
			 (setf (gethash tableName tables) (delete-if (constantly t) table :start 0 :count 1))
			 )
		 tables)

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
		   (princ "|")
  		   )
  		  widths
  		  (delete-if (constantly t) row :count 1 :from-end t))
	(princ (format nil (getFormatString lastWidth lastElem) lastElem))
	)
  )

(defun printTable (table)
  "just print table function in pretty way"
  (let ((widths (countWidths table)))
	(map 'list
		 (lambda (row)
		   (printRow widths row)
		   (terpri)
		   )
		 table)
	)
)

(defun query (queryStr)
  "this function parse query"
  (let ((columns (getColumns queryStr))
		(tableName (getTableName queryStr))
		(resultTable (simple-table:make-table))
		(indexes (simple-table:make-table)))
	; take a table
	(setq resultTable (gethash tableName tables))
	; take a table indexes
	(setq indexes (gethash tableName indexTables))
	; where
	(setq resultTable (where (getWhere queryStr) (gethash tableName indexTables) resultTable))
	; order by
	(setq resultTable (orderby (nth 0 (gethash (getOrderBy queryStr)
											   (gethash tableName indexTables)))
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

