;
(require 'asdf)
; load package for .csv and .tsv parsing
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)
; load package for .json parsing
(load "cl-json/cl-json.asd")
(asdf:load-system 'cl-json)

; load other code
(load "distinct.lisp")
(load "where.lisp")
(load "orderby.lisp")

; load parse files and save data to variables
(defvar map_zal (simple-table:read-csv #P"datasourse/map_zal-skl9.csv" t))
(defvar mp_assistants (simple-table:read-csv #P"datasourse/mp-assistants.csv" t))
(defvar mp_posts (simple-table:read-csv #P"datasourse/mp-posts_full.csv"))
(defvar plenary_register_mps (simple-table:read-tsv #P"datasourse/plenary_register_mps-skl9.tsv"))
(defvar mps_declarations_rada(json:decode-json (open "datasourse/mps-declarations_rada.json")))

; tables - hashmap where key is tablename and value is table
(defvar tables (make-hash-table :test 'equal))
(setf (gethash "map_zal-skl9" tables) map_zal)
(setf (gethash "mp-assistants" tables) mp_assistants)
(setf (gethash "mp-posts_full" tables) mp_posts)
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
	(setf (gethash "*" tmpHashTable) (generateSequence (- (array-total-size row) 1)))
    (makeIndexes (- (array-total-size row) 1) row tmpHashTable)
	)
  )

; indexTables - hash table that contain all index hashmaps (index hashmap for each table)
; key is tablename and value is index hashmap
(defvar indexTables (make-hash-table :test 'equal))
(maphash #'(lambda (tableName table)
			 (setf (gethash tableName indexTables) (makeIndexHashMap (simple-table:get-row 0 table)))
			 )
		 tables)

; delete first row from every table, becouse first row always contain columns names
(maphash #'(lambda (tableName table)
			 (setf (gethash tableName tables) (delete-if (constantly t) table :start 0 :count 1))
			 )
		 tables)

(defun convertToIndexes (columns indexes)
  "convert input list of columns names in list of indexes; like:
  '(col1 col2 col4 *) -> '(1 2 4 1 2 3 4 5 6)"
  (reduce #'(lambda(lst column)
			  (append lst (gethash column indexes))
			  )
		  columns
		  :initial-value ()
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
  'select * from table where cond1 and cond2' -> 'cond1 and cond2'"
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

(defun iterate (rowIndex table newTable fn)
  (cond
	((= rowIndex (simple-table:num-rows table)) newTable)
	(t (cond
		 ((funcall fn (simple-table:get-row rowIndex table))
		  (iterate (+ rowIndex 1)
				   table
				   (simple-table:add-to-table (simple-table:get-row rowIndex table) newTable)
				   fn))
		 (t (iterate (+ rowIndex 1)
					 table
					 newTable
					 fn))
		 ))
	)
  )

(defun ifDistinct (queryStr)
  "check if query contain distinct keyword"
  (cond
	((not (search "distinct" queryStr)) nil)
	(t t)
	)
  )

(defun selectColumns(queryStr)
  "cut columns names from query and return tham as list"
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

(defun printTable (simple_table row)
  "just print table function"
  (cond
	((= row 0) (pprint (simple-table:get-row 0 simple_table)))
    (t (printTable simple_table (- row 1))
	(pprint (simple-table:get-row row simple_table)))
    )
)

(defun query (queryStr)
  "this function parse query"
  (let ((columns (selectColumns queryStr))
		(tableName (getTableName queryStr))
		(resultTable (simple-table:make-table)))
	(setq columns (convertToIndexes columns (gethash tableName indexTables)))
	(setq resultTable (simple-table:select1 (gethash tableName tables) columns))
	(setq resultTable (iterate 0
							   resultTable
							   (simple-table:make-table)
							   (where (getWhere queryStr) (gethash tableName indexTables))
							   ))
	(setq resultTable (cond
						((not (ifDistinct queryStr)) resultTable)
						(t (distinct resultTable)
						   )
						))
	(pprint (getOrderBy queryStr))
	(pprint (gethash (getOrderBy queryStr) (gethash tableName indexTables)))
	(pprint (getOrderDirection queryStr))
	(setq resultTable (orderby (nth 0 (gethash (getOrderBy queryStr)
											   (gethash tableName indexTables)))
							   (getOrderDirection queryStr) resultTable))
	(printTable resultTable (- (simple-table:num-rows resultTable) 1))
	)
  )

(defun loadTable (tableName)
  "load table command: print whole table"
  (query (concatenate 'string "select * from " tableName))
  )

(defun parseCommand (commandQuery)
  "cut command name from user input"
  (let ((openBracketPosition (position #\( commandQuery)))
	(setq openBracketPosition (cond
								((not openBracketPosition) 0)
								(t openBracketPosition)
								))
	(subseq commandQuery 0 openBracketPosition)
	)
  )

(defun cutParameter (command)
  "cut command parameter (text inside '()') from user input"
  (subseq command (+ (position #\( command) 1) (position #\) command :from-end t))
  )

(defun execute (commandQuery)
  "execute entered text"
  (let ((command (parseCommand commandQuery)))
	(cond
	  ((string= command "exit") "EXIT")
	  ((string= command "query") (query (cutParameter commandQuery)))
	  ((string= command "load") (loadTable (cutParameter commandQuery)))
	  (t (pprint "Error: entered command not fund!!!"))
	  )
	)
  )

(defun run ()
  "run cli"
    (loop
        (terpri)
        (princ "[user@host ~]$: ")
		(terpri)
		(if (string= (execute (read-line)) "EXIT")
		  (return)
		  ()
		  )
    )
)

(run)

