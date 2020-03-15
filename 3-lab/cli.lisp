;
(require 'asdf)
; load package for .csv and .tsv parsing
(load "/home/qkation/Documents/LispFunctionalProgramming/3-lab/cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)
; load package for .json parsing
(load "/home/qkation/Documents/LispFunctionalProgramming/3-lab/cl-json/cl-json.asd")
(asdf:load-system 'cl-json)

; load parse files and save data to variables
(defvar map_zal (simple-table:read-csv #P"datasourse/map_zal-skl9.csv"))
(defvar mp_assistants (simple-table:read-csv #P"datasourse/mp-assistants.csv" t))
(defvar mp_posts (simple-table:read-csv #P"datasourse/mp-posts_full.csv"))
(defvar plenary_register_mps (simple-table:read-tsv #P"datasourse/plenary_register_mps-skl9.tsv"))
(defvar mps_declarations_rada(json:decode-json (open "datasourse/mps-declarations_rada.json")))

(defvar tables (make-hash-table :test 'equal))
(setf (gethash "map_zal-skl9" tables) map_zal)
(setf (gethash "mp-assistants" tables) mp_assistants)
(setf (gethash "mp-posts_full" tables) mp_posts)
;(setf (gethash "plenary_register_mps-skl9" tables) mps_declarations_rada)

(defun generateSequence (n)
  (cond ((< n 0) '())
	((= n 0) '(0))
	(t (append
		 (generateSequence (- n 1))
		 (list n)))
	)
  )

(defun makeIndexes (i row hashTable)
  (cond ((< i 0) hashTable)
		(t (setf (gethash (aref row i) hashTable) (list i))
		   (makeIndexes (- i 1) row hashTable)
		  )
		)
  )

(defun makeIndexHashMap (row)
  (let ((tmpHashTable (make-hash-table :test 'equal)))
	(setf (gethash "*" tmpHashTable) (generateSequence (- (array-total-size row) 1)))
    (makeIndexes (- (array-total-size row) 1) row tmpHashTable)
	)
  )

(defvar indexTables (make-hash-table :test 'equal))
(maphash #'(lambda (tableName table)
			 (setf (gethash tableName indexTables) (makeIndexHashMap (simple-table:get-row 0 table)))
			 )
		 tables)

(defun convertToIndexes (columns indexes)
  (reduce #'(lambda(lst column)
			  (append lst (gethash column indexes))
			  )
		  columns
		  :initial-value ()
     )
  )

#||
(defvar hashes (gethash "map_zal-skl9" indexTables))
(pprint (convertToIndexes '("row" "pos_x" "title") hashes))
(exit)
||#

(defun getTableName (queryStr)
  (setq queryStr (string-left-trim " " (subseq queryStr (+ (search "from" queryStr) 4))))
  (let ((spacePosition (position #\SPACE queryStr)))
	(setq spacePosition (cond
						  ((not spacePosition) (length queryStr))
						  (t spacePosition)
						  ))
	(subseq queryStr 0 spacePosition)
	)
  )

(defun ifDistinct (queryStr)
  (cond
	((not (search "distinct" queryStr)) nil)
	(t t)
	)
  )

(defun selectColumns(queryStr)
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
#||
(pprint (selectColumns "select distinct col1, col2, * from table" "table"))
(pprint (selectColumns "select   col1, col2, *   from table" "table"))
(pprint (selectColumns "select col1, col2, * from table" "table"))
(exit)
||#

(defun printTable (simple_table row)
  (cond
	((= row 0) (pprint (simple-table:get-row 0 simple_table)))
    (t (printTable simple_table (- row 1))
	(pprint (simple-table:get-row row simple_table)))
    )
)

(defun query (queryStr)
  (let ((columns (selectColumns queryStr))
		(tableName (getTableName queryStr))
		(resultTable (simple-table:make-table)))
	(setq columns (convertToIndexes columns (gethash tableName indexTables)))
	(setq resultTable (simple-table:select1 (gethash tableName tables) columns))
	(printTable resultTable (- (simple-table:num-rows resultTable) 1))
	)
  )

(defun loadTable (tableName)
  (query (concatenate 'string "select * from " tableName))
  )

(defun parseCommand (commandQuery)
  (let ((openBracketPosition (position #\( commandQuery)))
	(setq openBracketPosition (cond
								((not openBracketPosition) 0)
								(t openBracketPosition)
								))
	(subseq commandQuery 0 openBracketPosition)
	)
  )

(defun cutParameter (command)
  (subseq command (+ (position #\( command) 1) (position #\) command :from-end t))
  )

(defun execute (commandQuery)
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
