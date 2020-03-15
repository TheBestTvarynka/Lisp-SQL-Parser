;
(require 'asdf)
; load package for .csv and .tsv parsing
(load "/home/qkation/Documents/LispFunctionalProgramming/3-lab/cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)
; load package for .json parsing
(load "/home/qkation/Documents/LispFunctionalProgramming/3-lab/cl-json/cl-json.asd")
(asdf:load-system 'cl-json)

; load parse files and save data to variables
(defvar map_zal (simple-table:read-csv #P"map_zal-skl9.csv"))
(defvar mp_assistants (simple-table:read-csv #P"mp-assistants.csv" t))
(defvar mp_posts (simple-table:read-csv #P"mp-posts_full.csv"))
(defvar plenary_register_mps (simple-table:read-tsv #P"plenary_register_mps-skl9.tsv"))
(defvar mps_declarations_rada(json:decode-json (open "./mps-declarations_rada.json")))

(defvar tables (make-hash-table :test 'equal))
(setf (gethash "map_zal-skl9" tables) map_zal)
(setf (gethash "mp-assistants" tables) mp_assistants)
(setf (gethash "mp-posts_full" tables) mp_posts)
(setf (gethash "plenary_register_mps-skl9" tables) mps_declarations_rada)

(defun generateSequence(n)(cond ((< n 0) '())
                                ((= n 0) '(0))
                                (t (append
                                     (generateSequence (- n 1))
                                     (list n)))
                                ))

(defun makeIndexes(i row hashTable)
  (cond ((< i 0) hashTable)
		(t (setf (gethash (aref row i) hashTable) (list i))
		   (makeIndexes (- i 1) row hashTable)
		  )
		)
  )

(defun makeIndexHashMap(row)
  (setf tmpHashTable (make-hash-table :test 'equal))
  (setf (gethash "*" tmpHashTable) (generateSequence (- (array-total-size row) 1)))
  (makeIndexes (- (array-total-size row) 1) row tmpHashTable)
  )

(defun convertToIndexes(columnsNames tableName)
  (setf hashMap (makeIndexHashMap (simple-table:get-row 0 (gethash tableName tables))))
  (reduce #'(lambda(lst column)
			  (append lst (gethash column hashMap)))
		  columnsNames
		  :initial-value ()
     )
  )

(defun split-str-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
                   :from-end t
                   :test #'(lambda (x y)
                             (find y x :test #'string=)
							 )
			)
		 )
		)
    (if n
      (split-str-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r)
	  )
	)
  )

(defun split-str (string &optional (separator " "))
  (split-str-1 string separator))

(defun getTableName(tokens)
  (nth 1 (member "FROM" tokens :test #'string=))
  )

(defun getColumnsNames(tokens)
  (subseq tokens 1 (position "FROM" tokens :test #'string=))
  )

(defun getFunction(strFunction value)
  (cond
	((string= strFunction "=")
	 (cond
	   ((numberp value) #'=)
	   ((stringp value) #'string=)
	   (t nil)
	   ))
	((string= strFunction "<")
	 (cond
	   ((numberp value) #'<)
	   ((stringp value) #'string<)
	   (t nil)
	   ))
	)
  )

(defun getWhereClause(tokens tableName)
  (defvar wherePosition (position "WHERE" tokens :test #'string=))
  (simple-table:where-filter
	(getFunction (+ wherePosition 2) (+ wherePosition 3))
	(nth 0 (convertToIndexes (list (nth (+ wherePosition 1) tokens)) tableName) )
	(nth (+ wherePosition 3) tokens)
	)
  )

(defun selectColumns(columns table)
  (simple-table:select1 table columns)
  )

(defun compareVectors(index vector1 vector2)
  (cond
	((= index (array-total-size vector1)) nil)
	((funcall
	   (getEq (aref vector1 index)) (aref vector1 index) (aref vector2 index)
	   )
	 (compareVectors (+ index 1) vector1 vector2)
	 )
	(t (funcall
		 (getComparator (aref vector1 index)) (aref vector1 index) (aref vector2 index)
		 )
	   )
	)
  )
(defun compareV(vector1 vector2)(compareVectors 0 vector1 vector2))

(defun getEq(value)
  (cond
      ((numberp value) #'=)
	  ((vectorp value) #'equalp)
      ((stringp value) #'string=)
	  (t nil)
      )
  )

(defun getComparator(value)
  (cond
      ((numberp value) #'<)
	  ((vectorp value) #'compareVectors)
      ((stringp value) #'string<)
	  (t nil)
      )
  )

(defun selectDistinct(index rows table)
  (cond
	((= (+ index 1) rows) table)
	((equalp (simple-table:get-row index table) (simple-table:get-row (+ index 1) table))
	 (selectDistinct (+ index 1) rows (remove index table))
	 )
	(t (selectDistinct (+ index 1) rows (remove index table)))
	)
  )

(defun sortTable(table)
  (sort table #'compareV)
  )

(defun orderBy(index table)
  (simple-table:order-by
	table
	index
	(getComparator (simple-table:get-row-column index (simple-table:get-row 1 table)))
	)
  )

(defun where(tokens tableName table)
  (simple-table:where table (getWhereClause tokens tableName))
  )

(defvar test (simple-table:read-csv "test.csv" t))
(pprint test)
(terpri)
(pprint (where '("WHERE" "col" "<" "20") "map_zal-skl9" test))
(exit)

(defun distinct(table)
  (selectDistinct 0 (simple-table:num-rows table) (sortTable table))
  )

(defun query(tokens)
  (setf tableName (getTableName tokens))
  (setf columns (convertToIndexes (getColumnsNames tokens) tableName))
  (write columns)
  (setf resultTable (selectColumns columns (gethash tableName tables)))
  resultTable
  )

(write (query '("SELECT" "*" "title" "id_mp" "FROM" "map_zal-skl9")))
(exit)

(defun printTable(simple_table row)(cond
								 ((= row 0) (pprint (simple-table:get-row 0 simple_table)))
							     (t (printTable simple_table (- row 1))
									(pprint (simple-table:get-row row simple_table)))
								 )
)
(defun printAll(tableName)(cond
							((string= tableName "map_zal-skl9")
							 (printTable map_zal (- (simple-table:num-rows map_zal) 1)))
							((string= tableName "mp-assistants")
							 (printTable mp_assistants (- (simple-table:num-rows mp_assistants) 1)))
							((string= tableName "mp-posts_full")
							 (printTable mp_posts (- (simple-table:num-rows mp_posts) 1)))
							((string= tableName "plenary_register_mps-skl9")
							 (printTable plenary_register_mps (- (simple-table:num-rows plenary_register_mps) 1)))
							((string= tableName "mps-declarations_rada")
							 (printTable mps_declarations_rada (- (simple-table:num-rows mps_declarations_rada) 1)))
							(t (princ (concatenate 'string "table not found: " tableName)))
						  )
)

(defun cutName(command)(cond ((not (position #\( command)) command)
							 (t (subseq command 0 (position #\( command)))
					   )
)
(defun cutParameter(command)(subseq command (+ (position #\( command) 1) (position #\) command)))

(defun execute(command)(cond ((string= (cutName command) "exit") "EXIT")
							 ((string= (cutName command) "load")
							   (printAll (cutParameter command)))
							 (t (princ "command not found!"))
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
