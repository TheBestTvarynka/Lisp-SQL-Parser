(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)
; maybe instead of functions from this package I will write my own function

(defun countRows (column table)
  "count aggregate funciton"
  (concatenate 'string "count -> " (write-to-string (car column)))
  )

(defun maxRows (column table)
  "max aggregate funciton"
  (concatenate 'string "max -> " (write-to-string (car column)))
  )

(defun avgRows (column table)
  "average aggregate funciton"
  (concatenate 'string "avg -> " (write-to-string (car column)))
  )

; hashmap that contain aggregate functions where key is afunctionName and value is a funciton
(defvar aggregateFunctions (make-hash-table :test 'equal))
(setf (gethash "count" aggregateFunctions) #'countRows)
(setf (gethash "max" aggregateFunctions) #'maxRows)
(setf (gethash "avg" aggregateFunctions) #'avgRows)

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

(defun parseCommand (commandQuery) 
  "cut command name" 
  (let ((openBracketPosition (position #\( commandQuery))) 
    (setq openBracketPosition (cond 
                                ((not openBracketPosition) 0) 
                                (t openBracketPosition) 
                                )) 
    (subseq commandQuery 0 openBracketPosition) 
    ) 
  ) 
 
(defun cutParameter (command) 
  "cut command parameter (text inside '()')" 
  (subseq command (+ (position #\( command) 1) (position #\) command :from-end t)) 
  )

(defun executeAggregateFunctions (funcitonStr table indexes)
  (funcall (gethash (parseCommand funcitonStr) aggregateFunctions)
		   (convertToIndexes (list (cutParameter funcitonStr)) indexes)
		   table)
  )

(defun selectAggregateFunctions (functions table indexes resultTable)
  (cond
	((not functions) resultTable)
	(t (vector-push-extend (executeAggregateFunctions (car functions)
													  table
													  indexes)
						   resultTable)
	   (selectAggregateFunctions (cdr functions) table indexes resultTable)
	   )
	)
  )

(defun selectColumns (columns table)
  "select columns from table"
  (simple-table:select1 table columns)
  )

(defun select (columns tableIndexes table)
  (cond
	((string= (parseCommand (car columns)) "")
	 (selectColumns (convertToIndexes columns tableIndexes) table))
	(t (selectAggregateFunctions columns table tableIndexes (make-array 0 :fill-pointer 0)))
	)
  )
#||
(defvar indexes (make-hash-table :test 'equal))
(setf (gethash "col1" indexes) '(0))
(setf (gethash "col2" indexes) '(1))
(setf (gethash "col3" indexes) '(2))
(setf (gethash "*" indexes) '(0 1 2))

(pprint (select '("col1" "col2" "*") indexes #(#(1 2 3)
								#(4 5 6)
								#(7 8 9)
								#(10 11 12))
		  )
  )

(pprint (select '("max(col1)" "avg(col2)" "count(*)") indexes #(#(1 2 3)
								#(4 5 6)
								#(7 8 9)
								#(10 11 12))
		  )
  )
;||#

