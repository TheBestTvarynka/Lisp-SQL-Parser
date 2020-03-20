(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)
; maybe instead of functions from this package I will write my own function

(defun countRows (column table)
  "count aggregate funciton"
  (cond
	((= (length column) 1) (let ((index (nth 0 column)))
	 						 (reduce (lambda (cnt elem)
									 (cond
									   ((not (aref elem index)) cnt)
									   (t (+ cnt 1))
									   )
									 )
									 table
									 :initial-value 0)
							 ))
	((= (length column) (length (aref table 0))) (length table))
	(t "Error: Wrong arguments for count() function!")
	)
  )

(defun getComparator (value)
  (cond
        ((numberp value) #'<)
        ((stringp value) #'string<)
        (t (lambda (v1 v2)T))
        )
  )

(defun findMax (index table)
  "this finction finds max value in given column"
  (reduce (lambda (maxValue row)
			(let ((elem (aref row index)))
   			  (cond
   			    ; if maxValue is nil and elem is not nil
   			    ((and (not maxValue) elem) elem)
   			    ; if elem is nil then we return maxValue
   			    ((not elem) maxValue)
   			    ; standart case: we compare two numbers. if maxValue < elem, then return elem
   			    ((funcall (getComparator elem) maxValue elem) elem)
   			    ; else return maxValue
   			    (t maxValue)
   			    )
			  )
			)
		  table
		  :initial-value nil)
  )

(defun maxRows (column table)
  "max aggregate funciton"
  (cond
	((= (length column) 1) (findMax (nth 0 column) table))
	(t "Error: Wrong arguments for max() function!")
	)
  )

(defun findAverage (index table)
  "this funciton determine average value in given column"
  (let ((sum 0) (amount 0))
	(setq sum (reduce (lambda (sum row)
					   (let ((elem (aref row index)))
						 (cond
						   ((not elem) sum)
						   (t (setq amount (+ amount 1))
							  (+ sum elem))
						   )
						 )
			           )
					 table
					 :initial-value 0))
	(pprint sum)
	(pprint amount)
	(cond
	  ((= amount 0) nil)
	  (t (/ (float sum) amount))
	  )
	)
  )

(defun avgRows (column table)
  "average aggregate funciton"
  (cond
	((= (length column) 1) (findAverage (nth 0 column) table))
	(t "Error: Wrong arguments for max() function!")
	)
  )
#||
(pprint (avgRows '(0) #(
						  #(1 9 3)
						  #(4 nil 6)
						  #(7 8 9)
						  #(10 nil 12)
						  )
				   )
		)+
(exit)
;||#

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

