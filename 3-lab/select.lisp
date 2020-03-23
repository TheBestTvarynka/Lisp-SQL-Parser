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
  "returns function for comparing two element that have the same type as value"
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

(defun selectAggregateFunctions (functions sourceTable indexes resultTable)
  "execute all aggregate functions and collect result in a resultTable"
  (cond
	((not functions) resultTable)
	(t (let ((data (table-data resultTable)))
		 (vector-push-extend (aref data 0) (executeAggregateFunctions (car functions)
																	  data
																	  indexes))
		 (vector-push-extend (table-columnNames resultTable) (parseCommand (car functions)))
		 (selectAggregateFunctions (cdr functions) sourceTable indexes resultTable)
		 ))
	)
  )

(defun selectColumnNames (columns columnNames)
  "select column names. columns - indexes of columns. columnNames - array with column names"
  (reduce (lambda (resultColumns col)
			(vector-push-extend (aref columnNames col) resultColumns)
			resultColumns
			)
		  columns
		  :initial-value (make-array 0 :fill-pointer 0))
  )

(defun selectColumns (columns resultTable)
  "select columns from table"
  (let ((columnNames (table-columnNames resultTable))
		(data (table-data resultTable)))
	(setf (table-data resultTable) (simple-table:select1 data columns))
	(setf (table-columnNames resultTable) (selectColumnNames columns columnNames))
	resultTable
	)
  
  )

(defun select (columns tableIndexes resultTable)
  "select"
  (cond
	((string= (parseCommand (car columns)) "")
	 (selectColumns (convertToIndexes columns tableIndexes) resultTable))
	(t (selectAggregateFunctions columns resultTable tableIndexes (make-table :tableName "Result"
																			  :columnNames #()
																			  :data #())))
	)
  )

