
(defun convertObjectToRow (object)
  "converts object (list of cons (of pairs)) into one row. exapmle:
  '((:ID 1) (:NAME 'pasha')) -> #(1, 'pasha')"
  (reduce (lambda (row pair)
			(vector-push-extend (cdr pair) row)
			row
			)
		  object
		  :initial-value (make-array 0 :fill-pointer 0))
  )

(defun getColumnNames (object)
  "takes column names from given object. example:
  '((:ID 1) (:NAME 'pasha')) -> #(':ID' ':NAME');
  and returns result in new table with one row - this column names"
  (let ((table (make-array 0 :fill-pointer 0)))
	(vector-push-extend (reduce (lambda (row pair)
								  (vector-push-extend (string (car pair)) row)
								  row
								  )
		  						object
		  						:initial-value (make-array 0 :fill-pointer 0)) table)
	table
	)
  )

(defun convertToTable (json)
  "this function converts list of lists in table. json argument looks like:
  '(((:ID 1) (:NAME 'pasha')) ((:ID 2) (:NAME 'asan')) ...) ->
  #(#(ID NAME) #(1 'pasha') #(2 'asan'));
  if json object will have a bad structure then this function returns bad table"
  (let ((table (getColumnNames (nth 0 json))))
    (reduce (lambda (table object)
  			  (vector-push-extend (convertObjectToRow object) table)
  			  table
 			  )
 		    json
 		    :initial-value table)
	)
  )

