
(load "aggregatefunctions.lisp")
(load "where.lisp")
(load "textprocessing.lisp")
(load "functions.lisp")

(defun getSimpleAggFunction (fnname)
  "returns aggregate function corresponds to the function name 'fnname'"
  (cond
	((string= fnname "count") #'simpleCount)
	((string= fnname "max") #'simpleMax)
	((string= fnname "avg") #'simpleAverage)
	(t nil))
  )

(defun makeFn (havingStr table)
  "returns lambda function that return t/nil corresponds to the input row"
  (setf havingStr (string-trim " " havingStr))
  (let ((cnd (split-str havingStr #\SPACE)))
	(let ((value (parse-value (nth 2 cnd))))
	  (let ((compareFn (getFunctionByName (nth 1 cnd) value))
		    (aggFn (getSimpleAggFunction (parseCommand (nth 0 cnd))))
		    (colIndex (nth 0 (gethash (cutParameter (nth 0 cnd)) (table-columnIndexes table)))))
		(lambda (row)
		  (funcall compareFn (funcall aggFn (aref row colIndex)) value)
		  )
		)
	  )
	)
  )

(defun filter (fn data)
  "takes from data only suitable rows"
  (reduce (lambda (resData row)
			(cond
			  ((funcall fn row)
			   (vector-push-extend row resData)
			   resData)
			  (t resData)
			  )
			)
		  data
		  :initial-value (make-array 0 :fill-pointer 0))
  )

(defun having (havingStr table)
  "having function"
  (make-table :tableName (table-tableName table)
                  :columnNames (table-columnNames table)
                  :columnIndexes (table-columnIndexes table)
                  :data (filter (makeFn havingStr table) (table-data table)))
  )
