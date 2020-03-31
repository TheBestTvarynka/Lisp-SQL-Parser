(load "textprocessing.lisp")

(defun generateConditionAND (fn1 fn2)
  "returns function that check if fn1 statement AND fn2 statement are true"
  (lambda (row)(and (funcall fn1 row) (funcall fn2 row))
	)
  )

(defun generateConditionOR (fn1 fn2)
  "returns function that check if fn1 statement OR fn2 statement is true"
  (lambda (row)(or (funcall fn1 row) (funcall fn2 row))
	)
  )

(defun numeric-string-p (str)
  "check if str is number"
  (ignore-errors (parse-integer str))
  )

(defun parse-value (str)
  (cond
      ((not (numeric-string-p str)) (subseq str 1 (- (length str) 1)))
      (t (parse-integer str))
      )
  )

(defun getFunctionByName (fnStr value)
  "get function corresponds to the fnStr and value type"
  (cond
	((string= fnStr "=")
	 (cond
	   ((numberp value) #'=)
	   ((stringp value) #'string=)
	   (t (lambda (v1 v2)T))
	   )
	 )
	((string= fnStr "<")
	 (cond
	   ((numberp value) #'<)
	   ((stringp value) #'string<)
	   (t (lambda (v1 v2)T))
	   )
	 )
	((string= fnStr ">")
	 (cond
	   ((numberp value) #'>)
	   ((stringp value) #'string>)
	   (t (lambda (v1 v2)T))
	   )
	 )
	(t (lambda (v1 v2)T))
	)
  )

(defun findNextOperator (stringWhere)
  "returns position of the next operator in where statement"
  (let ((andPosition (search "and" stringWhere)) (orPosition (search "or" stringWhere)))
	  (setq andPosition (if (not andPosition) (length stringWhere)andPosition))
	  (setq orPosition (if (not orPosition) (length stringWhere)orPosition))
	  (min andPosition orPosition)
	)
  )

(defun removeCondition (stringWhere)
  "remove one first condition from stringWhere. example:
  'col > 6 and col < 25' -> 'and col < 25'"
  (setf stringWhere (string-left-trim " " stringWhere))
  (string-left-trim " " (subseq stringWhere (findNextOperator stringWhere)))
  )

(defun getCondition (stringWhere)
  "return first condition from stringWhere. example:
  'col > 6 and col < 25' -> 'col > 6'"
  (setf stringWhere (removeOperator stringWhere))
  (subseq stringWhere 0 (findNextOperator stringWhere))
  )

(defun getFnCondition (stringWhere columnIndexes)
  "return function that represent one where condition"
  (let (
		(column (getOperator stringWhere))
		(fn (getOperator (removeOperator stringWhere)))
		(value (getOperator (removeOperator (removeOperator stringWhere)))))
	(setq column (nth 0 (gethash column columnIndexes)))
	(setq value (parse-value value))
	(setq fn (getFunctionByName fn value))
	(lambda (row)
	  (funcall fn (aref row column) value)
	  )
	)
  )

(defun generateCondition (fn stringWhere columnIndexes)
  "make one function from whole where condition.
  this function takes one argument - row and returns t or nil"
  (setf fn (cond
			 ((string= (getOperator stringWhere) "and")
			  (generateConditionAND fn (getFnCondition (getCondition stringWhere) columnIndexes))
			  )
			 (t
			   (generateConditionOR fn (getFnCondition (getCondition stringWhere) columnIndexes))
			   )
			 )
	)
  (setf stringWhere (removeCondition (removeOperator stringWhere)))
  (cond
	((string= stringWhere "") fn)
	(t (generateCondition fn stringWhere columnIndexes))
    )
  )

(defun where (stringWhere resultTable)
  "where"
  (setf stringWhere (string-left-trim " " stringWhere))
  (cond
	((string= stringWhere "") resultTable)
	(t (let ((whereFn (generateCondition #'(lambda (row)T)
						  (concatenate 'string "and " stringWhere)
						  (table-columnIndexes resultTable)))
			 (data (table-data resultTable)))
		 (setf data (reduce (lambda (resultTable row)
							   (cond
								 ((funcall whereFn row) (vector-push-extend row resultTable)
														resultTable)
								 (t resultTable)
								 )
							   )
							 data
							 :initial-value (make-array 0 :fill-pointer 0)))
		 (setf (table-data resultTable) data)
		 resultTable
		 )
	  )
	)
  )

