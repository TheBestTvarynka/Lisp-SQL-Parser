
(defun generateConditionAND (fn1 fn2)
  (lambda (row)(and (funcall fn1 row) (funcall fn2 row))
	)
  )

(defun generateConditionOR (fn1 fn2)
  (lambda (row)(or (funcall fn1 row) (funcall fn2 row))
	)
  )

(defun numeric-string-p (str)(ignore-errors (parse-integer str)))

(defun parse-value (str)
  (cond
      ((not (numeric-string-p str)) (subseq str 1 (- (length str) 1)))
      (t (parse-integer str))
      )
  )

(defun getFunction (fnStr value)
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
  (let ((andPosition (search "and" stringWhere)) (orPosition (search "or" stringWhere)))
	  (setq andPosition (if (not andPosition) (length stringWhere)andPosition))
	  (setq orPosition (if (not orPosition) (length stringWhere)orPosition))
	  (min andPosition orPosition)
	)
  )

(defun removeOperator (stringWhere)
  (setf stringWhere (string-left-trim " " stringWhere))
  (let ((spasePosition (position #\SPACE stringWhere)))
	(setq spasePosition (if (not spasePosition)(length stringWhere)spasePosition))
	(string-left-trim " " (subseq stringWhere spasePosition))
	)
  )

(defun removeCondition (stringWhere)
  (setf stringWhere (string-left-trim " " stringWhere))
  (string-left-trim " " (subseq stringWhere (findNextOperator stringWhere)))
  )

(defun getOperator (stringWhere)
  (setf stringWhere (string-left-trim " " stringWhere))
  (let ((spasePosition (position #\SPACE stringWhere)))
	(setq spasePosition (if (not spasePosition)(length stringWhere)spasePosition))
	(subseq stringWhere 0 spasePosition)
	)
  )

(defun getCondition (stringWhere)
  (setf stringWhere (removeOperator stringWhere))
  (subseq stringWhere 0 (findNextOperator stringWhere))
  )

(defun getFnCondition (stringWhere columnIndexes)
  (pprint stringWhere)
  (let (
		(column (getOperator stringWhere))
		(fn (getOperator (removeOperator stringWhere)))
		(value (getOperator (removeOperator (removeOperator stringWhere)))))
	(setq column (gethash column columnIndexes))
	(setq value (parse-value value))
	(setq fn (getFunction fn value))
	(lambda (row)
	  (funcall fn (aref row column) value)
	  )
	)
  )

(defun generateCondition (fn stringWhere columnIndexes)
  (pprint stringWhere)
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
  (pprint stringWhere)
  (cond
	((string= stringWhere "") fn)
	(t (generateCondition fn stringWhere columnIndexes))
    )
  )

(defun where (stringWhere columnIndexes)
  (generateCondition #'(lambda (row)T)
					 (concatenate 'string "and " stringWhere)
					 columnIndexes)
  )

; test generateCondition and other functions
#||
(defvar hashes (make-hash-table :test 'equal))
(setf (gethash "col1" hashes) 0)
(setf (gethash "col2" hashes) 1)

(defvar fn (generateCondition #'(lambda (row)T) "and col1 < 4 or col1 > 8" hashes))

(defvar table
  #(#(1 "pasha")
	#(2 "pacha")
	#(3 "misha")
	#(4 "asan")
	#(5 "yter")
	#(6 "ethernet")
	#(7 "dima")
	#(8 "lisp")
	#(9 "java")
	#(10 "asus")
	)
  )
(pprint table)
(defun selectWhere (index rows table)
  (cond
	((= index rows) nil)
	(t (pprint (funcall fn (aref table index)))
	   (selectWhere (+ index 1) rows table)
	   )
    )
  )

(selectWhere 0 10 table)
||#
