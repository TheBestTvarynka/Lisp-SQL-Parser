(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)
; maybe instead of functions from this package I will write my own function

(load "distinct.lisp")
(load "stack/stack.lisp")

(defun generateValue (value)
  (concatenate 'string "[value:" (write-to-string value) "]")
  ;(lambda (row)
	;value
	;)
  )

(defun generateColumnValue (colname indexes)
  (concatenate 'string "[column:" colname "]")
  ;(setf colname (gethash colname indexes))
  ;(lambda (row)
	;(aref row colname)
	;)
  )

(defun appendValue (lst value)
  (append lst (list value))
  )

(defun ifOperator (ch)
  (cond
	((or (char= #\+ ch)
		 (char= #\- ch)
		 (char= #\* ch)
		 (char= #\/ ch)
		 (char= #\( ch)
		 (char= #\) ch)
		 (char= #\, ch)) t)
	(t nil)
	)
  )

(defun getPriority (fn)
  (cond
	((string= fn "=") 10)
	((or (string= fn "*")
		 (string= fn "/")) 8)
	((or (string= fn "+")
		 (string= fn "-")) 7)
	((string= fn "(") 6)
	(t 9)
	)
  )

(defun insertClosingBracket (operators stack)
  (let ((topOperator (stack-top stack)))
	(cond
	  ((string= topOperator "(")
	   (stack-pop stack)
	   operators)
	  (t (insertClosingBracket (appendValue operators topOperator) (stack-pop stack)))
	  )
	)
  )

(defun insertComa (operators stack)
  (let ((topOperator (stack-top stack)))
	(cond
	  ((stack-is-empty stack) (appendValue operators ","))
	  ((string= topOperator "(") operators)
	  (t (insertComa (appendValue operators topOperator) (stack-pop stack)))
	  )
	)
  )

(defun insertOperator (operator operators stack)
  (let ((topOperator (stack-top stack)))
	(cond
	  ((string= operator "(")
	   (stack-push operator stack)
	   operators)
	  ((stack-is-empty stack)
	   (stack-push operator stack)
	   operators)
	  ((>= (getPriority topOperator) (getPriority operator))
	   (insertOperator operator (appendValue operators topOperator) (stack-pop stack)))
	  (t (stack-push operator stack)
		 operators)
	  )
	)
  )

(defun insertOperatorInStack (operator operators stack)
  (let ((topOperator (stack-top stack)))
	(cond
	  ((string= operator ")") (insertClosingBracket operators stack))
	  ((string= operator ",") (insertComa operators stack))
	  (t (insertOperator operator operators stack))
	  )
	)
  )

(defun clearStack (operators stack)
  (cond
	((stack-is-empty stack)
	 ;(pprint "empty")
	 operators)
	(t (clearStack (appendValue operators (stack-top stack)) (stack-pop stack)))
	)
  )

(defun ifNameChar (ch)
  (setf ch (char-int ch))
  (cond
	((or (and (>= ch 48) (<= ch 57))
		 (and (>= ch 65) (<= ch 90))
		 (and (>= ch 97) (<= ch 122))) t)
	(t nil)
	)
  )

(defun readName (selectStr operators stack indexes)
  ;(pprint "readName")
  ;(pprint selectStr)
  ;(sleep 2)
  (let ((nameEnd (position-if-not #'ifNameChar selectStr)))
	(cond
	  ((not nameEnd)
	   ;(pprint "readname - column")
	   (setf operators (appendValue operators (generateColumnValue selectStr indexes)))
	   (parseSelect "" operators stack indexes))
	  ((char= #\( (char selectStr nameEnd))
	   ;(pprint "readname - function")
	   (let ((funName (subseq selectStr 0 nameEnd)))
	      (setf operators (insertOperatorInStack funName operators stack))
	      (setf selectStr (string-left-trim " " (subseq selectStr nameEnd)))
	      (parseSelect selectStr operators stack indexes)
		  ))
	  (t (pprint "readname - column")
		 (setf operators (appendValue operators (generateColumnValue (subseq selectStr 0 nameEnd) indexes)))
		 (setf selectStr (string-left-trim " " (subseq selectStr nameEnd)))
		 (parseSelect selectStr operators stack indexes))
	  )
	)
  )

(defun readStringValue (selectStr operators stack indexes)
  ;(pprint "readStringValue")
  ;(pprint selectStr)
  ;(sleep 2)
   (let ((value (subseq selectStr 1 (position #\' selectStr :start 1))))
	 (setf selectStr (string-left-trim " " (subseq selectStr (+ (length value) 2))))
	 (setf operators (appendValue operators (generateValue value)))
	 (parseSelect selectStr operators stack indexes)
	 )
  )

(defun readIntValue (selectStr operators stack indexes)
  ;(pprint "readIntValue")
  ;(pprint selectStr)
  ;(sleep 2)
  (let ((value (subseq selectStr 0 (position-if-not #'digit-char-p selectStr))))
	(setf selectStr (string-left-trim " " (subseq selectStr (length value))))
	(setf operators (appendValue operators (generateValue (read-from-string value))))
	(parseSelect selectStr operators stack indexes)
	)
  )

(defun readOperator (selectStr operators stack indexes)
  ;(pprint "readOperator")
  ;(pprint selectStr)
  ;(sleep 2)
  (let ((ch (char selectStr 0)))
    (setf selectStr (string-left-trim " " (subseq selectStr 1)))
    (parseSelect selectStr (insertOperatorInStack ch operators stack) stack indexes)
	)
  )

(defun parseSelect (selectStr operators stack indexes)
  ;(pprint "in parseSelect")
  ;(pprint selectStr)
  ;(pprint operators)
  ;(pprint stack)
  (cond
	((string= selectStr "")
	 ;(pprint "query is empty")
     (clearStack operators stack))
	(t
      (let ((ch (char selectStr 0)))
	    ;(pprint ch)
        (cond
  	      ((digit-char-p ch)
		   (readIntValue selectStr operators stack indexes))
  	      ((char= #\' ch)
		   (readStringValue selectStr operators stack indexes))
          ((ifOperator ch)
		   (readOperator selectStr operators stack indexes))
	      ((ifNameChar ch)
		   (readName selectStr operators stack indexes))
	      (t nil)
  	      )
		)
	  )
	)
  )

;(exit)
(pprint (parseSelect "1 + 3*(fn('value', id, 3) +2) - 4" '() (make-stack) nil))
(pprint (parseSelect "1 + 3*(fn('value', 43 + id * 6, 3) +2) - 4" '() (make-stack) nil))
(pprint (parseSelect "1 + 3*(fn('value', 43 + count(id) * 6, 3) +2) - 4" '() (make-stack) nil))
(pprint (parseSelect "concat(name, ' ', description)" '() (make-stack) nil))
(pprint (parseSelect "col1" '() (make-stack) nil))
(pprint (parseSelect "2 +id" '() (make-stack) nil))

