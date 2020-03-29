
(defun len (value)
  "returns length of value"
  (cond
	((not value) 3)
	((stringp value) (length value))
	((numberp value) (length (write-to-string value)))
	(t 1)
	)
  )

(defun countWidths (table)
  "returns array that contain width of each column of the table"
  (let ((columnsAmount (length (aref table 0))))
    (reduce (lambda (widths row)
  			(map 'vector
  				 (lambda (width elem)
  				   (max width (len elem))
  				   )
  				 widths
  				 row)
  			)
  		  table
  		  :initial-value (make-array columnsAmount :fill-pointer columnsAmount))
	)
  )

(defun getFormatString (width elem)
  "construct control-string for format function"
  (cond
	((stringp elem) (concatenate 'string "~" (write-to-string width) "A"))
	((numberp elem) (concatenate 'string "~" (write-to-string width) "D"))
	(t (concatenate 'string "~" (write-to-string width) "A"))
	)
  )

(defun printRow (widths row)
  "this function just print row"
  (let ((lastWidth (aref widths (- (length widths) 1))) (lastElem (aref row (- (length row) 1))))
    (map 'list
  	     (lambda (width elem)
  		   (princ (format nil (getFormatString width elem) elem))
		   (princ " | ")
  		   )
  		  widths
  		  (delete-if (constantly t) row :count 1 :from-end t))
	(princ (format nil (getFormatString lastWidth lastElem) lastElem))
	)
  )

(defun printTable (resultTable)
  "just print table function in pretty way"
	(let ((widths (countWidths (formatTable resultTable))))
	  (printRow widths (table-columnNames resultTable))
	  (terpri)
	  (princ (format nil "~v@{~A~:*~}" (+ (reduce (lambda (sum width)
										            (+ sum width)
										            )
										          widths) (* (- (length widths) 1) 3)) #\-))
	  (terpri)
	  (map 'list
		   (lambda (row)
		     (printRow widths row)
		     (terpri)
			 )
		   (table-data resultTable))
		)
  )
