
(defun len (value)
  (cond
	((stringp value) (length value))
	(t (length (write-to-string value)))
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
  (concatenate 'string " ~" (write-to-string width) "A")
  )

(defun printRow (widths row)
  "this function just print row"
  (let ((lastWidth (aref widths (- (length widths) 1))) (lastElem (aref row (- (length row) 1))))
	(princ "|")
    (map 'list
  	     (lambda (width elem)
  		   (princ (format nil (getFormatString width elem) (cond ((stringp elem) elem)(t (write-to-string elem)))))
		   (princ " |")
  		   )
  		  widths
  		  row)
	)
  )

(defun printTable (resultTable)
  "just print table function in pretty way"
	(let ((widths (countWidths (formatTable resultTable))))
	  (let ((widthSum (+ (reduce (lambda (sum width)
								   (+ sum width)
								   )
							      widths) (* (length widths) 3) 1)))
	    (princ (format nil "~v@{~A~:*~}" widthSum  #\-))
		(terpri)
	    (printRow widths (table-columnNames resultTable))
	    (terpri)
	    (princ (format nil "~v@{~A~:*~}" widthSum #\-))
	    (terpri)
	    (map 'list
		     (lambda (row)
		       (printRow widths row)
		       (terpri)
			   )
		     (table-data resultTable))
	    (princ (format nil "~v@{~A~:*~}" widthSum #\-))
		(terpri)
	    (princ "(")
	    (princ (length (table-data resultTable)))
	    (princ " row(s))")
	    (terpri)
		)
		)
  )

