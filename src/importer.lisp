; load additional packages
(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)
(load "cl-json/cl-json.asd")
(asdf:load-system 'cl-json)

(load "convert.lisp")

; table structure
(defstruct table
  tableName
  columnNames
  columnIndexes
  data
  )

(defun generateSequence (n)
  "generates sequence like '(0 1 2 3 ... )"
  (cond ((< n 0) '())
        ((= n 0) '(0))
        (t (append
                 (generateSequence (- n 1))
                 (list n)))
        )
  )

(defun makeIndexes (i row hashTable)
  "makes hashMap where the key is column name and the value is its sequence number.
  like: (col1 -> '(0))
        (col2 -> '(1)) ..."
  (cond ((< i 0) hashTable)
                (t (setf (gethash (aref row i) hashTable) (list i))
                   (makeIndexes (- i 1) row hashTable)
                  )
                )
  )

(defun makeIndexHashMap (row)
  "makes full hashmap with indexes. like:
  (col1 -> '(0))
  ...
  (* -> '(0 1 2 ...))"
  (let ((tmpHashTable (make-hash-table :test 'equal)))
        (setf (gethash "*" tmpHashTable) (generateSequence (- (length row) 1)))
    (makeIndexes (- (length row) 1) row tmpHashTable)
        )
  )

(defun createTable (tablename rawData)
  "creates table by tablename and rawData - readed file that represented in vector of vectors"
  (make-table :tableName tablename
			  :columnNames (aref rawData 0)
			  :columnIndexes (makeIndexHashMap (aref rawData 0))
			  :data (delete-if (constantly t) rawData :start 0 :count 1)
			  )
  )

(defun formatTable (resultTable)
  "joins columnNames and table data. as result we have table where first row is column names"
  (let ((titleRow (make-array 1 :initial-element (table-columnNames resultTable))))
	(concatenate 'vector titleRow (table-data resultTable))
	)
  )

(defun getFileType (filename)
  "cuts file extension from filename. example: 'test.csv' -> 'csv', 'test' -> ''"
  (let ((dotPosition (position #\. filename :from-end t)))
	(cond
	  ((not dotPosition) "")
	  (t (subseq filename (+ dotPosition 1)))
	  )
	)
  )

(defun getTableName (filename)
  "cuts tablename from filename. example:
  'test.csv' -> 'test', 'test' -> 'test', 't.e.s.t.csv' -> 't.e.s.t'"
  (let ((dotPosition (position #\. filename :from-end t))
		(slashPosition (position #\/ filename :from-end t)))
	(setf dotPosition (cond
	  					((not dotPosition) nil)
	  				    (t dotPosition)
						))
	(setf slashPosition (cond
						  ((not slashPosition) 0)
						  (t (+ slashPosition 1))
						  ))
	(subseq filename slashPosition dotPosition)
	)
  )

(defun readTableFromFile (filename &optional filetype)
  "reads table from file"
  (setf filetype (cond
				   ((not filetype) (getFileType filename))
				   (t filetype)
				   ))
  (cond
	((string= filetype "csv") (createTable (getTableName filename)
										   (simple-table:read-csv filename t)))
	((string= filetype "tsv") (createTable (getTableName filename)
										   (simple-table:read-tsv filename t)))
	((string= filetype "json") (createTable (getTableName filename)
											(convertToTable (json:decode-json (open filename)))))
	(t nil)
	)
  )

(defun table-len (table)
  "returns length of the table"
  (let ((data (table-data table)))
	(cond
	  ((not data) nil)
	  (t (length data))
	  )
	)
  )

(defun table-value (row col table)
  "returns the value in the given column from row"
  (aref (aref (table-data table) row) col)
  )

(defun table-column-number (table)
  "returns column amount"
  (length (table-columnNames table))
  )
