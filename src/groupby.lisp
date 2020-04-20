
(load "textprocessing.lisp")
(load "orderby.lisp")

(load "importer.lisp")

(defun getIndexes (groupSrt table)
  (let ((columns (mapcar (lambda (col)(string-trim " " col)) (split-str groupSrt #\,)))
		(indexes (table-columnIndexes table))
		(columnIndexes (make-array 0 :fill-pointer 0)))
	(mapcar (lambda (col)
			  (vector-push-extend (nth 0 (gethash col indexes)) columnIndexes)
			  )
			columns)
	columnIndexes
	)
  )

(defun equalRows (indexes row1 row2 comparators)
  "compares row1 and row2.
  indexes - array with indexes specified in group by statement
  comparators - hashmap where key is index and value is comparator"
  (cond
    ((= (length indexes) 0) t)
    (t (let ((elem1 (aref row1 (aref indexes 0)))
         (elem2 (aref row2 (aref indexes 0))))
		 (and (funcall (gethash (aref indexes 0) comparators) elem1 elem2)
			  (equalRows (subseq indexes 1) row1 row2 comparators))))
    )
  )

(defun getEqualComparator (indexes comparators)
  "returns lambda function that is comparator for rows"
  (lambda (row1 row2)
	(equalRows indexes row1 row2 comparators)
	)
  )

(defun createEqualsHashMap (indexes table)
  "returns new hashmao where key is index and value is comparator"
  (let ((row (aref (table-data table) 0)))
	(reduce (lambda (hashmap index)
			  (setf (gethash index hashmap) (getEqual (aref row index)))
			  hashmap
			  )
			indexes
			:initial-value (make-hash-table :test 'equal))
	)
  )

(defun makeSimpleSegment (row indexes)
  "makes one segment, that is the row where all elements are vectors with one value except elements with indexes specified in 'indexes'"
  (let ((segment (reduce (lambda (res value)
						   (vector-push-extend (make-array 1 :initial-element value :fill-pointer 1) res)
						   res)
						 row
						 :initial-value (make-array 0 :fill-pointer 0))))
	(reduce (lambda (someres index)
			  (setf (aref segment index) (aref (aref segment index) 0))
			  )
			indexes
			:initial-value 0)
	segment
	)
  )

(defun addToData (val data)
  "adds val to the data and returns the data"
  (vector-push-extend val data)
  data
  )

(defun addToSegmentCol (index val seg)
  "adds 'val' to column in segment 'seg' with index 'index' and returns final segment"
  (vector-push-extend val (aref seg index))
  seg
  )

(defun addToSegment (index row segment indexes)
  "adds row to segment and returns final segment"
  (cond
	((= index (length row)) segment)
	((= (length indexes) 0) (addToSegment (+ index 1) row (addToSegmentCol index (aref row index) segment) indexes))
	((= index (aref indexes 0)) (addToSegment (+ index 1) row segment (subseq indexes 1)))
	(t (addToSegment (+ index 1) row (addToSegmentCol index (aref row index) segment) indexes))
	)
  )

(defun devideIntoGroups (sampleRow comparator curSegment data resData indexes)
  "makes data segmented"
  (cond
	((= (length data) 0) (addToData curSegment resData))
	((funcall comparator sampleRow (aref data 0))
	 (devideIntoGroups sampleRow
					   comparator
					   (addToSegment 0 (aref data 0) curSegment indexes)
					   (subseq data 1)
					   resData
					   indexes))
	(t (devideIntoGroups (aref data 0)
						 comparator
						 (makeSimpleSegment (aref data 0) indexes)
						 (subseq data 1)
						 (addToData curSegment resData)
						 indexes))
	)
  )

(defun groupBy (groupSrt table)
  "group by function"
  (setf groupSrt (string-trim " " groupSrt))
  (let ((sortedTable (orderBy groupSrt table))
		(indexes (getIndexes groupSrt table)))
	(let ((equalComparator (getEqualComparator indexes (createEqualsHashMap indexes table)))
		  (firstRow (aref (table-data sortedTable) 0)))
	  (make-table :tableName (table-tableName sortedTable)
				  :columnNames (table-columnNames sortedTable)
				  :columnIndexes (table-columnIndexes sortedTable)
				  :data (devideIntoGroups firstRow
										  equalComparator
										  (makeSimpleSegment firstRow indexes)
										  (subseq (table-data sortedTable) 1)
										  (make-array 0 :fill-pointer 0)
										  indexes))
	  )
	)
  )

