
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
	(pprint columnIndexes)
	columnIndexes
	)
  )

(defun equalRows (indexes row1 row2 comparators)
  (cond
    ((= (length indexes) 0) t)
    (t (let ((elem1 (aref row1 (aref indexes 0)))
         (elem2 (aref row2 (aref indexes 0))))
		 (and (funcall (gethash (aref indexes 0) comparators) elem1 elem2)
			  (equalRows (subseq indexes 1) row1 row2 comparators))))
    )
  )

(defun getEqualComparator (indexes comparators)
  (lambda (row1 row2)
	(equalRows indexes row1 row2 comparators)
	)
  )

(defun createEqualsHashMap (indexes table)
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
  (let ((segment (reduce (lambda (res value)
						   (vector-push-extend (make-array 1 :initial-element value :fill-pointer 1) res)
						   res)
						 row
						 :initial-value (make-array 0 :fill-pointer 0))))
	(pprint segment)
	(reduce (lambda (someres index)
			  (setf (aref segment index) (aref (aref segment index) 0))
			  )
			indexes
			:initial-value 0)
	segment
	)
  )

(defun addToData (val data)
  (vector-push-extend val data)
  data
  )

(defun addToSegmentCol (index val seg)
  (vector-push-extend val (aref seg index))
  seg
  )

(defun addToSegment (index row segment indexes)
  (cond
	((= index (length row)) segment)
	((= (length indexes) 0) (addToSegment (+ index 1) row (addToSegmentCol index (aref row index) segment) indexes))
	((= index (aref indexes 0)) (addToSegment (+ index 1) row segment (subseq indexes 1)))
	(t (addToSegment (+ index 1) row (addToSegmentCol index (aref row index) segment) indexes))
	)
  )
; (pprint (addToSegment 0 #(1 2 3 4 5) (makeSimpleSegment #(10 20 3 4 50) #(2 3)) #(2 3)))
; (exit)

(defun devideIntoGroups (sampleRow comparator curSegment data resData indexes)
  (pprint sampleRow)
  (pprint curSegment)
  (cond
	((= (length data) 0) resData)
	((funcall comparator sampleRow (aref data 0))
	 (terpri)
	 (pprint "klei rnfireufnrei ugneriugneriu gneriuger gnerigneriu bneruigberuigber")
	 (terpri)
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
  (setf groupSrt (string-trim " " groupSrt))
  (let ((sortedTable (orderBy groupSrt table))
		(indexes (getIndexes groupSrt table)))
	(pprint groupSrt)
	(pprint indexes)
	(pprint (table-data sortedTable))
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

(defvar simpletable (readTableFromFile "datasource/test.csv"))
(pprint (groupBy "row" simpletable))
(terpri)
(pprint "fegregregergegre")
