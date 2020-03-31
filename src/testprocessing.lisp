
(defun removeOperator (stringWhere)
  "remove one first word from stringWhere"
  (setf stringWhere (string-left-trim " " stringWhere))
  (let ((spasePosition (position #\SPACE stringWhere)))
      (setq spasePosition (if (not spasePosition)(length stringWhere)spasePosition))
      (string-left-trim " " (subseq stringWhere spasePosition))
      )
  )

(defun getOperator (stringWhere)
  "return first word from stringWhere"
  (setf stringWhere (string-left-trim " " stringWhere))
  (let ((spasePosition (position #\SPACE stringWhere)))
      (setq spasePosition (if (not spasePosition)(length stringWhere)spasePosition))
      (subseq stringWhere 0 spasePosition)
      )
  )

(defun starts-with (str pattern)
  (let ((p (search pattern str)))
    (and p (= 0 p))
        )
  )

(defun appendToList (lst value)
  (append lst (list value))
  )

(defun split (words str separator)
  (let ((sepPos (position separator str)))
    (cond
	  ((not sepPos) (appendToList words str))
	  (t (split (appendToList words (subseq str 0 sepPos)) (subseq str (+ sepPos 1)) separator))
	  )
    )
  )

(defun split-str (str separator)
  (split '() str separator)
  )

