
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

