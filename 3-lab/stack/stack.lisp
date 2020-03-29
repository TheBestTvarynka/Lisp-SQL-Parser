

(defstruct stack
  (list '() :type list)
  )

(defun stack-push (value stack)
  (setf (stack-list stack) (append (list value) (stack-list stack)))
  stack
  )

(defun stack-pop (stack)
  (setf (stack-list stack) (cdr (stack-list stack)))
  stack
  )

(defun stack-top (stack)
  (car (stack-list stack))
  )

(defun stack-is-empty (stack)
  (cond
	((not (stack-list stack)) T)
	(t nil)
	)
  )

(defun stack-size (stack)
  (length (stack-list stack))
  )

#||
(pprint (stack-top (make-stack)))

(defun changeStack (stack)
  (stack-pop stack)
  (pprint stack)
  )
(defvar simple-stack (make-stack))
(setf simple-stack (stack-push "pasha" simple-stack))
(setf simple-stack (stack-push "pacha" simple-stack))
(pprint simple-stack)
(changeStack simple-stack)
(pprint simple-stack)
(exit)
(defvar simple-stack (make-stack))
(setf simple-stack (stack-push "pasha" simple-stack))
(pprint simple-stack)
(setf simple-stack (stack-push "pacha" simple-stack))
(pprint simple-stack)
(pprint (stack-top simple-stack))
(pprint (stack-size simple-stack))
(pprint (stack-is-empty simple-stack))
(setf simple-stack (stack-pop simple-stack))
(pprint simple-stack)
(setf simple-stack (stack-pop simple-stack))
(pprint simple-stack)
(pprint (stack-is-empty simple-stack))
;||#

