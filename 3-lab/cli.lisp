
(require 'asdf)
(load "priority-queue/priority-queue.asd")
(asdf:load-system 'priority-queue)
; load all functionality code
(load "importer.lisp")
(load "distinct.lisp")
(load "where.lisp")
(load "orderby.lisp")
(load "select.lisp")

; tables - hashmap where key is tablename and value is a table
(defvar tables (make-hash-table :test 'equal))
(setf (gethash "map_zal-skl9" tables) (readTableFromFile "datasource/map_zal-skl9.csv"))
(setf (gethash "mp-assistants" tables) (readTableFromFile "datasource/mp-assistants.csv"))
(setf (gethash "mp-posts_full" tables) (readTableFromFile "datasource/mp-posts_full.csv"))
(setf (gethash "mps-declarations_rada" tables) (readTableFromFile "datasource/mps-declarations_rada.json"))
(setf (gethash "test" tables) (readTableFromFile "datasource/test.csv"))
;(setf (gethash "plenary_register_mps-skl9" tables) mps_declarations_rada)

; define keywords for sql-query
(defvar keyWords #("select"
				   "from"
				   "right join"
				   "left join"
				   "full outer join"
				   "inner join"
				   "where"
				   "order by"
				   "group by"
				   "having"
				   "limit"))
(defvar priorities (make-hash-table :test 'equal))
(setf (gethash "from" priorities) 1)
(setf (gethash "inner join" priorities) 2)
(setf (gethash "left join" priorities) 2)
(setf (gethash "right join" priorities) 2)
(setf (gethash "full outer join" priorities) 2)
(setf (gethash "where" priorities) 3)
(setf (gethash "group by" priorities) 4)
(setf (gethash "having" priorities) 5)
(setf (gethash "order by" priorities) 6)
(setf (gethash "select" priorities) 7)
(setf (gethash "limit" priorities) 8)
(setf (gethash "" priorities) 0)

(defun getPriority (kword priorities)
  (pprint (gethash kword priorities))
  (gethash kword priorities)
  )

(defun starts-with (str pattern)
  (let ((p (search pattern str)))
    (and p (= 0 p))
	)
  )

(defun iterate (index arr str)
  (cond
	((= index (length arr)) "")
	((starts-with str (aref arr index)) (aref arr index))
	(t (iterate (+ index 1) arr str))
	)
  )

(defun getKeyWord (queryStr keyWords)
  (iterate 0 keyWords queryStr)
  )

(defun make-fn (functionStr parametersStr)
  (pprint "in make-fn")
  (pprint (concatenate 'string functionStr "(" parametersStr ")"))
  (lambda ()
	(pprint (concatenate 'string functionStr "(" parametersStr ")"))
	)
  )

(defun parseQuery (queryStr fnStr parameters queue)
  ;(pprint (concatenate 'string "rest query -> " queryStr))
  ;(pprint (concatenate 'string "fnStr -> " fnStr))
  ;(pprint (concatenate 'string "parameters -> " parameters))
  (let ((kword (getKeyWord queryStr keyWords)))
	(cond
	  ((string= queryStr "") (pqueue:pqueue-push (make-fn fnStr parameters)
												 (getPriority fnStr priorities)
												 queue))
	  ((string= kword "") (parseQuery (removeOperator queryStr)
									  fnStr
									  (concatenate 'string parameters " " (getOperator queryStr))
									  queue))
	  (t (setf queue (pqueue:pqueue-push (make-fn fnStr parameters)
								         (getPriority fnStr priorities)
								         queue))
		 (parseQuery (subseq queryStr (length kword)) kword "" queue))
	  )
	)
  )

(defun print-queue (queue)
  (cond
	((pqueue:pqueue-empty-p queue) nil)
	(t (funcall (pqueue:pqueue-pop queue))
	   (print-queue queue))
	)
  )

(defun query (queryStr)
  (let ((queue (parseQuery queryStr "" "" (pqueue:make-pqueue #'<
													          :key-type 'integer
													          :value-type 'function))))
	(pqueue:pqueue-pop queue)
	(print-queue queue)
	)
  )

(defun loadTable (tableName)
  "load table command: print whole table"
  (query (concatenate 'string "select * from " tableName))
  )

(defun execute (commandQuery)
  "execute entered text"
  (let ((command (parseCommand commandQuery)))
	(cond
	  ((string= command "exit") (exit))
	  ((string= command "query") (query (cutParameter commandQuery)))
	  ((string= command "load") (loadTable (cutParameter commandQuery)))
	  (t (pprint "Error: entered command not fund!!!"))
	  )
	)
  )

(defun run ()
  "run cli"
  (terpri)
  (princ "[user@host ~]$: ")
  (terpri)
  (execute (read-line))
  (run)
 )

(run)

