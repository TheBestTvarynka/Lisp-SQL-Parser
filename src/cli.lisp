
(require 'asdf)
(load "priority-queue/priority-queue.asd")
(asdf:load-system 'priority-queue)
; load all functionality code
(load "importer.lisp")
(load "print.lisp")
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

(defun from (tableStr &optional table)
  ;(pprint "in from")
  ;(pprint tableStr)
  (gethash (string-trim " " tableStr) tables)
  )

(defvar functions (make-hash-table :test 'equal))
(setf (gethash "from" functions) #'from)
(setf (gethash "inner join" functions) nil)
(setf (gethash "left join" functions) nil)
(setf (gethash "right join" functions) nil)
(setf (gethash "full outer join" functions) nil)
(setf (gethash "where" functions) #'where)
(setf (gethash "group by" functions) nil)
(setf (gethash "having" functions) nil)
(setf (gethash "order by" functions) #'orderBy)
(setf (gethash "select" functions) #'select)
(setf (gethash "limit" functions) nil)
(setf (gethash "" functions) nil)

(defun getPriority (kword priorities)
  (gethash kword priorities)
  )

(defun iterateArr (index arr str)
  (cond
	((= index (length arr)) "")
	((starts-with str (aref arr index)) (aref arr index))
	(t (iterateArr (+ index 1) arr str))
	)
  )

(defun getKeyWord (queryStr keyWords)
  (iterateArr 0 keyWords queryStr)
  )

(defun make-fn (functionStr parametersStr)
  ;(pprint (concatenate 'string functionStr "(" parametersStr ")"))
  (cond
	((search "join" functionStr)
	 (lambda (table)
	   (funcall (gethash functionStr functions) parametersStr table tables)
	   ))
	(t (lambda (table)
		 (funcall (gethash functionStr functions) parametersStr table)
		 ))
	)
  )

(defun parseQuery (queryStr fnStr parameters queue)
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

(defun execute-queue (table queue)
  (cond
	((pqueue:pqueue-empty-p queue) table)
	(t (execute-queue (funcall (pqueue:pqueue-pop queue) (copy-table table)) queue))
	)
  )

(defun query (queryStr)
  (let ((queue (parseQuery queryStr "" "" (pqueue:make-pqueue #'<
													          :key-type 'integer
													          :value-type 'function))))
	(pqueue:pqueue-pop queue)
	;(pprint queue)
	(printTable (execute-queue (make-table) queue))
	)
  )

(defun loadTable (tableName)
  "load table command: print whole table"
  (query (concatenate 'string "select * from " tableName))
  )

(defun parseCommand (commandQuery) 
  "cut command name" 
  (let ((openBracketPosition (position #\( commandQuery))) 
    (setq openBracketPosition (cond 
                                ((not openBracketPosition) 0) 
                                (t openBracketPosition) 
                                )) 
    (subseq commandQuery 0 openBracketPosition) 
    ) 
  )

(defun cutParameter (command)
  "cut command parameter (text inside '()')"
  (subseq command (+ (position #\( command) 1) (position #\) command :from-end t))
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

