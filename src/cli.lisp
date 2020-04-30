
(require 'asdf)
(load "priority-queue/priority-queue.asd")
(asdf:load-system 'priority-queue)
; load all functionality code
(load "getenv.lisp")

(load "importer.lisp")
(load "print.lisp")
(load "where.lisp")
(load "orderby.lisp")
(load "select.lisp")
(load "joins.lisp")
(load "union.lisp")
(load "groupby.lisp")
(load "having.lisp")
(load "limit.lisp")

; tables - hashmap where key is tablename and value is a table
(defvar tables (make-hash-table :test 'equal))
(setf (gethash "map_zal-skl9" tables) (readTableFromFile "datasource/map_zal-skl9.csv"))
(setf (gethash "mp-assistants" tables) (readTableFromFile "datasource/mp-assistants.csv"))
(setf (gethash "mp-posts_full" tables) (readTableFromFile "datasource/mp-posts_full.csv"))
(setf (gethash "mps-declarations_rada" tables) (readTableFromFile "datasource/mps-declarations_rada.json"))
(setf (gethash "test" tables) (readTableFromFile "datasource/test.csv"))
(setf (gethash "test2" tables) (readTableFromFile "datasource/test2.csv"))
(setf (gethash "test3" tables) (readTableFromFile "datasource/test3.csv"))
(setf (gethash "test4" tables) (readTableFromFile "datasource/test4.csv"))
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

; priorities for every operation
(defvar priorities (make-hash-table :test 'equal))
(setf (gethash "from" priorities) 1)
(setf (gethash "inner join" priorities) 2)
(setf (gethash "left join" priorities) 2)
(setf (gethash "right join" priorities) 2)
(setf (gethash "full outer join" priorities) 2)
(setf (gethash "where" priorities) 3)
(setf (gethash "group by" priorities) 4)
(setf (gethash "having" priorities) 5)
(setf (gethash "select" priorities) 6)
(setf (gethash "order by" priorities) 7)
(setf (gethash "limit" priorities) 8)
(setf (gethash "" priorities) 0)

(defun from (tableStr &optional table)
  "returns table by tablename"
  (gethash (string-trim " " tableStr) tables)
  )

; hashmap with all functions for quering
(defvar functions (make-hash-table :test 'equal))
(setf (gethash "from" functions) #'from)
(setf (gethash "inner join" functions) #'join)
(setf (gethash "left join" functions) #'join)
(setf (gethash "right join" functions) #'join)
(setf (gethash "full outer join" functions) #'join)
(setf (gethash "where" functions) #'where)
(setf (gethash "group by" functions) #'groupBy)
(setf (gethash "having" functions) #'having)
(setf (gethash "order by" functions) #'orderBy)
(setf (gethash "select" functions) #'select)
(setf (gethash "limit" functions) #'limit)
(setf (gethash "" functions) nil)

(defun getPriority (kword priorities)
  "returns priority of operation"
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
  "returns sql-keyword if queryStr starts with one of keyWords or '' if not"
  (iterateArr 0 keyWords queryStr)
  )

(defun make-fn (functionStr parametersStr)
  "makes function (lambda) that do some operation on the table corresponds to function name (functionStr)"
  (cond
	((search "join" functionStr)
	 (lambda (table)
	   (funcall (gethash functionStr functions) (concatenate 'string functionStr parametersStr) table tables)
	   ))
	(t (lambda (table)
		 (funcall (gethash functionStr functions) parametersStr table)
		 ))
	)
  )

(defun parseQuery (queryStr fnStr parameters queue)
  "this function parses one query. means without 'union'. only one select"
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
  "execute query. queue - queue with all operation that query contains"
  (cond
	((pqueue:pqueue-empty-p queue) table)
	(t (execute-queue (funcall (pqueue:pqueue-pop queue) (copy-table table)) queue))
	)
  )

(defun makeSimpleQuery (queryStr)
  "returns lambda that executes the query if we call it"
  (let ((queue (parseQuery queryStr "" "" (pqueue:make-pqueue #'<
															  :key-type 'integer
															  :value-type 'function))))
	(pqueue:pqueue-pop queue)
	(lambda ()
	  (execute-queue (make-table) queue)
	  )
	)
  )

(defun buildQueries (queryStr)
  "splits queryStr on the queries by 'union' operator and make tree-like structure.
  returns lambda that executes all queries and does union if it needs"
  (let ((unionPos (search "union" queryStr)))
	(cond
	  ((not unionPos) (makeSimpleQuery queryStr))
	  (t (let ()
		   (lambda ()
			 (funcall #'unionTables (makeSimpleQuery (subseq queryStr 0 unionPos)) (buildQueries (subseq queryStr (+ unionPos 6))))
			 )
		   ))
	  )
	)
  )

(defun query (queryStr)
  "builds function for executing the query and executes its
  prints result of quering"
  (let ((fn (buildQueries queryStr)))
	(printTable (funcall fn))
	)
  )

(defun loadTable (tableName)
  "loads table command: print whole table"
  (query (concatenate 'string "select * from " tableName))
  )

(defun execute (commandQuery)
  "executes entered text"
  (let ((command (parseCommand commandQuery)))
	(cond
	  ((string= command "exit") (exit))
	  ((string= command "query") (query (cutParameter commandQuery)))
	  ((string= command "load") (loadTable (cutParameter commandQuery)))
	  (t (princ "Error: entered command not found!!!"))
	  )
	)
  )

(defun run ()
  "runs cli"
  (terpri)
  (princ (format nil "[~A@~A] $: " (getEnvVariable "USER") (getEnvVariable "PWD")))
  (terpri)
  (execute (read-line))
  (run)
 )

(run)

