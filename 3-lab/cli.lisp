
(require 'asdf)
(load "/home/qkation/Documents/FunctionalProgramming/3-lab/cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(defvar map_zal (simple-table:read-csv #P"map_zal-skl9.csv" t))
(defvar mp_assistants (simple-table:read-csv #P"mp-assistants.csv" t))
;(defvar mp_posts (simple-table:read-csv #P"mp-posts_full.csv" t))
(defvar plenary_register_mps (simple-table:read-tsv #P"plenary_register_mps-skl9.tsv"))

(defun printTable(simple_table row)(cond
								 ((= row 0) (pprint (simple-table:get-row 0 simple_table)))
							     (t (printTable simple_table (- row 1))
									(pprint (simple-table:get-row row simple_table)))
								 )
)
(defun printAll(tableName)(cond
							((string= tableName "map_zal-skl9")
							 (printTable map_zal (- (simple-table:num-rows map_zal) 1)))
							((string= tableName "mp-assistants")
							 (printTable mp_assistants (- (simple-table:num-rows mp_assistants) 1)))
							((string= tableName "plenary_register_mps-skl9")
							 (printTable plenary_register_mps (- (simple-table:num-rows plenary_register_mps) 1)))
							(t (princ (concatenate 'string "table not found: " tableName)))
						  )
)

(defun cutName(command)(cond ((not (position #\( command)) command)
							 (t (subseq command 0 (position #\( command)))
					   )
)
(defun cutParameter(command)(subseq command (+ (position #\( command) 1) (position #\) command)))

(defun execute(command)(cond ((string= (cutName command) "exit") "EXIT")
							 ((string= (cutName command) "load")
							   (printAll (cutParameter command)))
							 (t (princ "command not found!"))
					   )
)

(defun run ()
    (loop
        (terpri)
        (princ "[user@host ~]$: ")
		(terpri)
		(if (string= (execute (read-line)) "EXIT")
		  (return)
		  ()
		  )
    )
)

(run)
