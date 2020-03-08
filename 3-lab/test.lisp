(defun myFun(i)(defvar name '(1 2 3 4)))

(myFun "NAME")
(princ name)


(defun generateSequence(n)(cond ((< n 1) '())
                                ((= n 1) '(1))
                                (t (append
                                     (generateSequence (- n 1))
                                     (list n)))
                                ))
(write (generateSequence 5))
