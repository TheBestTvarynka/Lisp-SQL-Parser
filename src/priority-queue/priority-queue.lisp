;;;; PRIORITY-QUEUE -- a priority queue for Common Lisp
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licence:
;;;;
;;;;  Permission is hereby granted, free of charge, to any person
;;;;  obtaining a copy of this software and associated documentation files
;;;;  (the "Software"), to deal in the Software without restriction,
;;;;  including without limitation the rights to use, copy, modify, merge,
;;;;  publish, distribute, sublicense, and/or sell copies of the Software,
;;;;  and to permit persons to whom the Software is furnished to do so,
;;;;  subject to the following conditions:
;;;;
;;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :priority-queue
  (:use :cl)
  (:nicknames :pqueue)
  (:export
   #:make-pqueue
   #:pqueue-p
   #:pqueue-length
   #:pqueue-empty-p
   #:pqueue-push
   #:pqueue-pop
   #:pqueue-front
   #:pqueue-front-value
   #:pqueue-front-key
   #:pqueue-clear))

(in-package :priority-queue)

(defstruct (pqueue (:constructor %make-pqueue)
		   (:print-function print-pqueue))
  length predicate keys values key-type value-type)

(defun print-pqueue (pqueue stream depth)
  "Print the priority queue."
  (declare (ignore depth))
  (print-unreadable-object (pqueue stream)
    (format stream "PRIORITY-QUEUE :COUNT ~d" (pqueue-length pqueue))))

(defun make-pqueue (predicate &key (key-type t) (value-type t))
  "Make a priority queue with the specfied types for keys and values."
  (%make-pqueue :length 0
		:predicate predicate
		:keys (make-array 11 :element-type key-type)
		:values (make-array 11 :element-type value-type)
		:key-type key-type
		:value-type value-type))

(defun increase (pqueue capacity)
  "Increase the capacity of the priority queue."
  (let ((n (pqueue-length pqueue)))
    (declare (fixnum n))
    (let* ((capacity2 (if (< n 64) (* 2 (1+ n)) (* 3 (truncate (/ n 2)))))
	   (capacity3 (max capacity capacity2)))
      (assert (>= capacity2 0))
      (assert (>= capacity3 0))
      (let ((keys (pqueue-keys pqueue))
	    (values (pqueue-values pqueue))
	    (keys2 (make-array capacity3 :element-type (pqueue-key-type pqueue)))
	    (values2 (make-array capacity3 :element-type (pqueue-value-type pqueue))))
	(loop for i from 0 below n
	   do (setf (aref keys2 i) (aref keys i)
		    (aref values2 i) (aref values i)))
	(setf (pqueue-keys pqueue) keys2
	      (pqueue-values pqueue) values2)))))

(defun sift-up (index predicate keys values key value) 
  "Sift up the key and value starting at the specified index."
  (declare (fixnum index))
  (if (zerop index)
      (setf (aref keys index) key
	    (aref values index) value)
      (let ((n (truncate (/ (1- index) 2))))
	(if (not (funcall predicate key (aref keys n)))
	    (setf (aref keys index) key
		  (aref values index) value)
	    (progn 
	      (setf (aref keys index) (aref keys n)
		    (aref values index) (aref values n))
	      (sift-up n predicate keys values key value))))))
		    
(defun sift-down (index length predicate keys values key value)
  "Sift down the key and value starting at the specified index."
  (declare (fixnum index length))
  (if (>= index (truncate (/ length 2)))
      (setf (aref keys index) key
	    (aref values index) value)
      (let* ((n3 (1+ (* 2 index)))
	     (n2 (1+ n3))
	     (n (if (and (< n2 length)
			 (funcall predicate (aref keys n2) (aref keys n3)))
		    n2
		    n3)))
	(if (not (funcall predicate (aref keys n) key))
	    (setf (aref keys index) key
		  (aref values index) value)
	    (progn
	      (setf (aref keys index) (aref keys n)
		    (aref values index) (aref values n))
	      (sift-down n length predicate keys values key value))))))

(defun pqueue-empty-p (pqueue)
  "Test whether the priority queue is empty."
  (zerop (pqueue-length pqueue)))

(defun pqueue-push (value key pqueue)
  "Enqueue a new value with the specified priority key and return the queue."
  (let ((index (pqueue-length pqueue)))
    (declare (fixnum index))
    (let ((predicate (pqueue-predicate pqueue))
	  (keys (pqueue-keys pqueue)))
      (when (>= (1+ index) (length keys))
	(increase pqueue (1+ (1+ index))))  ; 1+ to have a default value in the end
      (setf (pqueue-length pqueue) (1+ index))
      (let ((keys (pqueue-keys pqueue))
	    (values (pqueue-values pqueue)))
	(sift-up index predicate keys values key value)
	pqueue))))

(defun pqueue-pop (pqueue)
  "Dequeue the element with the minimal priority key and return 
a multiple value: the element value, its key."
  (let ((length (pqueue-length pqueue)))
    (declare (fixnum length))
    (assert (> length 0) nil "The priority queue is empty.")
    (let ((predicate (pqueue-predicate pqueue))
	  (keys (pqueue-keys pqueue))
	  (values (pqueue-values pqueue))
	  (index (1- length)))
      (setf (pqueue-length pqueue) index)
      (let ((key (aref keys index))
	    (key0 (aref keys 0))
	    ;; we use KEY* to release the old reference
	    (key* (aref keys length))
	    (value (aref values index))
	    (value0 (aref values 0))
	    ;; we use VALUE* to release the old reference
	    (value* (aref values length)))
	(setf (aref keys index) key*
	      (aref values index) value*)
	(sift-down 0 index predicate keys values key value)
	(values value0 key0)))))
	  
(defun pqueue-front (pqueue)
  "Take an element with the minimal priority key and return 
a multiple value: the element value, its key."
  (let ((length (pqueue-length pqueue)))
    (declare (fixnum length))
    (assert (> length 0) nil "The priority queue is empty.")
    (let ((keys (pqueue-keys pqueue))
	  (values (pqueue-values pqueue)))
      (let ((key0 (aref keys 0))
	    (value0 (aref values 0)))
	(values value0 key0)))))
  
(defun pqueue-front-value (pqueue)
  "Return the value with the minimal priority key."
  (let ((length (pqueue-length pqueue)))
    (declare (fixnum length))
    (assert (> length 0) nil "The priority queue is empty.")
    (let ((values (pqueue-values pqueue)))
      (aref values 0))))
    
(defun pqueue-front-key (pqueue)
  "Return the minimal priority key."
  (let ((length (pqueue-length pqueue)))
    (declare (fixnum length))
    (assert (> length 0) nil "The priority queue is empty.")
    (let ((keys (pqueue-keys pqueue)))
      (aref keys 0))))

(defun pqueue-clear (pqueue)
  "Clear the piority queue."
  (let ((length (pqueue-length pqueue)))
    (declare (fixnum length))
    (when (> length 0)
      (setf (pqueue-length pqueue) 0)
      (let ((keys (pqueue-keys pqueue))
	    (values (pqueue-values pqueue)))
	(let ((key* (aref keys length))
	      (value* (aref values length)))
	  (loop for i from 0 below length
	     do (setf (aref keys i) key*
		      (aref values i) value*)))))))
	  
