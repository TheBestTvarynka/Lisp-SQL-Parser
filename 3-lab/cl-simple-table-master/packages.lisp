;;;; Simple table data structure.
;;;; Copyright (c) 2013, Francisco Soto All rights reserved (see COPYING file for details).

(in-package :cl-user)

(defpackage :cl-simple-table
  (:nicknames :simple-table)
  (:use :cl)
  (:export :make-table
           :make-row
           :add-to-table
           :add-to-row
           :get-row
           :get-row-column
           :set-row-column
           :num-rows
           :num-cols
           :num-col
           :rectangular-table-p
           :sequence->row
           :row-sequence->table
           :with-rows
           :select
           :select1
           :distinct
           :top
           :order-by
           :where
           :where-filter
           :where-or
           :where-and
           :read-csv
           :read-tsv
           :split-string
           :table-from-file))
