#!/usr/local/bin/sbcl --script

; Author: Andy Nagels
; Date: 2016-08-23
;
; ledgerexport-tax.cl:
; Script that prepares data for the quarterly tax reports.
; It uses ledger data as a backend and also depends on vim for transforming
; the final report outputs from txt to pdf.

; f-usage:
; Print usage info.
(defun f-usage ()
  (format t "Usage: ledgerexport-tax.cl [Q1|Q2|Q3|Q4|month|-h]~%~%")
  (format t "Options:~%")
  (format t "~{~4tQ~a: exports the data for Q~a~%~}" (list 1 1 2 2 3 3 4 4))
  (format t "~4tmonth: exports the given month~%")
  (format t "~4t-h: shows this usage info message~%"))

; f-main
; Main code processing.
(defun f-main ()
  (f-usage))

; Main entry point, to start the code.
(f-main)
