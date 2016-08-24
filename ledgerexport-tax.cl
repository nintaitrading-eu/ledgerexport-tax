#!/usr/local/bin/sbcl --script

; Author: Andy Nagels
; Date: 2016-08-23
;
; ledgerexport-tax.cl:
; Script that prepares data for the quarterly tax reports.
; It uses ledger data as a backend and also depends on vim for transforming
; the final report outputs from txt to pdf.

; usage:
; Print usage info.
(defun usage ()
  (format t "Usage: ledgerexport-tax.cl [Q1|Q2|Q3|Q4|month|-h]~%~%")
  (format t "Options:~%")
  (format t "~{~4tQ~a: exports the data for Q~a~%~}" (list 1 1 2 2 3 3 4 4))
  (format t "~4tmonth: exports the given month~%")
  (format t "~4t-h: shows this usage info message~%"))

; get-export-name:
; Determine name to use for the output
(defun assemble-export-name ()
  ; TBD
  ; "reg_20160822_V001_btw_Q2.txt"
)

; export-to-txt:
; Export accounting register data to txt,
; for the given period.
(defun export-to-txt ()
  ; TBD
)

; main
; Main code processing.
(defun main ()
  (defconstant *g-months* (list "January" "February" "March" "April" "May" "June" "July" "August" "September" "Oktober" "November" "December"))
  (defconstant *g-termprefix* ">>> ")
  ; TODO: month->number?
  ; info: number->month = (nth 1 *g-months*) = February
  ; TODO: read cli params?
  (cond ((= (length sb-ext:*posix-argv*) 2) (export-to-txt))
        (T (usage))))

; Main entry point, to start the code.
(main)
