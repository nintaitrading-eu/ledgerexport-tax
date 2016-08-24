;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Author: Andy Nagels
; Date: 2016-08-23
;
; ledgerexport-tax.cl:
; Script that prepares data for the quarterly tax reports.
; It uses ledger data as a backend and also depends on vim for transforming
; the final report outputs from txt to pdf.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Package definition.
(defpackage :ledgerexport-tax
  (:use :common-lisp)
  (:export :main))

(ql:quickload :asdf)


; Global variables.
(defconstant +g-months+ (list 'january 'february 'march 'april 'may 'june 'july 'august 'september 'oktober 'november 'december))
(defconstant +g-termprefix+ ">>> ")
(defconstant +g-possible-arguments+ (list 'Q1 'Q2 'Q3 'Q4))

; usage:
; Print usage info.
(defun usage ()
  (format t "Usage: ledgerexport-tax.cl [Q1|Q2|Q3|Q4|month|-h]~%~%")
  (format t "Options:~%")
  (format t "~{~4tQ~a: exports the data for Q~a~%~}" (list 1 1 2 2 3 3 4 4))
  (format t "~4tmonth: exports the given month~%")
  (format t "~8t(valid months are: january, february, march, april, may,~%")
  (format t "~8t june, july, august, september, oktober, november, december)~%")
  (format t "~4t-h: shows this usage info message~%"))

; get-export-name:
; Determine name to use for the output
(defun assemble-export-name (a-argument)
  ; TODO: get current date etc.
  (concatenate 'string "reg_" "20160822" "_V001_btw_" (string-upcase a-argument) ".txt") 
  ; "reg_20160822_V001_btw_Q2.txt"
)

; export-to-txt:
; Export accounting register data to txt,
; for the given period.
(defun export-to-txt (a-output-file)
  (format t "~aExporting data to ~a...~%" +g-termprefix+ a-output-file)
  ; TODO: how to call external application?
  (run-program "ls")
  ;ledger -f ledger.dat -b "2016/06/01" -e "2016/07/01" reg | sort -n > reg_(date +%Y%m%d)_V001_btw_Q1
)

; process-arguments:
; Print usage info
; or start export for a valid given period.
(defun process-arguments (a-argument)
  (cond
    ((equal a-argument "-h") (usage))
    ; Note: (intern ...) = string->symbol
    ((or
      (member (intern a-argument) +g-possible-arguments+)
      (member (intern a-argument) +g-months+))
        (export-to-txt (assemble-export-name a-argument)))
    (T (usage))))

; main
; Main code processing.
(defun main ()
  ; TODO: month->number?
  ; info: number->month = (nth 1 +g-months+) = February
  ; TODO: read cli params?
  (format t "[DEBUG] Argument = ~a~%" (nth 1 sb-ext:*posix-argv*))
  (cond
    ((eq (length sb-ext:*posix-argv*) 2) (process-arguments (string-upcase (nth 1 sb-ext:*posix-argv*))))
    (T (usage))))

; Main entry point, to start the code.
(main)
