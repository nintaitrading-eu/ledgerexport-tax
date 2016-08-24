#!/usr/local/bin/sbcl --script

; Author: Andy Nagels
; Date: 2016-08-23
;
; ledgerexport-tax.cl:
; Script that prepares data for the quarterly tax reports.
; It uses ledger data as a backend and also depends on vim for transforming
; the final report outputs from txt to pdf.

(defun f-usage (
  (format t "Usage: ledgerexport-tax~%" )))

(f-usage)
