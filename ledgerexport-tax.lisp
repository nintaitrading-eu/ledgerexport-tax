;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Author: Andy Nagels
; Date: 2016-08-23
;
; ledgerexport-tax.lisp:
; Script that prepares data for the quarterly tax reports.
; It uses ledger data as a backend and also depends on vim for transforming
; the final report outputs from txt to pdf.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "asdf")


; Global variables.
(defconstant +g-months+ (list 'JANUARY 'FEBRUARY 'MARCH 'APRIL 'MAY 'JUNE 'JULY 'AUGUST 'SEPTEMBER 'OKTOBER 'NOVEMBER 'DECEMBER))
(defconstant +g-termprefix+ ">>> ")
(defconstant +g-quarters+ (list 'Q1 'Q2 'Q3 'Q4))

; usage:
; Print usage info.
(defun usage ()
  (format t "Usage: sbcl --noinform --script ledgerexport-tax.lisp \"path/to/ledger.dat\" [Q1|Q2|Q3|Q4|month|-h]~%~%")
  (format t "Options:~%")
  (format t "~{~4tQ~a: exports the data for Q~a~%~}" (list 1 1 2 2 3 3 4 4))
  (format t "~4tmonth: exports the given month~%")
  (format t "~8t(valid months are: january, february, march, april, may,~%")
  (format t "~8t june, july, august, september, oktober, november, december)~%")
  (format t "~4t-h: shows this usage info message~%"))

; current-date-string:
; Returns the current date as a string in YYYYMMDD format.
(defun current-date-string ()
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
    (get-decoded-time)
    (declare (ignore sec min hr dow dst-p tz))
    (format nil "~4,'0d~2,'0d~2,'0d" yr mon day)))

; get-export-name:
; Determine name to use for the output.
(defun assemble-export-name (a-argument)
  (concatenate 'string "reg_" (current-date-string) "_V001_btw_" (string-upcase a-argument) ".txt") 
)

; export-to-txt:
; Export accounting register data to txt,
; for the given period.
(defun export-to-txt (a-argument)
  (format t "~aExporting data to ~a...~%" +g-termprefix+ (assemble-export-name a-argument))
  ; TODO: The below is a windows test for application calling. Remove it.
  ;(uiop:run-program `("C:\\Program Files (x86)\\Gow\\bin\\ls.exe" "-lh") :output t :error-output t)
  ; TODO: if a-argument in Q1-4 then call ledger with the appropriate dates.
  ; if a-argument in months then call ledger with -p? But what about the year?
  ; TODO: the below with (intern a-argument), only for the months.
  ; Also check if -p "january this year" is a valid PERIOD_EXPRESSION in ledger.
  (cond
    ((member (intern a-argument) +g-months+) (uiop:run-program `("ledger -f ledger.dat" "-p \"" (a-argument) " this year\" reg | sort -n > " (assemble-export-name a-argument)) :output t :error-output t))
    ((member (intern a-argument) +g-quarters+) (uiop:run-program `("ledger -f ledger.dat" "-b TBD reg | sort -n > " (assemble-export-name a-argument)) :output t :error-output t))
    (T (format t "Error: Unknown argument ~a... export failed!~%" a-argument)))
  
  ;ledger -f ledger.dat -b "2016/06/01" -e "2016/07/01" reg | sort -n > reg_(date +%Y%m%d)_V001_btw_Q1
)

; process-arguments:
; Print usage info
; or start export for a valid given period.
(defun process-arguments (a-ledger-file a-argument)
  (cond
    ((equal a-argument "-h") (usage))
    ; Note: (intern ...) = string->symbol
    ((probe-file (make-pathname :directory '(:absolute "") :name a-ledger-file)) (usage))
    ((or
      (member (intern a-argument) +g-quarters+)
      (member (intern a-argument) +g-months+))
        (export-to-txt a-argument))
    (T (usage))))

; main
; Main code processing.
; Note: sbcl --noinform --script ledger.dat Q1
; That makes for 5 arguments.
(defun main ()
  ; TODO: month->number?
  ; info: number->month = (nth 1 +g-months+) = February
  ; TODO: read cli params?
  (format t "[DEBUG] arg-lengt = ~a | a-ledger-file = ~a | a-argument = ~a~%" (length sb-ext:*posix-argv*) (nth 1 sb-ext:*posix-argv*) (nth 2 sb-ext:*posix-argv*))
  (cond
    ((eq (length sb-ext:*posix-argv*) 3) (process-arguments (nth 1 sb-ext:*posix-argv*) (string-upcase (nth 2 sb-ext:*posix-argv*))))
    (T (usage))))

; Main entry point, to start the code.
(main)
