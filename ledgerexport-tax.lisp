;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ledgerexport-tax.lisp:
;;;; Script that prepares data for the quarterly tax reports.
;;;; It uses ledger data as a backend and also depends on vim for transforming
;;;; the final report outputs from txt to pdf.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Packages.

;;; Global variables.
(defconstant +g-months+ (list 'JANUARY 'FEBRUARY 'MARCH 'APRIL 'MAY 'JUNE 'JULY 'AUGUST 'SEPTEMBER 'OKTOBER 'NOVEMBER 'DECEMBER))
(defconstant +g-quarters+ (list 'Q1 'Q2 'Q3 'Q4))
(defconstant +g-termprefix+ ">>> ")
(defconstant +g-ledger-cmd+ "ledger.cmd") ; TODO: change command on freebsd to just ledger.

;;; Functions.
(defun usage ()
  "Print usage info."
  (format t "Usage: sbcl --noinform --script ledgerexport-tax.lisp \"path/to/ledger.dat\" [Q1|Q2|Q3|Q4|month|-h]~%~%")
  (format t "Options:~%")
  (format t "~{~4tQ~a: exports the data for Q~a~%~}" (list 1 1 2 2 3 3 4 4))
  (format t "~4tmonth: exports the given month~%")
  (format t "~8t(valid months are: january, february, march, april, may,~%")
  (format t "~8t june, july, august, september, oktober, november, december)~%")
  (format t "~4t-h: shows this usage info message~%"))

(defun current-date-string ()
  "Returns the current date as a string in YYYYMMDD format."
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
    (get-decoded-time)
    (declare (ignore sec min hr dow dst-p tz))
    (format nil "~4,'0d~2,'0d~2,'0d" yr mon day)))

(defun assemble-export-name (a-argument a-extension)
  "Determine name to use for the output."
  (concatenate 'string "reg_" (current-date-string) "_V001_btw_" (string-upcase a-argument) a-extension)
)

(defun remove-file-if-exists (a-file)
  "Checks if a file exists and if it does, it deletes it."
  (with-open-file (s a-file :direction :output :if-exists :error)
    (delete-file s)))

(defun print-done ()
  "Write Done. to standard output."
  (format t " Done.~%"))

(defun export-to-txt (a-ledger-file a-argument)
  "Export accounting register data to txt, for the given period."
  (format t "~aRemoving previous export data (~a)..." +g-termprefix+ (assemble-export-name a-argument ".txt"))
  (remove-file-if-exists (assemble-export-name a-argument ".txt"))
  (print-done)
  (format t "~aExporting data to ~a...~%" +g-termprefix+ (assemble-export-name a-argument ".txt"))
  ; TODO: if a-argument in Q1-4 then call ledger with the appropriate dates.
  ; if a-argument in months then call ledger with -p? But what about the year?
  ; TODO: the below with (intern a-argument), only for the months.
  ; Also check if -p "january this year" is a valid PERIOD_EXPRESSION in ledger.
  ; TODO: check if output-file exists and ask for removal
  ; TODO: implement file output in common-lisp directly:
  ; (with-open-file (a-stream "output.txt" :direction :output :if-exists :error)
  ;   (format a-stream (inferior-shell:run/ss `(inferior-shell:pipe (ls.exe) (grep ledger)))))
  (cond
    ((member (intern a-argument) +g-months+)
      (inferior-shell:run/ss `(inferior-shell:pipe
        (ls.exe -lh) (grep ledger)) ; windows
        ;("/bin/ls" "-lh") (grep "ledger")) ; FreeBSD
        :output (assemble-export-name a-argument ".txt")))
    ((member (intern a-argument) +g-quarters+)
      (inferior-shell:run/ss `(inferior-shell:pipe
        (ls.exe -lh) (grep ledger))) ; windows
        ;("/bin/ls" "-lh") (grep "ledger")) ; FreeBSD
        :output (assemble-export-name a-argument ".txt"))
    (T (format t "Error: Unknown argument ~a... export failed!~%" a-argument)))
  (print-done)
  ;ledger -f ledger.dat -b "2016/06/01" -e "2016/07/01" reg | sort -n > reg_(date +%Y%m%d)_V001_btw_Q1
)

(defun process-arguments (a-ledger-file a-argument)
  "Print usage info or start export for a valid given period."
  (cond
    ((equal a-argument "-h") (usage))
    ; Note: (intern ...) = string->symbol
    ((not (probe-file a-ledger-file)) (format t "Error: ~a does not exist...~%" a-ledger-file))
    ((or
      (member (intern a-argument) +g-quarters+)
      (member (intern a-argument) +g-months+))
        (export-to-txt a-ledger-file a-argument))
    (T (usage))))

(defun main ()
  "Main code processing.
Note: sbcl --noinform --script ledger.dat Q1
That makes for 5 arguments. But sbcl --noinform --script counts as 1 whole.
So that leaves 3 arguments to be checked for..."
  ; TODO: add extra code, that calls process-arguments for JANUARY/FEBRUARI/MARCH,
  ; when called with Q1. Do the same logic for Q2-4.
  (cond
    ((eq (length sb-ext:*posix-argv*) 3)
      (process-arguments (nth 1 sb-ext:*posix-argv*) (string-upcase (nth 2 sb-ext:*posix-argv*))))
    (T (usage))))

;;; Main entry point, to start the code.
(main)
