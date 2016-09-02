;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ledgerexport-tax.lisp:
;;;; Script that prepares data for the quarterly tax reports.
;;;; It uses ledger data as a backend and also depends on vim for transforming
;;;; the final report outputs from txt to pdf.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Global variables.
(defvar *g-months*
  (list 'JANUARY 'FEBRUARY 'MARCH 'APRIL 'MAY 'JUNE 'JULY 'AUGUST 'SEPTEMBER 'OKTOBER 'NOVEMBER 'DECEMBER) "A list of all the months in a year.")
(defvar *g-quarters*
  (list 'Q1 'Q2 'Q3 'Q4) "A list of all the quarters in a year.")
(defvar *g-termprefix*
  ">>> " "String to be prepended to output, for a fancier effect.")
(defvar *g-ledger-cmd*
  "ledger.cmd" "The ledger command that is used to retrieve/export the accounting data.") ; TODO: change command on freebsd to just ledger.

;;; Generic functions.
(defun terminate (a-status)
  "Exit program with exit status, but in a portable way."
  #+sbcl     (sb-ext:quit      :unix-status a-status)    ; SBCL
  #+ccl      (   ccl:quit      a-status)                 ; Clozure CL
  #+clisp    (   ext:quit      a-status)                 ; GNU CLISP
  #+cmu      (  unix:unix-exit a-status)                 ; CMUCL
  #+abcl     (   ext:quit      :status a-status)         ; Armed Bear CL
  #+allegro  (  excl:exit      a-status :quiet t)        ; Allegro CL
  (cl-user::quit))           ; Many implementations put QUIT in the sandbox CL-USER package.

(defun remove-file-if-exists (a-file)
  "Remove a file, if it exists."
  (with-open-file (z-stream a-file :direction :output :if-exists :supersede)
    (delete-file z-stream)))

(defun remove-file-or-abort (a-file)
  "Checks if a file exists and if it does, it deletes it.
Otherwise, the program aborts it's operation with exit status 1"
  (if (probe-file a-file)
    (progn
      (format t "Warning: Output file \"~a\" exists.~%" a-file)
      (cond ((y-or-n-p "Would you like to remove it and continue?") (remove-file-if-exists a-file))
            ((progn
              (format t "Aborted.~%")
              (terminate 1)))))))

(defun current-date-string (&optional a-with-dash)
  "Returns the current date as a string in YYYYMMDD format."
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
    (get-decoded-time)
    (declare (ignore sec min hr dow dst-p tz))
    (cond
      (a-with-dash (format nil "~4,'0d-~2,'0d-~2,'0d" yr mon day))
      (T (format nil "~4,'0d~2,'0d~2,'0d" yr mon day)))))

(defun current-year-int ()
  "Returns an integer, representing the current year."
  (nth-value 5 (get-decoded-time)))
  

(defun print-done ()
  "Write <space>Done. to standard output."
  (format t " Done.~%"))

(defun get-begindate-from-quarter (a-quarter)
  "Get the start-date for a given quarter. The given quarter should be a symbol, e.g. 'Q1"
  (cond
    ((equal a-quarter 'Q1) (format nil "~4,'0d-~2,'0d-~2,'0d" (current-year-int) 1 1))
    ((equal a-quarter 'Q2) (format nil "~4,'0d-~2,'0d-~2,'0d" (current-year-int) 4 1))
    ((equal a-quarter 'Q3) (format nil "~4,'0d-~2,'0d-~2,'0d" (current-year-int) 7 1))
    ((equal a-quarter 'Q4) (format nil "~4,'0d-~2,'0d-~2,'0d" (current-year-int) 10 1))
    (T (format t "Error: unknown quarter ~a~%" a-quarter))
  ))

(defun get-enddate-from-quarter (a-quarter)
  "Get the end-date for a given quarter. The given quarter should be a symbol, e.g. 'Q1"
  ; Note: ledger is not inclusive, so we return the end-date of the quarter + 1.
  ; e.g.: 'Q1 => 31st of March = enddate, but we return April 1st
  ; Note the year change for Q4.
  (cond
    ((equal a-quarter 'Q1) (format nil "~4,'0d-~2,'0d-~2,'0d" (current-year-int) 4 1))
    ((equal a-quarter 'Q2) (format nil "~4,'0d-~2,'0d-~2,'0d" (current-year-int) 7 1))
    ((equal a-quarter 'Q3) (format nil "~4,'0d-~2,'0d-~2,'0d" (current-year-int) 10 1))
    ((equal a-quarter 'Q4) (format nil "~4,'0d-~2,'0d-~2,'0d" (+ current-year-int 1) 1 1))
    (T (format t "Error: unknown quarter ~a~%" a-quarter))
  ))

;;; Application specific functions.
(defun usage ()
  "Print usage info."
  (format t "Usage: sbcl --noinform --script ledgerexport-tax.lisp \"path/to/ledger.dat\" [Q1|Q2|Q3|Q4|month|-h]~%~%")
  (format t "Options:~%")
  (format t "~{~4tQ~a: exports the data for Q~a~%~}" (list 1 1 2 2 3 3 4 4))
  (format t "~4tmonth: exports the given month~%")
  (format t "~8t(valid months are: january, february, march, april, may,~%")
  (format t "~8t june, july, august, september, oktober, november, december)~%")
  (format t "~4t-h: shows this usage info message~%"))

(defun assemble-export-name (a-argument a-extension)
  "Determine name to use for the output."
  (concatenate 'string "reg_" (current-date-string) "_V001_btw_" (string-upcase a-argument) a-extension)
)

(defun export-to-txt-cmd (a-file a-command-pipe)
  "Export accounting register data to txt. This executes the given
inferior-shell command with pipe, while exporting the results to a
given file."
  (with-open-file (z-stream a-file :direction :output :if-exists :error)
    (format z-stream (inferior-shell:run/ss a-command-pipe)))) 

(defun export-to-txt (a-ledger-file a-argument)
  "Export accounting register data to txt, for the given period."
  (format t "~aRemoving previous export data \"~a\")..." *g-termprefix* (assemble-export-name a-argument ".txt"))
  (remove-file-or-abort (assemble-export-name a-argument ".txt"))
  (print-done)
  (format t "~aExporting data to ~a...~%" *g-termprefix* (assemble-export-name a-argument ".txt"))
  ; TODO: if a-argument in Q1-4 then call ledger with the appropriate dates.
  ; if a-argument in months then call ledger with -p? But what about the year?
  ; TODO: the below with (intern a-argument), only for the months.
  ; Also check if -p "january this year" is a valid PERIOD_EXPRESSION in ledger.
  ; TODO: ask for removal of output file
  ;ledger -f ledger.dat -b "2016/06/01" -e "2016/07/01" reg | sort -n > reg_(date +%Y%m%d)_V001_btw_Q1
  (cond
    ((member (intern a-argument) *g-months*)
    (export-to-txt-cmd
      (assemble-export-name a-argument ".txt")
      `(inferior-shell:pipe (ls.exe -lh) (grep ledger))) ; windows
      ;`(inferior-shell:pipe (/bin/ls -lh) (grep ledger))) ; FreeBSD
    )
    ((member (intern a-argument) *g-quarters*)
    (export-to-txt-cmd
      (assemble-export-name a-argument ".txt")
        `(inferior-shell:pipe (ls.exe -lh) (grep ledger))) ; windows
        ;`(inferior-shell:pipe (/bin/ls -lh) (grep ledger))) ; FreeBSD
    )
    (T (format t "Error: Unknown argument ~a... export failed!~%" a-argument)))
  (print-done)
  ; TODO: use this to construct the commands
  (format t "[DEBUG] begindate Q1 = ~a~%" (get-begindate-from-quarter 'Q1))
  (format t "[DEBUG] enddate Q1 = ~a~%" (get-enddate-from-quarter 'Q1))
)

(defun process-arguments (a-ledger-file a-argument)
  "Print usage info or start export for a valid given period."
  (cond
    ((equal a-argument "-h") (usage))
    ; Note: (intern ...) = string->symbol
    ((not (probe-file a-ledger-file)) (format t "Error: ~a does not exist...~%" a-ledger-file))
    ((or
      (member (intern a-argument) *g-quarters*)
      (member (intern a-argument) *g-months*))
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
;; Note:
;; For testing cli parameters in sbcl:
;; (setf sb-ext:*posix-argv* (list "sbcl" "/home/rockwolf/doc/ledger/ledger.dat" "Q1"))
(main)
