;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ledgerexport-tax.lisp:
;;;; Script that prepares data for the quarterly tax reports.
;;;; It uses ledger data as a backend.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Global variables.
(defvar *g-months*
  (list 'JANUARY 'FEBRUARY 'MARCH 'APRIL 'MAY 'JUNE 'JULY 'AUGUST 'SEPTEMBER 'OKTOBER 'NOVEMBER 'DECEMBER) "A list of all the months in a year.")
(defvar *g-quarters*
  (list 'Q1 'Q2 'Q3 'Q4) "A list of all the quarters in a year.")
(defvar *g-termprefix*
  ">>> " "String to be prepended to output, for a fancier effect.")
(defvar *g-ledger-cmd*
  "/usr/local/bin/ledger" "The ledger command that is used to retrieve/export the accounting data.") ; FreeBSD

;;; Generic functions.
(defun terminate (a-status)
  "Exit program with exit status, but in a portable way."
  #+sbcl     (sb-ext:exit      :unix-status a-status)    ; SBCL
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
  (format t "Usage: ledgerexport-tax \"path/to/ledger.dat\" [Q1|Q2|Q3|Q4|month|-h]~%~%")
  (format t "Options:~%")
  (format t "~{~4tQ~a: exports the data for Q~a~%~}" (list 1 1 2 2 3 3 4 4))
  (format t "~4tmonth: exports the given month~%")
  (format t "~8t(valid months are: january, february, march, april, may,~%")
  (format t "~8t june, july, august, september, oktober, november, december)~%")
  (format t "~4t-h: shows this usage info message~%"))

(defun assemble-export-name (a-argument a-extension)
  "Determine name to use for the output."
  (concatenate 'string "reg_" (current-date-string) "_V001_btw_" (string-upcase (string a-argument)) a-extension)
)

(defun export-to-txt-cmd (a-file a-command-pipe)
  "Export accounting register data to txt. This executes the given
inferior-shell command with pipe, while exporting the results to a
given file."
  (with-open-file (z-stream a-file :direction :output :if-exists :error)
    (format z-stream (inferior-shell:run/ss a-command-pipe))))

(defun export-to-txt (a-ledger-file a-argument)
  "Export accounting register data to txt, for the given period."
  (format t "~aRemoving previous export data \"~a\"..." *g-termprefix* (assemble-export-name a-argument ".txt"))
  (remove-file-or-abort (assemble-export-name a-argument ".txt"))
  (print-done)
  (format t "~aExporting data to ~a...~%" *g-termprefix* (assemble-export-name a-argument ".txt"))
  (cond
    ((member a-argument *g-months*)
    (progn
      (export-month a-ledger-file a-argument)
    ))
    ((member a-argument *g-quarters*)
    (progn
      (export-quarter a-ledger-file a-argument)
    ))
    (T (format t "~%Error: Unknown argument ~a... export failed!~%" (string a-argument))))
  (print-done)
)

(defun export-month (a-ledger-file a-month)
  "Export accounting register data to txt, for the given month."
  (format t "~aExecuting command ~a -f ~a -p ~a reg | sort -n..." *g-termprefix* *g-ledger-cmd* a-ledger-file (concatenate 'string "\"" (string a-month) " " (write-to-string (current-year-int)) "\""))
  (export-to-txt-cmd
    (assemble-export-name a-month ".txt")
    `(inferior-shell:pipe (,*g-ledger-cmd* -f ,a-ledger-file -p ,(concatenate 'string (string a-month) " " (write-to-string (current-year-int))) reg) (sort -n)))
)

(defun export-quarter (a-ledger-file a-quarter)
  "Export accounting register data to several txt files, for the given quarter and also each month in the quarter."
  ; Everything from Qn
  (format t "~aExecuting command ~a -f ~a -b ~a -e ~a reg | sort -n..." *g-termprefix* *g-ledger-cmd* a-ledger-file (get-begindate-from-quarter a-quarter) (get-enddate-from-quarter a-quarter))
  (export-to-txt-cmd
    (assemble-export-name a-quarter ".txt")
    `(inferior-shell:pipe (,*g-ledger-cmd* -f ,a-ledger-file -b ,(get-begindate-from-quarter a-quarter) -e ,(get-enddate-from-quarter a-quarter) reg) (sort -n)))
  ; Everything from the 1st month of Qn
  (export-month a-ledger-file (get-month-1-from-quarter a-quarter))
  ; Everything from the 2nd month of Qn
  (export-month a-ledger-file (get-month-2-from-quarter a-quarter))
  ; Everything from the 3rd month of Qn
  (export-month a-ledger-file (get-month-3-from-quarter a-quarter))
)

(defun get-month-1-from-quarter (a-quarter)
  "Get the first month symbol, from a given quarter symbol."
  (nth 0 *g-months*) ; TODO: get the correct month
)

(defun get-month-2-from-quarter (a-quarter)
  "Get the second month symbol, from a given quarter symbol."
  (nth 1 *g-months*) ; TODO: get the correct month
)

(defun get-month-3-from-quarter (a-quarter)
  "Get the third month symbol, from a given quarter symbol."
  (nth 2 *g-months*) ; TODO: get the correct month
)

(defun process-arguments (a-ledger-file-str a-argument-str)
  "Print usage info or start export for a valid given period."
  (cond
    ((equal a-argument-str "-h") (usage))
    ; Note: (intern ...) = string->symbol
    ((not (probe-file a-ledger-file-str)) (format t "Error: ~a does not exist...~%" a-ledger-file-str))
    ((or
      (member (intern a-argument-str) *g-quarters*)
      (member (intern a-argument-str) *g-months*))
        (export-to-txt a-ledger-file-str (intern a-argument-str)))
    (T (usage))))

(defun main ()
  "Main code processing.
Note: sbcl --noinform --script ledger.dat Q1
That makes for 5 arguments. But sbcl --noinform --script counts as 1 whole.
So that leaves 3 arguments to be checked for..."
  (cond
    ((eq (length sb-ext:*posix-argv*) 3)
      (process-arguments
        (nth 1 sb-ext:*posix-argv*)
        (string-upcase (nth 2 sb-ext:*posix-argv*))))
    (T (usage))))

;;; Main entry point, to start the code.
;; Note:
;; For testing cli parameters in sbcl:
;; (ql:quickload 'ledgerexport-tax)
;; (setf sb-ext:*posix-argv* (list "sbcl" "/home/rockwolf/doc/ledger/ledger.dat" "Q1"))
;; (setf sb-ext:*posix-argv* (list "sbcl" "/home/rockwolf/doc/ledger/ledger.dat" "january"))
(main)
