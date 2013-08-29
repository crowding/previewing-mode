;; Previewing minor mode
;;
;; Peter Meilstrup, 2013
;;
;; Intention: Update a preview in an external program every time you
;; save the buffer. Flexible configuration for different styles of
;; build and preview. Do builds and previews asynchronously in
;; external processes and report back on errors.

(eval-when-compile (require 'cl))

;;;###autoload
(define-minor-mode previewing-mode

  "Toggle previewing mode.

   Interactively with no argument, this command toggles the mode.
   A positive prefix argument enables the mode, any other prefix
   argument disables it.  From Lisp, argument omitted or nil
   enables the mode, `toggle' toggles the state.

   When previewing mode is enabled, any time the buffer is saved,
   asynchronous shell commands will run to process the file and
   launch an external viewer. To disable this change
   `previewing-when-save'.

   Variables `previewing-build-command' and
   `previewing-view-command' control which commands are run to
   build and view files. If not specified, matching entries are
   sought in `previewing-build-command-list' and
   `previewing-view-command-list'."
  :lighter " Prev"

  (if previewing-mode
    (add-hook 'after-save-hook 'previewing-check-and-do-preview nil 'local)
    (remove-hook 'after-save-hook 'previewing-check-and-do-preview 'local)))

;;;; Mode variables.

(defvar previewing-when-save t
  "Set to t if the preview commands should be run after every save.")

(defvar previewing-trace t
  "Verbosely give trace messages about what previewing mode is up to.")


(defvar previewing-build-command-list

  `((,(lambda (file) poly-markdown+r-mode) previewing-sequence
     ("\\(.*\\)\\.Rmd$"
      ("Rscript" "-e"
       ,(concat "library(knitr);"
                "knit(commandArgs(trailingOnly=TRUE)[[1]],"
                " output=commandArgs(trailingOnly=TRUE)[[2]])")
       "\\&" "\\1.md"))
     ("\\(.*\\).md$"
      ("pandoc" "-f" "markdown" "\\&" "-t" "html" "-o" "\\1.html")))
    (markdown-mode
     "\\(.*\\).md$" ("pandoc" "-f" "markdown" "\\&"
                     "-t" "html" "-o" "\\1.html")))

  "A list of candidate commands for building a file.

   The first matching element is used as
   `previewing-build-command' when the latter is not set. For
   details on matching see `previewing-find-matching-command'.")

(defvar previewing-view-command-list

  '((".*.html$" previewing-browse-file))

  "A list of candidate commands for viewing a file.

   The first matching element is used as
   `previewing-view-command' when the latter is not set. For
   details on matching see `previewing-find-matching-command'.")

(defvar-local previewing-build-command nil
  "Specifies the command to run to build a file.

   For details on how this is interpreted see
   `previewing-command-doc'. If nil, the build command is
   determined by scanning `previewing-build-command-list'.")

(defvar-local previewing-view-command nil
  "Specifies the command to run to build a file.

   For details on how this is interpreted see
   `previewing-command-doc'. If nil, the view command is
   determined by scanning `previewing-view-command-list'.")

;;;; The mode command.

(defun previewing-check-and-do-preview ()
  "Start the preview if `previewing-when-save'."
  (when previewing-when-save (previewing-do-preview)))

(defun previewing-do-preview ()
  "Start processing the current buffer for previewing. When
   `previewing-mode' is active this is called from
   `after-save-hook'."
  (interactive)
  (save-some-buffers (list (current-buffer)))
  (previewing-trace "Previewing %S" (buffer-file-name))
  (let*
      ((build-command
        (or previewing-build-command
            (previewing-find-matching-command previewing-build-command-list)))
       (file
        (if build-command
            (previewing-do-command (buffer-file-name) build-command)
          (message "No build command found")
          (buffer-file-name)))
       (view-command
        (or previewing-view-command
            (previewing-find-matching-command previewing-view-command-list file))))
    (if view-command
        (previewing-do-command file view-command)
      (message "No view command found"))))

;;;; Scanning command lists.

(defun previewing-find-matching-command (command-list &optional file)
  "Scan the given command list for a matching command.

   Returns a list of a function and its extra arguments (FUN
   &rest ARGS), or nil if no matches found.

   Here is how items are matched. Items are scanned in order and
   the first match is returned. Each item is a list; If the first
   element is a string, it is matched as a regexp against the
   buffer file name, and `previewing-do-substitute-command' is
   used as the build command. If it is a symbol that looks like a
   mode name, it is matched against the current mode. Otherwise
   it is evaluated as an indirect function; if non-nil the
   remaining elements are used."
  (setq file (or file (buffer-file-name)))
  (previewing-trace "Looking for matching conversion for %S" file)
  (dolist (command command-list)
    (let ((head (car command))
          (tail (cdr command)))
      (previewing-trace "considering %S" head)
      (cond
       ((stringp head)                  ; String matches file name
        (previewing-trace "checking file %S : %S" file head)
        (when (string-match head (or file (buffer-file-name)))
          (previewing-trace "Found file name match")
          (if (and (listp (nth 2 command)) (stringp (car (nth 2 command))))
              (return command) ; special case for substitute-command
              (return tail))
          (return command)))
       ((and (symbolp head)             ; Symbol matches current major mode
             (previewing-symbol-mode-p head))
        (previewing-trace "checking mode %S : %S" head major-mode)
        (when (derived-mode-p head)
          (previewing-trace "Found mode match")
          (return tail)))
       ((apply (indirect-function head) (list file)) ;arbitrary function
        (previewing-trace "Found function match")
        (return tail))
       (nil)))))

;;;; Running commands

(defconst previewing-command-doc nil
  "A command is specified by a list where the first refers to a
   function and the rest are additional arguments. The function
   is called with the file name in the head position.

   If the first item is a string, the implicit command is
   `previewing-do-substitute-command'.

   Ths function may run synchronously or asynchronously. If it
   runs synchronously it should return nil or a string which is the
   file produced by the build. If it runs asynchronously it should
   return a list with optional
   keyword elements (:process PROCESS :continue CONTINUE :error ERROR ARGS).
   The process's exit status will be used to select among continuations. If no
   :continue is specified processing ends or proceeds from the
   view command to the build command. Asynchronous processes use
   (previewing-get-process-buffer) as a buffer for asychronous output.")

(defun previewing-sequence (file &rest commands)
  "Run a sequence of commands."
  (let ((result file))
    (dolist (command commands)
      (previewing-trace "Sequence doing command %S" command)
      (setq result (previewing-do-command-base result command)))
    result))

(defun previewing-do-command-base (file command)
  "Run a single command; don't process the async things."
  (when (stringp (car command))
    (setq command (list 'previewing-do-substitute-command
                        (car command) (nth 1 command))))
  (previewing-trace "Doing command %S" command)
  (apply (indirect-function (car command)) (cons file (cdr command))))

;;;; Running substituted shell commands
(defun previewing-do-substitute-command
  (file pattern parts &optional producing)
  "Run a shell command by pattern substituting.

   FILE is matched against PATTERN and the match substituted into
   each element of PARTS.

   The optional argument :producing is also substituted against file, and
   becomes the output file. If not specified, I take the last substitution in
   the command line."
  ;; parts may optionally specify a :producing
  (setq parts (previewing-substitute-parts file pattern parts))
  (previewing-run-external-command parts)
  (or producing (car (last parts))))

(defun previewing-substitute-parts (file pattern parts)
  "Match FILE against PATTERN and substitute into the PARTS.

   Any element of PARTS that is not a string is passed through
   unmodified.  The presumption is that FILE matches PATTERN. If
   not an error is signalled."
  (let ((string (buffer-file-name)))
    (save-match-data
      (or (string-match pattern file)
          (signal 'error "No match for" pattern file))
      (let* ((match (match-string 0 file)))
        (loop for part in parts collect
              (cond ((stringp part)
                     (string-match pattern match)
                     (replace-match part nil nil match))
                    part))))))

(defun previewing-run-external-command (parts)
  "Run a command synchronously, by launching it async and waiting"
  (let ((done nil)
        (process (previewing-launch-command parts))
        (cont (lambda (process data)
                (message "Completed %S" data) (setq done 'continue)))
        (err (lambda (process data)
               (message "did not complete %S" data) (setq done 'error))))
    (previewing-yield process cont err "foo")
    (while (not done) (sit-for 0.2))
    (case done
      (continue t)
      (error (previewing-report-error (list 'error "external command failed"))))))

(defun previewing-launch-command (parts)
  "Start a command asynchronously returning a process."
  (when previewing-process
    (previewing-trace "Aborting process")
    (previewing-stop-process))
  (previewing-trace "Starting program: %s"
                      (previewing-format-command-line parts))
  (apply 'start-process
         `("previewing" ,(previewing-get-process-buffer) ,@parts)))

(defun previewing-report-error (e)
  "Default error handler; show the preview output buffer and rethrow the error."
  (switch-to-buffer-other-window (previewing-get-process-buffer))
  (apply 'signal `(,(car e) ,(cdr e))))

;;;; Async stuff

(defun previewing-get-process-buffer ()
  "Ensure process buffer exists and return it."
  (or (and previewing-process-buffer
           (buffer-live-p previewing-process-buffer)
           previewing-process-buffer)
      (progn
        (setq previewing-process-buffer
              (generate-new-buffer
               (concat "*Previewing-[" (buffer-name) "]*")))
        (eval `(with-current-buffer previewing-process-buffer
                 (setq previewing-home-buffer ',(current-buffer))))
        previewing-process-buffer)))

(defun previewing-reset-process-buffer ()
  "Erase the process buffer and return it."
  (let ((buf (previewing-get-process-buffer)))
    (with-current-buffer buf (erase-buffer))
    buf))

(defun previewing-yield (process &optional continue error data)
  "Activate a sentinel on given process; store continuation and return.
   Nothing above this function should mess with the stack."
  (previewing-trace "yield: process: %S, continue: %S, error: %S, data: %S"
                    process continue error data)
  (setq previewing-process process)
  (let ((buf (current-buffer)))     ;leave pointer back to this buffer
        (with-current-buffer (process-buffer process))
        (setq previewing-home-buffer buf))
  (push data previewing-data)
  (push continue previewing-continue)
  (push error previewing-error)
  (set-process-sentinel process 'previewing-sentinel))

(defun previewing-stop-process ()
  "Stop the currently running process, if any; actually terminate with
   prejudice deleting any continuations left to run.
   TODO: maybe call the error continuation?"
  (when previewing-process
    (previewing-trace "Stopping process %S" previewing-process)
    (if (processp previewing-process)
        (progn (set-process-sentinel previewing-process nil)
               (when (process-live-p previewing-process)
                 (kill-process previewing-process)))
      (previewing-trace "Not a process!"))
    (setq previewing-process nil))
  (setq previewing-continue nil)
  (setq previewing-error nil)
  (setq previewing-data nil))

(defun previewing-sentinel (process change)
  "Invokes the next continuation."
  (previewing-trace "Got process change %S %S" process change)
  (previewing-trace "Current buffer is %S" (current-buffer))
  (cond
   ((eq process previewing-process)                            ;navigate back to the home buffer
    (with-current-buffer (process-buffer process)
      (with-current-buffer previewing-home-buffer
        (previewing-trace "Got back to buffer %S" (current-buffer))
        (cond
         ((save-match-data (string-match "^finished" change))
          (previewing-trace "Looks like process exited normally")
          (previewing-continue process))
         ((process-live-p process)
          (previewing-trace "Process still alive? (change %S, status %s)"
                            change (process-status process)))
         (t
          (previewing-trace "Process died (change %S, status %s)"
                            change (process-status process))
          (previewing-error process))))))
   (t
    (previewing-trace
     "Got signal for non-pending process? %S %S" process change)
    nil)))

(defun previewing-continue (process)    ;handler called by sentinel
  "Pop off the continuation and go"
  (catch 'return
    (while previewing-continue
      (let ((cont (pop previewing-continue))
            (err (pop previewing-error))
            (data (pop previewing-data)))
        (when cont
          (throw 'return (previewing-continue-on process cont data)))))))

(defun previewing-error (process)       ;handler called by sentinel
  "Pop off the error continuation and go"
  (catch 'return
    (while previewing-error
      (let ((cont (pop previewing-continue))
            (err (pop previewing-error))
            (data (pop previewing-data)))
        (when err (throw 'return (previewing-continue-on process err data)))))
    (signal 'error process)))

(defun previewing-continue-on (process cont data) ;handler called by both
  "Call the next function; if it returns a process +
   continuation, set pointer back and tohis buffer, add sentinel
   and put it on the stack; if it returns immediately"
  (previewing-trace "Continuing on %S %S %S" process cont data)
  (let ((results
         (condition-case e
             (apply (indirect-function cont) (list process data))
           (error (previewing-error e)))))
    (cond
     ((listp process)
      (destructuring-bind (process continue error data) results
        (previewing-yield process continue error data)))
     (t (previewing-continue results)))))

;;; we just have one async process per buffer, tracked in buffer-local variables
(defvar-local previewing-process nil
  "The process that previewing-mode is currently waiting on")
(defvar-local previewing-continue nil
  "A stack of functions that will be executed once the process
  completes successfully.")
(defvar-local previewing-error nil
  "A stack of functions that will be executed if the
   previewing process ends with nonzero exit code.")
(defvar-local previewing-result nil
  "Data that will be passed to identify the next continuation")
(defvar-local previewing-process-buffer nil
  "The buffer that receives input from the previewing process.")
(defvar-local previewing-home-buffer nil
  "A handle back to the home buffer of the previewing process buffer...")

;;;; Build/view functions

(defun previewing-browse-file (file)
  "Open a file in the browser"
  (message "Browsing %s" file)
  (browse-url (browse-url-file-url file)))

;;;; Other supporting functions

(defun previewing-symbol-mode-p (sym)
  "Return non-nil if a symbol refers to a mode.

   A symbol 'SYM is considered to refer to a mode if it is bound to
   a function, AND and ends in '-mode' OR the corresponding SYM-hook 
    exists."
  (let*
      ((name (symbol-name sym))
       (ends-in-mode (save-match-data (string-match "-mode$" name)))
       (hook-symbol (intern-soft (concat name "-hook")))
       (has-hook (and hook-symbol (boundp hook-symbol))))
    (and (fboundp sym)
         (or has-hook ends-in-mode))))

(defun previewing-format-command-line (command)
  "Format a command line for display in messages."
  (mapconcat 'shell-quote-argument command " "))

;;;###autoload
(add-hook 'markdown-mode-hook 'previewing-mode)
;;;###autoload
(add-hook 'html-mode-hook 'previewing-mode)
;;;###autoload
(add-hook 'latex-mode-hook 'previewing-mode)

(defun previewing-trace (format &rest args)
  (when previewing-trace
    (apply 'message (cons (concat "Previewing: " format) args))))

(provide 'previewing-mode)
