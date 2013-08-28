;; Previewing minor mode
;;
;; Peter Meilstrup, 2013
;;
;; Intention: Update a preview in an external program every time you
;; save the buffer. Flexible configuration for different styles of
;; build and preview. Do builds and previews asynchronously in
;; external processes and report back on errors.

(require 'cl)

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

(defvar previewing-when-save t
  "Set to t if the preview commands should be run after every save.")

(defun previewing-check-and-do-preview ()
  "Run teh preview if `previewing-when-save'"
  (when previewing-automatic-save
    (previewing-do-preview)))

;;;; Mode variables.


(defvar previewing-build-command-list

  '(((lambda (file) poly-markdown+r-mode) ;;; check minor mode for .Rmd files
     previewing-sequence
     ("\\(.*\\).Rmd$" ("Rscript" "-e" "library(knitr)" "\\&") "\\1.md")
     ("\\(.*\\).md$" ("pandoc" "-f" "markdown" "\\&"
                               "-t" "html" "-o" "\\1.html")))
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

(defun previewing-do-preview ()
  "Preview the current buffer. When `previewing-mode' is active
   this is called from `after-save-hook'."
  (interactive)
  (save-some-buffers (list (current-buffer)))
  (message "Previewing %s" (buffer-file-name))
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
  (dolist (command command-list)
    (let ((head (car command))
          (tail (cdr command))
          (file (or file (buffer-file-name))))
      (message "considering %S" head)
      (cond
       ((stringp head)                  ; String matches file name
        (message "checking file %S : %S" file head)
        (when (string-match head (or file (buffer-file-name)))
          (message "Found file name match")
          (if (and (listp (nth 2 command)) (stringp (car (nth 2 command))))
              (return command) ; special case for substitute-command
              (return tail))
          (return command)))
       ((and (symbolp head)             ; Symbol matches current major mode
             (message "Checking if %S is mode" head)
             (previewing-symbol-mode-p head))
        (message "checking mode %S : %S" major-mode head)
        (when (derived-mode-p head)
          (return tail)))
       ((apply (indirect-function head) (list file)) ;arbitrary function
        (return tail))
       (nil)))))

;;;; Running commands

(defconst previewing-command-doc nil
  "A command is specified by a list where the first refers to a
   function adn the rest are additional arguments. The function
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
  (let (result)
    (dolist (command commands)
      (setq result (previewing-do-command file command)))
    result))

(defun previewing-do-command (file command)
  "Run a single command."
  (when (stringp (car command))
    (setq command (list 'previewing-do-substitute-command
                        (car command) (nth 1 command))))
  (apply (indirect-function (car command)) (cons file (cdr command))))

;;;; Running substituted shell commands

(defun previewing-do-substitute-command (file pattern parts &optional producing)
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

(defun previewing-run-external-command (parts &optional continue error)
  "Run an external program synchronously, invoking optional
   continuation and error callbacks Default behavior is to show the buffer"
   (let (passed result)
    (condition-case e
        (progn
          (setq result (eval `(call-process
                               ,(car parts) nil
                               (previewing-reset-process-buffer)
                               t
                               ,@(cdr parts))))
          (if (= result 0)
              (setq passed t)
            (signal 'error
                    (list (format "Exit status %d from parts: %s"
                                  result
                                  (previewing-format-parts-line parts))))))
      (error
       (apply (indirect-function (or error 'previewing-report-error))
              (list e))))
    (when passed
      (apply (indirect-function (or continue 'identity)) (list result)))))

(defun previewing-report-error (e)
  "Default error handler; show the preview output buffer and rethrow the error."
  (switch-to-buffer-other-window (previewing-get-process-buffer))
  (apply 'signal `(,(car e) ,(cdr e))))

(defun previewing-run-command-async (command &optional continue error)
  (when previewing-process
      (message "Aborting process")
      (previewing-stop-process))
  (setq previewing-process
        (eval `(start-process
                "previewing" (previewing-get-process-buffer) ,@command)))
  (setq previewing-continue continue)
  (setq previewing-error error)
  (set-process-sentinel previewing-process 'previewing-sentinel))

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

(defun previewing-sentinel (process string)
  (with-current-buffer (process-buffer process)
    (with-current-buffer home-buffer
      )))

(defun previewing-stop-process ()
  "Stop the currently running process, if any.")

(defun previewing-format-command-line (command)
  "Format a command line for display in messages."
  (mapconcat 'shell-quote-argument command " "))

;;; we just have one async process per buffer, tracked in buffer-local variables
(defvar-local previewing-process nil
  "The process that previewing-mode is currently waiting on")
(defvar-local previewing-continue nil
  "A function that will be executed once the process completes successfully.")
(defvar-local previewing-error nil
  "A function that will be executed if the previewing process ends with
   nonzero exit code.")
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

;;;###autoload
(add-hook 'markdown-mode-hook 'previewing-mode)
;;;###autoload
(add-hook 'html-mode-hook 'previewing-mode)
;;;###autoload
(add-hook 'latex-mode-hook 'previewing-mode)

(provide 'previewing-mode)
