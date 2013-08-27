;; Previewing minor mode
;;
;; Peter Meilstrup, 2013
;;
;; Intention: Update a preview in an external program every time you
;; save the buffer. Flexible configuration for different styles of build
;; and preview. Do builds and previews asynchronously in externals threads and
;; report back on errors.

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
   launch an external viewer. To configure this behavior add entries to
   `previewing-build-command-list' and
   `previewing-view-command-list'."
  :lighter " Prev"

  (if previewing-mode
    (add-hook 'after-save-hook 'previewing-do-preview nil 'local)
    (remove-hook 'after-save-hook 'previewing-do-preview 'local)))

(defvar previewing-build-command-list

  '(("\\(.*\\).md$" "pandoc" "-f" "markdown" "\\&" "-t" "html" "-o" "\\1.html"))

  "A list of candidate commands for building a file. Each element
   is a list whose first element is a regexp matched against the
   buffer file name. The first matching element is used as
   `previewing-build-command' when the latter is not set.")

(defvar previewing-view-command-list

  '(("\\(.*\\).md$" "open" "-a" "Google Chrome" "\\1.html"))

  "A list of candidate commands for viewing a file. Each element
   is a list whose first element is a regexp matched against the
   buffer file name. The first matching element is used as
   `previewing-view-command' when the latter is not set.")

(defvar-local previewing-build-command nil
  "A list (PATTERN SUBSTITUTIONS...). The pattern is
   matched against the buffer file name, and the remaining parts
   are evaluated as regexp replacements to form a shell
   command and arguments. The shell command is used to launch an external
   viewer.

   If nil, the build command is determined from
   `previewing-build-command-list'.")

(defvar-local previewing-view-command nil
  "A list (PATTERN SUBSTITUTIONS...). The pattern is
   matched against the buffer file name, and the remaining parts
   are evaluated as regexp replacements to form a shell
   command and arguments. The shell command is used to launch an external
   viewer.

   If nil, the build command is determined from
   `previewing-build-command-list'.")

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
       (view-command
        (or previewing-view-command
            (previewing-find-matching-command previewing-view-command-list))))
    ;; todo: do build command asynchronously and wait for callback
    (if build-command
        (let ((build-command-line (previewing-make-command-line build-command)))
          (message "Build command: %s"
                   (previewing-format-command-line build-command-line))
          (previewing-run-command build-command-line))
      (message "No build command found"))
    ;; todo: do view command asynchronously and wait for callback
    (if view-command
        (let ((view-command-line (previewing-make-command-line view-command)))
          (message "View command: %s"
                   (previewing-format-command-line view-command-line))
          (previewing-run-command view-command-line))
      (message "No view command found"))))

(defun previewing-find-matching-command (command-list)
  "Scan the given command list for the appropriate command for
   the present buffer and return it."
  (dolist (command command-list)
    (when (string-match (car command) (buffer-file-name))
      (return command))))

(defun previewing-make-command-line (command-list)
  (let ((pattern (car command-list))
        (parts (cdr command-list))
        (string (buffer-file-name)))
    (save-match-data
      (string-match pattern string)
      (let* ((match (match-string 0 string)))
        (loop for part in parts collect
              (progn (string-match pattern match)
                     (replace-match part nil nil match)))))))

(defun previewing-run-command (command &optional continue error)
  "Run an external program synchronously, invoking optional
   continuation and error callbacks Default behavior is to show the buffer"
  (let (passed result)
    (condition-case e
        (progn
          (setq result (eval `(call-process
                               ,(car command) nil
                               (previewing-reset-process-buffer)
                               t
                               ,@(cdr command))))
          (if (= result 0)
              (setq passed t)
            (signal 'error
                    (list (format "Exit status %d from command: %s"
                                  result
                                  (previewing-format-command-line command))))))
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

;; we just have one async process per buffer, tracked in buffer-local variables
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

;;;###autoload
(add-hook 'markdown-mode-hook 'previewing-mode)
;;;###autoload
(add-hook 'html-mode-hook 'previewing-mode)
;;;###autoload
(add-hook 'latex-mode-hook 'previewing-mode)

(provide 'previewing-mode)
