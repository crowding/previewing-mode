;; Previewing minor mode
;;
;; Peter Meilstrup, 2013
;;
;; Intention: Update a preview in an external program every time you
;; save the buffer. Flexible configuration for different styles of
;; build and preview. Do builds and previews asynchronously in
;; external processes and report back on errors.

(eval-when-compile (require 'cl))

(defvar previewing-mode-map
  (let ((previewing-mode-map (make-sparse-keymap)))
    (define-key previewing-mode-map (kbd "C-c C-g") 'previewing-do-preview)
    (define-key previewing-mode-map (kbd "C-c C-h") 'previewing-stop-process)
    previewing-mode-map)
  "Keymap for previewing mode")

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

   The Previewing can be invoked and stopped manually by
   `previewing-do-preview' (\\[previewing-do-preview]) and
   `previewing-stop-process' (\\[previewing-stop-process]).

   Normally, previewing happens in the background and the
   compilation buffer is only shown on error. To always show it
   set `previewing-always-show-buffer'.

   Variables `previewing-build-command' and
   `previewing-view-command' control which commands are run to
   build and view files. If not specified, matching entries are
   sought in `previewing-build-command-list' and
   `previewing-view-command-list'."

  :lighter " Prev"
  :keymap previewing-mode-map

  (if previewing-mode
      (add-hook 'after-save-hook
                'previewing-check-and-do-preview nil 'local)
    (remove-hook 'after-save-hook
                 'previewing-check-and-do-preview 'local)))

;;;; Mode variables.

(defun previewing-start-or-stop ()
  (interactive)
  "Stops a previewing process if one is running. Otherwise starts one."
  (if previewing-process
      (previewing-stop-process)
    (previewing-do-preview)))

(defvar previewing-always-show-buffer nil
  "If non-nil, always shows previewing buffer when you start a job.")

(defvar previewing-when-save t
  "Set to t if the preview commands should be run after every save.")

(defvar previewing-trace 1
  "Level of verbosity of trace messages given by Previewing mode.
   Level 1: Basic end user feedback on end commands being launched.
   Level 2: Feedback when searching for commands
   Level 3: Every alternative considered
   Level 4: Feedback on asynchronous scheduling")

(defvar previewing-build-command-list

  `((,(lambda (file) poly-markdown+r-mode)
     ("\\(.*\\)\\.[Rr]md$"
      ("Rscript" "-e"
       "library(knitr); knit2html(commandArgs(trailingOnly=TRUE)[[1]])"
       "\\&") "\\1.html")
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

  `((".*.html$" previewing-browse-file)
    (,(lambda () t) previewing-show-compilation-buffer))

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

(defvar previewing-lockout nil
  "Set non-nil if is currently trying to start a preview. This is so that
   `save-some-buffers' doesn't start multiple previewing processes.")

(defun previewing-do-preview ()
  "Start processing the current buffer for previewing. When
   `previewing-mode' is active this is called from
   `after-save-hook'."
  (interactive)
  (when (not previewing-lockout)
    (setq previewing-lockout (current-buffer))
    (unwind-protect
        (progn
          (save-some-buffers (list (current-buffer)))
          (previewing-stop-process)
          (previewing-reset-process-buffer)
          (when previewing-always-show-buffer
            (previewing-show-compilation-buffer))
          (previewing-trace 1 "Previewing %S" (buffer-file-name))
          ;;continue with build, then continue with view
          (previewing-yield nil nil 'previewing-report-error)
          (previewing-yield nil 'previewing-do-view)
          (previewing-yield nil 'previewing-do-build)
          (previewing-maybe-continue (buffer-file-name)))
      (setq previewing-lockout nil))))

(defun previewing-do-build (file data)
  (let ((build-command
         (or previewing-build-command
             (previewing-find-matching-command
              previewing-build-command-list file))))
    (cond
     (build-command
      (previewing-trace 2 "Found build command %S" build-command)
      (previewing-maybe-continue
       (previewing-do-command file build-command)))
     (t
      (previewing-trace 1 "No build command found")
      (previewing-maybe-continue file)))))

(defun previewing-do-view (file data)
  (let ((view-command
         (or previewing-view-command
             (previewing-find-matching-command
              previewing-view-command-list file))))
    (cond
     (view-command
      (previewing-trace 2 "Found view command: %S" view-command)
      (previewing-maybe-continue
       (previewing-do-command file view-command)))
     (t (previewing-trace 1 "No view command found")
        (previewing-maybe-continue file)))))

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
  (previewing-trace 2 "Looking for matching conversion for %S" file)
  (dolist (command command-list)
    (let ((head (car command))
          (tail (cdr command)))
      (previewing-trace 3 "considering %S" head)
      (cond
       ((stringp head)                  ; String matches file name
        (previewing-trace 3 "checking file %S : %S" file head)
        (when (string-match head (or file (buffer-file-name)))
          (previewing-trace 3 "Found file name match")
          (if (and (listp (nth 2 command)) (stringp (car (nth 2 command))))
              (return command) ; special case for substitute-command
              (return tail))
          (return command)))
       ((and (symbolp head)             ; Symbol matches current major mode
             (previewing-symbol-mode-p head))
        (previewing-trace 3 "checking mode %S : %S" head major-mode)
        (when (derived-mode-p head)
          (previewing-trace 3 "Found mode match")
          (return tail)))
       ((progn (previewing-trace 3 "Trying function match %S" head)
          (apply (indirect-function head) (list file)))
        (previewing-trace 3 "Found function match")
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

(defun previewing-sequence (file commands)
  "Run a sequence of commands."
  (previewing-sequence-step file commands))

(cl-defun previewing-sequence-step (file (&optional command &rest rest))
  (previewing-trace 4 "Doing step %S on file %S" command file)
  (cond (command
         (previewing-yield rest 'previewing-sequence-step)
         ;; how does the previewing step get information to the next step?
         (previewing-maybe-continue (previewing-do-command file command)))
        (t (previewing-maybe-continue file))))

(defun previewing-do-command (file command)
  "Run a single command"
  (when (stringp (car command))
    (setq command
          `(previewing-do-substitute-command ,@command)))
  (previewing-trace 4 "Doing command %S on file %S" command file)
  (previewing-maybe-continue
   (apply (indirect-function (car command)) (list file (cdr command)))))

;;;; Running substituted shell commands

(cl-defun previewing-do-substitute-command
  (file (pattern
         parts
         &optional (producing (car (last parts)))))
  "Run a shell command by pattern substituting.

   FILE is matched against PATTERN and the match substituted into
   each element of PARTS.

   The optional argument :producing is also substituted against file, and
   becomes the output file. If not specified, I take the last substitution in
   the command line."
  (setq parts (previewing-substitute-parts file pattern parts))
  (setq previewing-return
        (car (previewing-substitute-parts file pattern (list producing))))
  (previewing-maybe-continue (previewing-launch-command parts)))

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

(defun previewing-launch-command (parts)
  "Start a command asynchronously; return a process."
  (previewing-trace 1 "Starting command: %s"
                    (previewing-format-command-line parts))
  (let ((buf (previewing-get-process-buffer)))
    (with-current-buffer buf
      (previewing-go-to-bottom)
      (let ((inhibit-read-only t))
        (insert (previewing-format-command-line parts)))
      (apply 'start-process
             `(,(previewing-format-command-line parts)
               ,buf
               ,@parts)))))

;;;; The compilation buffer and window management

(defun previewing-set-error-patterns (input patterns)
  "Buffer-locally add things to compilation-error-rexexp-alist"
  (with-current-buffer (previewing-get-process-buffer)
    (dolist (pat patterns)
      (make-variable-buffer-local 'compilation-error-rexexp-alist)
      (add-to-list 'compilation-error-rexexp-alist pat)))
  input)

(defun previewing-go-to-bottom ()
  (goto-char (point-max))
  (dolist (win (get-buffer-window-list (current-buffer)))
    (set-window-point win (point-max))))

(defun previewing-show-compilation-buffer (&optional input data)
  (cond
   ((processp input)
    (previewing-show-buffer-other-window
     (or (process-buffer input) (previewing-get-process-buffer))))
   (t (previewing-show-buffer-other-window (previewing-get-process-buffer))))
  input)

(defun previewing-show-buffer-other-window (buf)
  (unless (equal buf (window-buffer (frame-selected-window)))
    (display-buffer buf '(display-buffer-reuse-window))))

(defun previewing-get-process-buffer ()
  "Ensure process buffer exists and return it."
  (or (and previewing-process-buffer
           (buffer-live-p previewing-process-buffer)
           previewing-process-buffer)
      (progn
        (setq previewing-process-buffer
              (generate-new-buffer
               (concat "*Previewing-[" (buffer-name) "]*")))
        (let ((bbbuf (current-buffer)))
          (with-current-buffer previewing-process-buffer
            (setq previewing-home-buffer bbbuf)
            (compilation-mode)))
        previewing-process-buffer)))

(defun previewing-reset-process-buffer ()
  "Erase the process buffer and return it."
  (let ((buf (previewing-get-process-buffer))
        (dir default-directory))
    (previewing-trace 4 "Erasing buffer %S" buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory dir)))
    buf))

;;;; Async stuff

(defun previewing-maybe-continue (result)
  "Take a result. If it's a process, assume you've put any
   desired data into previewing-return and wait on the
   process. Otherwise go to the next continuation."
  (cond
   ((processp result)
    (previewing-trace 4 "Maybe-continue: waiting on %S" result)
    (previewing-set-sentinel result)
    result)
   (t
    (previewing-trace 4 "Maybe-continue: continuing with %S" result)
    (setq previewing-return result)
    (previewing-continue result))))

(defun previewing-yield (data &optional continue error)
  "Store continuation and return.
   Nothing above this function should mess with the stack.  Not
   really a 'yield,' more of a push your return address onto the
   stack. To return your value you may set previewing-return.
   the next continuation."
  (previewing-trace 4 "yield: data: %S, continue: %S, error: %S"
                    data continue error)
  (push data previewing-data)
  (push continue previewing-continue)
  (push error previewing-error))

(defun previewing-set-sentinel (process)
  (previewing-trace 4 "Setting sentinel on %S, process buffer is %S"
                    process (process-buffer process))
  (unless (process-buffer process)
    (set-process-buffer process (previewing-get-process-buffer)))
  (let ((buf (current-buffer)))     ;leave pointer back to this buffer
    (with-current-buffer (process-buffer process)
      (setq previewing-home-buffer buf)))
  (setq previewing-process process)
  (set-process-sentinel process 'previewing-sentinel))

(defun previewing-stop-process ()
  "Stop the currently running process, if any; actually terminate with
   prejudice deleting any continuations left to run.
   TODO: maybe call the error continuation?"
  (interactive)
  (when previewing-process
    (previewing-trace 1 "Killing previous build")
    (previewing-trace 4 "Stopping process %S" previewing-process)
    (if (processp previewing-process)
        (progn (set-process-sentinel previewing-process nil)
               (when (process-live-p previewing-process)
                 (kill-process previewing-process)))
      (previewing-trace 4 "Not a process!"))
    (setq previewing-process nil))
  (setq previewing-continue nil)
  (setq previewing-error nil)
  (setq previewing-data nil))

(defun previewing-sentinel (process change)
  "Invokes the next continuation."
  (previewing-trace 4 "Got process change %S %S" process change)
  (previewing-trace 4 "Current buffer is %S" (current-buffer))
  (unless (process-buffer process)
    (signal 'error "Got notice on process with no buffer"))
  (with-current-buffer (process-buffer process)
    (with-current-buffer previewing-home-buffer
      (cond
       ((eq process previewing-process)  ;navigate back to the home buffer
        (previewing-trace 4 "Got back to buffer %S" (current-buffer))
        (cond
         ((string-match "^finished" change)
          (previewing-trace 4 "Looks like process exited normally")
          (setq previewing-process nil)
          (let ((retval (or previewing-return process)))
            (setq previewing-return nil)
            (previewing-continue retval)))
         ((process-live-p process)
          (previewing-trace 4 "Process still alive? (change %S, status %s)"
                            change (process-status process)))
         (t
          (previewing-trace 1 "Process died (change %S, status %s)"
                            change (process-status process))
          (setq previewing-process nil)
          (previewing-error
           (list 'error 'previewing-process-died process change)))))
       (t
        (previewing-trace 4
         "Got signal for non-pending process? %S %S, was waiting for %S"
         process change previewing-process)
        nil)))))

(defun previewing-continue (retval)    ;handler called by sentinel
  "Pop off the continuation and go"
  (previewing-trace 4 "Continuing with return %S" retval)
  (catch 'return
    (while previewing-continue
      (let ((cont (pop previewing-continue))
            (err (pop previewing-error))
            (data (pop previewing-data)))
        (previewing-trace 4 "pop for continue")
        (when cont
          (throw 'return (previewing-continue-on retval cont data)))))
    (previewing-trace 4 "Found no continuation")
    retval))

(defun previewing-error (errinfo) ;handler called by sentinel
  "Pop off the error continuation and go"
  (previewing-trace 4 "Continuing with error %S" errinfo)
  (catch 'return
    (while previewing-error
      (previewing-trace 4 "pop for error")
      (let ((cont (pop previewing-continue))
            (err (pop previewing-error))
            (data (pop previewing-data)))
        (when err (throw 'return (previewing-continue-on errinfo err data)))))
    ;if we're out of continuations, control throws the error back to emacs.
    (signal (car errinfo) (cdr errinfo))))

(defun previewing-continue-on (retval cont data) ;handler called by both
  "Call the next function; if it returns a process +
   continuation, set pointer back to this buffer, add sentinel
   and put it on the stack; if it returns immediately"
  (previewing-trace 4 "Continuing with return %S; call %S; data %S"
                    retval cont data)
  (let ((results
         (condition-case e
             (apply (indirect-function cont) (list retval data))
           (error (previewing-error e)))))
   (previewing-maybe-continue results)))

;;; we just have one async process per buffer, tracked in buffer-local variables
(defvar-local previewing-data nil
  "Data that will be passed to identify the next continuation")
(defvar-local previewing-continue nil
  "A stack of functions that will be executed once the process
  completes successfully.")
(defvar-local previewing-error nil
  "A stack of functions that will be executed if the
   previewing process ends with nonzero exit code.")

(defvar-local previewing-process nil
  "The process that previewing-mode is currently waiting on")
(defvar-local previewing-return nil
  "Set this value to return some thing from an async function")

(defvar-local previewing-process-buffer nil
  "The buffer that receives input from the previewing process.")
(defvar-local previewing-home-buffer nil
  "A handle back to the home buffer from the process buffer.")

;;;; Build/view functions

(defun previewing-browse-file (file data)
  "Open a file in the browser"
  (message "Browsing %s" file)
  ;this returns a process, too
  (browse-url (browse-url-file-url file)))


(defun previewing-report-error (e &optional data)
  "Default error handler; show the preview output buffer and rethrow the error."
  (previewing-trace 4 "Showing buffer %S" (previewing-get-process-buffer))
  (let ((win (previewing-show-buffer-other-window (previewing-get-process-buffer))))
    (with-current-buffer (previewing-get-process-buffer)
      (previewing-trace 4 "%S" (point))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\nError: " (error-message-string e) "\n")
        (compilation-parse-errors (point-min) (point-max)))
      (with-selected-window win
        (goto-char (point-min))
        (with-demoted-errors
          (compilation-next-error 1))))) ; not sure why this is necessary
  (signal (car e) (cdr e)))

;;;; Other supporting functions

(defun previewing-build-path (first &optional next &rest rest)
  (if next
      (apply 'previewing-build-path
             (cons (concat (file-name-as-directory first) next) rest))
    first))

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

(defun previewing-format-command-line (parts)
  "Format a command line for display in messages."
  (mapconcat (lambda (part)
               (let ((tsh (tramp-shell-quote-argument part)))
                 (if (equal tsh part) part (format "%S" part))))
             parts " "))

;;;###autoload
(add-hook 'markdown-mode-hook 'previewing-mode)
;;;###autoload
(add-hook 'html-mode-hook 'previewing-mode)
;;;###autoload
(add-hook 'latex-mode-hook 'previewing-mode)

(defun previewing-trace (pri format &rest args)
  (when (or (and (numberp previewing-trace) (<= pri previewing-trace))
            (and (booleanp previewing-trace) previewing-trace))
    (apply 'message (cons (concat "Previewing: " format) args))))

(provide 'previewing)
