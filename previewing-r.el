;;; Driving R unit testing from previewing mode.

;;; to enable this behavior add these lines to your .init.el:
;; (add-hook 'ess-mode-hook 'previewing-mode)
;; (add-hook 'ess-mode-hook 'previewing-r-setup)
;; (autoload 'previewing-r-setup "previewing-r")

(defconst previewing-r-markdown-expression
  "\\(.*\\)\\.[Rr]\\(?:md\\|mkd\\|markdown\\)$"
  "Expression used to match R markdown files.")

(defvar previewing-build-rmd-command
  ;(setq previewing-build-rmd-command
  `(previewing-do-substitute-command
    ,previewing-r-markdown-expression
    ("Rscript" "-e"
     "library(knitr); knit2html(commandArgs(trailingOnly=TRUE)[[1]])"
     "\\&") "\\1.html")
  "Command used to build R-markdown files.")

;;;#autoload
(defun previewing-r-setup ()
  "Install previewing handlers for R packages files"
  (add-to-list 'previewing-build-command-list
             '(previewing-is-R-package-with-tests
               previewing-sequence
               (previewing-show-compilation-buffer)
               (previewing-run-R-unit-tests)))

  ;; more indirect variable in how this list is interpreted?
  (add-to-list 'previewing-build-command-list
               `(,previewing-r-markdown-expression
                 ,@previewing-build-rmd-command))

  (add-to-list 'compilation-error-regexp-alist-alist
             '(r-testthat "^[0-9a-zA_Z]\\. \\(\\(Failure\\|Error\\): .*\\) ---"
                          nil nil 2 1))
  (add-to-list 'compilation-error-regexp-alist 'r-testthat)
  (remove-hook 'ess-mode-hook 'previewing-r-setup))

(defun previewing-is-R-package-with-tests (filename)
  (let (packagedir)
    (and (derived-mode-p 'ess-mode)
         (save-match-data (string-match "\\.[Rr]$" filename))
                                        ; is there a DESCRIPTION?
         (setq packagedir (previewing-find-R-package-dir filename))
         (or (file-in-directory-p "test" packagedir)
             (and (file-in-directory-p "inst" packagedir)
                  (file-in-directory-p
                   "tests" (previewing-build-path packagedir "inst"))))
         packagedir)))

(defun previewing-find-R-unit-test-convention (&optional filename)
  (setq filename (or filename (buffer-file-name) default-directory))
  (let* ((packagedir (previewing-find-R-package-dir filename))
         (tests-dir (previewing-build-path packagedir "tests"))
         (has-tests-dir (file-directory-p tests-dir))
         (inst-tests-dir (previewing-build-path packagedir "inst" "tests"))
         (has-inst-tests-dir (file-directory-p inst-tests-dir))
         (testthat-dir (previewing-build-path packagedir "tests" "testthat"))
         (has-testthat-dir (file-directory-p testthat-dir)))
    (cond
     (has-testthat-dir
      (previewing-trace 2 "Running tests from tests/testthat")
      'tests-testthat)
     (has-inst-tests-dir
      (previewing-trace 2 "Running tests from inst/tests")
      'inst-tests)
     (has-tests-dir
      (previewing-trace 2 "Running anything in tests/")
      'any))))

(defun previewing-find-R-package-dir (filename)
  "Locate the root directory of an R package, or nil."
  (locate-dominating-file filename "DESCRIPTION"))

(defun previewing-run-R-unit-tests (filename data)
  (let*
      ((packagedir (previewing-find-R-package-dir filename))
       (firstargs
           (list
            "R"
            "-e" "library(devtools)"
            "-e" "library(testthat)"
            "-e" "args <- commandArgs(trailingOnly=TRUE)"
            "-e" "load_all(args[1])"
            "-e" "rep <- SummaryReporter$new()"
            "-e" "env <- new.env(parent=ns_env(args[1]))"))
       (restargs
        (case (previewing-find-R-unit-test-convention filename)
          ('tests-testhat
           (list
            "-e" "test_dir(args[2], reporter=rep, env=env)"
            "-e" "if (rep$failed) stop('tests failed')"
            "--args"
            packagedir
            (previewing-build-path packagedir "tests" "testthat")))
          ('inst-tests
           (list
            "-e" "x <- test_dir(args[2], reporter=rep, env=env)"
            "-e" "if (rep$failed) stop('tests failed')"
            "--args"
            packagedir
            (previewing-build-path packagedir "inst" "tests")))
          ('any
           (list
            "-e" "source_dir(args[2], env=env)"
            "--args"
            packagedir
            (previewing-build-path packagedir "tests"))))))
    (previewing-launch-command `(,@firstargs ,@restargs))))

(provide 'previewing-r)
