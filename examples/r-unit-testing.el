;; setup to automatically run R package unit tests...

(add-to-list 'compilation-error-regexp-alist-alist
             '(r-testthat "^[0-9a-zA_Z]\\. \\(\\(Failure\\|Error\\): .*\\) ---"
                          nil nil 2 1))

(add-to-list 'compilation-error-regexp-alist 'r-testthat)

(add-to-list 'previewing-build-command-list
             '(previewing-is-R-package-with-tests
               previewing-sequence
               (previewing-show-compilation-buffer)
               (previewing-run-R-unit-tests)))

(add-to-list 'previewing-view-command-list
            `(,(lambda () t) previewing-show-compilation-buffer) t)

;;;; these following may move into separate file.

(defun previewing-is-R-package-with-tests (filename)
  (let (packagedir)
    (and (derived-mode-p 'ess-mode)
         (save-match-data (string-match "\\.[Rr]$" filename))
                                        ; is there a DESCRIPTION?
         (setq packagedir (locate-dominating-file filename "DESCRIPTION"))
         (file-in-directory-p "test" packagedir)
         packagedir)))

(defun previewing-find-R-unit-test-convention (&optional filename)
  (setq filename (or filename (buffer-file-name) default-directory))
  (let ((packagedir (locate-dominating-file filename "DESCRIPTION")))
    (cond
     ((member "testthat"
              (directory-files (previewing-build-path packagedir "tests")))
      (previewing-trace 2 "Running tests from tests/testthat")
      'tests-testthat)
     ((member "tests"
              (directory-files (previewing-build-path packagedir "inst")))
      (previewing-trace 2 "Running tests from inst/tests")
      'inst-tests)
     (t
      (previewing-trace 2 "Running anything in tests/")
      'any))))

(defun previewing-run-R-unit-tests (filename data)
  (let
      ((firstargs
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
