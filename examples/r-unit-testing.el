;; Code to automatically run R unit tests is included in the function
;; "previewing-r.el". To activate it, add these lines to your Emacs
;; initialization:

(add-hook 'ess-mode-hook 'previewing-mode)
(add-hook 'ess-mode-hook 'previewing-r-setup)
(autoload 'previewing-r-setup "previewing-r")
