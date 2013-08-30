## Previewing-mode

A minor mode for running commands every time you save your file.

For example, when writing with [knitr][knitr], you cah have it automatically
knit and reload your web browser after every save.

[knitr]: http://yihui.name/knitr/

Use it for Markdown, HTML, LaTeX, sweave, knitr, pic and other markup
files and literate programming modes (configuration details
forthcoming.)

Use it to automatically run your unit tests.

Supplants the built-in `M-x compile` functionality by sequencing
multiple commands (e.g. process the file and then open the result in
the browser,) being easily configurable to select appropriate commands
based on mode, filename or other criteria, acting independently per
buffer, and paying attention to exit statuses.

Fairly easily configurable to do any build/view process you can phrase
in terms of regexps and shell commands. Extensible with Lisp functions
as well. Does most of its work asynchronously so you can keep typing
away in the foreground.

### Installation and configuration

Download and put this in your `.emacs.d`:

```emacs-lisp
(add-to-path 'load-path "path/to/previewing-mode")
(require 'previewing)
```

Perhaps you want to have automatic previewing when you work on
`.Rhtml` files. You'd configure a conversion using regexps against the
file name to make a command line:

```emacs-lisp

(add-to-list 'previewing-build-command-alist
             `("\\(.*\\)\\.[Rr]\\(html?\\)$" ; match file name
               ("R" "-e"
                ,(concat "library(knitr);"
                         "knit(commandArgs(trailingOnly=TRUE)[[1]],"
                         "     output=commandArgs(trailingOnly=TRUE)[[2]])")
                "\\&" "\\1.\\2")))      ; command args based on match

(add-mode-hook 'html-mode-hook 'previewing-mode)

```

You can also use file-local veriables to specify preveiw steps for
particular files. For more configuration examples see the
`examples` directory.

### TODO:

* Default fall back on existing emacs compiling infrastructure
* Maybe a mechanism for asking about overwrites
* Support farming out work to (persistent) inferior shells
* Do async in a less ad hoc way
* Extend "build" and "view" to maybe other verbs? ("test", "tangle",
  "extract")
