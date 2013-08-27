##Previewing-mode

A minor mode that automatically compiles and views the file you're
working on whenever you `save-buffer.`

Use it for Markdown, HTML, LaTeX, sweave, knitr, pic and other markup
files (configuration details forthcoming)

Use it to automatically run your unit tests.

Fairly easily configurable to do any build/view process you can phrase
in terms of shell commands. Use file local variables to configure it
for each file.

###TODO:

* make commands run asynchronously
* generalize elisp functions instead of shell command regexps
  * more specifically regexp command is a special case.
* allow command lookup by major-mode not just buffer file name
* maybe a mechanism for asking about overwrites
* A mechanism for the build step returning a "target" file that
we select from.
* Default fall back on emacs compiling infrastructure that already
  exists `:)`

Whee, look at this, it updates! In my browser! Every time I press `C-x
C-s`!
