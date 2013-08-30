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

* maybe a mechanism for asking about overwrites
* Default fall back on emacs compiling infrastructure that already
  exists `:)`
* Support external processes that are kept alive in inverior process
  buffers?
