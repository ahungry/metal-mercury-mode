# Metal Mercury Mode

[![MELPA](http://melpa.org/packages/metal-mercury-mode.svg)](http://melpa.org/#/metal-mercury-mode)

Major mode for editing mercury (http://mercurylang.org) files.

The default mercury-mode (derived from prolog-mode) seems to have
many issues (syntax and indentation both fail).

In this case, it seems easier to rewrite mercury-mode as an
independent mode that just handles exactly what it needs to.

## Installation
To install, clone the repository via:

```
cd ~/.emacs.d
git clone https://github.com/ahungry/metal-mercury-mode.git
```

Then, make sure to add the following to your ~/.emacs:

### Using require

```lisp
(add-to-list 'load-path "~/.emacs.d/metal-mercury-mode/")
(require 'metal-mecury-mode)
```

### Using metal-mercury-mode.el

Keybindings:

- `C-c C-c` : Compile your current file via `mmc --make file_name`
- `C-c C-r` : Compile and run your current file (no interactive input)

## License
GPLv3
