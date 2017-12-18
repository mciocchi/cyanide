# cyanide (CyanIDE's Yet Another Non-IDE)

## Quick Start

1) If you have not installed cask, follow the instructions at:
   [cask.readthedocs.io](https://cask.readthedocs.io/en/latest/guide/installation.html)
2) From inside emacs, invoke "M-x package-list-packages"
3) In the *packages* buffer which just opened, find package "cyanide" listed in
   the melpa archive
4) press "i" on cyanide, and then "x" to execute the installation
5) make sure that you have the bare minimum in your init.el for a cyanide
   installation via cask:

```lisp
;; ~/.emacs.d/init.el

;; Your cask file will be in a different location depending upon which version
;; you installed:
(require 'cask "~/.emacs.d/.cask/25.3/elpa/cask-20170917.1107/cask.el")

(cask-initialize)

;; Allow CyanIDE to discover the .cy.el file we created:
(setq cyanide-project-toplevel-directories '("~/projects/"))

;; Import cyanIDE
(require 'cyanide)

;; Enable CyanIDE global-minor-mode on startup.
(setq-default cyanide-mode t)
```

5) create a "projects" toplevel with an example project directory inside of it:

```bash
mkdir -p ~/projects/example
```

6) create a .cy.el project config file:

```lisp
;; ~/projects/example/.cy.el

(cyanide-project :id 'example-project
                 :display-name "example-project"
                 :default-view 'cyanide-minimal-view
                 :tasks '(hello-world-task))

(cyanide-task :id 'hello-world-task
              :display-name "Hello World"
              :func (lambda ()
                    (interactive)
                    (async-shell-command "echo Hello, world!")))
```

7) eval-buffer on the .cy.el file you created, or just close and re-open emacs
8) invoke "C-c c l" to load the project you just created

## Goals

1) To help beginner emacs users with minimal knowledge of elisp get started
   with emacs quickly by providing core functionality which most projects need.

2) To help intermediate emacs users by providing hooks for them to extend
   cyanide projects with additional customizations.

3) To help advanced emacs users by providing a common framework which they can
   use to work together to develop modern features with speed and simplicity.

## Features

* nearly instant project aware search via helm-ag

* hooks to run arbitrary elisp at project load time

* easy project specific configuration via .cy.el dotfiles

* a dead-simple means for users to define project lifecycle tasks (compile,
  test, run, etc.)

* a way to optionally "pop" into arbitrary buffer and window configurations,
  especially at project load time

* an extensible API for advanced users to work with projects and artifacts,
  implemented in EIEIO CLOS

## Keybindings

cyanide-mode-map provides the following keybindings, which may be altered or
overridden just like any other emacs mode-map:

```
"C-c c l" cyanide-load-project-prompt
"C-c c d" cyanide-disable-current-view
"C-c c v" cyanide-enable-view-prompt
"C-c c t" cyanide-task-prompt
"C-c c a" cyanide-helm-ag
"C-c c f" cyanide-helm-find
"C-c c o" cyanide-helm-occur
```

These keybindings are mostly self-explanatory, but if you wish, you may refer to
the interactive documentation exposed through the standard emacs help keybinding
"C-h c" for more information.

## Screenshots

![Load Project](https://i.imgur.com/z14mLs8.png "Load Project Prompt")

![Tasks Menu](https://i.imgur.com/76YKADT.png "Tasks Menu")

![Task Executed](https://i.imgur.com/IrLEIWF.png "Task Executed")

## Thanks

There are a number of projects without which CyanIDE would not have been
possible. Of course I must extend a special thanks to the thousands of
developers who have contributed to emacs over the years, legendary heroes from
time immemorial, too numerous to name.

I would also like to thank [Geoff Greer](https://github.com/ggreer) for
developing the blindingly-fast Silver Searcher, without which CyanIDE search
would have been greatly hampered. Also credit is due to Thierry Volpiatto and
the other contributors to [helm](https://emacs-helm.github.io/helm/) for
developing the helm search narrowing framework which I use with Syohei Yoshida's
[emacs-helm-ag](https://github.com/syohex/emacs-helm-ag). In the early days I
used [Wilfred Hughes](https://github.com/Wilfred)' ag.el for the same
purpose. CyanIDE is really just a wrapper bundling together these utilities
which have already solved the difficult problems for me, and in that sense I am
just standing on the shoulders of giants.

I would also like to thank [Alex Kesling](https://github.com/akesling) for
providing invaluable advice regarding some development decisions I had to make,
along with [Rachael Hobbs](https://github.com/rahobbs) and [Christopher
Fox](https://github.com/cdfox). In the same vein, last but definitely not least,
I would like to thank [Gregory Maglio](https://github.com/gmaglio) for his
constant encouragement and- separately but simultaneously- along with [Roman
Khmelichek](https://github.com/rkhmelichek) and [Jack
Man](https://github.com/jdotman), tolerating my rambling about functional
programming constructs for more than a year!
