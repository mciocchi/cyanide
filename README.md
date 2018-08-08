# cyanide (CyanIDE's Yet Another Non-IDE)

## Features

* hooks to allow users to run arbitrary elisp at project load time

* easy project specific configuration via .cy/init.el files

* A way to automatically initialize projects from git or from an arbitrary local directory

* a dead-simple means for users to define project lifecycle tasks (compile,
  test, run, etc.)

* a way to "pop" into arbitrary buffer and window configurations, especially at
  project load time

* an extensible API for advanced users to work with projects and artifacts,
  implemented in EIEIO CLOS

* nearly instant project aware search

* ability to dynamically reload projects after changing their configuration via
  cyanide-reload and cyanide-reload-project-dotfiles.

## Goals

* To help beginner emacs users with minimal knowledge of elisp get started
   with emacs quickly by providing core functionality which most projects need.

* To help intermediate emacs users by providing hooks for them to extend
   cyanide projects with additional customizations.

* To help advanced emacs users by providing a common framework which they can
   use to work together to develop modern features with speed and simplicity.

## Quick Start

### 1) Begin with a spacemacs installation.

If you would like your CyanIDE install to work with zero configuration, choose
"helm" when spacemacs prompts you which search-narrowing framework you would
like to use:

```bash
EMACSD=~/.emacs.d;
LISP=$EMACSD/lisp;

git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d &&

    mkdir -p $LISP &&

    git clone https://github.com/mciocchi/cyanide.git $LISP"/cyanide" &&

    nohup emacs >> /dev/null &
```

### 2) Once emacs is done bootstrapping, change the dotspacemacs/user-config
function in your .spacemacs file to load CyanIDE. You may wish to copy the
example below to get started.

```lisp
(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (let ((default-directory (concat user-emacs-directory "lisp")))
    (normal-top-level-add-subdirs-to-load-path))

  (setq cyanide-project-toplevel-directories
        `("~/projects/"))

  (require 'cyanide)

  ;; make cyanide scope and classes available everywhere
  (setq-default cyanide-mode t)

  ;; If you prefer ivy instead of helm, uncomment these lines. You may also
  ;; set alternate search functions here.
  ;; (setq cyanide-keyword-search-function 'counsel-projectile-ag)
  ;; (setq cyanide-find-file-function 'counsel-projectile-find-file)
  ;; (setq cyanide-occur-function 'swiper)

  ;; Optional but useful: start with a single project defined, for working
  ;; inside of dot-emacs directory
  (cyanide-project :id 'dot-emacs
                   :display-name "dot-emacs"
                   ;; views are also optional and you can use CyanIDE without
                   ;; ever using them. cyanide-minimal-view is designed to be as
                   ;; uninvasive to the traditional emacs user experience as
                   ;; possible and will only render the CyanIDE menu, cd into
                   ;; the project root, and change the window title.
                   :default-view cyanide-minimal-view
                   :load-hook '((lambda ()
                                  (dired (cyanide-project-oref :path))))
                   :tasks '()
                   :teardown-hook '()
                   :path "~/.emacs.d/")
  )
```

### 3) create a "projects" toplevel directory:

```bash
mkdir ~/projects
```

### 4) create an example project directory inside of the toplevel:

Either invoke cyanide-project-initialize by typing C-c c i, or run it via the
minibuffer. You may use it to generate projects which look something like the
example below, however, cyanide-project-initialize does not yet automatically
create lifecycle tasks.

cyanide-project-initialize may also initialize projects by cloning them from git.

```lisp
;; ~/projects/example/.cy/init.el

(cyanide-project :id 'example
                 :display-name "example"
                 :default-view 'cyanide-minimal-view
                 :tasks '(hello-world-task))

(cyanide-task :id 'hello-world-task
              :display-name "Hello World"
              :func (lambda ()
                    (interactive)
                    (async-shell-command "echo Hello, world!")))
```

## Keybindings

cyanide-mode-map provides the following keybindings, which may be altered or
overridden globally just like any other emacs mode-map. It is also possible to
override these variables on a project-by-project basis by setting them inside of
a project's load-hook.

```
"C-c c i" cyanide-project-initialize
"C-c c l" cyanide-load-project-prompt
"C-c c d" cyanide-disable-current-view
"C-c c v" cyanide-enable-view-prompt
"C-c c t" cyanide-task-prompt
"C-c c a" cyanide-keyword-search-function (defaults to helm-projectile-ag)
"C-c c f" cyanide-find-file-function      (defaults to cyanide-helm-find)
"C-c c o" cyanide-occur-function          (defaults to helm-occur)
```

These keybindings are mostly self-explanatory, but if you wish, you may refer to
the interactive documentation exposed through the standard emacs help keybinding
"C-h c" for more information.

## Project Roadmap (in order of priority)

* write a CyanIDE spacemacs layer

* cyanide-project-initialize should be able to be configured to work with other
  version control systems and OSes.
  
* reconsider whether CyanIDE should have anything to do with project search

  This is a crowded space and there are a lot of utilities that already do this.

* look into getting CyanIDE into MELPA

  This will require putting everything in one file. Is it really worth it?

* write more tests with ert - we need better code coverage

* fix CyanIDE menu - consider replacing with [hydra](https://github.com/abo-abo/hydra)

## CyanIDE Ecosystem

If you use CyanIDE, there are several other utilities which may be useful to you:

### [cyanide-org-integration](https://github.com/mciocchi/cyanide-org-integration)

allows org-mode configuration on a per-project basis

### [cyanide-treemacs-view](https://github.com/mciocchi/cyanide-treemacs-view)

launches a [treemacs](https://github.com/Alexander-Miller/treemacs) sidebar in the current project
which can be configured to automatically pop up at project load time

### [cyanide-shell-view](https://github.com/mciocchi/cyanide-shell-view)

can instantly launch or close a full screen shell instance in the current
project root.

## Screenshots

![Load Project](https://i.imgur.com/z14mLs8.png "Load Project Prompt")

![Tasks Menu](https://i.imgur.com/76YKADT.png "Tasks Menu")

![Task Executed](https://i.imgur.com/IrLEIWF.png "Task Executed")
