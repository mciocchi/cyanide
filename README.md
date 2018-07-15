# cyanide (CyanIDE's Yet Another Non-IDE)

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
                   :default-view 'cyanide-minimal-view
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

```bash
mkdir ~/projects/example
```

### 5) create a cyanide config directory inside of our example project:

```bash
mkdir ~/projects/example/.cy
```

### 6) create a project-specific init file inside of the config directory:

```lisp
;; ~/projects/example/.cy/init.el

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

### 7) eval-buffer on the init.el file you created, or just close and re-open emacs

### 8) invoke "C-c c l" to load the project you just created

## Goals

1) To help beginner emacs users with minimal knowledge of elisp get started
   with emacs quickly by providing core functionality which most projects need.

2) To help intermediate emacs users by providing hooks for them to extend
   cyanide projects with additional customizations.

3) To help advanced emacs users by providing a common framework which they can
   use to work together to develop modern features with speed and simplicity.

## Features

* nearly instant project aware search via helm-projectile-ag

* hooks to run arbitrary elisp at project load time

* easy project specific configuration via .cy/init.el files

* a dead-simple means for users to define project lifecycle tasks (compile,
  test, run, etc.)

* a way to optionally "pop" into arbitrary buffer and window configurations,
  especially at project load time

* an extensible API for advanced users to work with projects and artifacts,
  implemented in EIEIO CLOS

## Keybindings

cyanide-mode-map provides the following keybindings, which may be altered or
overridden globally just like any other emacs mode-map. It is also possible to
override these variables on a project-by-project basis by setting them inside of
a project's load-hook.

```
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
