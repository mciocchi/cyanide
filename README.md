# cyanide (CyanIDE's Yet Another Non-IDE)

* about

CyanIDE is a global minor mode that provides an extensible development
environment which runs on top of emacs. It provides functionality for
project-aware searching and browsing of java, perl, python, clojure, ruby,
javascript, and emacs-lisp code.

I decided to write CyanIDE because I was looking for an emacs mode to do the
same thing, but could not find one with sensible defaults that was easy to
configure and extend. CyanIDE will run out of the box with almost no
configuration.

* install instructions:

** install dependencies:

ag.el https://github.com/Wilfred/ag.el
Note- ag.el can also be installed from melpa.org via package.el

<code>

cd ~/.emacs.d/

git clone https://github.com/mciocchi/cyanide.git

</code>

In .emacs or .emacs.d/init.el add the following. Adjust proj-root, display-name,

default-view, etc. as the need arises.

<code>

(require 'eieio)

(require 'cyanide)

(cyanide-mode 1)

(puthash 'dot-emacs

         (cyanide-project
         
                          :display-name "dot-emacs"
                          
                          ;; Use cyanide-default-view for other languages,
                          ;; or write your own view.
                          :default-view 'cyanide-elisp-view
                          
                          :load-hook '()
                          
                          :proj-root "/home/user/.emacs.d/")

         cyanide-projects)
</code>

Follow the example above to map other projects into cyanide.

* views and projects

CyanIDE is roughly based upon a model-view-controller design. cyanide.el
controls bootstrap logic and high-level functions. cyanide-views.el defines view
objects that manage setup and teardown of different buffer and window
arrangements. Projects are modeled by the user in their .emacs, just like in the
example above.

Currently, only cyanide-elisp-view and cyanide-default-view are available, but
cyanide-default-view provides decent searching and code-browsing support for the
languages listed above.

The philosophy of CyanIDE is to be as extensible as possible while also
providing sane defaults that work "out of the box" with minimal configuration.
It should be relatively easy for users to define new views for various needs if
they copy those that already exist in cyanide-views.el. I would be very
grateful to anyone who writes these or can help with any bugs and contribute
back.

* to do/known issues

** IN PROGRESS object-oriented abstraction layer for buffer/window management

Still looking into the feasibility/sustainability of this in the long-term.

** IN PROGRESS "subview" abstractions to simplify and clean up views