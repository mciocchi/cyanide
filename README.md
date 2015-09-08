# cyanide
CYANIDE: (CyanIDE's Yet Another Non-IDE)

* about

CyanIDE is a global minor mode that provides an extensible development
framework which runs on top of emacs. It provides functionality for
project-aware code searching and browsing.

I decided to write CyanIDE because I was looking for an emacs mode to
do the same thing, but could not find one with sensible defaults that
was easy to configure and extend. CyanIDE will run out of the box
with almost no configuration.

* install instructions:

<code>
cd ~/.emacs.d/

git clone https://github.com/mciocchi/cyanide.git
</code>
In .emacs add the following:

<code>
(require 'eieio)

(require 'cyanide)

(cyanide-mode 1)

(puthash 'dot-emacs

         (cyanide-project "dot-emacs"
         
                          :display-name "dot-emacs"
                          
                          :default-view 'cyanide-elisp-view
                          
                          :load-hook '()
                          
                          :proj-tree '(("/home/user/.emacs")
                          
                                       ("/home/user/.emacs.d/" ".*\.el$")))
                                       
         cyanide-projects)
</code>

Follow the example above to map more projects into cyanide.

* views and projects

CyanIDE is roughly based upon the model-view-controller design pattern. cyanide.el
controls bootstrap logic and high-level functions. cyanide-views.el defines view
objects to manage setup and teardown of different window arrangements. Projects
are modeled by the user in their .emacs, and are the only config that is actually
required for cyanide to work.

Currently, only cyanide-elisp-view and cyanide-default-view are available,
but cyanide-default-view provides decent code-browsing support for elisp,
java, ruby, python, perl, clojure, and javascript.

The philosophy of CyanIDE is to be as extensible as possible. It should be
relatively easy for users to define new views for various needs if they
copy those that already exist in cyanide-views.el. I would be very grateful
to anyone who writes these or can help with any bugs and contribute back.

* to do/known issues

* bug while selecting region in one buffer that is open in two windows

Cursor jumps to wrong window somehow.

Workaround in progress:

Disable cyanide-panel-search in these instances if window count in same frame
for a given window > 1.

* cyanide-default-disabler does not tear down views cleanly

cyanide-frame-windows-locked and cyanide-frame-windows-dedicated do not fully
unlock-and un-dedicate all windows. Needs more investigation.

* three-way incompatibility between emacs-lock, occur, and multi-frame editing

Currently working around this by having cyanide-panel not search when multiple
frames are open. This is basically a problem with the emacs internals themselves
as far as I can tell.
