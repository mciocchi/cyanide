# cyanide (CyanIDE's Yet Another Non-IDE)

This file is part of CyanIDE.

CyanIDE is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

CyanIDE is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with CyanIDE.  If not, see <http://www.gnu.org/licenses/>.

* About

  CyanIDE is a global minor mode that provides an extensible development
  environment which runs on top of emacs. It provides functionality for
  project-aware searching and browsing of java, perl, python, clojure, ruby,
  javascript, and emacs-lisp code. I have taken pains to make CyanIDE fast and
  easy to use. It boasts a near-instant load time, and thanks to integration
  with [The Silver Searcher](https://github.com/ggreer/the_silver_searcher),
  search times are also nearly instant, even in projects with thousands of
  files.

  I decided to write CyanIDE because I was looking for an emacs mode to do the
  same thing, but could not find one with sensible defaults that was easy to
  configure and extend. CyanIDE will run out of the box with almost no
  configuration.

* Installation Instructions:

  * Dependencies:

    * emacs 25 or higher

      Check whether this package is available from your OS. If it is not, you
      will need to follow the
      [instructions](https://savannah.gnu.org/projects/emacs) to download emacs
      directly.

    * helm

      Installation instructions can be found here:

      https://github.com/emacs-helm/helm/wiki#install

    * helm-ag

      Installation instructions can be found here:

      https://github.com/syohex/emacs-helm-ag#installation

  * CyanIDE Install

    ```bash
    cd ~/.emacs.d/

    git clone https://github.com/mciocchi/cyanide.git
    ```

* Configuration

  * Project Structure

    CyanIDE is different from other IDEs because projects are mapped in a simple
    declarative syntax that is actually a domain specific language composed of
    emacs lisp functions. CyanIDE does not require users to have any knowledge
    of emacs lisp, but its core API is designed to be quickly and easily
    extended by advanced users to declare new project structures to suit their
    needs.

    CyanIDE provides an advantage to developers by mapping configuration
    manually in this manner. Some IDEs generate project structure XML
    "automagically" by manipulating icons. Mapping project structure icons in
    this manner may initially appear to be simple and "user friendly," but in
    actuality, it can serve to obscure what the IDE is actually doing.

    Worse than that, when the time comes to "export" project structure XML from
    a traditional IDE, perhaps to port to a new machine, or to share with a
    colleague, it never seems to work 100% correctly. At this point, users of
    traditional IDEs usually must resort to painstakingly recording a laundry
    list of GUI operations in order to restore their environment back to its
    original working state.

    CyanIDE takes a radically different approach to this problem. CyanIDE does
    not attempt to do anything "automagically." CyanIDE does not do anything
    unless you tell it to do so. CyanIDE does not attempt to simplify problems
    by closing them over with a GUI, obscuring what it is doing. Rather, just
    like emacs itself, being 100% Free and Open Source software, CyanIDE "opens
    up" project internals and makes them freely available to the developer in an
    ergonomic human-readable form, rather than machine-generated XML. Like emacs
    itself, CyanIDE project structure abstractions can be gradually peeled away
    like the layers of an onion, in order to expose or even replace core
    functionality.

    Below is an example of a typical CyanIDE project structure. Note that it is
    not strictly necessary to map any projects at all in order to use CyanIDE,
    but many of the features of CyanIDE require a project to be loaded.

```elisp
;; we cannot enable CyanIDE unless we tell emacs to import it
(require 'cyanide)

;; we need to make cyanide scope and classes available before declaring anything
(setq-default cyanide-mode t)

;; Make a test project with some tasks. This is the bare minimum of what you
;; need. It is possible to run cyanide-minor-mode without any projects defined,
;; albeit somewhat pointless.
(cyanide-project-builder
 '(:id 'test-project
   :display-name "Test Project"
   ; I want to "pop" into my default preferred view when this project is loaded
   :default-view 'cyanide-elisp-view
   :path "/home/user/test-project"
   ; functions to call at load time
   :load-hook '()
   ; List of functions to call at project teardown time. Using this generic
   ; teardown hook will prevent multiple views from activating at the same time
   ; when switching between projects.
   :teardown-hook '(cyanide-disable-current-view-if-exists)
   ; scripts to automate repetitive bits of work I keep having to do by
   ; hand. These are convenient to have, but not required.
   :tasks '(systemd-nspawn
            maven
            do-something
            do-something-else)))

;; Everything after this point is optional.

;; I want to use maven with this project. There are a lot of maven tasks I use,
;; so I should declare this as a separate sub-menu using cyanide-menu-builder.
(cyanide-menu-builder '(:id 'maven
                        :display-name "Maven"
                        :members '(mvn-validate
                                   mvn-compile
                                   mvn-test
                                   mvn-package
                                   mvn-verify
                                   mvn-install
                                   mvn-deploy)))

;; echo to act as a placeholder for a real shell command, just to test
(cyanide-task-builder '(:id 'mvn-validate
                        :display-name "Validate"
                        :func (lambda () (interactive)
                                (async-shell-command
                                 (concat "echo \"mvn validate\"")))))

(cyanide-task-builder '(:id 'mvn-compile
                        :display-name "Compile"
                        :func (lambda () (interactive)
                                (async-shell-command
                                 (concat "echo \"mvn compile\"")))))

(cyanide-task-builder '(:id 'mvn-test
                        :display-name "Test"
                        :func (lambda () (interactive)
                                (async-shell-command
                                 (concat "echo \"mvn test\"")))))

(cyanide-task-builder '(:id 'mvn-package
                        :display-name "Package"
                        :func (lambda () (interactive)
                                (async-shell-command
                                 (concat "echo \"mvn package\"")))))

(cyanide-task-builder '(:id 'mvn-verify
                        :display-name "Verify"
                        :func (lambda () (interactive)
                                (async-shell-command
                                 (concat "echo \"mvn verify\"")))))

(cyanide-task-builder '(:id 'mvn-install
                        :display-name "Install"
                        :func (lambda () (interactive)
                                (async-shell-command
                                 (concat "echo \"mvn install\"")))))

(cyanide-task-builder '(:id 'mvn-deploy
                        :display-name "Deploy"
                        :func (lambda () (interactive)
                                (async-shell-command
                                 (concat "echo \"mvn deploy\"")))))

;; I want to deploy my project inside a controlled environment for testing. This
;; just requires me to map a few init scripts here.
(cyanide-menu-builder '(:id 'systemd-nspawn
                        :display-name "Systemd Nspawn"
                        :members '(jail-start
                                   jail-stop)))

(cyanide-task-builder '(:id 'jail-start
                        :display-name "Start Test Project Dev Environment"
                        :func (lambda () (interactive)
                                (async-shell-command
                                 (concat "/home/"
                                         "user/"
                                         "projects/"
                                         "test-project/"
                                         "jail-test-project-start.sh")))))

(cyanide-task-builder '(:id 'jail-stop
                        :display-name "Stop Test Project Dev Environment"
                        :func (lambda () (interactive)
                                (async-shell-command
                                 (concat "/home/"
                                         "user/"
                                         "projects/"
                                         "test-project/"
                                         "jail-test-project-stop.sh")))))

;; I want to declare some other miscellaneous tasks that don't belong in either
;; of the menus I declared above
(cyanide-task-builder '(:id 'do-something
                        :display-name "Do Something"
                        :func (lambda () (interactive)
                                (async-shell-command
                                        "echo \"Doing something!\""))))

(cyanide-task-builder '(:id 'do-something-else
                        :display-name "Do Something Else"
                        :func (lambda () (interactive)
                                (async-shell-command
                                        "echo \"Doing something else!\""))))
```
    Use the example above to map other projects into cyanide. Tasks, Views, and
    Projects are dynamically pulled into emacs at load time like this:

![Load Project](https://i.imgur.com/TYNtCyf.png "Load Project")

![Load Project Prompt](https://i.imgur.com/hxq7ufn.png "Load Project Prompt")

![Tasks Menu](https://i.imgur.com/mY5QUWh.png "Tasks Menu")

![Tasks Prompt](https://i.imgur.com/thYUaBO.png "Tasks Prompt")

  * Views

    CyanIDE is roughly based upon a model-view-controller design. Projects are
    modeled by the user in their init.el, just like in the example
    above. cyanide.el controls bootstrap logic and high-level functions. Views
    are objects that manage setup and teardown of different buffer and window
    arrangements.

    Currently, only cyanide-elisp-view and cyanide-default-view are available,
    but cyanide-default-view provides decent code searching and browsing support
    for the languages listed above.

    If users prefer a certain window and buffer arrangement for a specific
    project, they need only define it and map it to the :default-view of their
    project structure. :default-view does not limit a project to that view,
    however. Users may pop into and out of views at any time. If a user prefers
    a certain view whenever executing a given task, such as starting a debugger
    or executing a diff, views can even be enabled from inside a cyanide-task.

    It should also be noted that views are additive: enabling one will set
    cyanide-current-view to the value of its :id, but will not necessarily
    disable the previous view unless users explicitly configure it to do so. By
    implication, this property of views allows them to "stack up" and delegate
    responsibility.

    For instance: in a simple setup, a view might enable two subviews, one for
    window A, and one for window B. Alternately, multiple buffers in a single
    window can be "nested" like Russian dolls.

    The sky is the limit with views, but users are advised to keep it simple, as
    interactions can happen when enabling multiple views at the same time that
    were not explicitly designed to work together. In the upcoming release of
    CyanIDE, the cyanide-current-view global variable will be replaced by a
    linked list that better represents the "stacking" behavior of views.

    Users that have written views, tasks, and other extensions that they find
    particularly useful are encouraged to submit them for potential inclusion in
    CyanIDE.

  * Tasks

    Tasks represent units of work manually executed by CyanIDE users. The views
    bundled with CyanIDE by default render tasks in the CyanIDE menu in the
    menubar, but this behavior is entirely view-specific and may not be
    available in third-party views.

    Tasks are constructed with a :func property which can store any emacs lisp
    lambda including async-shell-command, which allows CyanIDE users to compile
    and deploy their code using whatever build tool they prefer.

  * Keybindings

    cyanide-mode-map provides the following keybindings out of the box, which
    may be altered or overridden just like any other emacs mode-map. Functions
    in CyanIDE appended with "-prompt" invoke a generic cyanide-prompt function
    under the hood that provides tab completion by default.

    ```
    "C-c c l" cyanide-load-project-prompt
    "C-c c d" cyanide-disable-current-view
    "C-c c v" cyanide-enable-view-prompt
    "C-c c t" cyanide-task-prompt
    "C-c c a" cyanide-helm-ag
    "C-c c f" cyanide-helm-find
    ```

    These should be self-explanatory, but if you wish you may refer to the
    inline documentation exposed through the emacs help interface "C-h c" for
    more information.

  * Thanks

    There are a number of projects without which CyanIDE would not have been
    possible. Of course I must extend a special thanks to the emacs team at
    [gnu.org](https://www.gnu.org) for their continued hard work on emacs and
    their tireless dedication to keeping software free.

    I would also like to thank [Geoff Greer](https://github.com/ggreer) for
    developing the blindingly-fast Silver Searcher, without which CyanIDE search
    would have been greatly hampered. Towards that end, I would also like to
    thank [Wilfred Hughes](https://github.com/Wilfred) for his work on ag.el,
    which allowed me to seamlessly interface CyanIDE with it.

    I would also like to thank [Alex Kesling](https://github.com/akesling) for
    providing invaluable advice regarding some development decisions I had to
    make, and also, along with [Rachael Hobbs](https://github.com/rahobbs) and
    [Christopher Fox](https://github.com/cdfox) offering me consolation and
    delicious cocktails whenever the bugs started coming out of the woodwork and
    I was feeling overwhelmed. In the same vein, last but definitely not least,
    I would like to thank [Gregory Maglio](https://github.com/gmaglio) for his
    constant encouragement and- separately but simultaneously- along with
    [Roman Khmelichek](https://github.com/rkhmelichek) and
    [Jack Man](https://github.com/jdotman), tolerating my obsessive rambling
    about functional programming constructs for more than a year!
