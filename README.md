# cyanide (CyanIDE's Yet Another Non-IDE)

* About

  CyanIDE is a global minor mode that provides an extensible development
  environment for emacs. It provides functionality for project-aware searching
  and browsing of code for all popular programming languages. I have taken pains
  to make CyanIDE fast and easy to use. It boasts a near-instant load time, and
  thanks to integration with
  [The Silver Searcher](https://github.com/ggreer/the_silver_searcher), search
  times are also nearly instant, even in projects with thousands of files.

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
    declarative syntax composed of emacs lisp functions. CyanIDE does not
    require users to have any knowledge of emacs lisp, but its core API is
    designed to be quickly and easily extended by advanced users to declare new
    project structures to suit their needs.

    CyanIDE provides an advantage to developers by mapping configuration
    manually in this manner. Some IDEs generate project structure XML
    "automagically" by manipulating icons. Mapping project structure icons in
    this manner may initially appear to be simple and "user friendly," but in
    actuality, it can serve to obscure what the IDE is actually doing.

    Worse still, when the time comes to "export" project structure XML from a
    traditional IDE, perhaps to port to a new machine, or to share with a
    colleague, it never seems to work 100% correctly. At this point, users of
    traditional IDEs usually must resort to painstakingly executing a laundry
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

    Below is a working example of a simple init.el file containing a typical
    CyanIDE project structure. Note that it is not strictly necessary to map any
    projects at all in order to use CyanIDE, but many of the features of CyanIDE
    require a project to be loaded.

```lisp
;; init.el

;; Make files in dot emacs config directory available to be imported
(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path))

;; Import cyanIDE
(require 'cyanide)

;; Enable CyanIDE global-minor-mode on startup.
(setq-default cyanide-mode t)

;; Allow CyanIDE to discover .cy.el project structure files like this:
;; ~/projects/demo-project-2/.cy.el
;; ~/projects/demo-project-3/.cy.el
;; ...
(setq cyanide-project-toplevel-directories '("~/projects"))

;; Define a new project inside init.el
(cyanide-project :id 'demo-project
                 :display-name "Demo Project"
                 :description "Demo project for CyanIDE"
                 :path "~/projects/demo-project"
                 :default-view 'cyanide-minimal-view
                 :load-hook '(demo-project-load-hook)
                 :teardown-hook '()
                 :tasks '(hello-world-task))

;; Define a task for this project
(cyanide-task :id 'hello-world-task
              :display-name "Hello World"
              :func 'hello-world)

;; Define a hello-world function which is only available after our Demo Project
;; has been loaded
(defun demo-project-load-hook ()
  (defun hello-world ()
    (interactive
      (async-shell-command "echo 'Hello, world!'"))))
```

  * .cy.el Project Files

    CyanIDE project structures like the examples above may be mapped via two
    different methods. They may be mapped either in the users .init.el, or in
    .cy.el files placed inside the project root. There are two vars which
    control how CyanIDE checks for these project configuration files:
    "cyanide-project-toplevel-directories" and
    "cyanide-project-config-file-name."

    "cyanide-project-toplevel-directories" is a list of paths which CyanIDE will
    scan for project directories containing .cy.el project structure files.

    At startup time, CyanIDE will scan all directories, to a depth of one,
    within each toplevel directory path to determine whether they contain .cy.el
    files. Any directory found which contains a .cy.el file will be considered
    by CyanIDE to be a project, and CyanIDE will attempt to evaluate the .cy.el
    file contained therein.

    It should be noted that a .cy.el file may not necessarily contain a
    cyanide-project definition, and in fact it may contain any arbitrary elisp
    code. Users should therefore carefully audit .cy.el files imported from
    third-party sources before loading them.

    "cyanide-project-config-file-name" is a var which CyanIDE uses to deterimine
    what name to use to search for project structure files. It defaults to
    ".cy.el," but users may opt to choose a different name.

```lisp
;; ~/projects/demo-project-2/.cy.el

;; When defining a project inside of a .cy.el file, it is not necessary to
;; provide a project :path. CyanIDE will infer the :path from the location of
;; the .cy.el file which is being loaded:
(cyanide-project :id 'demo-project-2
                 :display-name "Demo Project"
                 :description "Demo project for CyanIDE"
;;               :path "~/projects/demo-project-2"
                 :default-view 'cyanide-minimal-view
                 :load-hook '(demo-project-2-load-hook)
                 :teardown-hook '()
                 :tasks '(hello-world-task))
;; ...

```

    The examples above can be used as a guide to map other projects into
    CyanIDE. Tasks, Views, and Projects are dynamically pulled into emacs at
    project load time like this:

![Load Project](http://imgur.com/3aKGWZ9 "Load Project Prompt")

![Tasks Menu](http://imgur.com/A8ehwfg "Tasks Menu")

![Task Executed](http://imgur.com/xGtxCGf "Task Executed")

  * Views

    CyanIDE is roughly based upon a model-view-controller design. Projects are
    modeled by the user in their init.el, just like the examples
    above. cyanide.el controls bootstrap logic and high-level functions. Views
    are objects that manage setup and teardown of different configurations,
    including buffer and window arrangements.

    Currently, only cyanide-minimal-view, cyanide-elisp-view and
    cyanide-default-view are available. To stick with standard emacs behavior
    and avoid re-arranging windows at load-time, use cyanide-minimal-view, which
    does not do anything else other than adding project tasks to the menu-bar.

    If users prefer a certain window and buffer arrangement for a specific
    project, they need only define it and map it to the :default-view of their
    project structure. :default-view does not limit a project to that view,
    however. Users may pop into and out of views at any time. If a user prefers
    a certain view whenever executing a given task, such as starting a debugger
    or executing a diff, views can even be enabled from inside a corresponding
    cyanide-task.

    It should also be noted that views are additive: enabling one will add its
    :id to the cyanide-current-views list, but will not necessarily disable the
    previous view unless users explicitly configure it to do so. By implication,
    this property of views allows them to "stack up" and delegate
    responsibility. For instance: in a simple setup, a view might enable two
    "subviews," one for window A, and one for window B.

    When views are disabled, they are popped off of the cyanide-current-views
    list and disabled in "last in, first out" order. There are currently two
    methods to disable views: cyanide-disable-current-view, which only disables
    the most recent view, and cyanide-disable-all-views.

    The sky is the limit with views, but users are advised to keep it simple, as
    interactions can happen when enabling multiple views at the same time that
    were not explicitly designed to work together.

    Users that have written views, tasks, and other extensions that they find
    particularly useful are encouraged to submit them for potential inclusion in
    CyanIDE.

  * Tasks

    Tasks represent units of work manually executed by CyanIDE users. The views
    bundled with CyanIDE by default render tasks in the CyanIDE menu in the
    menu-bar, but this behavior is entirely view-specific and may not be
    available in third-party views.

    Tasks are constructed with a :func property which can store any emacs lisp
    function, including async-shell-command, which allows CyanIDE users to
    compile and deploy their code using whatever build tools they prefer.

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
    "C-c c o" cyanide-helm-occur
    ```

    These should be self-explanatory, but if you wish, you may refer to the
    inline documentation exposed through the emacs help interface "C-h c" for
    more information.

  * Thanks

    There are a number of projects without which CyanIDE would not have been
    possible. Of course I must extend a special thanks to the thousands
    of developers who have contributed to emacs over the years, legendary heroes
    from time immemorial, too numerous to name.

    I would also like to thank [Geoff Greer](https://github.com/ggreer) for
    developing the blindingly-fast Silver Searcher, without which CyanIDE search
    would have been greatly hampered. Also credit is due to Thierry Volpiatto
    and the other contributors to [helm](https://emacs-helm.github.io/helm/) for
    developing the helm search narrowing framework which I use with Syohei
    Yoshida's [emacs-helm-ag](https://github.com/syohex/emacs-helm-ag). In the
    early days I used [Wilfred Hughes](https://github.com/Wilfred)' ag.el for
    the same purpose. CyanIDE is really just a wrapper bundling together these
    utilities which have already solved the difficult problems for me, and in
    that sense I am just standing on the shoulders of giants.

    I would also like to thank [Alex Kesling](https://github.com/akesling) for
    providing invaluable advice regarding some development decisions I had to
    make, and also, along with [Rachael Hobbs](https://github.com/rahobbs) and
    [Christopher Fox](https://github.com/cdfox) offering me consolation and
    delicious cocktails whenever I felt overwhelmed facing difficult
    problems. In the same vein, last but definitely not least, I would like to
    thank [Gregory Maglio](https://github.com/gmaglio) for his constant
    encouragement and- separately but simultaneously- along with
    [Roman Khmelichek](https://github.com/rkhmelichek) and 
    [Jack Man](https://github.com/jdotman), tolerating my obsessive rambling
    about functional programming constructs for more than a year!
