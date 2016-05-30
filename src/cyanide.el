;; This file is part of CyanIDE.
;;
;; CyanIDE is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; CyanIDE is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with CyanIDE.  If not, see <http://www.gnu.org/licenses/>.

(defvar cyanide-mode-map
  (let ((map (make-sparse-keymap)))
    (progn
      (define-key map (kbd "C-c c l") 'cyanide-load-project-prompt)
      (define-key map (kbd "C-c c d") 'cyanide-disable-current-view)
      (define-key map (kbd "C-c c v") 'cyanide-enable-view-prompt)
      (define-key map (kbd "C-c c t") 'cyanide-task-prompt)
      (define-key map (kbd "C-c c a") 'cyanide-ag-search)
      (define-key map (kbd "C-c c f") 'cyanide-find-dired)) map))

(define-minor-mode cyanide-mode
  "CyanIDE's Yet Another Non-IDE"  ; docstring
  nil                              ; init-value
  " cyanide "                      ; lighter
  :keymap cyanide-mode-map         ; keymap
  (progn                           ; body

    (require 'ag)

    (defvar cyanide-window-local-variables (make-hash-table :test 'equal))

    (defvar cyanide-treenodes '()
      "This collection holds all cyanide-treenode objects.")

    (defvar cyanide-current-view nil
      "This var stores a symbol used by cyanide to determine
       what view it's currently in.")

    (defvar cyanide-current-project nil
      "This var stores a symbol used by cyanide to determine
       what project it's currently in.")

    (defvar cyanide-verbose nil
      "non-nil if cyanide should use verbose logging.")

    (defvar cyanide-find-dired-exclude-vc
      "-not -path \"*\.svn*\" -not -path \"*\.git*\" "
      "Exclude version control dot directories from
       cyanide-find-dired. If this is set to an empty
       string, CyanIDE will not exclude vc directories.")

    (defvar cyanide-view-collection '()
      "cyanide-views are all stored in this list.")

    (defvar cyanide-project-collection '()
      "cyanide-projects are all stored in this list.")

    (defvar cyanide-menu-item-collection '()
      "cyanide-menu-items are stored in this list.")

    (defun cyanide-missing-arg-error (arg)
      (error (concat "Required argument"
                     " "
                     (format "%s" arg)
                     " "
                     "missing from"
                     " "
                     (format "%s" kwargs))))

    (defun cyanide-arg-required (arg kwargs)
      (when (not (memq arg kwargs)) (cyanide-missing-arg-error arg)))

    (defun cyanide-kwargobj-builder (class
                                     kwargs
                                     &optional
                                     required-kwargs
                                     lst)
      "Check arbitrary KWARGS and `cyanide-missing-arg-error' if
       there exist any REQUIRED-KWARGS that are not present.
       Construct object of class CLASS with KWARGS and
       `add-to-list' LST if it is present."
      (progn
        (when required-kwargs
          (mapcar
           (lambda (required-kwarg) (cyanide-arg-required required-kwarg kwargs))
           required-kwargs))
        (let ((obj (eval (cons class kwargs))))
          (when lst
            (add-to-list lst obj))
          obj)))

    (defun cyanide-project-builder (kwargs)
      "Constructor for `cyanide-project'."
      (cyanide-kwargobj-builder 'cyanide-project
                                kwargs
                                '(:id
                                  :display-name
                                  :default-view
                                  :project-root)
                                'cyanide-project-collection))

    (defun cyanide-view-builder (kwargs)
      "Constructor for `cyanide-view'"
      (cyanide-kwargobj-builder 'cyanide-view
                                kwargs
                                '(:id
                                  :display-name
                                  :enable
                                  :disable)
                                'cyanide-view-collection))

    (defclass cyanide-menu-item ()
      ((id           :initarg :id
                     :type symbol
                     :initform nil)
       (display-name :initarg :display-name
                     :type string
                     :initform ""))
      "Abstract class that provides functionality common
       to both `cyanide-menu' and `cyanide-menu-function'.")

    (defclass cyanide-menu (cyanide-menu-item)
      ((members :initarg :members
                :type list
                :initform '()))
      "Class that represents  a menu-panel and contains
       members which inherit from `cyanide-menu-item'.")

    (defclass cyanide-menu-function (cyanide-menu-item)
      ((func :initarg :func
             :type nil))
      "Class that represents functions stored in a
       `cyanide-menu'.")

    ; Tasks are nothing more than cyanide-menu-functions for now, but it is
    ; likely that they will need to provide additional functionality soon.
    (defclass cyanide-task (cyanide-menu-function)
      ()
      "Class that represents repetitive jobs that need to be
       manually executed.")

    (defun cyanide-menu-builder (kwargs)
      "Constructor for `cyanide-menu'."
      (cyanide-kwargobj-builder 'cyanide-menu
                                kwargs
                                '(:id :display-name)
                                'cyanide-menu-item-collection))

    (defun cyanide-menu-function-builder (kwargs)
      "Constructor for `cyanide-menu-function'."
      (cyanide-kwargobj-builder 'cyanide-menu-function
                                kwargs
                                '(:id :display-name :func)
                                'cyanide-menu-item-collection))

    (defun cyanide-task-builder (kwargs)
      "Constructor for cyanide-tasks."
      (cyanide-kwargobj-builder 'cyanide-task
                                kwargs
                                '(:id :display-name :func)
                                'cyanide-menu-item-collection))

    (cyanide-menu-builder '(:id 'cyanide-default-menu
                            :display-name "CyanIDE"
                            :members '(load-project
                                       silver-search-project
                                       find-in-project
                                       enable-view
                                       disable-current-view)))

    (cyanide-menu-function-builder '(:id 'load-project
                                     :display-name "Load a Project"
                                     :func (lambda ()
                                             (interactive)
                                             (call-interactively
                                              'cyanide-load-project-prompt))))

    (cyanide-menu-function-builder '(:id 'silver-search-project
                                     :display-name "Silver Search Project"
                                     :func (lambda ()
                                             (interactive)
                                             (call-interactively
                                              'cyanide-ag-search))))

    (cyanide-menu-function-builder '(:id 'find-in-project
                                     :display-name "Find in Project"
                                     :func (lambda ()
                                             (interactive)
                                             (call-interactively
                                              'cyanide-find-dired))))

    (cyanide-menu-function-builder '(:id 'enable-view
                                     :display-name "Enable a View"
                                     :func (lambda ()
                                             (interactive)
                                             (call-interactively
                                              'cyanide-enable-view-prompt))))

    (cyanide-menu-function-builder '(:id 'disable-current-view
                                     :display-name "Disable Current View"
                                     :func (lambda ()
                                             (interactive)
                                             (call-interactively
                                              'cyanide-disable-current-view))))

    ;; vectorize:
    ;; cast one string/function pair to a vector.
    ;; example output:
    ;; ["mvn clean" (lambda () (print "Executing mvn clean."))]
    (cl-defmethod cyanide-vectorize ((menu-function cyanide-menu-function))
      (vector (oref menu-function :display-name)
              (oref menu-function :func)))

    ;; vectorize:
    ;; if it's a menu-function, invoke vectorize on one item.
    ;; if it's a menu, invoke vectorize on all members, including sub-menus
    ;; example output:
    ;; ("CyanIDE Test Menu"
    ;;  ["mvn clean"
    ;;   (lambda nil
    ;;     (interactive)
    ;;     (print "executing mvn clean"))]
    ;;  ["mvn package"
    ;;   (lambda nil
    ;;     (interactive)
    ;;     (print "executing mvn package"))])
    (cl-defmethod cyanide-vectorize ((menu cyanide-menu))
      (cons (oref menu :display-name)
            (mapcar 'cyanide-vectorize (cyanide-get-menu-members menu))))

    (cl-defmethod cyanide-menu-render ((menu cyanide-menu)
                                       menu-symbol
                                       menu-mode-map)
      "Render a CyanIDE menu object to the menu bar"
      (easy-menu-define menu-symbol
                        menu-mode-map
                        (oref menu :display-name)
                        (cyanide-vectorize menu)))

    (defun cyanide-task-prompt ()
      "Prompt user for task to execute and execute it."
      (interactive
       (let ((menu (cyanide-get-one-by-slot 'tasks
                                            cyanide-menu-item-collection
                                            ":id"
                                            'eq)))
         (let ((tasks-collection (cyanide-unroll-all-menu-functions 'tasks)))
           (let ((task-names
                  (cyanide-list-display-names
                   tasks-collection)))
             (cyanide-prompt (lambda (x) (funcall (oref x :func)))
                             "Tasks: "
                             task-names
                             tasks-collection
                             ":display-name"
                             'equal
                             nil
                             1))))))

    (defclass cyanide-project ()
      ((id            :initarg :id
                      :initform nil
                      :type symbol)
       (display-name  :initarg :display-name
                      :initform ""
                      :type string
                      :documentation
                      "Display name for a cyanide-project")
       (default-view  :initarg :default-view
                      :type symbol
                      :documentation
                      "Default view at startup for a cyanide-project.")
       (project-root  :initarg :project-root
                      :initform ""
                      :type string
                      :documentation
                      "Project root.")
       (load-hook     :initarg :load-hook
                      :type list
                      :documentation
                      "hook called at project load-time.")
       (teardown-hook :initarg :teardown-hook
                      :type list
                      :documentation
                      "hook called at project teardown.")
       (tasks     :initarg :tasks
                  :type list
                  :documentation
                  "Jobs that can be launched to do
                   work on a cyanide-project.")))

    (cl-defmethod cyanide-load-project ((proj cyanide-project))
      "Load a cyanide-project"
      (let ((load-hook (oref proj load-hook))
            (default-view (cyanide-get-one-by-slot (oref proj default-view)
                                                   cyanide-view-collection
                                                   ":id"
                                                   'eq))
            (sym (oref proj :id)))
        (if cyanide-current-view (cyanide-disable-current-view))
        (when load-hook
          (cyanide-hook-executor load-hook))
        (setq cyanide-current-project sym)
        (funcall (oref default-view enable))
        nil))

    (defun cyanide-prompt (prompt-func
                           prompt-str
                           prompt-names
                           collection
                           stringified-slot
                           equality-func
                           &optional
                           predicate
                           require-match
                           initial-input
                           hist
                           def
                           inherit-input-method)
      "Execute PROMPT-FUNC on obj from
       `cyanide-get-one-by-slot' while prompting the user
       with PROMPT-STR. Possible completions will come from
       PROMPT-NAMES. For documentation on PREDICATE,
       REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
       INHERIT-INPUT-METHOD args see `completing-read'."
      (progn
        (funcall prompt-func (cyanide-get-one-by-slot (completing-read
                                                       prompt-str
                                                       prompt-names
                                                       predicate
                                                       require-match
                                                       initial-input
                                                       hist
                                                       def
                                                       inherit-input-method)
                                                       collection
                                                       stringified-slot
                                                       equality-func))
        nil))

    (defun cyanide-load-project-prompt ()
      "Prompt the user for a project to load, take user input,
       and then load it."
      (interactive
       (let ((project-names (mapcar
                             (lambda (x)
                               (oref x :display-name))
                             cyanide-project-collection)))
         (cyanide-prompt 'cyanide-load-project
                         "Load project: "
                         project-names
                         cyanide-project-collection
                         ":display-name"
                         'equal
                         nil
                         1))))

    (defun cyanide-windows-dedicated (bool &optional minibuf all-frames)
      "Toggle window dedication for all windows
       in the current frame.

       For more information on minibuf and all-frames args,
       see `walk-windows'."
      (let ((f (lambda (x)
                 (set-window-dedicated-p x bool))))
        (walk-windows f minibuf all-frames)))

    (defun cyanide-windows-locked (lock-arg &optional minibuf all-frames)
      "Set window locking for all windows in the current frame.

       If lock-arg is nil, unlock locked buffers.
       If lock-arg is non-nil, lock unlocked buffers and pass
       lock-arg into emacs-lock-mode to indicate the type of
       lock.

       For more information on emacs-lock-mode types, refer
       to documentation for `emacs-lock-mode'.

       For more information on minibuf and all-frames args,
       see `walk-windows'."
      (let ((f (lambda (x)
                 (progn
                   (select-window x)
                   (when (boundp 'emacs-lock-mode)
                     (if (not (eq emacs-lock-mode lock-arg))
                         (if (not lock-arg)
                             (call-interactively 'emacs-lock-mode)
                           (emacs-lock-mode lock-arg)))) ; else
                   nil))))                               ; else
        (walk-windows f minibuf all-frames)))

    (defun cyanide-disable-current-view ()
      "Disable current cyanide-view."
      (interactive
       (progn
         (when (not cyanide-current-view)
           (error (concat "Cannot disable cyanide-current-view "
                          "if cyanide-current-view is nil")))
         (cyanide-call-disable
          (cyanide-get-one-by-slot cyanide-current-view
                                   cyanide-view-collection
                                   ":id"
                                   'eq)))))

    (defun cyanide-default-disabler ()
      (progn
        (cyanide-windows-dedicated nil)
        (cyanide-windows-locked nil)
        (delete-other-windows)
        (setq cyanide-current-view nil)
        ;; Revert window settings back to default.
        (if split-height-threshold-orig
            (setq split-height-threshold split-height-threshold-orig))
        (if split-width-threshold
            (setq split-width-threshold split-width-threshold-orig))
        (if ag-reuse-window-orig
            (setq ag-reuse-window ag-reuse-window-orig))
        (if ag-reuse-buffers-orig
            (setq ag-reuse-buffers ag-reuse-buffers-orig))
        (cyanide-delete-menu-object 'tasks)
        nil))

    (defclass cyanide-view ()
      ((id           :initarg :id
                     :initform nil
                     :type symbol)
       (display-name :initarg :display-name
                     :initform ""
                     :type string
                     :custom string
                     :documentation "Display name for a cyanide-view.")
       ;; UI setup
       (enable       :initarg :enable
                     :type function
                     :documentation "Enable this cyanide-view.")
       ;; Teardown
       (disable      :initarg :disable
                     :type symbol
                     :documentation "Disable this cyanide-view."))
      "Definition of a cyanide-view configuration.")

    (require 'cyanide-views)

    ;; Get quoted function from cyanide-view and execute.
    (cl-defmethod cyanide-call-enable ((view cyanide-view))
      "Enable a cyanide-view."
      (funcall (oref view enable)))

    (cl-defmethod cyanide-call-disable ((view cyanide-view))
      "Disable a cyanide-view."
      (funcall (oref view disable)))

    ;; This won't work if there are two buffers with the same name open.
    ;; Need to include window and maybe frame to prevent this.
    (defun seek-window-by-buffer-name (name)
      (let ((starting-buffer-name
             (buffer-name))
            (thunk
             (lambda (i)
               (if (not (equal name (buffer-name)))
                   (if (not (> i (length (window-list))))
                       (progn (other-window 1) (funcall thunk (+ i 1)))
                     (seek-window-by-buffer-name starting-buffer-name))
                 nil))))
        (funcall thunk 0)))

    (defun cyanide-enable-view-prompt ()
      "Prompt user to enable a cyanide-view, and then enable it."
      (interactive
       (let ((view-names (cyanide-list-display-names cyanide-view-collection)))
         (cyanide-prompt 'cyanide-call-enable
                         "Enable view: "
                         view-names
                         cyanide-view-collection
                         ":display-name"
                         'equal
                         nil
                         1))))

    (defun cyanide-multi-occur-all-buffers (str)
      "Generic search for arbitrary string across all buffers."
      (interactive "MOccur String: ")
      (multi-occur-in-matching-buffers ".*" str))

    (defun cyanide-select-buffer-window-worker (sought-buffer
                                                &optional all-frames)
      (let ((sought-buffer-window (get-buffer-window sought-buffer all-frames)))
        (let ((sought-buffer-frame (window-frame sought-buffer-window)))
          (progn
            (if sought-buffer-window
                (progn
                  (select-frame-set-input-focus sought-buffer-frame)
                  (select-window sought-buffer-window))
              nil) ;; else
            (switch-to-buffer sought-buffer)))))

    (defun cyanide-select-buffer-window (sought-buffer
                                         &optional all-frames)
      "Place cursor in window.

            - if window is visible, switch to it.

            - If window is not visible, switch to buffer.

            - If buffer does not exist, create it.

            - If all-frames is t, consider all frames. See
              `get-buffer-window' for details regarding the specific
              behavior of that arg. In this case, if a buffer is open
              in multiple frames, cyanide-select-buffer-window will
              prefer to select the window of the buffer in the current
              frame."
      (cyanide-select-buffer-window-worker sought-buffer all-frames))

    ;; Source: https://github.com/Wilfred/ag.el/blob/master/ag.el
    ;; Tiny alteration on ag/search from Wilfred's ag.el to make it
    ;; cyanide-project-aware.
    (cl-defun cyanide-ag-search (string
                                 &key
                                 (regexp nil)
                                 (file-regex nil)
                                 (file-type nil))
      "Run ag searching for the STRING given in DIRECTORY.
       If REGEXP is non-nil, treat STRING as a regular expression."
      (let ((arguments ag-arguments)
            (shell-command-switch "-c"))
        (unless regexp
          (setq arguments (cons "--literal" arguments)))
        (if ag-highlight-search
            (setq arguments
                  (append '("--color" "--color-match" "30;43") arguments))
          (setq arguments
                (append '("--nocolor") arguments)))
        (when (char-or-string-p file-regex)
          (setq arguments
                (append `("--file-search-regex" ,file-regex) arguments)))
        (when file-type
          (setq arguments (cons (format "--%s" file-type) arguments)))
        (when ag-ignore-list
          (setq arguments (append (ag/format-ignore ag-ignore-list) arguments)))
        (unless (file-exists-p default-directory)
          (error "No such directory %s" default-directory))
        (let ((command-string
               (mapconcat #'shell-quote-argument
                          (append
                           (list ag-executable)
                           arguments
                           (list string cyanide-current-project))
                          " ")))
          ;; If we're called with a prefix, let the user modify
          ;; the command before
          ;; running it. Typically this means they want to pass
          ;; additional arguments.
          (when current-prefix-arg
            ;; Make a space in the command-string for the user
            ;; to enter more arguments.
            (setq command-string
                  (ag/replace-first command-string " -- " "  -- "))
            ;; Prompt for the command.
            (let ((adjusted-point
                   (- (length command-string) (length string) 5)))
              (setq command-string
                    (read-from-minibuffer "ag command: "
                                          (cons
                                           command-string adjusted-point)))))
          ;; Call ag.
          (compilation-start
           command-string
           #'ag-mode
           `(lambda (mode-name) ,(ag/buffer-name string "merp" regexp))))))

    (defun cyanide-tokenize-window-treenode (node)
      (if (windowp node)
          'window
        (if (booleanp node)
            'split-direction
          (let ((x (car node)))
            (let ((type (type-of x)))
              (if (equal 'window type)
                  'tree                    ; tree without properties
                (if (booleanp x)           ; else if
                    'tree
                  (if (integerp  x)
                      'edges
                    (if (booleanp (car x))
                        'tree
                      (error               ; else error- guard input
                       (concat "Invalid input to "
                               "cyanide-tokenize-window-treenode")))))))))))

    (defun cyanide-ag-search (string)
      (interactive (list (ag/read-from-minibuffer "Search string")))
      (if cyanide-current-project
          (let ((directory
                 (oref
                  (cyanide-get-one-by-slot cyanide-current-project
                                           cyanide-project-collection
                                           ":id"
                                           'eq)
                  project-root)))
            (ag/search string directory))
        (error (concat "cyanide-current-project is nil. " ; else
                       "Cannot invoke cyanide-ag-search "
                       "before loading a cyanide-project."))))

    (defun cyanide-case-sensitive (re)
      "Respect emacs defaults and determine whether cyanide
       should attempt to match with case-sensitivity.

       For more information, see `cyanide-case-sensitive-test'"
      (or (let ((case-fold-search nil))
            (string-match "[$.*[:upper:].*^]" re))
          (not case-fold-search)))

    (defun cyanide-find-dired (string)
      (interactive "Mstring: ")
      (if cyanide-current-project
          (let ((directory
                 (oref
                  (cyanide-get-one-by-slot cyanide-current-project
                                           cyanide-project-collection
                                           ":id"
                                           'eq)
                  project-root))
                (find-case-sensitive-arg
                 (if (cyanide-case-sensitive string) "" "i")))
            (find-dired directory (concat
                                   " -"
                                   find-case-sensitive-arg
                                   "name "
                                   "'*"
                                   string
                                   "*' "
                                   cyanide-find-dired-exclude-vc
                                   "-type f ")))
        (error (concat "cyanide-current-project is nil. " ; else
                       "Cannot invoke cyanide-find-dired "
                       "before loading a cyanide-project."))))

    (defclass cyanide-treenode ()
      ((id         :initarg :id
                   :initform nil
                   :type symbol
                   :documentation
                   "Use id so that we don't have to rely solely upon object
                    hashing or lower-level constructs like window id.")
       (position   :initarg :position
                   :initform -2
                   :type integer
                   :documentation
                  "Position of this cyanide-treenode inside of its
                   super-tree. -2 indicates that this value was not
                   initialized. -1 indicates that this treenode is a root and
                   therefore has no position because it has no super-tree.")
       (super-tree :initarg :super-tree
                   :initform []
                   :type vector
                   :documentation
                   "Object that refers to the super-tree of this treenode.")
       (frame      :initarg :frame
                   :initform []
                   :type vector
                   :documentation
                   "Object to encapsulate emacs frames.")
       (edges      :initarg :edges
                   :initform []
                   :type vector
                   :documentation
                   "Object to encapsulate window edge dimensions.
                    see `window-edges' for more information.")))

    (cl-defmethod cyanide-set-super-tree ((node cyanide-treenode)
                                          super-tree)
      (oset node :super-tree super-tree))

    (cl-defmethod cyanide-get-super-tree ((node cyanide-treenode))
      (oref node :super-tree))

    (cl-defmethod cyanide-set-position ((node cyanide-treenode)
                                        position)
      (oset node :position position))

    (cl-defmethod cyanide-set-frame ((node cyanide-treenode)
                                     frame)
      (oset node :frame frame))

    ;; if no super-tree, node is a root.
    (defun cyanide-parse-treenodes (nodes pos &optional super-tree)
      "Convert entire emacs `window-tree' into
       object-oriented constructs for CyanIDE."
      (when nodes  ; else break out
        (let ((node (car nodes))
              (nodes (cdr nodes))
              (pos (+ 1 pos)))
          (cyanide-parse-treenode node pos super-tree)
          (cyanide-parse-treenodes nodes pos super-tree))))

    (defun cyanide-parse-treenode (node pos &optional super-tree)
      "Convert individual nodes of emacs `window-tree' into
       object-oriented constructs for CyanIDE."
      ;; TO DO: need to pass frame into these constructors also.
      (let ((token (cyanide-tokenize-window-treenode node)))
        (if (eq 'split-direction token)
            (cyanide-parse-split-direction node pos super-tree) ; TO DO
          (if (eq 'edges token)
              (cyanide-parse-edges node pos super-tree)         ; TO DO
            (if (eq 'window token)
                (cyanide-parse-window node pos super-tree)      ; TO DO
              (if (eq 'tree token)
                  (cyanide-parse-tree node pos super-tree)   ; TO DO mostly done
                (error "cyanide-parse-treenode: unrecognized token")))))))

    (defun cyanide-parse-tree (tree pos frame super-tree)
      (cyanide-tree-builder tree pos frame super-tree))

    (defun cyanide-parse-window (tree pos super-tree)
      (cyanide-window-builder window pos super-tree))

    (defclass cyanide-window (cyanide-treenode)
      ((window-number :initarg :window-number
                      :initform 0
                      :type integer)
       (window   :initarg :window
                 :type window
                 :documentation "")
       (buffer   :initarg :buffer
                 :type buffer
                 :documentation ""))
      "CyanIDE class to encapsulate emacs windows.")

    (defun cyanide-window-builder (window pos super-tree)
      "Constructor for `cyanide-window'."
      (let ((position pos)
            (id (cl-gensym))
            (window-number (cyanide-window-number window))
            (buffer (window-buffer))
            (edge-left (car (window-edges window)))
            (edge-top (cadr (window-edges window)))
            (edge-right (car (cddr (window-edges window))))
            (edge-bottom (cadr (cddr (window-edges window)))))
        (let ((edges (cyanide-edge-builder `(,edge-left
                                             ,edge-top
                                             ,edge-right
                                             ,edge-bottom))))
          (let ((frame (cyanide-frame-builder (window-frame window))))
            (let ((win (cyanide-window
                        :window window
                        :id id
                        :position pos
                        :window-number window-number
                        :buffer buffer
                        :frame frame
                        :edges edges)))
              (add-to-list cyanide-treenodes win)
              (cyanide-add-sub-treenode super-tree win)
              (cyanide-set-super-tree win super-tree))))))

    (defclass cyanide-tree (cyanide-treenode)
      ((sub-treenodes :initarg :sub-treenodes
                      :initform '()
                      :type list
                      :documentation
                      "Collection containing sub-treenodes of this node.")
       (split-direction :initarg :split-direction
                        :type boolean))
      "CyanIDE class to encapsulate emacs window-tree.")

    (cl-defmethod cyanide-add-sub-treenode ((super-tree cyanide-tree)
                                            sub-treenode)
      (object-add-to-list super-tree :sub-treenodes sub-treenode))

    (cl-defmethod cyanide-remove-sub-treenode ((super-tree cyanide-tree)
                                               sub-treenode)
      (object-remove-from-list super-tree :sub-treenodes sub-treenode))

    (cl-defmethod cyanide-set-sub-treenodes ((tree cyanide-tree)
                                             (sub-treenodes list))
      (oset tree :sub-treenodes sub-treenodes))

    ;; TO DO    (cl-defmethod cyanide-get-sub-treenodes ((tree cyanide-tree))
    ;; TO DO      (cyanide-get-sub-treenodes tree)
    ;;          should return list instead of object.
    (cl-defmethod cyanide-get-sub-treenodes ((tree cyanide-tree))
      (oref tree :sub-treenodes))

    ;; TO DO:
    ;;       construct tree-obj without sub-treenodes
    ;;       handle root nodes
    (defun cyanide-tree-builder (tree pos frame-obj &optional super-tree)
      "Constructor for `cyanide-tree'. "
      (let ((id (cl-gensym)))
        (let ((tree-obj (cyanide-tree :id id)))
          (cyanide-set-frame tree-obj frame-obj)
          (cyanide-set-position tree-obj pos)
          ;; TO DO
          ;;          (cyanide-parse-treenodes tree pos tree-obj)
          tree-obj))) ; return tree-obj

    (defun cyanide-window-number (&optional win)
      "Return window number of an emacs window type."
      (let ((window-configuration-change-hook nil)
            (original-win (selected-window))
            (f (lambda (wn)
                 (string-to-int
                  (car
                   (split-string
                    (cadr
                     (split-string
                      (format "%s" wn) "window "))
                    " on "))))))
        (if win (funcall f win)                     ; if optional arg
          (funcall f (selected-window)))))          ; else use selected window

    (defun cyanide-window-list ()
      "Return an alist of (window-number . window) for each
       window in the current frame."
      (mapcar
       (lambda (w)
         `(,(cyanide-window-number w)
           ,(selected-window)))
       (window-list)))

    (defclass cyanide-edges ()
      ((id     :initarg :id
               :initform nil
               :type symbol)
       (left   :initarg :left
               :initform -1
               :type integer)
       (top    :initarg :top
               :initform -1
               :type integer)
       (right  :initarg :right
               :initform -1
               :type integer)
       (bottom :initarg :bottom
               :initform -1
               :type integer))
      "CyanIDE class to encapsulate window-edges.")

    (defun cyanide-edge-builder (edge-list)
      "Constructor for `cyanide-edges'."
      (let ((id (cl-gensym)))
        (let ((edge-obj (cyanide-edges :id id)))
          (cyanide-set-edges edge-obj edge-list)
          edge-obj))) ; return edge-obj

    (cl-defmethod cyanide-set-edge ((edges cyanide-edges)
                                    edge-name
                                    value)
      "Set the value of a `cyanide-window' edge by name.
       Valid names are :left :top :right or :bottom."
      (progn
        (when (not (memq edge-name '(:left :top :right :bottom)))
          (error (concat "Invalid edge name: " (format "%s" edge-name))))
        (when (not (eq value (abs value)))
          (error "Window dimensions cannot be negative"))
        (eval `(oset edges ,edge-name value))))

    (cl-defmethod cyanide-get-edge ((edges cyanide-edges)
                                    edge-name)
      "Get the value of a `cyanide-window' edge by name.
       Valid names are :left :top :right or :bottom."
      (progn
        (when (not (memq edge-name '(:left :top :right :bottom)))
          (error (concat "Invalid edge name: " (format "%s" edge-name))))
        (eval `(oref edges ,edge-name))))

    (cl-defmethod cyanide-set-edges ((edges cyanide-edges) new-edge-list)
      "Set multiple `cyanide-window' edge values at the same
       time. This can be used to easily parse
       `window-edges'."
      (progn
        (cyanide-set-edge edges :left (car new-edge-list))
        (cyanide-set-edge edges :top (cadr new-edge-list))
        (cyanide-set-edge edges :right (car (cddr new-edge-list)))
        (cyanide-set-edge edges :bottom (car (last new-edge-list)))))

    (cl-defmethod cyanide-get-edges ((edges cyanide-edges))
      "Get the value of all `cyanide-window' edges in a
       structure identical to `window-edges'."
      `(,(cyanide-get-edge edges :left)
        ,(cyanide-get-edge edges :top)
        ,(cyanide-get-edge edges :right)
        ,(cyanide-get-edge edges :bottom)))

    (defclass cyanide-frame ()
      ((id    :initarg :id
              :initform nil
              :type symbol)
       (frame :initarg :frame
              :type frame))
      "CyanIDE class to encapsulate emacs frames.")

    (defun cyanide-frame-builder (frame)
      "Constructor for `cyanide-frame'."
      (let ((id (cl-gensym)))
        (cyanide-frame :id id :frame frame)))

    (cl-defmethod cyanide-get-frame ((frame cyanide-frame))
      (oref frame :frame))

    (cl-defmethod cyanide-set-frame ((frame cyanide-frame) value)
      (oset frame :frame value))

    (defun cyanide-get-one-by-slot (sym
                                    lst
                                    stringified-slot
                                    equality-func)
      "Return one obj from LST where SYM matches with
       EQUALITY-FUNC the value stored in STRINGIFIED-SLOT.
       Optimized lookup: return the first relevant result
        from the list and stop looking."
      (let ((obj nil)
            (i nil)
            (l lst)
            (slot (intern stringified-slot)))
        (while (and (eq nil obj)
                     l)
          (setq i (pop l))
          (when (funcall equality-func (eval `(oref i ,slot)) sym)
            (setq obj i)))
        obj))

    (defun cyanide-filter (lst)
      "Return LST with nil values removed."
      (delq nil lst))

    (defun cyanide-hook-executor (hooks)
      "Execute hook functions in HOOKS with some extra
       logging."
      (let ((f (lambda (func)
                 (progn
                   (message (concat "cyanide-hook-executor calling"
                                    " "
                                    (format "%s" func)))
                   (funcall func)))))
        (mapcar f hooks)))

    (defun cyanide-list-display-names (lst)
      "Return a list of :display-name slots from an
       arbitrary LST of objects."
      (mapcar
       (lambda (x)
         (oref x :display-name))
       lst))

    (defun cyanide-return-if-true (test sym1 sym2 retval)
      "Apply TEST to sym1 and sym2 and return RETVAL if TEST
       returns true. Else return nil."
      (when (funcall test sym1 sym2)
        retval))

    (defun cyanide-get-many-by-slot (sym lst stringified-slot equality-func)
      "Return all objects from LST where SYM matches with
       EQUALITY-FUNC the value stored in STRINGIFIED-SLOT."
      (let ((res '())
            (l lst)
            (slot (intern stringified-slot)))
        (cyanide-filter (mapcar (lambda (x)
                                  (cyanide-return-if-true equality-func
                                                          sym
                                                          (eval `(oref x ,slot))
                                                          x)) l))))

    ;; Not faster than a hashtable, but still pretty fast... and might prove
    ;; faster in best-case. copy-tree is required below because otherwise delq
    ;; will destructively operate on members as a referent, rather than as a
    ;; value.
    (cl-defmethod cyanide-get-menu-members ((menu cyanide-menu))
      "Return `cyanide-menu-item' objects from
       `cyanide-menu' MENU."
      (let ((members (copy-tree (oref menu :members)))
            (lst cyanide-menu-item-collection)
            (itm nil)
            (retval '()))
        (while (and members lst)
          (setq itm (pop lst))
          (when (memq (oref itm :id) members)
            (progn (push itm retval)
                   (delq (oref itm :id) members))))
        retval))

    (defun cyanide-delete-menu-object (menu-id)
      "Delete a menu from `cyanide-menu-item-collection'."
      (let ((old-menu (cyanide-get-one-by-slot menu-id
                                               cyanide-menu-item-collection
                                               ":id"
                                               'eq)))
        (when old-menu
            (setq cyanide-menu-item-collection
                  (delq old-menu cyanide-menu-item-collection)))))

    (defun cyanide-tasks-menu-builder (project-id)
      "Build tasks menu object for a `cyanide-project'."
      (let ((project (cyanide-get-one-by-slot project-id
                                              cyanide-project-collection
                                              ":id"
                                              'eq)))
        (when (not project) (error (concat "cyanide-tasks-menu-builder"
                                           " "
                                           "could not find project"
                                           " "
                                           (format "%s" project-id))))
        (let ((members (oref project
                             :tasks)))
          (cyanide-delete-menu-object 'tasks)
          (let ((menu (cyanide-menu-builder '(:id 'tasks
                                              :display-name "Tasks"
                                              :members members))))
            menu))))

    (defun cyanide-render-menu-with-tasks (project-id
                                           menu-id)
      "Dynamically generate tasks sub-menu for a
       `cyanide-project' and render it in context with its
       super-menu."
      (let ((menu (cyanide-get-one-by-slot menu-id
                                           cyanide-menu-item-collection
                                           ":id"
                                           'eq)))
        (cyanide-tasks-menu-builder project-id)
        (cyanide-menu-render menu
                             menu-id
                             cyanide-mode-map)))

    (defun cyanide-class-implements (class obj)
      "Test whether an object implements a class."
      (or (memq (eieio-object-class (car cyanide-menu-item-collection))
                (eieio-class-children 'cyanide-menu))
          (eq 'cyanide-menu
              (eieio-object-class (car cyanide-menu-item-collection)))))

    (defun cyanide-unroll-all-menu-functions (menu-id)
      "Recursively unroll all menu functions into a list."
      (let ((menu (cyanide-get-one-by-slot menu-id
                                           cyanide-menu-item-collection
                                           ":id"
                                           'eq))
            (lst '())
            (g (lambda (y) (mapcar f (cyanide-get-menu-members y))))
            (f (lambda (x) (if (cyanide-class-implements 'cyanide-menu-function
                                                         x)
                               (push x lst)
                             (if (cyanide-class-implements 'cyanide-menu
                                                           x)
                                 (funcall g x)
                               (error (concat "cyanide-unroll-all-menu-items "
                                              "cannot parse "
                                              (format "%s" x)))))))) ; else
        (if menu
            (funcall g menu)
          (error (concat "cyanide-unroll-all-menu-functions "
                         "no such menu: "
                         (format "%s" menu-id))))
        lst)) ; return lst

    ;; It is not enough to check whether cyanide-mode is initialized. At certain
    ;; points in the stack, for instance, right when starting a
    ;; global-minor-mode at init time, before the user actually does anything,
    ;; the mode will still be set to nil, even after cyanide-mode has already
    ;; explicitly been enabled. When the user first interacts with the UI, at
    ;; that point the mode switches to t. This appears to be an issue with emacs
    ;; global minor modes and I am opening a bug report. In the meantime we need
    ;; a var as a guard here that does not suffer from the same flakiness.
    (defvar cyanide-menu-initialized nil
      "This is an internal variable used by CyanIDE and
       should not be used by anything except CyanIDE. When
       `cyanide-menu-initialized' is nil, CyanIDE will
       attempt to render the CyanIDE menu, at which point
       `cyanide-menu-initialized' will be set to t to
       prevent unnecessary GUI re-rendering.")

    (when (not cyanide-menu-initialized)
      (progn
        (cyanide-menu-render (cyanide-get-one-by-slot
                              'cyanide-default-menu
                              cyanide-menu-item-collection
                              ":id"
                              'eq)
                             'cyanide-default-menu
                             cyanide-mode-map)
        (setq cyanide-menu-initialized t)))) :global t)

(define-globalized-minor-mode global-cyanide-mode cyanide-mode
  (lambda () (cyanide-mode 1)))
(global-cyanide-mode 1)

(provide 'cyanide)
