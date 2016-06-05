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

(defun cyanide-main ()
  (progn

    (require 'ag)
    (require 'cyanide-globals)
    (require 'cyanide-kwarg-utils)
    (require 'cyanide-project)
    (require 'cyanide-menu)
    (require 'cyanide-menu-function)
    (require 'cyanide-task)

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

    (defun cyanide-task-prompt ()
      "Prompt user for task to execute and execute it."
      (interactive
       (let ((menu (cyanide-get-one-by-slot 'tasks
                                            cyanide-menu-item-collection
                                            ":id"
                                            'eq)))
         (if menu
             (let ((tasks-collection
                    (cyanide-unroll-all-menu-functions 'tasks)))
               (let ((task-names
                      (cyanide-list-display-names
                       tasks-collection)))
                 (cyanide-prompt (lambda (x) (call-interactively
                                              (oref x :func)))
                                 "Tasks (tab for completion): "
                                 task-names
                                 tasks-collection
                                 ":display-name"
                                 'equal
                                 nil
                                 1)))
           (message "No tasks menu defined.") ; else
           nil))))

    (require 'cyanide-views)

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
                                           'eq))
            (project (cyanide-get-one-by-slot cyanide-current-project
                                              cyanide-project-collection
                                              ":id"
                                              'eq)))
        (when (cyanide-slot-boundp project :tasks)
          (cyanide-tasks-menu-builder project-id)
          (cyanide-menu-render menu
                               menu-id
                               cyanide-mode-map))))

    (defun cyanide-unroll-all-menu-functions (menu-id)
      "Recursively unroll all menu functions into a list."
      (let ((menu (cyanide-get-one-by-slot menu-id
                                           cyanide-menu-item-collection
                                           ":id"
                                           'eq))
            (lst '())
            (g (lambda (y) (mapcar f (cyanide-get-menu-members y))))
            (f (lambda (x) (if (child-of-class-p (eieio-object-class x)
                                                 'cyanide-menu-function)
                               (push x lst)
                             (if (child-of-class-p (eieio-object-class x)
                                                   'cyanide-menu)
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

    ;; Certainly there must be a less stupid way of doing this? The interpreter
    ;; (and slot-boundp, by extension) does not allow quoted colon-prefixed
    ;; slots like ':foo. To circumvent this, we need our own slot-boundp that
    ;; forcibly dequotes the slot before evaluating the expression, but this
    ;; requires some indirection here.
    (defmacro cyanide-slot-boundp (obj slt)
      "Return t if slot SLT is bound, else return nil."
      `(funcall (lambda ()
                  (condition-case nil
                      (when (oref ,obj ,slt) t)
                    (error nil)))))

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
        (setq cyanide-menu-initialized t)))))
(provide 'cyanide-main)
