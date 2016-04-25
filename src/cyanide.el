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
      (define-key map (kbd "C-c c a") 'cyanide-ag-search)
      (define-key map (kbd "C-c c f") 'cyanide-find-dired)) map))

(defun cyanide-generate-task (tsk)
  (eval `(vector ,(oref tsk :display-name)
                 ,(oref tsk :func))))

(defun cyanide-generate-task-list (tsk-list)
  (cons "Tasks" (mapcar 'cyanide-generate-task tsk-list)))

; TO DO. Prompt with completion showing executable tasks.
(defun cyanide-task-prompt ()
  ())

    (defclass cyanide-task ()
      ((display-name
        :initarg :display-name
        :type string
        :initform ""
        :custom string
        :documentation
        "Name shown in Cyanide Tasks sub-menu.")
       (func :initarg :func
             :type function
             :custom function
             :documentation
             "Function that runs elisp or an external shell
              command.")
       (sub-menu-name :initarg :sub-menu-name
                      :type string
                      :initform ""
                      :custom string
                      :documentation
                      "Optional grouping for similar tasks
                       to appear in the same sub-menu of the
                       task bar.")))

(easy-menu-define cyanide-menu cyanide-mode-map "CyanIDE"
  `("CyanIDE"
    ["Load Project"
     cyanide-load-project-prompt t]
    ["cyanide-ag-search Project"
     cyanide-ag-search t]
    ["Find in Project"
     cyanide-find-dired t]
    ["Enable View"
     cyanide-enable-view-prompt t]
    ["Disable Current View"
     cyanide-disable-current-view t]))

(define-minor-mode cyanide-mode
  "CyanIDE's Yet Another Non-IDE"  ; docstring
  nil                              ; init-value
  " cyanide "                      ; lighter
  :keymap cyanide-mode-map         ; keymap
  (progn                           ; body

    (defvar cyanide-views (make-hash-table :test 'equal)
      "this collection holds all cyanide-view objects.")
    ;; The find-lisp package is distributed with emacs, but needs to be included
    ;; explicitly like this to make its functions available in userland.
    (require 'find-lisp)
    (require 'ag)

    (defvar cyanide-projects (make-hash-table :test 'equal)
      "This collection holds all cyanide-project objects.")

    (defvar cyanide-window-local-variables (make-hash-table :test 'equal))

    ;; this was just dumb. does not offer any advantages vs. a simple list.
     ;; (defclass cyanide-treenode-collection ()
     ;;   ((treenodes :initarg :treenodes
     ;;               :initform '()
     ;;               :type list)))

     ;; (cl-defmethod cyanide-add-treenode ((nodes cyanide-treenode-collection)
     ;;                                    node)
     ;;   (object-add-to-list nodes :treenodes node))

     ;; (cl-defmethod cyanide-remove-treenode ((nodes cyanide-treenode-collection)
     ;;                                       node)
     ;;   (object-remove-from-list nodes :treenodes node))

     ;; (cl-defmethod cyanide-get-treenodes ((nodes cyanide-treenode-collection))
     ;;   (oref nodes :treenodes))

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

    (defclass cyanide-project ()
      ((display-name :initarg :display-name
                     :initform ""
                     :type string
                     :documentation "Display name for a cyanide-project")
       (default-view :initarg :default-view
         :type symbol
         :documentation "Default view at startup for a cyanide-project.")
       (proj-root :initarg :proj-root
                  :initform ""
                  :type string
                  :documentation "Project root.")
       (load-hook :initarg :load-hook
                  :type list
                  :documentation "hook called at project load-time.")
       (teardown-hook :initarg :teardown-hook
                      :type list
                      :documentation "hook called at project teardown.")
       (task-list :initarg :task-list
                  :type list
                  :documentation
                  "External jobs that can be launched to do
                   work on the environment of a
                   cyanide-project.")))

    (cl-defmethod cyanide-load-project ((proj cyanide-project))
      "Load a cyanide-project"
      ;; Override these hooks to avoid repeatedly running
      ;; occur in cyanide-panel for every new buffer.
      (let ((window-configuration-change-hook nil)
            (bookmark-after-jump-hook nil)
            (occur-mode-find-occurrence-hook nil)
            (load-hook (oref proj load-hook))
            (default-view (gethash (oref proj default-view) cyanide-views))
            (sym (cdr (assoc (oref proj display-name)
                             (cyanide-hash-by-display-name cyanide-projects)))))
        (if cyanide-current-view (cyanide-disable-current-view))
        (when load-hook
          (mapcar
           'funcall load-hook))
        (setq cyanide-current-project sym)
        (funcall (oref default-view enable))
        nil))

    (defun cyanide-load-project-prompt ()
      "Prompt the user for a project to load, take user input,
       and then load it."
      (interactive
       (let ((projects (cyanide-hash-by-display-name cyanide-projects)))
         (let ((project-names (mapcar 'car projects)))
           (cyanide-load-project (cyanide-get-by-display-name
                                  (completing-read "Load project: "
                                                   project-names nil 1)
                                  projects
                                  cyanide-projects))))))

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
                           (emacs-lock-mode lock-arg)))) ;; else
                   nil)))) ;; else
        (walk-windows f minibuf all-frames)))

    (defun cyanide-disable-current-view ()
      "Disable current cyanide-view"
      (interactive
       (cyanide-call-disable
        (gethash cyanide-current-view cyanide-views))))

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
        nil))

    (defclass cyanide-view ()
      ;; Display name for user interface, separate from impementation.
      ((display-name :initarg :display-name
                     :initform ""
                     :type string
                     :custom string
                     :documentation "Display name for a cyanide-view.")
       ;; UI setup
       (enable :initarg :enable
               :type function
               :documentation "Enable this cyanide-view.")
       ;; Teardown
       (disable :initarg :disable
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
       (let ((views (cyanide-hash-by-display-name cyanide-views)))
         (let ((names (mapcar 'car views)))
           (cyanide-call-enable
            (cyanide-get-by-display-name
             (completing-read "Enable view: " names nil 1)
             views
             cyanide-views))))))

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

    (defun cyanide-hash-by-display-name (hash)
      "Take in a hash, associate display names to keys
       for speedy lookup in UI."
      (let ((x '()))
        (maphash (lambda (key val)
                   (push `(,(oref val display-name) . ,key) x))
                 hash) x))

    (defun cyanide-get-by-display-name (display-name display-names hash)
      "Get from hash by display-name."
      (let ((sym (cdr (assoc display-name display-names))))
        (gethash sym hash)))

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
                  (gethash cyanide-current-project
                           cyanide-projects) proj-root)))
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
                  (gethash cyanide-current-project
                           cyanide-projects) proj-root))
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

    ; if no super-tree, node is a root.
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
      ; TO DO: need to pass frame into these constructors also.
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

    ; call cyanide-tree-builder
    ; build sub-treenodes            TO DO
    (defun cyanide-parse-tree (tree pos frame super-tree)
      (cyanide-tree-builder tree pos frame super-tree))

    (defun cyanide-parse-window (tree pos super-tree)
      (cyanide-window-builder window pos super-tree))

    ; CyanIDE class to encapsulate emacs windows.
    (defclass cyanide-window (cyanide-treenode)
      ((window-number :initarg :window-number
                      :initform 0
                      :type integer)
       (window   :initarg :window
                 :type window
                 :documentation "")
       (buffer   :initarg :buffer
                 :type buffer
                 :documentation "")))

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

    ; CyanIDE class to encapsulate emacs window-tree.
    (defclass cyanide-tree (cyanide-treenode)
      ((sub-treenodes :initarg :sub-treenodes
                      :initform '()
                      :type list
                      :documentation
                      "Collection containing sub-treenodes of this node.")
       (split-direction :initarg :split-direction
                        :type boolean)))

    (cl-defmethod cyanide-add-sub-treenode ((super-tree cyanide-tree)
                                            sub-treenode)
      (object-add-to-list super-tree :sub-treenodes sub-treenode))

    (cl-defmethod cyanide-remove-sub-treenode ((super-tree cyanide-tree)
                                               sub-treenode)
      (object-remove-from-list super-tree :sub-treenodes sub-treenode))

    (cl-defmethod cyanide-set-sub-treenodes ((tree cyanide-tree)
                                             (sub-treenodes list))
      (oset tree :sub-treenodes sub-treenodes))

; TO DO    (cl-defmethod cyanide-get-sub-treenodes ((tree cyanide-tree))
; TO DO      (cyanide-get-sub-treenodes tree)
;          should return list instead of object.
    (cl-defmethod cyanide-get-sub-treenodes ((tree cyanide-tree))
      (oref tree :sub-treenodes))

    ; TO DO:
    ;       construct tree-obj without sub-treenodes
    ;       handle root nodes
    (defun cyanide-tree-builder (tree pos frame-obj &optional super-tree)
      "Constructor for `cyanide-tree'. "
      (let ((id (cl-gensym)))
        (let ((tree-obj (cyanide-tree :id id)))
          (cyanide-set-frame tree-obj frame-obj)
          (cyanide-set-position tree-obj pos)
          ;; TO DO - temporary
          ;;          (cyanide-parse-treenodes tree pos tree-obj)
          tree-obj))) ; return tree-obj

    (defun cyanide-window-number (&optional win)
      "Derive window number by casting window to string, parsing
       it out, and casting to integer."
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
      (mapcar
       (lambda (w)
         `(,(cyanide-window-number w)
           ,(selected-window)))
       (window-list)))

    ; "CyanIDE class to encapsulate window-edges."
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
               :type integer)))

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

    ; "CyanIDE class to encapsulate emacs frames."
    (defclass cyanide-frame ()
      ((id    :initarg :id
              :initform nil
              :type symbol)
       (frame :initarg :frame
              :type frame)))

    (defun cyanide-frame-builder (frame)
      "Constructor for `cyanide-frame'."
      (let ((id (cl-gensym)))
        (cyanide-frame :id id :frame frame)))

    (cl-defmethod cyanide-get-frame ((frame cyanide-frame))
      (oref frame :frame))

    (cl-defmethod cyanide-set-frame ((frame cyanide-frame) value)
      (oset frame :frame value))) :global t)

(define-globalized-minor-mode global-cyanide-mode cyanide-mode
  (lambda () (cyanide-mode 1)))
(global-cyanide-mode 1)

(provide 'cyanide)
