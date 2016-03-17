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

(easy-menu-define cyanide-menu cyanide-mode-map "CyanIDE"
  '("CyanIDE"
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

    (defvar cyanide-projects (make-hash-table :test 'equal)
      "This collection holds all cyanide-project objects")

    (defvar cyanide-window-local-variables (make-hash-table :test 'equal))

    (defvar cyanide-windows (make-hash-table))

    (defvar cyanide-trees (make-hash-table))

    (defvar cyanide-global-id-pool '())

    ;; The find-lisp package is distributed with emacs, but needs to be included
    ;; explicitly like this to make its functions available in userland.
    (require 'find-lisp)
    (require 'cyanide-views)
    (require 'cyanide-buffer-excursion)
    (require 'ag)

    (defvar cyanide-current-view nil
      "This var stores a symbol used by cyanide to determine
       what view it's currently in.")

    (defvar cyanide-current-project nil
      "This var stores a symbol used by cyanide to determine
       what project it's currently in.")

    (defvar cyanide-verbose nil
      "non-nil if cyanide should use verbose logging.")

    (defun cyanide-find-file-project-tree (proj-tree)
      "Load a project directory tree using dtable dispatch table.
       If length of proj-tree branch is 1, find-file, if 2,
       find-file-subtree, else, nil."
      ;; I may be guilty of being a bit too dynamic here,
      ;; but the alternative to a dynamically-generated
      ;; dispatch table is parsing via a nest of
      ;; if-statements, which is worse.
      (let ((worker
             (lambda (branch)
               (let ((dtable
                      `((1 . (find-file ,(car branch)))
                        (2 . (cyanide-find-file-subtree
                              ,(car branch)
                              ,(car (cdr branch)))))))
                 (eval
                  (cdr
                   (assq (length branch) dtable)))))))
        (mapcar worker proj-tree)
        nil))

    (defmethod cyanide-load-project ((proj cyanide-project))
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

    (defun cyanide-find-file-subtree (dir regex)
      "Open every file in an arbitrary subdirectory tree."
      (interactive "DDir: \nMregex: ")
      (mapc 'find-file (find-lisp-find-files dir regex)))

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
                  :documentation "init-hook called at project load-time.")))

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

    ;; Get quoted function from cyanide-view and execute.
    (defmethod cyanide-call-enable ((view cyanide-view))
      "Enable a cyanide-view."
      (funcall (oref view enable)))

    (defmethod cyanide-call-disable ((view cyanide-view))
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
                                   "-type f ")))
        (error (concat "cyanide-current-project is nil. " ; else
                       "Cannot invoke cyanide-find-dired "
                       "before loading a cyanide-project."))))

    (defun cyanide-gensym (id-length)
      (let ((max-lisp-eval-depth 2400)
            (f (lambda (id)
                 (if (< (length id) id-length)
                     (progn
                       (random t)
                       (let ((rand (abs (random 127))))
                         (if (or
                              (and (< 47 rand) (> 58 rand))
                              (and (< 96 rand) (> 123 rand))
                              (and (< 47 rand) (> 58 rand)))
                             (funcall f (concat id (format "%c" rand)))
                           (funcall f id))))
                   id))))
        (funcall f "")))

    (defun cyanide-gensym ()
      "Gensym for symbols 'discovered' in the environment to be wrapped in
       objects. Can't use a user-generated unique symbol here because these are
       constructed automatically, so we need to use a gensym as a pointer to
       retreive objects."
      (let ((uuid
             (intern
              (concat (cyanide-gensym 8) "-"
                      (cyanide-gensym 4) "-"
                      (cyanide-gensym 4) "-"
                      (cyanide-gensym 4) "-"
                      (cyanide-gensym 12)))))
        (if (not (memql uuid cyanide-global-id-pool)) ; check for collisions
            (progn
              (setq cyanide-global-id-pool
                    (cons
                     uuid cyanide-global-id-pool))
              uuid)                             ; return unique id
          (cyanide-gensym))))

    (defclass cyanide-treenode ()
      ((uuid :initarg :uuid
             :initform nil
             :type symbol
             :documentation
             "Use uuid so that we don't have to rely solely upon object hashing
              or lower-level constructs like window id.")
       (position :initarg :position
                 :initform -2
                 :type integer
                 :documentation
                 "Position of this cyanide-treenode inside of its
                  super-treenode. -2 indicates that this value was not
                  initialized. -1 indicates that this treenode is a root and
                  therefore has no position because it has no super-treenode.")
       (super-treenode :initarg :super-treenode
                       :initform []
                       :type vector
                       :documentation
                       "Pass an object as a vector here to refer to the
                        super-treenode of this treenode.")
       (frame :initarg :frame
              :type frame)
       (edge-left :initarg :edge-left
                  :initform 0
                  :type integer
                  :documentation "")
       (edge-top :initarg :edge-top
                 :initform 0
                 :type integer
                 :documentation "")
       (edge-right :initarg :edge-right
                   :initform 0
                   :type integer
                   :documentation "")
       (edge-bottom :initarg :edge-bottom
                    :initform 0
                    :type integer
                    :documentation "")))

    ; TO DO - it may not be possible to construct a node without being aware
    ;         of the super-treenode of that node, unless "node" here is a
    ;         root. This is because node could always be EDGES or
    ;         SPLIT-DIRECTION.
    ;
    ;         Therefore this needs a refac. Either change node to root, and
    ;         always construct the entire treenode from root (shit is that even
    ;         possible?) OR always call cyanide-treenode-builder with
    ;         super-treenode-obj except when node is a root. Is that possible?
    ;; (defun cyanide-treenode-builder (node)
    ;;   (let ((win-tree (cyanide-tree-builder node)))
    ;;     (let ((f (lambda (sub-treenode)
    ;;                (cyanide-tokenize-window-treenode node sub-treenode))))
    ;;       (mapcar f win-tree))))
    ;; if you have the super-treenode of every window, you do not need to get
    ;; cyanide-treenodes directly, only via their constituent cyanide-windows
    ;; during split / recombine, which are only invoked publicly on windows.
    ;;
    ;; well- that's not exactly true, because split / recombine has some times
    ;; when new windows are created in a new tree. At that point, neither the
    ;; window, nor the tree are objects yet, which means we can't get by
    ;; super-treenode or sub-treenode, because they're not defined yet.
    ;;
    ;; At that point, we will have to infer which split was newly created, and
    ;; we have the following contextual information to guide us: 1) constituent
    ;; window number (one is already an object, the other is also new)
    ;; 2) position of the new tree. Position and :super-treenode
    ;; should be the same as the old :position and :super-treenode of the window
    ;; which was split.
    ;;
    ;; The other implication of this is that I'm going to have to write a
    ;; function that traverses window-tree every time a split takes place
    ;; and constructs new objects for the new split and window, and also
    ;; adjusts position and super-treenode of the window that was split after
    ;; the split. It will need to be aware of co-ordinates like this:
    ;; (3 4 1) which indicates 3rd position, 4th position, 1st position in each
    ;; split. e.g.:
    ;;
    ;; (N-SUPERNODE-POSITIONS TREENODE-POSITION)
    ;;
    ;; recombine will have a similar issue.

    (defun cyanide-treenode-builder (node super-tree)
      "- If super-tree is nil, assume node is a
         root.

       - If node is a window, build a `cyanide-window'

       - If node is a tree, build a `cyanide-tree'

       - If node is SPLIT-DIRECTION or EDGES:

         add this property to the super-tree"

      (let  (token (cyanide-tokenize-window-treenode node))
        (if (eq 'window token)
            (cyanide-window-builder node)
          (if (eq 'tree token)
              (if super-tree
                  (cyanide-tree-builder node super-tree)  ; TO DO
                (cyanide-tree-root-builder
                 node)) ; else node is a root.              TO DO
            (if (eq 'edges token)
                (set-edges super-tree node)               ; TO DO
              (if (eq 'split-direction token)             ; TO DO
                  (set-split-direction super-tree node)   ; TO DO
                (error
                 (concat "Invalid input to "
                         "cyanide-treenode-builder.")))))))) ; else error.

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

    (defun cyanide-window-builder (window)
      (let ((position (cyanide-position window))
            (uuid (cyanide-gensym))
            (window-number (cyanide-window-number window))
            (buffer (window-buffer))
            (frame (window-frame window))
            (edge-left (car (window-edges)))
            (edge-top (cadr (window-edges)))
            (edge-right (car (cddr (window-edges))))
            (edge-bottom (cadr (cddr (window-edges)))))
        (puthash window-number
                 (cyanide-window
                  window-number
                  :window window
                  :uuid uuid
                  :window-number window-number
                  :buffer buffer
                  :frame frame
                  :edge-left edge-left
                  :edge-top edge-top
                  :edge-right edge-right
                  :edge-bottom edge-bottom)
                 cyanide-windows)))

    (defclass cyanide-tree (cyanide-treenode)
      ((sub-treenodes :initarg :sub-treenodes
                      :initform '()
                      :type list)
       (split-direction :initarg :split-direction
                        :type boolean)))

    (defun cyanide-tree-builder (tree super-tree)
      (let ((uuid (cyanide-gensym))
            (frame (window-frame window))
            (edge-left (car (window-edges)))
            (edge-top (cadr (window-edges)))
            (edge-right (car (cddr (window-edges))))
            (edge-bottom (cadr (cddr (window-edges))))
            (sym (cyanide-gensym)))
        (let ((tree-obj (cyanide-tree sym)))
          (set-uuid tree-obj sym)    ; TO DO
          (set-frame tree-obj frame) ; TO DO
          (let ((f (lambda (x) (cyanide-treenode-builder x tree-obj))))
            (add-sub-treenode super-tree tree-obj)  ; TO DO
            (puthash sym
                     tree-obj
                     cyanide-treenodes) ; Is this really necessary? TO DO
            (mapcar f tree)
            tree-obj)))) ; return tree-obj

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
)

    :global t)

(define-globalized-minor-mode global-cyanide-mode cyanide-mode
  (lambda () (cyanide-mode 1)))
(global-cyanide-mode 1)

(provide 'cyanide)
