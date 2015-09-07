(provide 'cyanide)

(defvar cyanide-mode-map
  (let ((map (make-sparse-keymap)))
    (progn
      (define-key map (kbd "C-c c l") 'cyanide-load-project-prompt)
      (define-key map (kbd "C-c c d") 'cyanide-disable-current-view)
      map)))

(easy-menu-define cyanide-menu cyanide-mode-map "CyanIDE"
		      '("CyanIDE"
			["Load Project"
                         cyanide-load-project-prompt t]
                        ["Disable Current View"
                         cyanide-disable-current-view t]
                        ["Enable View"
                         nil t]))

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
    
    (require 'find-lisp)
    (require 'cyanide-views)
    (require 'cyanide-panel)

    (defvar cyanide-current-view nil
      "This var is used by cyanide to determine what view it's currently in.")

    (defvar cyanide-current-project nil
      "This var is used by cyanide to determine what project it's currently in.")

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
            (proj-tree (oref proj proj-tree))
            (load-hook (oref proj load-hook))
            (default-view (gethash (oref proj default-view) cyanide-views)))

        (progn
          (if cyanide-current-view (cyanide-disable-current-view))
          (if load-hook (funcall load-hook))
          (cyanide-find-file-project-tree proj-tree)
          (funcall (oref default-view enable))
          (setq cyanide-current-project (oref proj display-name))
          nil)))

    (defun cyanide-load-project-prompt ()
      "Prompt the user for a project to load, take user input, and then load it."
      (interactive
       (let ((projects '())
             (names '()))
         (progn
           (maphash
            (lambda (key val)
              (progn (push `(,(oref val display-name) . ,val) projects)
                     (push (oref val display-name) names)))
            cyanide-projects)
           (cyanide-load-project
            (cdr (assoc (completing-read "Load project: " names nil 1)
                        projects)))))))

    (defun cyanide-find-file-subtree (dir regex)
      "Open every file in an arbitrary subdirectory tree."
      (interactive "DDir: \nMregex: ")
      (mapc 'find-file (find-lisp-find-files dir regex)))

    (defun cyanide-frame-windows-dedicated (bool)
      "Toggle window dedication for all windows
       in the current frame."
      (let ((window-configuration-change-hook nil)
            (bookmark-after-jump-hook nil)
            (occur-mode-find-occurrence-hook nil))
        (mapcar (lambda (win)
                  (set-window-dedicated-p win bool))
                (window-list))))

    (defun cyanide-frame-windows-locked (lock-arg)
      "Set window locking for all windows
       in the current frame."
      (let ((window-configuration-change-hook nil)
            (bookmark-after-jump-hook nil)
            (occur-mode-find-occurrence-hook nil))
        (mapcar (lambda (buf)
                  (progn
                    (switch-to-buffer buf)
                    (emacs-lock-mode lock-arg))) (buffer-list))))

    (defun cyanide-disable-current-view ()
      "Disable current cyanide-view"
      (interactive
       (funcall
        (oref
         (gethash cyanide-current-view cyanide-views)
         disable))))

    (defun cyanide-default-disabler ()
      (progn
        (cyanide-panel-disable)
        (cyanide-frame-windows-dedicated nil)
        (cyanide-frame-windows-locked nil)
        (delete-other-windows)
        (setq cyanide-current-view nil)))

          (defmethod default-cyanide-load-hook (proj cyanide-project)
            "Default cyanide load-hook"
            (let ((view
                   (gethash (oref proj default-view)
                            cyanide-views)))
              (progn
                (cyanide-call-enable view)
                (cyanide-call-find-file-cmd proj))))

          (defmethod cyanide-call-find-file-cmd (proj cyanide-project)
            (let ((find-file-cmd
                   (oref proj find-file-cmd)))
              
              (funcall find-file-cmd)))

          (defclass cyanide-project ()
            ((display-name :initarg :display-name
                           :initform ""
                           :type string
                           :documentation "Display name for a cyanide-project")
             (default-view :initarg :default-view
               :type symbol
               :documentation "Default view at startup for a cyanide-project.")
             (proj-tree :initarg :proj-tree
                  :initform ()
                  :type list
                  :documentation "Project tree.")
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

          (defun seek-window-by-buffer-name (name)
            (let ((starting-buffer-name (buffer-name))
                  (thunk
                   (lambda (i) (if (not (equal name (buffer-name)))
                                   (if (not (> i (length (window-list))))
                                       (progn (other-window 1) (funcall thunk (+ i 1)))
                                     (seek-window-by-buffer-name starting-buffer-name))
                                 nil))))
              (funcall thunk 0))))
  :global t)

(define-globalized-minor-mode global-cyanide-mode cyanide-mode
  (lambda () (cyanide-mode 1)))
(global-cyanide-mode 1)
