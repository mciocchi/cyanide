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
      (define-key map (kbd "C-c c m") 'cyanide-multi-occur-all-buffers)) map))

(easy-menu-define cyanide-menu cyanide-mode-map "CyanIDE"
  '("CyanIDE"
    ["Load Project"
     cyanide-load-project-prompt t]
    ["Search all buffers"
     cyanide-multi-occur-all-buffers t]
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
    
    ;; The find-lisp package is distributed with emacs, but needs to be included
    ;; explicitly like this to make its functions available in userland.
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
            (default-view (gethash (oref proj default-view) cyanide-views))
            (display-name (oref proj display-name)))

        (progn
          (if cyanide-current-view (cyanide-disable-current-view))
          (if load-hook (funcall load-hook))
          (setq cyanide-current-project display-name)
          (cyanide-find-file-project-tree proj-tree)
          (funcall (oref default-view enable))
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
                   (if (not (eq emacs-lock-mode lock-arg))
                                        (if (not lock-arg)
                                            (call-interactively 'emacs-lock-mode)
                                          (emacs-lock-mode lock-arg))) ;; else
                       nil)))) ;; else
        (walk-windows f minibuf all-frames)))

    (defun cyanide-disable-current-view ()
      "Disable current cyanide-view"
      (interactive
       (cyanide-call-disable
         (gethash cyanide-current-view cyanide-views))))

    (defun cyanide-default-disabler ()
      (progn
        (cyanide-panel-disable)
        (cyanide-windows-dedicated nil)
        (cyanide-windows-locked nil)
        (delete-other-windows)
        (setq cyanide-current-view nil)
        ;; Revert window settings back to default.
        (if split-height-threshold-orig
            (setq split-height-threshold split-height-threshold-orig))
        (if split-width-threshold
            (setq split-width-threshold split-width-threshold-orig))
        nil))

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
             (let ((views '())
                   (names '()))
               (progn
                 (maphash (lambda (key val)
                            (progn
                              (push `(,(oref val display-name) . ,val) views)
                              (push (oref val display-name) names)))
                          cyanide-views)
                 (cyanide-call-enable
                  (cdr (assoc (completing-read "Enable view: " names nil 1) views)))))))
          
          (defun cyanide-multi-occur-all-buffers (str)
            "Generic search for arbitrary string across all buffers."
            (interactive "MOccur String: ")
            (multi-occur-in-matching-buffers ".*" str))
          (defun cyanide-select-buffer-window-worker (sought-buffer &optional all-frames)
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

;; TO DO:
;; - Implement cyanide-window-excursion to seek via window instead for UI
;;   adjustments, because duplicate buffers are fine during editing, but
;;   not good for UI adjustments when seeking via buffer-name.
;; - Address copy and paste code problem between cyanide-buffer-excursion and
;;   cyanide-select-buffer-window.
          (defun cyanide-buffer-excursion (func
                                           sought-buffer
                                           &optional all-frames)
            "Place cursor in buffer-window, eval code, go
             back to starting location, return output of
             funcall func.

            - If window is visible, switch to it.

            - If window is not visible, switch to buffer.

            - If buffer does not exist, create it.

            - If all-frames is t, consider all frames. See
              `get-buffer-window' for details regarding the specific
              behavior of that arg. In this case, if a buffer is open
              in multiple frames, cyanide-buffer-excursion will
              prefer to select the window of the buffer in the current
              frame."
            (let ((starting-buffer (current-buffer))
                  (starting-window (selected-window))
                  (starting-frame  (selected-frame)))
              (progn
                (print
                 (concat
                  "Beginning cyanide buffer excursion. Sought-buffer: "
                  sought-buffer))
                (cyanide-select-buffer-window-worker sought-buffer all-frames)
                (let ((return (funcall func)))
                  (progn
                    (select-frame-set-input-focus starting-frame)
                    (select-window starting-window)
                    return))))))
  :global t)

(define-globalized-minor-mode global-cyanide-mode cyanide-mode
  (lambda () (cyanide-mode 1)))
(global-cyanide-mode 1)

(provide 'cyanide)
