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

(require 'cyanide-globals "lib/controller/cyanide-globals")
(require 'cyanide-get-one-by-slot "lib/controller/cyanide-get-one-by-slot")
(require 'cyanide-nameable "lib/models/cyanide-nameable")
(require 'cyanide-hookable "lib/models/cyanide-hookable")

(defclass cyanide-view (eieio-instance-tracker
                        cyanide-identifiable
                        cyanide-nameable
                        cyanide-hookable
                        cyanide-describeable)
  ((tracking-symbol :initform cyanide-view-collection))
  "Definition of a cyanide-view configuration.")

(cl-defun enable (view-or-views)
  "enable accepts either a view object, an id corresponding to a view object, or
a list of those two types, and invokes their `:load-hook'.

Views that are loaded in this manner are added to `cyanide-current-views.'"
  (let ((enabl (lambda (view)
                 (if (symbolp view)
                     (enable (cyanide-get-by-id view cyanide-view-collection))
                   (if (eieio-object-p view)
                       (progn (push (oref view :id) cyanide-current-views)
                              (run-load-hook view))
                     (error (format "type err: %s" (type-of view))))))))
    (if (listp view-or-views)
        (mapcar enabl view-or-views)
      (funcall enabl view-or-views))))

(cl-defun disable (view-or-views)
  "disable accepts either a view object, an id corresponding to a view object, or
a list of those two types, and invokes their `:teardown-hook'.

Views that are torn down in this manner are removed from `cyanide-current-views.'"
  (let ((disabl (lambda (view)
                  (if (symbolp view)
                      (disable (cyanide-get-by-id view cyanide-view-collection))
                    (if (eieio-object-p view)
                        (if (equal (oref view :id) (car cyanide-current-views))
                            (progn (run-teardown-hook view)
                                   (pop cyanide-current-views))
                          (error (concat "Can not tear down view- other views have been enabled "
                                         "on top of it, you must disable them first!")))
                      (error (format "type err: %s" (type-of view))))))))
    (if (listp view-or-views)
        (mapcar disabl view-or-views)
      (funcall disabl view-or-views))))

(defun cyanide-disable-current-view ()
  (interactive
   (let ((id (car cyanide-current-views)))
     (when id
       (disable id))
     nil)))

(defun cyanide-disable-all-views ()
  (interactive
   (while (bound-and-true-p cyanide-current-views)
     (call-interactively 'cyanide-disable-current-view))))

(defun cyanide-refresh-all-views ()
  (interactive)
  (cyanide-refresh-all-views-1))

(defun cyanide-refresh-all-views-1 ()
  (let ((previous-views cyanide-current-views))
    (call-interactively 'cyanide-disable-all-views)
    (cyanide-enable-multiple-views (reverse previous-views))))

(defun cyanide-enable-multiple-views (new-views)
  (progn
    (while (and (not (eq nil cyanide-current-views))
                (not (eq (nil new-views))))
      (call-interactively 'cyanide-disable-current-view))
    (while (not (eq nil new-views))
      (enable (cyanide-get-by-id (car new-views) cyanide-view-collection))
      (setq new-views (cdr new-views)))))

(defun cyanide-enable-view-prompt ()
  "Prompt user to enable a cyanide-view, and then enable it."
  (interactive
   (let ((view-names (cyanide-list-display-names cyanide-view-collection)))
     (cyanide-prompt 'enable
                     "Enable view (tab for completion): "
                     view-names
                     cyanide-view-collection
                     ":display-name"
                     'equal
                     nil
                     1))))

(defun cyanide-default-disabler ()
  (progn
    (cyanide-windows-dedicated nil)
    (cyanide-windows-locked nil)
    (delete-other-windows)
    ;; Revert window settings back to default.
    (if (bound-and-true-p split-height-threshold-orig)
        (setq split-height-threshold split-height-threshold-orig))
    (if (bound-and-true-p split-width-threshold)
        (setq split-width-threshold split-width-threshold-orig))
    (cyanide-delete-menu-object 'tasks)
    nil)
)

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

(defun cyanide-windows-dedicated (bool &optional minibuf all-frames)
  "Toggle window dedication for all windows
   in the current frame.

   For more information on minibuf and all-frames args,
   see `walk-windows'."
  (let ((f (lambda (x)
             (set-window-dedicated-p x bool))))
    (walk-windows f minibuf all-frames)))

(defun cyanide-get-current-views ()
  "Get all views that are currently active."
  (mapcar (lambda (view)
            (cyanide-get-one-by-slot view
                                     cyanide-view-collection
                                     ":id"
                                     'eq))
          cyanide-current-views))

(defmacro cyanide-views-oref (key)
  "Get a property from each currently active cyanide-view."
  `(mapcar (lambda (view)
             (oref view ,key))
           (cyanide-get-current-views)))

(defun cyanide-project-and-views-frame-title ()
  "Render text for window title bar in the format:
PROJECT_DISPLAY_NAME (VIEW_DISPLAY_NAMES)"
  (format "%s %s"
          (cyanide-project-oref :display-name)
          (cyanide-views-oref :display-name)))

(defun cyanide-project-and-views-frame-title-disabling ()
  "Render text for window title bar in the format:
PROJECT_DISPLAY_NAME (VIEW_DISPLAY_NAMES)"
  (format "%s %s"
          (cyanide-project-oref :display-name)
          (cdr (cyanide-views-oref :display-name))))

(defun cyanide-most-recent-view ()
  (car (cyanide-get-current-views)))

(provide 'cyanide-view)
