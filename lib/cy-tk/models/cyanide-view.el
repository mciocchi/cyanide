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

(require 'cyanide-globals)
(require 'cyanide-get-one-by-slot)

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
                 :type function
                 :documentation "Disable this cyanide-view."))
  "Definition of a cyanide-view configuration.")

;; Get quoted function from cyanide-view and execute.
(cl-defmethod cyanide-call-enable ((view cyanide-view))
  "Enable a cyanide-view."
  (funcall (oref view enable)))

(cl-defmethod cyanide-call-disable ((view cyanide-view))
  "Disable a cyanide-view."
  (funcall (oref view disable)))

(defun cyanide-enable-view-prompt ()
  "Prompt user to enable a cyanide-view, and then enable it."
  (interactive
   (let ((view-names (cyanide-list-display-names cyanide-view-collection)))
     (cyanide-prompt 'cyanide-call-enable
                     "Enable view (tab for completion): "
                     view-names
                     cyanide-view-collection
                     ":display-name"
                     'equal
                     nil
                     1))))

(defun cyanide-view-builder (kwargs)
  "Constructor for `cyanide-view'"
  (cyanide-kwargobj-builder 'cyanide-view
                            kwargs
                            '(:id
                              :display-name
                              :enable
                              :disable)
                            'cyanide-view-collection))

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

(provide 'cyanide-view)
