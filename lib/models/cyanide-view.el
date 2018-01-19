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

(cl-defmethod enable ((view cyanide-view))
  (let ((id (oref view :id)))
    (push id cyanide-current-views)
    (run-load-hook view)))

(cl-defmethod disable ((view cyanide-view))
  (let ((id (oref view :id)))
    (if (equal id (car cyanide-current-views))
        (progn
          (pop cyanide-current-views)
          (run-teardown-hook view))
      (error (concat "Can not tear down view- other views have been enabled "
                     "on top of it, you must disable them first!")))))

(defun cyanide-disable-current-view ()
  (interactive
   (let ((id (car cyanide-current-views)))
     (when id
       (let ((view (cyanide-get-by-id id cyanide-view-collection)))
         (if view
             (disable view)
           (error "Invalid id from cyanide-current-views!"))))
     nil)))

(defun cyanide-disable-all-views ()
  (interactive
   (while (bound-and-true-p cyanide-current-views)
     (call-interactively 'cyanide-disable-current-view))))

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

(provide 'cyanide-view)
