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

    (require 'cyanide-globals)
    (require 'cyanide-kwarg-utils)
    (require 'cyanide-project)
    (require 'cyanide-view)
    (require 'cyanide-menu)
    (require 'cyanide-menu-function)
    (require 'cyanide-task)
    (require 'cyanide-default-menu-with-tasks)
    (require 'cyanide-default-view)
    (require 'cyanide-elisp-view)
    (require 'cyanide-ag-search)
    (require 'cyanide-find-dired)
    (require 'cyanide-get-many-by-slot)
    (require 'cyanide-misc-utils)
    (require 'cyanide-menu-utils)

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