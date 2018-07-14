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


(require 'eieio)
(require 'eieio-base)
(require 'cyanide-project)

(defun cyanide-main ()
  (progn

    (require 'cyanide-globals "lib/controller/cyanide-globals")
    (require 'cyanide-project "lib/models/cyanide-project")
    (require 'cyanide-view "lib/models/cyanide-view")
    (require 'cyanide-menu "lib/models/cyanide-menu")
    (require 'cyanide-menu-function "lib/models/cyanide-menu-function")
    (require 'cyanide-task "lib/models/cyanide-task")
    (require 'cyanide-default-menu-with-tasks "lib/views/cyanide-default-menu-with-tasks")
    (require 'cyanide-elisp-view "lib/views/cyanide-elisp-view")
    (require 'cyanide-minimal-view "lib/views/cyanide-minimal-view")
    (require 'cyanide-helm-wrapper "lib/controller/cyanide-helm-wrapper")
    (require 'cyanide-misc-utils "lib/controller/cyanide-misc-utils")
    (require 'cyanide-menu-utils "lib/views/cyanide-menu-utils")
    (require 'cyanide-loader "lib/controller/cyanide-loader")

    (cyanide-menu :id 'cyanide-default-menu
                  :display-name "CyanIDE"
                  :members '(load-project
                             keyword-search-project
                             find-in-project
                             enable-view
                             disable-current-view))

    (cyanide-menu-function :id 'load-project
                           :display-name "Load a Project"
                           :func 'cyanide-load-project-prompt)

    (cyanide-menu-function :id 'keyword-search-project
                           :display-name "Search string in Project"
                           :func cyanide-keyword-search-function)

    (cyanide-menu-function :id 'find-in-project
                           :display-name "Find in Project"
                           :func cyanide-find-file-function)

    (cyanide-menu-function :id 'enable-view
                           :display-name "Enable a View"
                           :func 'cyanide-enable-view-prompt)

    (cyanide-menu-function :id 'disable-current-view
                           :display-name "Disable Current View"
                           :func 'cyanide-disable-current-view)

    (defvar cyanide-initialized nil
      "This is an internal variable used by CyanIDE and should not be used by
      anything except CyanIDE. When `cyanide-initialized' is nil, CyanIDE will
      attempt to render the CyanIDE menu, at which point `cyanide-initialized'
      will be set to t to prevent unnecessary GUI re-rendering.")

    (when (not cyanide-initialized)
      (progn
        (cyanide-menu-render (cyanide-get-one-by-slot
                              'cyanide-default-menu
                              cyanide-menu-item-collection
                              ":id"
                              'eq)
                             'cyanide-default-menu
                             cyanide-mode-map)
        (cyanide-load-project-dotfiles)
        (setq cyanide-initialized t)))))

(provide 'cyanide-main)
