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
(require 'cyanide-kwarg-utils)
;;(require 'cyanide-view)
(require 'cyanide-prompt)

(defun cyanide-project-builder (kwargs)
  "Constructor for `cyanide-project'."
  (cyanide-kwargobj-builder 'cyanide-project
                            kwargs
                            '(:id
                              :display-name
                              :default-view
                              :project-root)
                            'cyanide-project-collection))

(defclass cyanide-project ()
  ((id            :initarg :id
                  :initform nil
                  :type symbol)
   (display-name  :initarg :display-name
                  :initform ""
                  :type string
                  :documentation
                  "Display name for a cyanide-project")
   (default-view  :initarg :default-view
                  :type symbol
                  :documentation
                  "Default view at startup for a cyanide-project.")
   (project-root  :initarg :project-root
                  :initform ""
                  :type string
                  :documentation
                  "Project root.")
   (load-hook     :initarg :load-hook
                  :type list
                  :documentation
                  "hook called at project load-time.")
   (teardown-hook :initarg :teardown-hook
                  :type list
                  :documentation
                  "hook called at project teardown.")
   (tasks     :initarg :tasks
              :type list
              :documentation
              "Jobs that can be launched to do
               work on a cyanide-project.")))

(cl-defmethod cyanide-load-project ((proj cyanide-project))
  "Load a cyanide-project"
  (let ((load-hook (oref proj load-hook))
        (default-view (cyanide-get-one-by-slot (oref proj default-view)
                                               cyanide-view-collection
                                               ":id"
                                               'eq))
        (sym (oref proj :id)))
    (if cyanide-current-view (cyanide-disable-current-view))
    (when load-hook
      (cyanide-hook-executor load-hook))
    (setq cyanide-current-project sym)
    (funcall (oref default-view enable))
    nil))

(defun cyanide-load-project-prompt ()
  "Prompt the user for a project to load, take user input,
   and then load it."
  (interactive
   (let ((project-names (mapcar
                         (lambda (x)
                           (oref x :display-name))
                         cyanide-project-collection)))
     (cyanide-prompt 'cyanide-load-project
                     "Load project (tab for completion): "
                     project-names
                     cyanide-project-collection
                     ":display-name"
                     'equal
                     nil
                     1))))

(provide 'cyanide-project)