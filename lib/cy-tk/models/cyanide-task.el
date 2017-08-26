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

(require 'cyanide-menu-function)
(require 'cyanide-kwarg-utils)
(require 'cyanide-globals)
(require 'cyanide-get-one-by-slot)
(require 'cyanide-prompt)
(require 'cyanide-describeable)

; Tasks are nothing more than cyanide-menu-functions for now, but it is
; likely that they will need to provide additional functionality soon.
(defclass cyanide-task (eieio-instance-tracker
                        cyanide-menu-function
                        cyanide-describeable)
  ((tracking-symbol :initform cyanide-menu-item-collection))
  "Class that represents jobs that need to be manually executed.")

(defun cyanide-task-prompt ()
  "Prompt user for task to execute and execute it."
  (interactive
   (let ((menu (cyanide-get-one-by-slot 'tasks
                                        cyanide-menu-item-collection
                                        ":id"
                                        'eq)))
     (if menu
         (let ((tasks-collection
                (cyanide-unroll-all-menu-functions 'tasks)))
           (let ((task-names
                  (cyanide-list-display-names
                   tasks-collection)))
             (cyanide-prompt (lambda (x) (call-interactively
                                          (oref x :func)))
                             "Tasks (tab for completion): "
                             task-names
                             tasks-collection
                             ":display-name"
                             'equal
                             nil
                             1)))
       (message "No tasks menu defined.") ; else
       nil))))

(provide 'cyanide-task)
