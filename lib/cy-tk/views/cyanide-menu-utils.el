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

(require 'cyanide-get-one-by-slot)
(require 'cyanide-menu)

(defun cyanide-tasks-menu-builder (project-id)
  "Build tasks menu object for a `cyanide-project'."
  (let ((project (cyanide-get-one-by-slot project-id
                                          cyanide-project-collection
                                          ":id"
                                          'eq)))
    (when (not project) (error (concat "cyanide-tasks-menu-builder"
                                       " "
                                       "could not find project"
                                       " "
                                       (format "%s" project-id))))
    (let ((members (oref project
                         :tasks)))
      (cyanide-delete-menu-object 'tasks)
      (let ((menu (cyanide-menu-builder '(:id 'tasks
                                          :display-name "Tasks"
                                          :members members))))
        menu))))

(defun cyanide-delete-menu-object (menu-id)
  "Delete a menu from `cyanide-menu-item-collection'."
  (let ((old-menu (cyanide-get-one-by-slot menu-id
                                           cyanide-menu-item-collection
                                           ":id"
                                           'eq)))
    (when old-menu
      (setq cyanide-menu-item-collection
            (delq old-menu cyanide-menu-item-collection)))))

(defun cyanide-render-menu-with-tasks (project-id
                                       menu-id)
  "Dynamically generate tasks sub-menu for a
   `cyanide-project' and render it in context with its
   super-menu."
  (let ((menu (cyanide-get-one-by-slot menu-id
                                       cyanide-menu-item-collection
                                       ":id"
                                       'eq))
        (project (cyanide-get-one-by-slot cyanide-current-project
                                          cyanide-project-collection
                                          ":id"
                                          'eq)))
    (when (cyanide-slot-boundp project :tasks)
      (cyanide-tasks-menu-builder project-id)
      (cyanide-menu-render menu
                           menu-id
                           cyanide-mode-map))))

(defun cyanide-unroll-all-menu-functions (menu-id)
  "Recursively unroll all menu functions into a list."
  (let ((menu (cyanide-get-one-by-slot menu-id
                                       cyanide-menu-item-collection
                                       ":id"
                                       'eq))
        (lst '())
        (g (lambda (y) (mapcar f (cyanide-get-menu-members y))))
        (f (lambda (x) (if (child-of-class-p (eieio-object-class x)
                                             'cyanide-menu-function)
                           (push x lst)
                         (if (child-of-class-p (eieio-object-class x)
                                               'cyanide-menu)
                             (funcall g x)
                           (error (concat "cyanide-unroll-all-menu-items "
                                          "cannot parse "
                                          (format "%s" x)))))))) ; else
    (if menu
        (funcall g menu)
      (error (concat "cyanide-unroll-all-menu-functions "
                     "no such menu: "
                     (format "%s" menu-id))))
    lst)) ; return lst

(provide 'cyanide-menu-utils)
