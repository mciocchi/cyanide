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

(defclass cyanide-taskable ()
  ((tasks :initarg :tasks
          :type list
          :documentation
          "List of jobs that can be executed to perform units of work."))
  "Objects that inherit from `cyanide-taskable' contain a list of tasks. This
list is composed of task objects or ids corresponding to task objects."
  :abstract t)

(defmethod tasks-of ((taskable cyanide-taskable))
  "return a list of `cyanide-task's from `cyanide-taskable'. For each element of
:tasks, if it is an object, return it. If the element is not an object, try to
`cyanide-get-by-id' a `cyanide-task' from `cyanide-menu-item-collection'
corresponding to that symbol. If `cyanide-get-by-id' returns nil, try to
evaluate the symbol and return it."
  (mapcar (lambda (elt)
            (if (eieio-object-p elt)
                elt
              (let ((retval (cyanide-get-by-id elt cyanide-menu-item-collection)))
                (if (bound-and-true-p retval)
                    retval
                  (eval elt)))))
          (oref taskable :tasks)))

(provide 'cyanide-taskable)
