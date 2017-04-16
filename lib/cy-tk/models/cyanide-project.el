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
(require 'cyanide-prompt)
(require 'cyanide-identifiable)
(require 'cyanide-nameable)
(require 'cyanide-taskable)
(require 'cyanide-hookable)
(require 'cyanide-pathable)
(require 'cyanide-viewable)

(defclass cyanide-project (eieio-instance-tracker
                           cyanide-identifiable
                           cyanide-nameable
                           cyanide-pathable
                           cyanide-viewable
                           cyanide-hookable
                           cyanide-taskable)
  ((tracking-symbol :initform cyanide-project-collection)))

(cl-defmethod cyanide-load-project ((proj cyanide-project))
  "Load a cyanide-project:

   1) if there is a previous project loaded, tear it down.
   2) set `cyanide-current-project'
   3) if `cyanide-current-project' has a load-hook, execute it.
   4) if `cyanide-current-project' has a default-view, enable it"
  (let ((load-hook (oref proj load-hook))
        (default-view (cyanide-get-one-by-slot (oref proj default-view)
                                               cyanide-view-collection
                                               ":id"
                                               'eq))
        (sym (oref proj :id))
        (previous-proj (cyanide-get-one-by-slot cyanide-current-project
                                                cyanide-project-collection
                                                ":id"
                                                'eq)))
    (when previous-proj (cyanide-hook-executor
                         (oref previous-proj :teardown-hook)))
    (setq cyanide-current-project sym)
    (when load-hook
      (cyanide-hook-executor load-hook))
    (when default-view (funcall (oref default-view :enable)))
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

(defun cyanide-get-current-project-path ()
  (cyanide-project-oref :path))

(defun cyanide-get-current-project ()
  """
  Return object representing `cyanide-current-project'. If
  no project is loaded, return nil.
  """
  (cyanide-get-by-id cyanide-current-project cyanide-project-collection))

(defmacro cyanide-project-oref (key)
  """
  Get property stored at key of `cyanide-current-project'.
  """
  `(oref (cyanide-get-current-project)  ,key))



(provide 'cyanide-project)
