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

(require 'cyanide-menu-function "lib/models/cyanide-menu-function")
(require 'cyanide-globals "lib/controller/cyanide-globals")
(require 'cyanide-get-one-by-slot "lib/controller/cyanide-get-one-by-slot")
(require 'cyanide-prompt "lib/controller/cyanide-prompt")
(require 'cyanide-describeable "lib/models/cyanide-describeable")
(require 'cyanide-taskable)

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
   (if (not (bound-and-true-p cyanide-current-project))
       (progn (message "No cyanide-project has been loaded, no tasks list to view.")
              nil)
     (let ((tasks-collection (tasks-of (cyanide-get-current-project))))
       (if (not (bound-and-true-p tasks-collection))
           (progn (message "No tasks defined for this project")
                  nil)
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
                           1)))))
   nil))

(cl-defmacro cyanide-simple-task (&key
                               id
                               display-name
                               description
                               command
                               prefix
                               suffix
                               buffer)
  "Define a `cyanide-task'. If DISPLAY-NAME or DESCRIPTION are not provided, use
COMMAND instead. Wrap COMMAND with optional PREFIX and SUFFIX before running in
BUFFER. Prompt the user with the COMMAND before executing it in order to allow
the COMMAND to be modified."
  `(cyanide-task
    :id ,(if (bound-and-true-p id) id (error "id is required"))
    :display-name ,(if (bound-and-true-p display-name) display-name command)
    :description ,(if (bound-and-true-p description) description command)
    :func (lambda ()
            (interactive)
            (let ((default-directory (cyanide-project-oref :path))
                  (async-shell-command-buffer 'confirm-kill-process))
              (async-shell-command
               (read-string "Async shell command: "
                            (format "%s \n %s \n %s"
                                    ,(if (bound-and-true-p prefix) prefix "")
                                    ,(if (bound-and-true-p command)
                                         command
                                       (error "command is required"))
                                    ,(if (bound-and-true-p suffix) suffix "")))
               ,(if (bound-and-true-p buffer)
                    buffer
                  (concat "*" command "*")))))))

;; TODO consider replacing `cyanide-simple-task' with `cyanide-task-advice'
(defun cyanide-task-advice (orig-fun &key foo &rest args)
  (message (format "foo: %s" foo))
  (message (format "args: %s" args))
  ;; `(cyanide-task
  ;;   :id ,(if (bound-and-true-p id) id (error "id is required"))
  ;;   :display-name ,(if (bound-and-true-p display-name) display-name command)
  ;;   :description ,(if (bound-and-true-p description) description command)
  ;;   :func (lambda ()
  ;;           (interactive)
  ;;           (let ((default-directory (cyanide-project-oref :path))
  ;;                 (async-shell-command-buffer 'confirm-kill-process))
  ;;             (async-shell-command
  ;;              (read-string "Async shell command: "
  ;;                           (format "%s \n %s \n %s"
  ;;                                   ,prefix
  ;;                                   ,(if (bound-and-true-p command)
  ;;                                        command
  ;;                                      (error "command is required"))
  ;;                                   ,suffix))
  ;;              ,buffer))))
  (apply orig-fun args))

(provide 'cyanide-task)
