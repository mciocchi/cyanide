(require 'cyanide-menu-function)
(require 'cyanide-kwarg-utils)
(require 'cyanide-globals)
(require 'cyanide-get-one-by-slot)
(require 'cyanide-prompt)

; Tasks are nothing more than cyanide-menu-functions for now, but it is
; likely that they will need to provide additional functionality soon.
(defclass cyanide-task (cyanide-menu-function)
  ()
  "Class that represents repetitive jobs that need to be
       manually executed.")

(defun cyanide-task-builder (kwargs)
  "Constructor for cyanide-tasks."
  (cyanide-kwargobj-builder 'cyanide-task
                            kwargs
                            '(:id :display-name :func)
                            'cyanide-menu-item-collection))

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
