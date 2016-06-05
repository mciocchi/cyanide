(require 'cyanide-menu-function)
(require 'cyanide-kwarg-utils)
(require 'cyanide-globals)

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

(provide 'cyanide-task)
