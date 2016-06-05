(require 'cyanide-menu-item)
(require 'cyanide-kwarg-utils)

(defclass cyanide-menu-function (cyanide-menu-item)
  ((func :initarg :func
         :type nil))
  "Class that represents functions stored in a
   `cyanide-menu'.")

(defun cyanide-menu-function-builder (kwargs)
  "Constructor for `cyanide-menu-function'."
  (cyanide-kwargobj-builder 'cyanide-menu-function
                            kwargs
                            '(:id :display-name :func)
                            'cyanide-menu-item-collection))

;; vectorize:
;; cast one string/function pair to a vector.
;; example output:
;; ["mvn clean" (lambda () (print "Executing mvn clean."))]
(cl-defmethod cyanide-vectorize ((menu-function cyanide-menu-function))
  (vector (oref menu-function :display-name)
          (oref menu-function :func)))

(provide 'cyanide-menu-function)
