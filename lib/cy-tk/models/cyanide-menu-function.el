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
