(defclass cyanide-menu-item ()
  ((id           :initarg :id
                 :type symbol
                 :initform nil)
   (display-name :initarg :display-name
                 :type string
                 :initform ""))
  "Abstract class that provides functionality common
   to both `cyanide-menu' and `cyanide-menu-function'.")

(provide 'cyanide-menu-item)
