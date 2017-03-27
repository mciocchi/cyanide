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
(require 'cyanide-globals)

(defclass cyanide-menu (cyanide-menu-item)
  ((members :initarg :members
            :type list
            :initform '()))
  "Class that represents  a menu-panel and contains
   members which inherit from `cyanide-menu-item'.")

;; vectorize:
;; if it's a menu-function, invoke vectorize on one item.
;; if it's a menu, invoke vectorize on all members, including sub-menus
;; example output:
;; ("CyanIDE Test Menu"
;;  ["mvn clean"
;;   (lambda nil
;;     (interactive)
;;     (print "executing mvn clean"))]
;;  ["mvn package"
;;   (lambda nil
;;     (interactive)
;;     (print "executing mvn package"))])
(cl-defmethod cyanide-vectorize ((menu cyanide-menu))
  (cons (oref menu :display-name)
        (mapcar 'cyanide-vectorize (cyanide-get-menu-members menu))))

(cl-defmethod cyanide-menu-render ((menu cyanide-menu)
                                    menu-symbol
                                    menu-mode-map)
  "Render a CyanIDE menu object to the menu bar"
  (easy-menu-define menu-symbol
    menu-mode-map
    (oref menu :display-name)
    (cyanide-vectorize menu)))

    ;; Not faster than a hashtable, but still pretty fast... and might prove
    ;; faster in best-case. copy-tree is required below because otherwise delq
    ;; will destructively operate on members as a referent, rather than as a
    ;; value.
    (cl-defmethod cyanide-get-menu-members ((menu cyanide-menu))
      "Return `cyanide-menu-item' objects from
       `cyanide-menu' MENU."
      (let ((members (copy-tree (oref menu :members)))
            (lst cyanide-menu-item-collection)
            (itm nil)
            (retval '()))
        (while (and members lst)
          (setq itm (pop lst))
          (when (memq (oref itm :id) members)
            (progn (push itm retval)
                   (delq (oref itm :id) members))))
        retval))

(provide 'cyanide-menu)
