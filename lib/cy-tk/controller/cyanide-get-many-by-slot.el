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

(require 'cyanide-misc-utils)

(defun cyanide-get-many-by-slot (sym lst stringified-slot equality-func)
  "Return all objects from LST where SYM matches with
       EQUALITY-FUNC the value stored in STRINGIFIED-SLOT."
  (let ((res '())
        (l lst)
        (slot (intern stringified-slot)))
    (cyanide-filter (mapcar (lambda (x)
                              (cyanide-return-if-true equality-func
                                                      sym
                                                      (eval `(oref x ,slot))
                                                      x)) l))))

(provide 'cyanide-get-many-by-slot)
