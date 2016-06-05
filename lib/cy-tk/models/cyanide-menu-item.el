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
