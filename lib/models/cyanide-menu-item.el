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

(require 'cyanide-identifiable "lib/models/cyanide-identifiable")
(require 'cyanide-nameable "lib/models/cyanide-nameable")

(defclass cyanide-menu-item (eieio-instance-tracker
                             cyanide-identifiable
                             cyanide-nameable)
  ((tracking-symbol :initform cyanide-menu-item-collection))
  :abstract t
  :documentation
  "Abstract class that provides functionality common
   to both `cyanide-menu' and `cyanide-menu-function'.")

(provide 'cyanide-menu-item)
