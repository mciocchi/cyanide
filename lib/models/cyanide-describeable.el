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

(defclass cyanide-describeable ()
  ((description :initarg :description
                :initform ""
                :type string
                :documentation
                "Brief description of this object instance."))
  "Objects that inherit from `cyanide-describeable' provide a helpful
:description field describing how the instance is intended to be used."
  :abstract t)

(provide 'cyanide-describeable)
