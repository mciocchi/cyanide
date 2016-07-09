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

(defun cyanide-get-one-by-slot (sym
                                lst
                                stringified-slot
                                equality-func)
  "Return one obj from LST where SYM matches with
   EQUALITY-FUNC the value stored in STRINGIFIED-SLOT.
   Optimized lookup: return the first relevant result
   from the list and stop looking."
  (let ((obj nil)
        (i nil)
        (l lst)
        (slot (intern stringified-slot)))
    (while (and (eq nil obj)
                l)
      (setq i (pop l))
      (when (funcall equality-func (eval `(oref i ,slot)) sym)
        (setq obj i)))
    obj))

(defun cyanide-get-by-id (id coll)
  (cyanide-get-one-by-slot id
                           coll
                           ":id"
                           'eq))

(provide 'cyanide-get-one-by-slot)
