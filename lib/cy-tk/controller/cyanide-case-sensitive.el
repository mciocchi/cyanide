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

(defun cyanide-case-sensitive (re)
  "Respect emacs defaults and determine whether cyanide
   should attempt to match with case-sensitivity.

   For more information, see `cyanide-case-sensitive-test'"
  (or (let ((case-fold-search nil))
        (string-match "[$.*[:upper:].*^]" re))
      (not case-fold-search)))

(provide 'cyanide-case-sensitive)
