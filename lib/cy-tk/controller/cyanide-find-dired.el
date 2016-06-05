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

(require 'cyanide-get-one-by-slot)
(require 'cyanide-globals)
(require 'cyanide-case-sensitive)

(defun cyanide-find-dired (string)
  (interactive "Mstring: ")
  (if cyanide-current-project
      (let ((directory
             (oref
              (cyanide-get-one-by-slot cyanide-current-project
                                       cyanide-project-collection
                                       ":id"
                                       'eq)
              project-root))
            (find-case-sensitive-arg
             (if (cyanide-case-sensitive string) "" "i")))
        (find-dired directory (concat
                               " -"
                               find-case-sensitive-arg
                               "name "
                               "'*"
                               string
                               "*' "
                               cyanide-find-dired-exclude-vc
                               "-type f ")))
    (error (concat "cyanide-current-project is nil. " ; else
                   "Cannot invoke cyanide-find-dired "
                   "before loading a cyanide-project."))))

(provide 'cyanide-find-dired)
