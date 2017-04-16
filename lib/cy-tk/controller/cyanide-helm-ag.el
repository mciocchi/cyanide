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

(require 'cyanide-globals)
(require 'cyanide-project)
(require 'helm-ag)

(defun cyanide-helm-ag ()
  """
  Find string in current `cyanide-project' with `helm-ag'.
  """
  (interactive)
  (if cyanide-current-project
      (helm-ag (cyanide-get-current-project-path))
    ;; If no project is loaded, error out:
    (error (concat "cyanide-current-project is nil. "
                   "Cannot invoke cyanide-helm-ag "
                   "before loading a cyanide-project."))))

(provide 'cyanide-helm-ag)
