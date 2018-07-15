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

(require 'cyanide-globals "lib/controller/cyanide-globals")
(require 'cyanide-project "lib/models/cyanide-project")

(defun cyanide-helm-find ()
  """
  Find file in current `cyanide-project' with `helm-find'.
  """
  (interactive)
  (require 'helm-find)
  (if cyanide-current-project
      (let ((default-directory (cyanide-get-current-project-path)))
        (helm-find nil))
    (error (concat "cyanide-current-project is nil. "
                   "Cannot invoke cyanide-helm-find "
                   "before loading a cyanide-project."))))

(defun cyanide-helm-projectile-find-file-dwim ()
  """
  Find file in current `cyanide-project' with `helm-projectile-find-file-dwim'.
  """
  (interactive)
  (require 'helm-projectile)
  (if cyanide-current-project
      (let ((default-directory (cyanide-get-current-project-path)))
        (helm-projectile-find-file-dwim))
    (error (concat "cyanide-current-project is nil. "
                   "Cannot invoke cyanide-helm-find "
                   "before loading a cyanide-project."))))

(provide 'cyanide-helm-wrapper)
