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

(defun cyanide-get-project-directories-from-toplevel ()
  (let ((project-directories '()))
    (mapcar (lambda (toplevel)
              (mapcar (lambda (dir)
                        (when (and (file-directory-p (concat toplevel dir))
                                   (not (equal "." dir))
                                   (not (equal ".." dir)))
                          (push (concat toplevel dir) project-directories)))
                      (directory-files toplevel)))
            cyanide-project-toplevel-directories)
    project-directories))

(defun cyanide-import-projects-from-toplevel ()
  (mapcar (lambda (dir)
            (when (and (file-exists-p dir)
                       (file-exists-p (concat
                                       dir
                                       "/"
                                       cyanide-project-config-file-name)))
              (load (concat
                     dir
                     "/"
                     cyanide-project-config-file-name))))
          (cyanide-get-project-directories-from-toplevel)))

(provide 'cyanide-loader)
