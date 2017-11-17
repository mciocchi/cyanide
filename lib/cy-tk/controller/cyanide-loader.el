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

(defun cyanide-try-load-dotfile (dotfile memo)
  (condition-case err
      (load dotfile)
    ('error (message (concat "error while loading dotfile: "
                             dotfile
                             "\nerror: "
                             (format "%s" err))))))

(defun cyanide-load-toplevel (memo toplevel)
  (if (and (file-exists-p toplevel)
             (directory-name-p toplevel))
    (let ((memo-and-subdirs (cons memo (directory-files toplevel))))
      (reduce (lambda (memo subdir)
                (when (and (file-directory-p (concat toplevel subdir))
                           (not (equal "." subdir))
                           (not (equal ".." subdir))
                           (file-exists-p (concat toplevel subdir))
                           (file-exists-p (concat
                                           toplevel
                                           subdir
                                           "/"
                                           cyanide-project-config-file-name)))
                  ;; also check for dupe project ids at project construction
                  ;; time, throw away and raise alert message if one already
                  ;; exists. This should be part of cyanide-identifiable
                  (cyanide-try-load-dotfile (concat
                                             toplevel
                                             subdir
                                             "/"
                                             cyanide-project-config-file-name)
                                            memo))
                memo)
              memo-and-subdirs))
    ; else
    (message (concat "could not load projects from toplevel which is not a "
                     "directory: " toplevel))))

(defun cyanide-load-dotfiles ()
  (let ((memo-and-toplevels (cons '() cyanide-project-toplevel-directories)))
    (reduce (lambda (memo elt) (cyanide-load-toplevel memo elt))
            memo-and-toplevels)))

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
