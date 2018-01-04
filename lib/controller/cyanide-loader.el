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

(require 'cl)
(require 'cyanide-globals "lib/controller/cyanide-globals")

(defun cyanide-try-load-dotfile (dotfile memo)
  "Try to load elisp file at long path DOTFILE. Catch and report any errors."
  (condition-case err
      (load dotfile)
    ('error (message (concat "error while loading dotfile: "
                             dotfile
                             "\nerror: "
                             (format "%s" err))))))

(defun cyanide-load-toplevel (memo toplevel)
  "Iterate over subdirectories of TOPLEVEL path, to a depth of one.

  If the subdirectory contains a directory named
  `cyanide-project-config-dotdir-name', check for a file inside of it named
  `cyanide-project-config-file-name'.

  If `cyanide-project-config-file-name' exists inside of the dotdir, attempt to
  evaluate it with `cyanide-try-load-dotfile'"
  (if (and (file-exists-p toplevel)
             (directory-name-p toplevel))
    (let ((memo-and-subdirs (cons memo (directory-files toplevel)))
          (project-dir nil)
          (project-config-dir nil)
          (project-init-file nil))

      (reduce (lambda (memo subdir)
                (setq project-dir
                      (concat toplevel
                              subdir
                              "/"))

                (setq project-config-dir
                      (concat project-dir
                              cyanide-project-config-dotdir-name
                              "/"))

                (setq project-init-file
                      (concat project-config-dir
                              cyanide-project-config-file-name))

                (when (and (file-directory-p project-dir)
                           (not (equal "." subdir))
                           (not (equal ".." subdir))
                           (file-directory-p project-config-dir)
                           (file-exists-p project-init-file))
                  (cyanide-try-load-dotfile project-init-file memo))
                memo)
              memo-and-subdirs))
    ; else
    (message (concat "could not load projects from toplevel which is not a "
                     "directory: " toplevel))))

(defun cyanide-load-project-dotfiles ()
  "Load all files of `cyanide-project-config-file-name' from project directories
  in `cyanide-project-toplevel-directories'."
  (let ((memo-and-toplevels (cons '() cyanide-project-toplevel-directories)))
    (reduce (lambda (memo elt) (cyanide-load-toplevel memo elt))
            memo-and-toplevels)))

(provide 'cyanide-loader)
