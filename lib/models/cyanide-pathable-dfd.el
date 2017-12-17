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

(require 'cyanide-pathable "lib/models/cyanide-pathable")

(defun cyanide-default-pathable-directory ()
  "return the directory of `load-file-name' if `load-file-name' is non-nil,
otherwise return `default-directory'."
  (let ((dir default-directory))
    (when load-file-name
      (setq dir (file-name-directory load-file-name)))
    dir))

(defclass cyanide-pathable-dfd (cyanide-pathable)
  ((path :initarg :path
         :type string
         :initform (cyanide-default-pathable-directory)
         :documentation
         "Directory path."))
  "Objects that inherit from `cyanide-pathable-dfd' possess a :path reference to
resources on disk. When they are constructed, objects that inherit from
`cyanide-pathable-dfd' will use the value of :path. If no :path is passed to the
constructor, :path will default to whatever is returned from
`cyanide-default-pathable-directory'"
  :abstract t)

(provide 'cyanide-pathable-dfd)
