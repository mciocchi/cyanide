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

(defvar cyanide-window-local-variables (make-hash-table :test 'equal))

(defvar cyanide-treenodes '()
  "This collection holds all cyanide-treenode objects.")

(defvar cyanide-current-view nil
  "This var stores a symbol used by cyanide to determine
       what view it's currently in.")

(defvar cyanide-current-project nil
  "This var stores a symbol used by cyanide to determine
       what project it's currently in.")

(defvar cyanide-verbose nil
  "non-nil if cyanide should use verbose logging.")

(defvar cyanide-find-dired-exclude-vc
  "-not -path \"*\.svn*\" -not -path \"*\.git*\" "
  "Exclude version control dot directories from
       cyanide-find-dired. If this is set to an empty
       string, CyanIDE will not exclude vc directories.")

(defvar cyanide-view-collection '()
  "cyanide-views are all stored in this list.")

(defvar cyanide-project-collection '()
  "cyanide-projects are all stored in this list.")

(defvar cyanide-menu-item-collection '()
  "cyanide-menu-items are stored in this list.")

(provide 'cyanide-globals)
