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

(defvar cyanide-current-views nil
  "List of views active in cyanide at any given time.")

(defvar cyanide-current-project nil
  "This var stores a symbol used by cyanide to determine
       what project it's currently in.")

(defvar cyanide-view-collection '()
  "cyanide-views are all stored in this list.")

(defvar cyanide-project-collection '()
  "cyanide-projects are all stored in this list.")

(defvar cyanide-menu-item-collection '()
  "cyanide-menu-items are stored in this list.")

(defvar cyanide-project-config-file-name ".cy.el"
  "This variable stores the name of config files which cyanide will search for
  in order to find cyanide project directories in
  `cyanide-project-toplevel-directories'")

(defvar cyanide-project-toplevel-directories '()
  "This list stores paths to toplevel directories which contain cyanide
   project directories which cyanide discovers by searching for project config
   files named `cyanide-project-config-file-name'")

(provide 'cyanide-globals)
