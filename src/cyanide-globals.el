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
