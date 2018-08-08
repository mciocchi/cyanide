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
(require 'cyanide-prompt "lib/controller/cyanide-prompt")
(require 'cyanide-identifiable "lib/models/cyanide-identifiable")
(require 'cyanide-nameable "lib/models/cyanide-nameable")
(require 'cyanide-taskable "lib/models/cyanide-taskable")
(require 'cyanide-hookable "lib/models/cyanide-hookable")
(require 'cyanide-pathable-dfd "lib/models/cyanide-pathable-dfd")
(require 'cyanide-viewable "lib/models/cyanide-viewable")
(require 'cyanide-describeable "lib/models/cyanide-describeable")
(require 'cyanide-crypto "lib/controller/cyanide-crypto")

(defun cyanide-infer-project-path-from-dotdir ()
  "Return the parent of the parent directory of current `load-file-name' if
  `load-file-name' is non-nil, otherwise return `default-directory'."
  (let ((dir default-directory))
    (when load-file-name
      (setq dir (file-name-directory
                 (directory-file-name
                  (file-name-directory load-file-name)))))
    dir))

(defclass cyanide-project (eieio-instance-tracker
                           cyanide-identifiable
                           cyanide-nameable
                           cyanide-describeable
                           cyanide-pathable
                           cyanide-viewable
                           cyanide-hookable
                           cyanide-taskable)
  ((tracking-symbol :initform cyanide-project-collection)
   (path :initarg :path
         :type string
         :initform  (cyanide-infer-project-path-from-dotdir)
         :documentation
         "Directory path.")))

(cl-defmethod cyanide-load-project ((proj cyanide-project))
  "Load a cyanide-project:

   1) if there are any views enabled, disable all of them
   2) if there is a previous project loaded, tear it down.
   3) set `cyanide-current-project'
   4) if `cyanide-current-project' has a load-hook, execute it.
   5) if `cyanide-current-project' has a default-view, enable it"
  (let ((load-hook (oref proj load-hook))
        (default-view nil)
        (proj-id (oref proj :id))
        (previous-proj (cyanide-get-by-id cyanide-current-project
                                          cyanide-project-collection)))
    (call-interactively 'cyanide-disable-all-views)
    (when previous-proj (run-teardown-hook previous-proj))
    (setq cyanide-current-project proj-id)
    (run-load-hook proj)
    (when (slot-boundp proj :default-view)
      (enable (oref proj :default-view)))
    nil))

(defun cyanide-load-project-prompt ()
  "Prompt the user for a project to load, take user input,
   and then load it."
  (interactive
   (let ((project-names (mapcar
                         (lambda (x)
                           (oref x :display-name))
                         cyanide-project-collection)))
     (cyanide-prompt 'cyanide-load-project
                     "Load project (tab for completion): "
                     project-names
                     cyanide-project-collection
                     ":display-name"
                     'equal
                     nil
                     1))))

(defun cyanide-get-current-project-path ()
  (cyanide-project-oref :path))

(defun cyanide-get-current-project ()
  """
  Return object representing `cyanide-current-project'. If
  no project is loaded, return nil.
  """
  (cyanide-get-by-id cyanide-current-project cyanide-project-collection))

(defmacro cyanide-project-oref (key)
  """
  Get property stored at key of `cyanide-current-project'.
  """
  `(oref (cyanide-get-current-project) ,key))

(cl-defmethod cyanide-export-command ((proj cyanide-project)
                                      destination-dir
                                      extension
                                      &optional
                                      encrypt
                                      pw)
  "Generate a shell command to serialize a cyanide-project with gzip compression
   and gpg2 encryption, and then export it to DESTINATION DIR"
  (let ((orig-dir default-directory)
        (basename (file-name-nondirectory
                   (directory-file-name (oref proj :path))))
        (path (oref proj :path))
        (destination nil)
        (command nil))
    (setq destination (format "%s%s%s" destination-dir basename extension))
    ;; if encrypt is nil, we will create an unencrypted .tar.gz
    (if (eq nil encrypt)
        (progn
          (setq command
                (format "cd %s && cd ../ && tar -cvzf %s %s ;\n"
                        path destination basename)))
      ;; if value of encrypt is 'symmetric, prompt for gpg2 passphrase,
      ;; and then encrypt.
      (if (eq 'symmetric encrypt)
          (progn
            (assert (eq 'string (type-of pw)) nil "should be a string")
            (setq
             command
             (format
              (concat "cd %s && cd ../ && tar -cvz %s "
                      "|gpg2 --output %s "
                      "--batch --passphrase %s --symmetric ;\n")
              path basename destination pw)))
        ;; else if encrypt is a string, we will try to match it against a key in
        ;; our gpg2 keyring. If a gpg2 key is found, we will create a .tar.gz
        ;; file and use the key to encrypt it.
        (if (eq 'string (type-of encrypt))
            (progn
              (setq command
                    (format
                     (concat "cd %s && cd ../ && tar -cvz %s "
                             "|gpg2 --output %s --recipient %s -e ;\n")
                     path basename destination encrypt)))
          ;; else if encrypt is neither a string nor 'symmetric nor nil, error.
          (error (format "unhandled argument to encrypt: %s" encrypt)))))
    `(:command ,command
      :basename ,basename
      :source ,(directory-file-name (oref proj :path))
      :destination ,destination
      :encrypt ,encrypt)))

(defun cyanide-export-all-projects-1 (memo)
  (let ((existing-dest-files (plist-get memo :existing-dest-files)))
    (async-shell-command (concat (mapconcat (lambda (elt)
                                              (concat "rm -fv " elt))
                                            existing-dest-files
                                            ";\n")
                                 (if existing-dest-files ";\n" "")
                                 (plist-get memo :command)))))

(defun cyanide-prompt-before-export-all-projects-1 (memo)
  (interactive)
  (let ((input (read-string (concat "Files already exist at "
                                    "the following export "
                                    "destinations:\n"
                                    (mapconcat
                                     (lambda (dest) dest)
                                     (plist-get
                                      memo
                                      :existing-dest-files)
                                     "\n")
                                    "\nOverwrite (O) or exit (X): "))))
    (if (equal input "O")
        (export-all-projects-1 memo)
      (if (equal input "X")
          (message "Exiting!"))
      (message
       (concat "Unhandled input, please select (O)verwrite or e(X)it.")))))

(defun cyanide-export-all-projects (destination-dir extension &optional encrypt)
  "Export all cyanide-projects."
  (let ((pw nil))
    (when (eq 'symmetric encrypt)
      (setq pw
            (cyanide-pw cyanide-pw-use-salt-hashing
                        cyanide-pw-salt-iterations)))
    ;; bail out and raise alert if password does not match.
    (if (eq 'MISMATCH pw)
        (message "Passwords did not match, exiting!")
      ;; else continue
      (let ((memo (cons '(:command
                          ""
                          :existing-dest-files ())
                        cyanide-project-collection))
            (func (lambda (memo elt)
                    (let ((props (cyanide-export-command
                                  elt destination-dir extension encrypt pw)))
                      `(:command
                        ,(concat (plist-get memo :command)
                                 (plist-get props :command))
                        :existing-dest-files
                        ,(if (file-exists-p (plist-get props :destination))
                             (cons (plist-get props :destination)
                                   (plist-get memo :existing-dest-files))
                           (plist-get memo :existing-dest-files)))))))
        (let ((retval (reduce func memo)))
          ;; first check here if passwords matched, and bail out if they didn't.
          (if (not (eq '() (plist-get retval :existing-dest-files)))
              (cyanide-prompt-before-export-all-projects-1 retval)
            (cyanide-export-all-projects-1 retval)))))))

(defun cd-proj-root ()
  "Change directory to the root of the currently active project."
  (when (not (bound-and-true-p cyanide-current-project))
    (error "No cyanide-project loaded!"))
  (cd (cyanide-project-oref :path))
  nil)

(cl-defmethod toplevel-of ((proj cyanide-project))
  "Return the location of the directory one level up from PROJ"
  (mapconcat
   (lambda (str)
     str)
   (butlast
    (split-string
     (directory-file-name
      (oref
       (car cyanide-project-collection)
       :path))
     "/"))
   "/"))

(cl-defmethod dotdir-of ((proj cyanide-project))
  "Guess the dotdir location of a `cyanide-project'"
  (concat
   (oref
    proj
    :path)
   cyanide-project-config-dotdir-name))

(cl-defmethod dotdir-of ((proj string))
  "Guess the dotdir location of a `cyanide-project'"
  (concat
   (directory-file-name proj)
   "/"
   cyanide-project-config-dotdir-name))

(cl-defmethod dotfile-of ((proj cyanide-project))
  "Guess the dotfile location of a `cyanide-project'"
  (concat
   (dotdir-of
    proj)
   "/"
   cyanide-project-config-file-name))

(cl-defmethod dotfile-of ((proj string))
  "Guess the dotfile location of a `cyanide-project'"
  (concat
   (dotdir-of
    proj)
   "/"
   cyanide-project-config-file-name))

(defun cyanide-project-find-dotfile ()
  "Find the dotfile of the `cyanide-current-project' and open it in a buffer."
  (interactive)
  (find-file (dotfile-of (cyanide-get-current-project))))

(defun cyanide-project-initialize-new-directory-handler (path)
  "Prompt user whether to create an empty project directory, or clone one from
git."
  (interactive)
  (let ((response (read-string
                  (concat
                   path
                   " does not exist. "
                   "clone from [g]it, "
                   "or [m]ake new directory? "))))
    (when (equal response "g") (cyanide-project-initialize-from-git path))
    (when (equal response "m") (cyanide-project-initialize-new-directory path))))

(defun cyanide-project-initialize-existing-directory (path)
  "Initialize a `cyanide-project' in an existing directory. If the directory has
a pre-existing project init file, prompt whether to load it."
  (interactive)
  (if (file-exists-p (dotfile-of path))
      (when
          (equal
           "y"
           (read-string
            (concat
             "a dotfile exists in that directory. "
             "Would you like to load it? [y] ")))
        (cyanide-try-load-dotfile (dotfile-of path) '()))
    (let (view-name project-name)
      (setq view-name (completing-read
                       "default-view: "
                       (cons
                        "nil"
                        (mapcar
                         (lambda
                           (view)
                           (oref view :display-name))
                         cyanide-view-collection)))
            project-name (file-name-nondirectory (directory-file-name path)))
      (switch-to-buffer "*tmp*")
      (insert
       (format
        "%s%s%s%s%s%s%s%s%s%s%s%s"
        "(cyanide-project :id "
        "'"
        project-name
        " "
        ":display-name "
        "\""
        project-name
        "\""
        " "
        ":default-view '"
        (when (not (equal view-name "nil")) (oref
                                             (cyanide-get-one-by-slot
                                              view-name
                                              cyanide-view-collection
                                              ":display-name"
                                              'equal)
                                             :id))
        ")"))
      (message "writing file, path: " path)
      (write-file (dotfile-of path))
      (kill-buffer (file-name-nondirectory (dotfile-of path)))
      (cyanide-try-load-dotfile (dotfile-of path) '())
      (cyanide-ask-to-load (intern project-name)))))

(defun cyanide-project-initialize-new-directory (path)
  "Create a new empty `cyanide-project' directory."
  (interactive)
  (let ((proj-path (directory-file-name path))
        (view-name (completing-read
                    "default-view: "
                    (cons
                     "nil"
                     (mapcar
                      (lambda
                        (view)
                        (oref view :display-name))
                      cyanide-view-collection))))
        (name nil)
        init-path)
    (setq name (file-name-nondirectory proj-path))
    (setq init-path (concat proj-path "/.cy/init.el" ))
    (switch-to-buffer "*tmp*")
    (insert
     (format
      "%s%s%s%s%s%s%s%s%s%s%s%s"
      "(cyanide-project :id "
      "'"
      name
      " "
      ":display-name "
      "\""
      name
      "\""
      " "
      ":default-view "
      (when (not (equal view-name "nil")) (oref
                                           (cyanide-get-one-by-slot
                                            view-name
                                            cyanide-view-collection
                                            ":display-name"
                                            'equal)
                                           :id))
      ")"))
    (write-file init-path)
    (kill-buffer (file-name-nondirectory init-path))
    (cyanide-try-load-dotfile init-path '())
    (cyanide-ask-to-load (intern name))))

(defun cyanide-project-initialize-from-git (path)
  "Clone an arbitrary git repository, and initialize it as a `cyanide-project'."
  (interactive)
  (let ((source (read-string "source (git url): "))
        response
        retval)
    (shell-command
     (format
      "%s%s%s%s"
      "git clone "
      source
      " "
      path))
    (cyanide-project-initialize-existing-directory path)))

(defun cyanide-project-initialize (&optional path)
  "Create a new cyanide-project directory.

1) If PATH exists, designate it as a `cyanide-project'.

2) If PATH does not exist, clone a git project at that location, or leave it
empty.

3) if the PATH does not contain a CyanIDE init file, attempt to create one.

4) Ask the user whether to load the CyanIDE init file.

In order for the project to be discovered at startup, PATH should correspond to
a directory inside one of the `cyanide-project-toplevel-directories'.
"
  (interactive)
  (when (not (bound-and-true-p path))
    (setq
     path (read-file-name "destination path for new project: "
                          (car cyanide-project-toplevel-directories))))
  (message (format "%s" (file-exists-p path)))
  (if (file-exists-p path)
      (cyanide-project-initialize-existing-directory path)
    (cyanide-project-initialize-new-directory-handler path)))

(defun cyanide-ask-to-load (id)
  "Prompt whether to load a `cynide-project' by id."
  (interactive)
  (let ((proj (cyanide-get-by-id id cyanide-project-collection)))
    (let ((response (read-string
                     (concat
                      "would you like to load "
                      (oref proj :display-name)
                      " "
                      "[y]?: ")
                     nil
                     nil
                     "y")))
      (when
          (equal
           response
           "y")
        (cyanide-load-project proj)))))

(provide 'cyanide-project)
