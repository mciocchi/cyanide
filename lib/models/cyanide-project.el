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

(defclass cyanide-project (eieio-instance-tracker
                           cyanide-identifiable
                           cyanide-nameable
                           cyanide-describeable
                           cyanide-pathable-dfd
                           cyanide-viewable
                           cyanide-hookable
                           cyanide-taskable)
  ((tracking-symbol :initform cyanide-project-collection))
  "Total representation of a project, including its path on the filesystem,
  name, views, and hooks to toggle configurations at project load time.")

(cl-defmethod cyanide-load-project ((proj cyanide-project))
  "Load a cyanide-project:

   1) if there is a previous project loaded, tear it down.
   2) set `cyanide-current-project'
   3) if `cyanide-current-project' has a load-hook, execute it.
   4) if `cyanide-current-project' has a default-view, enable it"
  (let ((load-hook (oref proj load-hook))
        (default-view nil)
        (sym (oref proj :id))
        (previous-proj (cyanide-get-one-by-slot cyanide-current-project
                                                cyanide-project-collection
                                                ":id"
                                                'eq)))
    (when (slot-boundp proj :default-view)
      (setq default-view (cyanide-get-one-by-slot (oref proj :default-view)
                                                  cyanide-view-collection
                                                  ":id"
                                                  'eq)))
    (when previous-proj (run-teardown-hook previous-proj))
    (setq cyanide-current-project sym)
    (run-load-hook proj)
    (when default-view (enable default-view))
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
  "Export all "
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

(provide 'cyanide-project)
