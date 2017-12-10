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
(require 'cyanide-prompt)
(require 'cyanide-identifiable)
(require 'cyanide-nameable)
(require 'cyanide-taskable)
(require 'cyanide-hookable)
(require 'cyanide-pathable-dfd)
(require 'cyanide-viewable)
(require 'cyanide-describeable)
(require 'cyanide-crypto)

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

(defun cyanide-export-all-projects-2 (memo)
  "Handle export command execution for cyanide-projects."
  (let ((existing-dest-files (plist-get memo :existing-dest-files)))
    (async-shell-command (concat (mapconcat (lambda (elt)
                                              (concat "rm -fv " elt))
                                            existing-dest-files
                                            ";\n")
                                 (if existing-dest-files ";\n" "")
                                 (plist-get memo :command)))))

(defun cyanide-prompt-before-export-overwrite-projects (memo)
  "Prompt user whether to overwrite files when exporting cyanide-projects."
  (interactive)
  (let ((input (completing-read (concat "Files already exist at "
                                        "the following export "
                                        "destinations:\n"
                                        (mapconcat
                                         (lambda (dest) dest)
                                         (plist-get
                                          memo
                                          :existing-dest-files)
                                         "\n")
                                        "\nOverwrite (O) or exit (X): ")
                                '("O" "X")
                                nil
                                t)))
    (if (equal input "O")
        (cyanide-export-all-projects-2 memo)
      (if (equal input "X")
          (message "Exiting!")
        (message
         (concat "Unhandled input, please select (O)verwrite or e(X)it."))))))

(defun cyanide-export-all-projects ()
  "Compress and optionally encrypt all cyanide-projects before exporting them to
   DESTINATION-DIR."
  (interactive)
  (let ((destination-dir nil)
        (destination-dir-msg nil)
        (extension ".cy.tar.gz.gpg")
        (encrypt-arg nil)
        (encrypt-msg nil)
        (encrypt nil)
        (no-op nil))
    (setq destination-dir-msg
          "Select project export directory: ")
    (setq destination-dir (read-directory-name destination-dir-msg))
    (setq encrypt-msg
          "Select encryption type: (S)ymmetric, (A)symmetric, (N)one e(X)it: ")
    (setq encrypt-arg (completing-read encrypt-msg '("S" "A" "N" "X") nil t))
    (if (equal "S" encrypt-arg)
        (setq encrypt 'symmetric)
      (if (equal "A" encrypt-arg)
          (setq encrypt (read-string "select recipient gpg2 key: "))
        (if (equal "N" encrypt-arg)
            (setq extension ".cy.tar.gz")
          (if (equal "X" encrypt-arg)
              (progn
                (message "Exiting.")
                (setq no-op t))
            (progn
              (message (concat "unhandled argument: " encrypt-arg))
              (setq no-op t))))))
    (when (not no-op)
      (cyanide-export-all-projects-1 destination-dir extension encrypt))))

(defun cyanide-export-all-projects-1 (destination-dir
                                      extension
                                      &optional
                                      encrypt)
  "Compress and optionally encrypt all cyanide-projects before exporting them to
   DESTINATION-DIR."
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
              (cyanide-prompt-before-export-overwrite-projects retval)
            (cyanide-export-all-projects-2 retval)))))))

(defun cyanide-prompt-for-overwrite-project ()
  (let ((input (completing-read
                (concat
                 "A directory already exists at that path. Would you like to "
                 "(O)verwrite it, (C)reate a project inside of it, or e(X)it? ")
                '("O" "C" "X") nil t)))
    (if (equal "O" input)
        'overwrite
      (if (equal "C" input)
          'create
        (if (equal "X" input)
            'exit
                                        ; else
          (error "unhandled input"))))))

(defun cyanide-initialize-project ()
  (interactive)
  (let ((path (completing-read "choose a toplevel directory for this project: "
                               cyanide-project-toplevel-directories nil t))
        (dir (read-string "choose a base directory for this project: "))
        (id nil))
    (setq id (read-string (format "choose a unique id for this project (%s): "
                                  dir)))
    (when (equal "" id) (setq id dir))
    (cyanide-initialize-project-1 id (concat path dir))))

(defun cyanide-initialize-project-overwrite (id path display-name))

(defun cyanide-initialize-project-create-inside (id path display-name))

(defun cyanide-initialize-project-new-directory (id path display-name))

(defun cyanide-initialize-project-1 (id path)
  (let ((display-name "")
        (overwrite nil))
    (when (file-exists-p path)
      (setq overwrite (cyanide-prompt-for-overwrite-project)))
    (if (eq 'exit overwrite)
        nil ; no-op
      (progn
        (setq display-name
              (read-string
               (format "choose a display name for this project. (%s): " id)))
        (when (equal "" display-name) (setq display-name id))
        ;; - default-view
        ;; - load-hook
        ;; - teardown-hook
        ;; - tasks
        (if (eq 'overwrite overwrite)
            (cyanide-initialize-project-overwrite (intern id) path display-name)
          (if (eq 'create overwrite)
              (cyanide-initialize-project-create-inside
               (intern id) path display-name)
            (if (eq nil overwrite)
                (cyanide-initialize-project-new-directory
                 (intern id) path display-name)
              (error "unhandled input"))))))))

(provide 'cyanide-project)
