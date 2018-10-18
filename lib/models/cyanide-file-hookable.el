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

(require 'cyanide-pathable)
(require 'cyanide-hookable)

;; Object corresponding to resources loaded from disk. Consider renaming this to
;; `cyanide-file' because isn't every file loadable anyway?
;; TODO consider that pathable should be able to be set to nil. How else do we
;; indicate a file which has been deleted or has yet to be instantiated? This is
;; especially relevant due to `cyanide-delete-file-advice'.
(defclass cyanide-file (cyanide-pathable)
  ((path :initarg :path
         :documentation
         "Directory path.")))

(defclass cyanide-directory (cyanide-file)
  ())

(defun cyanide-file-advice (orig-fun &rest args)
  "if ORIG-FUN is passed a `cyanide-file', get the path from that, and then
invoke ORIG-FUN upon it. Otherwise, invoke ORIG-FUN normally."
  (let ((file (pop args)))
    (when (and (eieio-object-p file)
               (child-of-class-p (eieio-object-class file)
                                 'cyanide-file))
      (setq file (oref file :path)))
    (apply orig-fun (cons file args))))

(defclass cyanide-file-hookable ()
  ((load-hook :initarg :load-hook
              :type cyanide-file
              :documentation
              "File evaluated at object load-time.")
   (teardown-hook :initarg :teardown-hook
                  :type cyanide-file
                  :documentation
                  "File evaluated at object teardown time.")))



(setq b (cyanide-file-hookable
         :load-hook (cyanide-file
                     :path
                     "/home/matt-lm/.emacs.d/lisp/cyanide/lib/models/test2.el")))

(setq c (cyanide-file
         :path
         "/home/matt-lm/.emacs.d/lisp/cyanide/lib/models/test2.el"))

(setq d (cyanide-directory
         :path
         "/home/matt-lm/.emacs.d/lisp/cyanide/lib/models/"))

(setq e (cyanide-file
         :path
         "/home/matt-lm/.emacs.d/lisp/cyanide/lib/models/test.el"))

(defun cyanide-rename-file-advice (orig-fun &rest args)
  "if ORIG-FUN is passed a `cyanide-file', get the path from that, and then
invoke ORIG-FUN upon it. Otherwise, invoke ORIG-FUN normally."
  (let ((file (pop args))
        (newname (expand-file-name (pop args)))
        obj)
    (setq obj (and (eieio-object-p file)
                   (child-of-class-p (eieio-object-class file)
                                     'cyanide-file)
                   file)
          file (if obj (oref obj :path) file)
          obj (when obj (oset obj :path newname))
          args (cons newname args)
          args (cons file args))
    (apply orig-fun args)))

;; TODO copy-file should return a new file object
(defun cyanide-copy-file-advice (orig-fun &rest args)
  "if ORIG-FUN is passed a `cyanide-file', get the path from that, and then
invoke ORIG-FUN upon it. Otherwise, invoke ORIG-FUN normally."
  (let ((file (pop args))
        (newname (expand-file-name (pop args)))
        obj
        retval)
    (setq obj (and (eieio-object-p file)
                   (child-of-class-p (eieio-object-class file)
                                     'cyanide-file)
                   file)
          file (if obj (oref obj :path) file)
          args (cons newname args)
          args (cons file args)
          retval (apply orig-fun args))
    (if obj
        (cyanide-file :path newname)
      retval)))

(defun cyanide-delete-file-advice (orig-fun &rest args)
  "if ORIG-FUN is passed a `cyanide-file', get the path from that, and then
invoke ORIG-FUN upon it. Otherwise, invoke ORIG-FUN normally."
  (let ((file (pop args))
        (newname (expand-file-name (pop args)))
        obj
        retval)
    (setq obj (and (eieio-object-p file)
                   (child-of-class-p (eieio-object-class file)
                                     'cyanide-file)
                   file)
          file (if obj (oref obj :path) file)
          args (cons newname args)
          args (cons file args)
          retval (apply orig-fun args))
    (if obj
        (oset obj :path nil)
      retval)))

;; TODO pathable should handle `convert-standard-filename'
;; TODO look into `expand-file-name'
;; https://stackoverflow.com/questions/3964715/what-is-the-correct-way-to-join-multiple-path-components-into-a-single-complete/3964732
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Components.html
;; TODO replace `basename' with `file-name-base'
;; TODO `rename-file'
;; TODO `delete-file'
;; TODO `copy-directory'
;; TODO `delete-directory'
;; TODO `file-name-directory'
;; `file-name-nondirectory'
;; `file-name-extension'
;; `file-name-sans-extension'
;; `file-relative-name'
;; `expand-file-name'
;; `directory-files'
(cl-defmethod run-load-hook ((hookable cyanide-file-hookable))
  (load-file (oref hookable :load-hook)))

(cl-defmethod run-teardown-hook ((hookable cyanide-file-hookable))
  (load-file (oref hookable :teardown-hook)))

(advice-add 'load-file :around #'cyanide-file-advice)

(advice-add 'find-file :around #'cyanide-file-advice)

(advice-add 'find-file-literally :around #'cyanide-file-advice)

(advice-add 'insert-file-literally :around #'cyanide-file-advice)

(advice-add 'insert-file :around #'cyanide-file-advice)

(advice-add 'f-directory-p :around #'cyanide-file-advice)

(advice-add 'file-directory-p :around #'cyanide-file-advice)

(advice-add 'file-exists-p :around #'cyanide-file-advice)

(advice-add 'rename-file :around #'cyanide-rename-file-advice)

(advice-add 'copy-file :around #'cyanide-copy-file-advice)

(advice-add 'delete-file :around #'cyanide-delete-file-advice)

(provide 'cyanide-file-hookable)
