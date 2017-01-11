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

(require 'cyanide-misc-utils)

(defun cyanide-missing-arg-error (arg)
  (error (concat "Required argument"
                 " "
                 (format "%s" arg)
                 " "
                 "missing from"
                 " "
                 (format "%s" kwargs))))

(defun cyanide-arg-required (arg kwargs)
  (when (not (memq arg kwargs)) (cyanide-missing-arg-error arg)))

(defun cyanide-kwargobj-builder (class
                                 kwargs
                                 &optional
                                 required-kwargs
                                 lst)
  "Check arbitrary KWARGS and `cyanide-missing-arg-error' if
       there exist any REQUIRED-KWARGS that are not present.
       Construct object of class CLASS with KWARGS and
       `add-to-list' LST if it is present."
  (progn
    (when required-kwargs
      (mapcar
       (lambda (required-kwarg)
         (cyanide-arg-required required-kwarg kwargs))
       required-kwargs))
    (let ((obj (eval (cons class kwargs))))
      (when lst
        (add-to-list lst obj))
      obj)))

;; Next step- handle adding to collection, preferably with a lambda
;;
;; Invocation:
;;
;; ELISP> (cyanide-builder :collection 'baz :constructor cyanide-project :constructor-args
;;              (:id 'foo :display-name "bar"))
;; [eieio-class-tag--cyanide-project foo "bar" unbound "" unbound unbound unbound]
;;
;; (defun cyanide-builder (&rest args)
;;   (let ((constructor      (plist-get args :constructor))
;;         (collection       (plist-get args :collection))
;;         (constructor-args (plist-get args :constructor-args)))
;;     (let ((obj (eval (eval `(append `(,constructor) constructor-args)))))
;;       (cyanide-add-to-collection obj))))

;; invocation goal:
;; (cyanide-build 'cyanide-project
;;                :id 'test-project
;;                :display-name "test-project"
;;                :project-root "/home/matt/projects/test-project")

(defun cyanide-build-1 (&rest args)
  (let ((constructor      (plist-get args :constructor))
        (constructor-args (plist-get args :constructor-args)))
    (let ((obj (eval (eval `(append `(,constructor) constructor-args)))))
      (print "obj:")
      (print obj)
      (init obj)
      obj)))

;; (defmacro cyanide-build (&rest args)
;;   (let ((constructor (pop args))
;;         (constructor-args args))
;;     (print "constructor:")
;;     (print constructor)
;;     (print "constructor-args:")
;;     (print constructor-args)
;;     (append `(,constructor) constructor-args)))

;; (defun cyanide-build (&rest args)
;;   (print "args:")
;;   (print args)
;; ;;  (print "(pop args):")
;; ;;  (print (pop args))
;;   (let ((constructor (pop args))
;;         (constructor-args (eval `(quote (quote ,args)))))
;;     (print "constructor:")
;;     (print constructor)
;;     (print "constructor-args:")
;;     (print constructor-args)
;;     (let ((form (append `(,constructor) constructor-args)))
;;       ;;(init obj)
;;       (print "form:")
;;       (print form)
;;       (eval form))))

;;tests
(defun superlambda (args body)
  `(lambda ,args
     ,body))

(defun test3 (&rest args)
  `(quote ,args))

(defun test4 (&rest args)
  args)

(defun test5 (&rest args)
  (eval `(quote (quote ,args))))

;;
;; This works, but the reader macro is incorrect. Too many levels of
;; quoting/dequoting for args:
;;
;; (cyanide-build 'cyanide-project
;;                :id ''test-project2
;;                :display-name "test project2"
;;                :collection test-collection)
;;
;; Consider using this one to add an additional level of quoting for args that
;; are being passed as symbols:
;;
(defun requote-expression (args)
  (mapcar (lambda (arg)
            (if (and (symbolp arg) (not (boundp (quote arg))))
                (eval `(quote (quote ,arg)))
              arg)) ; else
          args))    ; return args

(defun test7 (&rest args) (pop args) args)

(defun cyanide-build (&rest args)
  (print "args:")
  (print args)
;;  (print "(pop args):")
;;  (print (pop args))
  (let ((constructor (pop args))
        (constructor-args args))
    (print "constructor:")
    (print constructor)
    (print "constructor-args:")
    (print constructor-args)
    (let ((form (append `(,constructor) constructor-args)))
      ;;(init obj)
      (print "form:")
      (print form)
      (eval form))))

(provide 'cyanide-kwarg-utils)
