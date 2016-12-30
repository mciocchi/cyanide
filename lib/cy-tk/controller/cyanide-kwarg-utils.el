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

;;
;;Example Invocation:
;;
;;ELISP> (funcall (superlambda '(&optional x) '(print (concat "foo " x)))
;; "bar")
;;
;;"foo bar"
;;
;;"foo bar"
;;
;;ELISP> (funcall (superlambda '(&optional x) '(print (concat "foo " x))))
;;
;;"foo "
;;
;;"foo "
;;
(defun superlambda (args body)
  `(lambda ,args
     ,body))

(defun test3 (&rest args)
  `(quote ,args))

;; Next step- handle adding to collection, preferably with a lambda
;;
;; Invocation:
;;
;; ELISP> (cyanide-builder :collection 'baz :constructor cyanide-project :constructor-args
;;              (:id 'foo :display-name "bar"))
;; [eieio-class-tag--cyanide-project foo "bar" unbound "" unbound unbound unbound]
;;
(defmacro cyanide-builder (&rest args)
  (let ((constructor      (plist-get args :constructor))
        (collection       (plist-get args :collection))
        (constructor-args (plist-get args :constructor-args)))
    (let ((obj (eval (eval `(append `(,constructor) constructor-args)))))
      (print "obj:")
      (print obj)
      (collect obj)))) ; to do

(provide 'cyanide-kwarg-utils)
