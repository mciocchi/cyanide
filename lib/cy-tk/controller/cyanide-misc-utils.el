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

(defun cyanide-filter (lst)
  "Return LST with nil values removed."
  (delq nil lst))

(defun cyanide-return-if-true (test sym1 sym2 retval)
  "Apply TEST to sym1 and sym2 and return RETVAL if TEST
   returns true. Else return nil."
  (when (funcall test sym1 sym2)
    retval))

(defun cyanide-hook-executor (hooks)
  "Execute hook functions in HOOKS with some extra
   logging."
  (let ((f (lambda (func)
             (progn
               (message (concat "cyanide-hook-executor calling"
                                " "
                                (format "%s" func)))
               (funcall func)))))
    (mapcar f hooks)))

(defun cyanide-list-display-names (lst)
  "Return a list of :display-name slots from an
   arbitrary LST of objects."
  (mapcar
   (lambda (x)
     (oref x :display-name))
   lst))

;; Certainly there must be a less stupid way of doing this? The interpreter
;; (and slot-boundp, by extension) does not allow quoted colon-prefixed
;; slots like ':foo. To circumvent this, we need our own slot-boundp that
;; forcibly dequotes the slot before evaluating the expression, but this
;; requires some indirection here.
(defmacro cyanide-slot-boundp (obj slt)
  "Return t if slot SLT is bound, else return nil."
  `(funcall (lambda ()
              (condition-case nil
                  (when (oref ,obj ,slt) t)
                (error nil)))))

(defun assocdr (sym lst)
  (cdr (assoc sym lst)))

(defun assocar (sym lst)
  (car (assoc sym lst)))

(provide 'cyanide-misc-utils)
