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

(defclass cyanide-hookable ()
  ((load-hook :initarg :load-hook
              :initform '()
              :type list
              :documentation
              "hook called at object load-time.")
   (teardown-hook :initarg :teardown-hook
                  :initform '()
                  :type list
                  :documentation
                  "hook called at object teardown."))
  "Objects that inherit from `cyanide-hookable' should implement load and
teardown methods. Inheriting from `cyanide-hookable' is intended to be treated
as a contract: Ideally, all changes to the global emacs runtime environment that
are put into effect by :load-hook should be rolled back when :teardown-hook is
called.

Functions in :load-hook and :teardown-hook are executed from left to right. As
such, it is a matter of good practice to tear down configurations in reverse
of the order that they were loaded.

For example:

:load-hook '(do-1 do-2)
:teardown-hook '(undo-2 undo-1)

The implication of this is that ideally, hookable objects should apply
configurations to the emacs global runtime in layers. Ideally, calling the
:teardown-hook of a `cyanide-hookable' should leave the runtime environment
exactly in the same state that it was in before the :load-hook was called.

By adhering strictly to this contract, `cyanide-hookable' objects represent
atomic units of change that can be applied to the global emacs runtime and then
just as easily stripped away on an ad-hoc basis."
  :abstract t)

(defmacro run-hooks-on-object-property (obj prop)
  `(let ((hook (oref ,obj ,prop)))
     (message (format "running hooks" ""))
     (run-hooks 'hook)))

(cl-defmethod run-load-hook ((hookable cyanide-hookable))
  (run-hooks-on-object-property hookable :load-hook))

(cl-defmethod run-teardown-hook ((hookable cyanide-hookable))
  (run-hooks-on-object-property hookable :teardown-hook))

(provide 'cyanide-hookable)
