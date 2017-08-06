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
              :type list
              :documentation
              "hook called at object load-time.")
   (teardown-hook :initarg :teardown-hook
                  :type list
                  :documentation
                  "hook called at object teardown."))
  "Objects that inherit from `cyanide-hookable' should implement load and
teardown methods. Inheriting from `cyanide-hookable' is intended to be treated
as a contract: Ideally, all changes to the global emacs runtime environment that
are put into effect by :load-hook should be rolled back when :teardown-hook is
called.

The implication of this is that ideally, hookable objects should leave no trace
after they have been torn down. Calling the :teardown-hook should leave the
runtime environment exactly in the same state that it was in before the
:load-hook was called.

By adhering strictly to this contract, `cyanide-hookable' objects represent
atomic units of change that can be applied to the global emacs runtime and then
just as easily stripped away on an ad-hoc basis."
  :abstract t)

(provide 'cyanide-hookable)
