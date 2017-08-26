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

(require 'ert)
(require 'cyanide-hookable)

(ert-deftest cyanide-hookable-test ()
  (defclass hookable-test-class (cyanide-hookable)
    ())

  (defun do-1 ()
    'do-1)

  (defun do-2 ()
    'do-2)

  (defun undo-1 ()
    'undo-1)

  (defun undo-2 ()
    'undo-2)

  (setq test-hookable-obj (hookable-test-class :load-hook '(do-1 do-2)
                                               :teardown-hook '(undo-2 undo-1)))

  (should (equal '(do-1 do-2) (run-load-hook test-hookable-obj)))

  (should (equal '(undo-2 undo-1) (run-teardown-hook test-hookable-obj))))

(provide 'cyanide-hookable-test)
