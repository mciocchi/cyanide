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
(require 'cyanide-view "lib/models/cyanide-view")
(require 'cl)

(ert-deftest cyanide-view-should-construct ()
  (setq test-view (cyanide-view :id 'test-view
                                :display-name "test view"
                                :description "test view description"
                                :load-hook '()
                                :teardown-hook '()))
  (should (bound-and-true-p test-view)))

(defun set-x-to-a ()
  (setq x '(a)))

(defun remove-a-from-x ()
  (setq x (delq 'a x)))

(defun assert-x-empty ()
  (should (equal x '())))

(defun unbind-x ()
  (makunbound 'x))

(ert-deftest cyanide-view-enable-and-disable ()
  (setq x nil)

  (setq test-view-2 (cyanide-view :id 'test-view-2
                                  :display-name "test view 2"
                                  :description "test view 2 description"
                                  :load-hook '(set-x-to-a)
                                  :teardown-hook '(remove-a-from-x
                                                   assert-x-empty
                                                   unbind-x)))

  (enable test-view-2)
  (should (equal x '(a)))
  (disable test-view-2)
  (should (not (boundp 'x))))

(defun add-b-to-x ()
  (push 'b x))

(defun remove-b-from-x ()
  (setq x (delq 'b x)))

(ert-deftest manage-multiple-views-at-once ()
  (setq cyanide-current-views '())
  (setq x '())
  (setq test-view-3 (cyanide-view :id 'test-view-3
                                  :display-name "test view 3"
                                  :description "test view 3 description"
                                  :load-hook '(set-x-to-a)
                                  :teardown-hook '(remove-a-from-x)))

  (setq test-view-4 (cyanide-view :id 'test-view-4
                                  :display-name "test view 4"
                                  :description "test view 4 description"
                                  :load-hook '(add-b-to-x)
                                  :teardown-hook '(remove-b-from-x)))
  (enable test-view-3)
  (should (equal x '(a)))
  (enable test-view-4)
  (should (equal x '(b a)))
  (should (equal '(test-view-4 test-view-3) cyanide-current-views))
  (call-interactively 'cyanide-disable-current-view)
  (should (equal '(test-view-3) cyanide-current-views))
  (should (equal '(a) x))
  (call-interactively 'cyanide-disable-current-view)
  (should (equal '() cyanide-current-views))
  (should (equal '() x))
  (call-interactively 'cyanide-disable-current-view)
  (should (equal '() cyanide-current-views)))

(ert-deftest disable-all-views ()
  (setq cyanide-current-views '())
  (setq test-view-5 (cyanide-view :id 'test-view-5
                                  :display-name "test view 5"
                                  :description "test view 5 description"
                                  :load-hook '(set-x-to-a)
                                  :teardown-hook '(remove-a-from-x)
                                  ))

  (setq test-view-6 (cyanide-view :id 'test-view-6
                                  :display-name "test view 6"
                                  :description "test view 6 description"
                                  :load-hook '(add-b-to-x)
                                  :teardown-hook '(remove-b-from-x)
                                  ))
  (enable test-view-5)
  (enable test-view-6)
  (should (equal '(test-view-6 test-view-5) cyanide-current-views))
  (should (equal '(b a) x))
  (call-interactively 'cyanide-disable-all-views)
  (should (equal '() x))
  (should (equal '() cyanide-current-views)))

(provide 'cyanide-view-test)
