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


(require 'cyanide-view-simple)

(cyanide-view-simple :id 'cyanide-elisp-view
                     :teardown-hook '((lambda nil
                                        (progn
                                          (set-window-dedicated-p ielm-window nil)
                                          (delete-window ielm-window))))
                     :load-hook '((lambda nil
                                    (progn
                                      (split-window-vertically
                                       (* (/ (window-total-height) 10) 9))
                                      (other-window 1)
                                      (setq ielm-window (selected-window))
                                      (switch-to-buffer "*ielm*")
                                      (set-window-dedicated-p ielm-window 1)
                                      (ielm)
                                      (other-window 1)))))

(provide 'cyanide-elisp-view)
