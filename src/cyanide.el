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

(require 'cyanide-main)

(defvar cyanide-mode-map
  (let ((map (make-sparse-keymap)))
    (progn
      (define-key map (kbd "C-c c l") 'cyanide-load-project-prompt)
      (define-key map (kbd "C-c c d") 'cyanide-disable-current-view)
      (define-key map (kbd "C-c c v") 'cyanide-enable-view-prompt)
      (define-key map (kbd "C-c c t") 'cyanide-task-prompt)
      (define-key map (kbd "C-c c a") 'cyanide-ag-search)
      (define-key map (kbd "C-c c f") 'cyanide-find-dired)) map))

(define-minor-mode cyanide-mode
  "CyanIDE's Yet Another Non-IDE"  ; docstring
  nil                              ; init-value
  " cyanide "                      ; lighter
  :keymap cyanide-mode-map         ; keymap
  (cyanide-main)                    ; body
  :global t)

(define-globalized-minor-mode global-cyanide-mode cyanide-mode
  (lambda () (cyanide-mode 1)))
(global-cyanide-mode 1)

(provide 'cyanide)
