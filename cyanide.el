;;; cyanide.el --- CyanIDE's Yet Another Non-IDE
;;
;; Copyright (C) 2015-2016 Matt Ciocchi <mciocchi@gmail.com>
;;
;; Author: Matt Ciocchi <mciocchi@gmail.com>
;; Created: 06 September 2015
;; Version: 5.0.0
;; Package-Requires: ((ag "0.47"))
;; Keywords: project, window, buffer
;;
;;; License:
;;
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
;;
;;; Commentary:
;;
;; CyanIDE stands for CyanIDE's Yet Another Non-IDE. It provides project-aware
;; configuration, instant search, an object-oriented API, and dozens of helper
;; functions in one convenient toolkit.
;;
;; For screenshots and installation instructions, refer to the README, or
;; read it online at https://github.com/mciocchi/cyanide
;;
;;; Code:
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
  (cyanide-main)                   ; body
  :global t)

(define-globalized-minor-mode global-cyanide-mode cyanide-mode
  (lambda () (cyanide-mode 1)))
(global-cyanide-mode 1)

(provide 'cyanide)
;;; cyanide.el ends here
