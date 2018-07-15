;;; cyanide.el --- CyanIDE's Yet Another Non-IDE
;;
;; Author: Matt Ciocchi <mciocchi@gmail.com>
;; Created: 06 September 2015
;; Version: 6.2.3
;; Keywords: convenience, extensions, files, frames, lisp, maint, matching, processes, tools

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

;;; Commentary:
;;
;; CyanIDE stands for CyanIDE's Yet Another Non-IDE.
;; 
;; The two goals of CyanIDE are:
;;
;; 1) To help beginner emacs users with minimal knowledge of elisp get started
;;    with emacs quickly by providing core functionality which most projects
;;    need.
;;
;; 2) To help intermediate emacs users by providing hooks for them to extend
;;    cyanide projects with additional customizations.
;;
;; 3) To help advanced emacs users by providing a common framework which they
;;    can use to work together to develop modern features with speed and
;;    simplicity.
;;
;; With these goals in mind, CyanIDE offers:
;;
;; * nearly instant project aware search via helm-ag
;;
;; * hooks to run arbitrary elisp at project load time
;;
;; * easy project specific configuration via .cy.el dotfiles
;;
;; * a dead-simple means for users to define project lifecycle tasks (compile,
;;   test, run, etc.)
;;
;; * a way to optionally "pop" into arbitrary buffer and window configurations,
;;   especially at project load time
;;
;; * an extensible API for advanced users to work with projects and artifacts,
;;   implemented in EIEIO CLOS
;;
;; For help getting started, refer to the quick start section of the README, or
;; read it online at https://github.com/mciocchi/cyanide#quick-start

;;; Code:

(require 'cyanide-main "lib/cyanide-main")

(defvar cyanide-mode-map
  (let ((map (make-sparse-keymap)))
    (progn
      (define-key map (kbd "C-c c l") 'cyanide-load-project-prompt)
      (define-key map (kbd "C-c c d") 'cyanide-disable-current-view)
      (define-key map (kbd "C-c c v") 'cyanide-enable-view-prompt)
      (define-key map (kbd "C-c c t") 'cyanide-task-prompt)
      (define-key map (kbd "C-c c a") (lambda ()
                                        (interactive)
                                        (funcall cyanide-keyword-search-function)))
      (define-key map (kbd "C-c c f") (lambda ()
                                        (interactive)
                                        (funcall cyanide-find-file-function)))
      (define-key map (kbd "C-c c o") (lambda ()
                                        (interactive)
                                        (funcall cyanide-occur-function))))
    map)
  "Keybindings for CyanIDE.")

(define-minor-mode cyanide-mode
  "CyanIDE's Yet Another Non-IDE"  ; docstring
  nil                              ; init-value
  " cyanide "                      ; lighter
  :keymap cyanide-mode-map         ; keymap
  (cyanide-main)                   ; body
  :require 'cyanide
  :global t)

(define-globalized-minor-mode global-cyanide-mode cyanide-mode
  (lambda () (cyanide-mode 1)))
(global-cyanide-mode 1)

(provide 'cyanide)
;;; cyanide.el ends here
