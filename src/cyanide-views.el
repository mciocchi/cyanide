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

(puthash 'cyanide-default-view
         (cyanide-view "cyanide-default-view"
                       :display-name "cyanide-default-view"
                       :enable (lambda nil
                                 (progn
                                   (setq cyanide-current-view
                                         'cyanide-default-view)
                                   ;; Tear down any other windows first.
                                   (delete-other-windows)
                                   (setq frame-title-format
                                         (oref (gethash cyanide-current-project
                                                        cyanide-projects)
                                               display-name))
                                   ;; Prevent annoying emacs habit of splitting
                                   ;; windows without prompting from the user.
                                   ;; Remember original values so that they can
                                   ;; be restored when the view is torn down.
                                   (setq split-height-threshold-orig
                                         split-height-threshold)
                                   (setq split-width-threshold-orig
                                         split-width-threshold)
                                   (setq split-height-threshold 80)
                                   (setq split-width-threshold 9999)
                                   ;; Set up new window geometry.
                                   (split-window-vertically
                                    (* (/ (window-total-height) 10) 9))
                                   (split-window-vertically
                                    (* (/ (window-total-height) 10) 9))
                                   (split-window-horizontally
                                    (* (/ (window-total-width) 10) 2))
                                   ;; ag search configuration
                                   (setq ag-reuse-window-orig ag-reuse-window)
                                   (setq ag-reuse-buffers-orig ag-reuse-buffers)
                                   (setq ag-reuse-window t)
                                   (setq ag-reuse-buffers t)
                                   ;; Horiz. bottom window for build/deployment
                                   (other-window 2)
                                   (switch-to-buffer "*Async Shell Command*")
                                   (set-window-dedicated-p
                                    (get-buffer-window (current-buffer)) 1)
                                   ;; Horiz. bottommost window for moccur search
                                   (other-window 1)
                                   (switch-to-buffer "*Occur*")
                                   (set-window-dedicated-p
                                    (get-buffer-window (current-buffer)) 1)
                                   (emacs-lock-mode 'kill)
                                   ;; Vert. panel with function/class defs
                                   (other-window 1)
                                   (switch-to-buffer "cyanide-panel")
                                   (setq cyanide-panel-search-regexp
                                         (concat "\\(defun\\|"
                                                 "defmacro\\|"
                                                 "defmethod\\|"
                                                 "defclass\\|"
                                                 "class \\|"
                                                 "interface .*\{\\|"
                                                 "def \\|"
                                                 "defn \\|"
                                                 "fn \\|"
                                                 "function .*{\\|"
                                                 "sub .*\{\\|"
                                                 "defn \\|"
                                                 "public .*\{\\|"
                                                 "private .*\{\\|"
                                                 "protected .*\{\\|"
                                                 "^*+ \\)"))
                                   (set-window-dedicated-p
                                    (get-buffer-window (current-buffer)) 1)
                                   (emacs-lock-mode 'kill)
                                   ;; Main browsing buffer
                                   (cyanide-panel-enable)
                                   (other-window 1)))
                       :disable 'cyanide-default-disabler)
         ;; Every cyanide-view object is stored in this hashtable.
         cyanide-views)

(puthash 'cyanide-elisp-view
         (cyanide-view "cyanide-elisp-view"
                       :display-name "cyanide-elisp-view"
                       :enable (lambda nil
                                 (progn
                                   (setq cyanide-current-view
                                         'cyanide-elisp-view)
                                   (delete-other-windows)
                                   (setq frame-title-format
                                         (oref (gethash cyanide-current-project
                                                        cyanide-projects)
                                               display-name))
                                   (setq split-height-threshold-orig
                                         split-height-threshold)
                                   (setq split-width-threshold-orig
                                         split-width-threshold)
                                   (setq split-height-threshold 80)
                                   (setq split-width-threshold 9999)
                                   (split-window-vertically
                                    (* (/ (window-total-height) 10) 9))
                                   (split-window-vertically
                                    (* (/ (window-total-height) 10) 9))
                                   (split-window-horizontally
                                    (* (/ (window-total-width) 10) 2))
                                   ;; ag search configuration
                                   (setq ag-reuse-window-orig ag-reuse-window)
                                   (setq ag-reuse-buffers-orig ag-reuse-buffers)
                                   (setq ag-reuse-window t)
                                   (setq ag-reuse-buffers t)
                                   (other-window 2)
                                   (switch-to-buffer "*ielm*")
                                   (ielm)
                                   (set-window-dedicated-p
                                    (get-buffer-window (current-buffer)) 1)
                                   (other-window 1)
                                   (switch-to-buffer "*Occur*")
                                   (set-window-dedicated-p
                                    (get-buffer-window (current-buffer)) 1)
                                   (emacs-lock-mode 'kill)
                                   (other-window 1)
                                   (switch-to-buffer "cyanide-panel")
                                   (setq cyanide-panel-search-regexp
                                         (concat
                                          "\\(defun\\|"
                                          "defmacro\\|"
                                          "defmethod\\|"
                                          "defclass\\|"
                                          "^*+ \\)"))
                                   (emacs-lock-mode 'kill)
                                   (set-window-dedicated-p
                                    (get-buffer-window (current-buffer)) 1)
                                   (cyanide-panel-enable)
                                   (other-window 1)))
                       :disable 'cyanide-default-disabler)
         cyanide-views)

(provide 'cyanide-views)
