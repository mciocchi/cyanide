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

(cyanide-view-builder
 '(:id 'cyanide-elisp-view
   :display-name "cyanide-elisp-view"
   :disable 'cyanide-default-disabler
   :enable (lambda nil
             (progn
               (when cyanide-current-view
                 (call-interactively 'cyanide-disable-current-view))
               (setq cyanide-current-view
                     'cyanide-elisp-view)
               (when cyanide-current-project
                 (setq frame-title-format
                       (oref
                        (cyanide-get-one-by-slot cyanide-current-project
                                                 cyanide-project-collection
                                                 ":id"
                                                 'eq)
                        display-name)))
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
               ;; ag search configuration
               (setq ag-reuse-window-orig ag-reuse-window)
               (setq ag-reuse-buffers-orig ag-reuse-buffers)
               (setq ag-reuse-window t)
               (setq ag-reuse-buffers t)
               (other-window 2)
               (switch-to-buffer "*Occur*")
               (set-window-dedicated-p
                (get-buffer-window (current-buffer)) 1)
               (other-window 2)
               (switch-to-buffer "*ielm*")
               (set-window-dedicated-p
                (get-buffer-window (current-buffer)) 1)
               (ielm)
               (other-window 2)
               (if cyanide-current-project
                   (cyanide-render-menu-with-tasks cyanide-current-project
                                                   'cyanide-default-menu-with-tasks)
                 (cyanide-menu-render (cyanide-get-one-by-slot 'cyanide-default-menu
                                                               cyanide-menu-item-collection
                                                               ":id"
                                                               'eq)
                                      'cyanide-default-menu
                                      cyanide-mode-map))))))

(provide 'cyanide-elisp-view)
