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
         (cyanide-view
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
                                   ;; ag search configuration
                                   (setq ag-reuse-window-orig ag-reuse-window)
                                   (setq ag-reuse-buffers-orig ag-reuse-buffers)
                                   (setq ag-reuse-window t)
                                   (setq ag-reuse-buffers t)
                                   (other-window 1)
                                   (switch-to-buffer "*Occur*")
                                   (set-window-dedicated-p
                                    (get-buffer-window (current-buffer)) 1)
                                   (other-window 1)
                                   ;; (easy-menu-define
                                   ;;   cyanide-menu
                                   ;;   cyanide-mode-map
                                   ;;   "CyanIDE"
                                   ;;   `("CyanIDE"
                                   ;;     ,(cyanide-menu-item-list-vectorize
                                   ;;       (oref (gethash
                                   ;;              cyanide-current-project
                                   ;;              cyanide-projects) :task-list))
                                   ;;     ["Load Project"
                                   ;;      cyanide-load-project-prompt t]
                                   ;;     ["cyanide-ag-search Project"
                                   ;;      cyanide-ag-search t]
                                   ;;     ["Find in Project"
                                   ;;      cyanide-find-dired t]
                                   ;;     ["Enable View"
                                   ;;      cyanide-enable-view-prompt t]
                                   ;;     ["Disable Current View"
                                   ;;      cyanide-disable-current-view t]))
                                   ))
                       :disable 'cyanide-default-disabler)
         ;; Every cyanide-view object is stored in this hashtable.
         cyanide-views)

(puthash 'cyanide-elisp-view
         (cyanide-view
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
                                   ;; (easy-menu-define
                                   ;;   cyanide-menu
                                   ;;   cyanide-mode-map
                                   ;;   "CyanIDE"
                                   ;;   `("CyanIDE"
                                   ;;     ,(cyanide-menu-item-list-vectorize
                                   ;;       (oref (gethash
                                   ;;              cyanide-current-project
                                   ;;              cyanide-projects) :task-list))
                                   ;;     ["Load Project"
                                   ;;      cyanide-load-project-prompt t]
                                   ;;     ["cyanide-ag-search Project"
                                   ;;      cyanide-ag-search t]
                                   ;;     ["Find in Project"
                                   ;;      cyanide-find-dired t]
                                   ;;     ["Enable View"
                                   ;;      cyanide-enable-view-prompt t]
                                   ;;     ["Disable Current View"
                                   ;;      cyanide-disable-current-view t]))
                                   ))
                       :disable 'cyanide-default-disabler)
         cyanide-views)

(provide 'cyanide-views)
