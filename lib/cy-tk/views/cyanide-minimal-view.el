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


(cyanide-view :id 'cyanide-minimal-view
              :display-name "cyanide-minimal-view"
              :disable 'cyanide-default-disabler
              :enable (lambda nil
                        (progn
                          (when cyanide-current-view
                            (call-interactively 'cyanide-disable-current-view))
                          (setq cyanide-current-view 'cyanide-minimal-view)
                          (when cyanide-current-project
                            (setq frame-title-format
                                  (oref
                                   (cyanide-get-one-by-slot cyanide-current-project
                                                            cyanide-project-collection
                                                            ":id"
                                                            'eq)
                                   display-name)))
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
                          (if cyanide-current-project
                              (cyanide-render-menu-with-tasks cyanide-current-project
                                                              'cyanide-default-menu-with-tasks)
                            (cyanide-menu-render (cyanide-get-one-by-slot 'cyanide-default-menu
                                                                          cyanide-menu-item-collection
                                                                          ":id"
                                                                          'eq)
                                                 'cyanide-default-menu
                                                 cyanide-mode-map)))))

(provide 'cyanide-minimal-view)
