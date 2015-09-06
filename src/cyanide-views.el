1(provide 'cyanide-views)

(puthash 'cyanide-default-view
         (cyanide-view "cyanide-default-view"
                       :display-name "cyanide-default-view"
                       :enable (lambda nil
                                 (progn
                                   (setq cyanide-current-view
                                         'cyanide-default-view)
                                   ;; Tear down any other windows first.
                                   (delete-other-windows)
                                   ;; Set up new window geometry.
                                   (split-window-vertically
                                    (* (/ (window-total-height) 10) 9))
                                   (split-window-vertically
                                    (* (/ (window-total-height) 10) 9))
                                   (split-window-horizontally
                                    (* (/ (window-total-width) 10) 2))
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
                                   ;; Vert. bookmark list- tracebacks/postmortems
                                   (other-window 1)
                                   (switch-to-buffer "*Bookmark List*")
                                   (bookmark-bmenu-list)
                                   (set-window-dedicated-p
                                    (get-buffer-window (current-buffer)) 1)
                                   (add-hook 'window-configuration-change-hook
                                             'cyanide-sidebar-match)
                                   (add-hook 'bookmark-after-jump-hook
                                             'cyanide-sidebar-match)
                                   (add-hook 'occur-mode-find-occurrence-hook
                                             'cyanide-sidebar-match)
                                   ;; Main browsing buffer- square of golden rectangle
                                   (other-window )))
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
                                   (split-window-vertically
                                    (* (/ (window-total-height) 10) 9))
                                   (split-window-vertically
                                    (* (/ (window-total-height) 10) 9))
                                   (split-window-horizontally
                                    (* (/ (window-total-width) 10) 2))
                                   (other-window 2)
                                   (switch-to-buffer "ielm")
                                   (ielm)
                                   (set-window-dedicated-p
                                    (get-buffer-window (current-buffer)) 1)
                                   (other-window 1)
                                   (switch-to-buffer "*Occur*")
                                   (set-window-dedicated-p
                                    (get-buffer-window (current-buffer)) 1)
                                   (emacs-lock-mode 'kill)
                                   (other-window 1)
                                   (switch-to-buffer "*Bookmark List*")
                                   (bookmark-bmenu-list)
                                   (set-window-dedicated-p
                                    (get-buffer-window (current-buffer)) 1)
;;debug                                   (cyanide-sidebar-enable)
                                   (other-window 1)))
                       :disable 'cyanide-default-disabler)
         cyanide-views)
