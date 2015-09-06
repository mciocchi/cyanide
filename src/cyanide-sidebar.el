(provide 'cyanide-sidebar)
;; cyanide sidebar fields:
;; excluded-buffer-regexps
;; match-regexps
;; call

(setq cyanide-sidebar nil)

;; prototyping
(defun cyanide-sidebar-match ()
  (if cyanide-sidebar
      (if (not (string-match (buffer-name)
                             ".*Occur.*\\|.*Minibuf.*\\|.*Messages.*\\|.*Completions.*"))
          (occur "\\(defun\\|defmacro\\|defmethod\\)"))))

(add-hook 'window-configuration-change-hook
          'cyanide-sidebar-match)

(add-hook 'bookmark-after-jump-hook
          'cyanide-sidebar-match)

(add-hook 'occur-mode-find-occurrence-hook
          'cyanide-sidebar-match)

(defun cyanide-sidebar-enable ()
  (setq cyanide-sidebar t))

(defun cyanide-sidebar-disable ()
  (setq cyanide-sidebar nil))
