(provide 'cyanide-sidebar)
;; cyanide sidebar object fields:
;; excluded-buffer-regexps
;; match-regexps
;; call

;;debug This should be set by the view.
;; (setq cyanide-sidebar-search-regexp "\\(defun\\|defmacro\\|defmethod\\)")
(defvar cyanide-sidebar-search-regexp nil
  "This variable controls what shows up in cyanide-sidebar.")

;; Checking (length (frame-list)) is necessary to maintain compatability with
;; make-frame-command. A three-way incompatibility that surfaces when
;; make-frame-command, window-configuration-change-hook, and emacs-lock are used
;; at the same time.
(defun cyanide-sidebar-search ()
  (if cyanide-sidebar
      (if (not (string-match (buffer-name)
                             ".*Occur.*\\|.*Minibuf.*\\|.*Messages.*\\|.*Completions.*\\|.*cyanide-sidebar.*"))
          (if (not (< 1 (length (frame-list))))
              (cyanide-sidebar-search-worker)))))

;; High-level overview of what needs to be done here:
;; change *Occur* buffer to *Occur2*
;; change cyanide-sidebar to *Occur*
;; call cyanide-sidebar-search
;; rename *Occur* cyanide-sidebar
;; rename *Occur2* to *Occur*
;; position cursor back in original buffer
(defun cyanide-sidebar-search-worker ()
  (let ((starting-buffer-name (buffer-name))
        (original-occur-buffer-name (generate-new-buffer-name "*Occur*")))
    (progn
      (seek-window-by-buffer-name "*Occur*")
      (rename-buffer (generate-new-buffer-name "*Occur*"))
      (seek-window-by-buffer-name "cyanide-sidebar")
      (rename-buffer "*Occur*")
      (seek-window-by-buffer-name starting-buffer-name)
      (occur cyanide-sidebar-search-regexp)
      (message nil) ;; immediately blank out annoying occur minibuffer messages.
      (seek-window-by-buffer-name "*Occur*")
      (rename-buffer "cyanide-sidebar")
      (seek-window-by-buffer-name original-occur-buffer-name)
      (rename-buffer "*Occur*")
      (seek-window-by-buffer-name starting-buffer-name))))

(add-hook 'window-configuration-change-hook
          'cyanide-sidebar-search)

(add-hook 'bookmark-after-jump-hook
          'cyanide-sidebar-search)

(add-hook 'occur-mode-find-occurrence-hook
          'cyanide-sidebar-search)

(defun cyanide-sidebar-enable ()
  (setq cyanide-sidebar t))

(defun cyanide-sidebar-disable ()
  (setq cyanide-sidebar nil))

;; Initial state = disabled.
(cyanide-sidebar-disable)

  
