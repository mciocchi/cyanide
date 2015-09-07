(provide 'cyanide-panel)

(defvar cyanide-panel-search-regexp nil
  "This variable controls what shows up in cyanide-panel.")

;; Checking (length (frame-list)) is necessary to maintain compatability with
;; make-frame-command. A three-way incompatibility that surfaces when
;; make-frame-command, window-configuration-change-hook, and emacs-lock are used
;; at the same time.
(defun cyanide-panel-search ()
  (if cyanide-panel
      (if (not (string-match (buffer-name)
                             ".*Occur.*\\|.*Minibuf.*\\|.*Messages.*\\|.*Completions.*\\|.*cyanide-panel.*"))
          (if (not (< 1 (length (frame-list))))
              (cyanide-panel-search-worker)))))

(defun cyanide-panel-search-worker ()
  (let ((starting-buffer-name (buffer-name))
        (original-occur-buffer-name (generate-new-buffer-name "*Occur*")))
    (progn
      (seek-window-by-buffer-name "*Occur*")
      (rename-buffer (generate-new-buffer-name "*Occur*"))
      (seek-window-by-buffer-name "cyanide-panel")
      (rename-buffer "*Occur*")
      (seek-window-by-buffer-name starting-buffer-name)
      (occur cyanide-panel-search-regexp)
      (message nil) ;; immediately blank out annoying occur minibuffer messages.
      (seek-window-by-buffer-name "*Occur*")
      (rename-buffer "cyanide-panel")
      (seek-window-by-buffer-name original-occur-buffer-name)
      (rename-buffer "*Occur*")
      (seek-window-by-buffer-name starting-buffer-name))))

(add-hook 'window-configuration-change-hook
          'cyanide-panel-search)

(add-hook 'bookmark-after-jump-hook
          'cyanide-panel-search)

(add-hook 'occur-mode-find-occurrence-hook
          'cyanide-panel-search)

(defun cyanide-panel-enable ()
  (setq cyanide-panel t))

(defun cyanide-panel-disable ()
  (setq cyanide-panel nil))

;; Initial state = disabled.
(cyanide-panel-disable)
