;; (defun foccur-output-worker (&optional enabled-mode)
;;   (progn
;;     (insert (concat "[[" long-name "::" linum "][" base-name ": "
;;                     (replace-regexp-in-string "^[[:space:]]+" "" res)
;;                     "]]" "\n"))))

;; to do: pass through foccur-buffer-name.
;; to do: need three commands to work on entire buffer instead of one line:
;; mark-whole-buffer delete-forward-char org-mode. How to integrate these into
;; a function that works on lines?
;; option 1: place it outside the worker function. Messy.
;; option 2: use semaphores (better)- can't- it's a map
;; it turns out these functions need these three buffer-level side-effects that
;; ruin the atomicity of the map.
;; Perhaps I can use vars in lexical or global scope as a shitty alternative.
(defun foccur-output (long-name base-name res linum &optional foccur-buffer-name)
  (if (and long-name base-name linum res)
      (progn
        (insert (concat "[[" long-name "::" linum "][" base-name ": "
                        (replace-regexp-in-string "^[[:space:]]+" "" res)
                        "]]" "\n")))))
      

;; this converts grep -Hn output in a buffer into org-mode links.
;; TO DO: this needs to insert into *Foccur* buffer instead of print to stdout
;; note- the end goal of foccur is to provide a faster alternative to Occur.
;; Occur is good, but requires all buffers that it searches to be loaded into
;; memory. This requires a lot of initial CPU cycles, which result in a long
;; initial load time for large projects. When making minor changes to large
;; projects, it is extremely inconvenient for the user to pay the cost of
;; such a large initial load time. In that use case, Foccur should be more
;; convenient than Occur.
;;
;; CyanIDE will eventually be smart enough to infer whether to load the project
;; into memory and use Occur or Foccur, depending upon whether
;; cyanide-proj-file-count is greater than
;; cyanide-load-proj-find-file-threshold.
;;
;; If a project's find-file attribute is explicitly configured for t or nil,
;; instead of the implicit default, 'auto, it will override this logic.
;;
;; <2015-10-12 Mon> This is a good start, but I need to get rid of
;; current-buffer and other potential side-effects.
;; Foccur-output Should be atomic:
;; (foccur-outer str) V -> interactive, window/buffer/process side effects
;;     (async-shell-command (concat foccur-find-cmd foccur-find-args foccur-grep-cmd foccur-grep-args)
;;     if all lowercase- case insensitive grep
;;     else- case sensitive.
;;     (foccur-parse-grep-output str) V
;;         (foccur-transform-grep-line long-name base-name res linum) V
;;     transformed-lines '()
;;     (foccur-put-to-buffer transformed-lines)
(defun foccur-grep-output-current-buffer ()
  (mapcar (lambda (x) (let ((str1 (split-string x ":[0-9]+:")))
                        (let ((long-name (car str1))
                              (res (car (cdr str1))))
                          (if (and long-name res)
                              (let ((linum
                                     (car
                                      (cdr
                                       (split-string
                                        (car
                                         (split-string
                                          (car
                                           (cdr
                                            (split-string x long-name)))
                                          res)) ":"))))
                                    (base-name
                                     (car (last (split-string long-name "/")))))
                                (if (and linum base-name)
                                    (foccur-output
                                     long-name base-name res linum)))))))
          (split-string (substring-no-properties (buffer-string)) "\n")))

(defun foccur-buffer-string (buffer &optional all-frames)
  "Call `buffer-string' on target buffer. If all-frames is
   true, seek buffer across all frames. Return buffer-string"
  (cyanide-buffer-excursion 'buffer-string buffer all-frames))

(defun foccur-worker (grep-str)
  "Implementation of worker logic for foccur. This worker
   gets mapped to every line of a buffer with grep -Hn
   output."
  (let ((str1 (split-string grep-str ":[0-9]+:")))
                        (let ((long-name (car str1))
                              (res (car (cdr str1))))
                          (if (and long-name res)
                              (let ((linum
                                     (car
                                      (cdr
                                       (split-string
                                        (car
                                         (split-string
                                          (car
                                           (cdr
                                            (split-string grep-str long-name)))
                                          res)) ":"))))
                                    (base-name
                                     (car (last (split-string long-name "/")))))
                                (if (and linum base-name)
                                    (foccur-output
                                     long-name base-name res linum)))))))

(defun foccur-parse-buffer (buffer &optional input-all-frames output-all-frames)
  (let ((worker (lambda ()
                  (progn
                    (org-mode)
                    (erase-buffer)
                    (foccur-parse-buffer-1 buffer input-all-frames)))))
        (cyanide-buffer-excursion
         worker
         (or "*Foccur*" foccur-buffer-name)
         output-all-frames)))

(defun foccur-parse-buffer-1 (buffer &optional all-frames)
  (mapcar 'foccur-worker
          (split-string
           (substring-no-properties
            (foccur-buffer-string buffer all-frames)) "\n")))

(defun foccur-generate-buffer (cmd &optional output-buffer error-buffer)
  (shell-command cmd output-buffer error-buffer))

;; case sensitive if search string has caps, or if case-fold-search is nil
(defun foccur (regexp dir &optional nlines)
  (let ((case-sensitive-arg (if (bound-and-true-p case-fold-search-e) "i" "")))
    (let ((cmd (concat
                (or (bound-and-true-p foccur-default-find-cmd) "find") " "
                (or (bound-and-true-p foccur-default-find-args)
                    "-type f -exec grep -Hn" " ")
                (if (bound-and-true-p case-fold-search) "i" ""))))
          (output-buffer (or (bound-and-true-p foccur-default-output-buffer)
                             "*Foccur Shell Command*"))
          (error-buffer (or (bound-and-true-p foccur-default-error-buffer)
                            "*Foccur Errors*"))
          (input-all-frames (or (bound-and-true-p foccur-default-input-all-frames)
                                t))
          (output-all-frames (or (bound-and-true-p foccur-default-output-all-frames)
                                 t)))
    (interactive "DDir: \nMregex: \n")
     (progn
       (foccur-generate-buffer cmd output-buffer error-buffer)
       (foccur-parse-buffer cmd-buffer input-all-frames output-all-frames))))

;; case sensitive if search string has caps, or if case-fold-search is nil
;; generate find cmd from cyanide proj-tree and excludes
;;(defun cyanide-foccur
 ;;          
;; (progn (foccur-generate-buffer (concat "find ~/projects/sellside/trunk"
;;                                        "-not -path '*\.svn*' -type f"
;;                                        "-exec grep -Hni renderqueue {} +")
;;                                (foccur-parse-buffer "*Async Shell Command*")))

(provide 'foccur)
