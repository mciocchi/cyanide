;; This file is part of Foccur.
;;
;; Foccur is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Foccur is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Foccur.  If not, see <http://www.gnu.org/licenses/>.

;; Foccur: fast/find occur by Matt Ciocchi.
;;
;; This lib provides string-match with jump-to-definition for searching code.
;; The end goal of foccur is to provide a string-match solution that provides
;; an interface familar to users of Multi-Occur which does not require a long
;; initial load-time. It does this by parsing the output of find but avoiding
;; the use of find-dired and grep-find modes, which suffer from clunky UI and
;; annoying params.
;;
;; Multi-Occur is very good, but requires all buffers that it searches to be
;; loaded into memory and indexed. This requires a lot of initial CPU cycles,
;; which can result in a very long initial load time for large projects.
;; For that use case, Foccur should be more efficient than Multi-Occur.
;;
;; Eventually this module could be replaced by an even faster Multi-Occur with
;; persistent cacheing and indexing, but the current solution has the advantage
;; of simplicity and works pretty well for now.

(require 'cyanide-buffer-excursion)

(defvar foccur-verbose nil
  "non-nil if foccur should use verbose logging.")

;; Case sensitive if search string has caps, or if case-fold-search is nil.
(defun foccur (dirs find-regexp grep-regexp &optional nlines)
  (interactive "Ddirs: \nMfind-regexp: \nMgrep-regexp: \n")
  (foccur-1 dirs find-regexp grep-regexp nlines))

(defun foccur-1 (dirs find-regexp grep-regexp &optional nlines)
  (foccur-2 dirs
            find-regexp
            grep-regexp
            'foccur-cmd-builder
            'foccur-parse-buffer
            'foccur-generate-buffer nlines))

(defun foccur-2 (dirs find-regexp grep-regexp cmd-builder parser generator
                      &optional nlines)
  (let ((cmd (funcall cmd-builder dirs find-regexp grep-regexp nlines))
        (error-buffer (or (bound-and-true-p foccur-default-error-buffer)
                          "*Foccur Errors*"))
        (input-all-frames (or (bound-and-true-p foccur-default-input-all-frames)
                              t))
        (output-all-frames (or
                            (bound-and-true-p foccur-default-input-all-frames)
                            t))
        (input-buffer (or (bound-and-true-p foccur-default-input-buffer)
                          "*Foccur Shell Command*"))
        (output-buffer (or (bound-and-true-p foccur-default-output-buffer)
                           "*Foccur Shell Command*")))
    (funcall generator cmd output-buffer error-buffer)
    (funcall parser input-buffer input-all-frames output-all-frames)))

(defun foccur-cmd-builder (dirs find-regexp grep-regexp &optional nlines)
  (let ((grep-case-sensitive-arg
         (if (foccur-case-sensitive grep-regexp) "" "i"))
        (find-case-sensitive-arg
         (if (foccur-case-sensitive find-regexp) "" "i")))
    (let ((error-buffer (or (bound-and-true-p foccur-default-error-buffer)
                            "*Foccur Errors*"))
          (input-all-frames
           (or (bound-and-true-p foccur-default-input-all-frames)
               t))
          (output-all-frames
           (or (bound-and-true-p foccur-default-input-all-frames)
               t))
          (input-buffer (or (bound-and-true-p foccur-default-input-buffer)
                            "*Foccur Shell Command*"))
          (output-buffer (or (bound-and-true-p foccur-default-output-buffer)
                             "*Foccur Shell Command*")))
      (let ((cmd
             (concat
              (or (bound-and-true-p foccur-default-find-cmd) "find ")
              dirs
              (or (bound-and-true-p foccur-default-find-grep-args)
                  (concat " -"
                          find-case-sensitive-arg
                          "name "
                          find-regexp
                          " "
                          "-type f "
                          "-exec grep -"
                          grep-case-sensitive-arg
                          "Hn "
                          grep-regexp
                          " {} +")))))
        (progn
          (foccur-message (concat "foccur-cmd-builder built cmd " cmd))
          (if (not (equal output-buffer input-buffer))
              (message
               (concat "output-buffer " output-buffer " and input-buffer "
                       input-buffer " are not the same. foccur may not "
                       "work!")))
          cmd))))) ;; return

(defun foccur-case-sensitive (re)
  "Respect emacs defaults and determine whether foccur
   should attempt to match with case-sensitivity.

   For more information, see `foccur-case-sensitive-test'"
  (or (let ((case-fold-search nil))
        (string-match "[$.*[:upper:].*^]" re))
      (not case-fold-search)))

(defun foccur-parse-buffer (input-buffer &optional
                                         input-all-frames
                                         output-all-frames)
  (let ((worker (lambda ()
                  (progn
                    (org-mode)
                    (erase-buffer)
                    (foccur-message
                     (concat "foccur-parse-buffer executing "
                             "foccur-parse-buffer-1 on input-buffer "
                             input-buffer))
                    (foccur-parse-buffer-1 input-buffer input-all-frames)))))
    (cyanide-buffer-excursion
     worker
     (or "*Foccur*" foccur-buffer-name)
     output-all-frames)))

(defun foccur-parse-buffer-1 (input-buffer &optional all-frames)
  (progn
    (foccur-message (concat
                     "foccur-parse-buffer-1 executing foccur-worker on "
                     "input-buffer " input-buffer))
    (mapcar 'foccur-worker
            (split-string
             (substring-no-properties
              (foccur-buffer-string input-buffer all-frames)) "\n"))))

(defun foccur-generate-buffer (cmd &optional output-buffer error-buffer)
  (foccur-message (concat "foccur-generate-buffer executing Foccur cmd: " cmd
                          "\n"
                          "foccur-generate-buffer executing cmd with "
                          "output-buffer " output-buffer
                          "\n"
                          "foccur-generate-buffer executing cmd with "
                          "error-buffer " error-buffer))
  (shell-command cmd output-buffer error-buffer))

(defun foccur-output (long-name linum base-name res)
  (if (and long-name base-name linum res)
      (progn
        (insert (concat "[[" long-name "::" linum "]["linum ":" base-name ": "
                        (replace-regexp-in-string "^[[:space:]]+" "" res)
                        "]]" "\n")))))

(defun foccur-buffer-string (buffer &optional all-frames)
  "Call `buffer-string' on target buffer. If all-frames is
   true, seek buffer across all frames. Return buffer-string"
  (cyanide-buffer-excursion 'buffer-string buffer all-frames))

(defun foccur-worker (grep-str match-num)
  "Implementation of worker logic for foccur. This worker
   gets mapped to every line of a buffer with grep -Hn
   output."
  (progn
    (foccur-message (concat "Executing foccur-worker with grep-str " grep-str))
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
                  (progn
                    (foccur-message
                     (concat "foccur-worker executing foccur-output on "
                             "long-name: " long-name " "
                             "base-name: " base-name " "
                             "res: " res " "
                             "linum: " linum))
                    (foccur-output
                     long-name linum base-name res)))))))))

(defun foccur-message (str)
  "Message str if foccur-verbose is non-nil. "
  (if foccur-verbose
      (message str)))

(provide 'foccur)
