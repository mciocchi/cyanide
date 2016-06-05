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

(require 'ag)
(require 'cyanide-get-one-by-slot)
(require 'cyanide-globals)

;; Source: https://github.com/Wilfred/ag.el/blob/master/ag.el
;; Tiny alteration on ag/search from Wilfred's ag.el to make it
;; cyanide-project-aware.
(cl-defun cyanide-ag-search (string
                             &key
                             (regexp nil)
                             (file-regex nil)
                             (file-type nil))
  "Run ag searching for the STRING given in DIRECTORY.
       If REGEXP is non-nil, treat STRING as a regular expression."
  (let ((arguments ag-arguments)
        (shell-command-switch "-c"))
    (unless regexp
      (setq arguments (cons "--literal" arguments)))
    (if ag-highlight-search
        (setq arguments
              (append '("--color" "--color-match" "30;43") arguments))
      (setq arguments
            (append '("--nocolor") arguments)))
    (when (char-or-string-p file-regex)
      (setq arguments
            (append `("--file-search-regex" ,file-regex) arguments)))
    (when file-type
      (setq arguments (cons (format "--%s" file-type) arguments)))
    (when ag-ignore-list
      (setq arguments (append (ag/format-ignore ag-ignore-list) arguments)))
    (unless (file-exists-p default-directory)
      (error "No such directory %s" default-directory))
    (let ((command-string
           (mapconcat #'shell-quote-argument
                      (append
                       (list ag-executable)
                       arguments
                       (list string cyanide-current-project))
                      " ")))
      ;; If we're called with a prefix, let the user modify
      ;; the command before
      ;; running it. Typically this means they want to pass
      ;; additional arguments.
      (when current-prefix-arg
        ;; Make a space in the command-string for the user
        ;; to enter more arguments.
        (setq command-string
              (ag/replace-first command-string " -- " "  -- "))
        ;; Prompt for the command.
        (let ((adjusted-point
               (- (length command-string) (length string) 5)))
          (setq command-string
                (read-from-minibuffer "ag command: "
                                      (cons
                                       command-string adjusted-point)))))
      ;; Call ag.
      (compilation-start
       command-string
       #'ag-mode
       `(lambda (mode-name) ,(ag/buffer-name string "merp" regexp))))))

(defun cyanide-ag-search (string)
  (interactive (list (ag/read-from-minibuffer "Search string")))
  (if cyanide-current-project
      (let ((directory
             (oref
              (cyanide-get-one-by-slot cyanide-current-project
                                       cyanide-project-collection
                                       ":id"
                                       'eq)
              project-root)))
        (ag/search string directory))
    (error (concat "cyanide-current-project is nil. " ; else
                   "Cannot invoke cyanide-ag-search "
                   "before loading a cyanide-project."))))

(provide 'cyanide-ag-search)
