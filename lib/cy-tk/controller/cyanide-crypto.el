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

(defvar cyanide-pw-use-salt-hashing nil
  "If non-nil, cyanide will salt passwords a number of times equal to
  `cyanide-pw-salt-iterations' and will use
  `cyanide-pw-use-salt-hashing' as a salt string.")

(defvar cyanide-pw-salt-iterations 0 "Number of times for for CyanIDE to hash
  and salt passwords. Must be set as a positive nonzero integer in order for
  CyanIDE to apply hashing to passwords via `cyanide-pw-use-salt-hashing'.")

(defvar cyanide-default-salt-hash-func 'cyanide-sha512sum)

(defun cyanide-sha1sum (val)
  "Calculate the sha1sum of value VAL."
  (car
   (split-string
    (shell-command-to-string (format "echo '%s' |sha256sum" val)) "  -\n")))

(defun cyanide-sha256sum (val)
  "Calculate the sha256sum of value VAL."
  (car
   (split-string
    (shell-command-to-string (format "echo '%s' |sha256sum" val)) "  -\n")))

(defun cyanide-sha512sum (val)
  "Calculate the sha512sum of value VAL."
  (car
   (split-string
    (shell-command-to-string (format "echo '%s' |sha512sum" val)) "  -\n")))

(defun cyanide-salted-hashed-value (val salt i &optional hash-func)
  "Apply HASH-FUNC to SALT+VAL a number of times equal to I. If HASH-FUNC is
   nil, use `cyanide-default-salt-hash-func'"
  (let ((hash (if hash-func hash-func cyanide-default-salt-hash-func)))
    (assert (eq 'integer (type-of i))
            nil (concat "when cyanide-pw-use-salt-hashing, "
                        "cyanide-pw-salt-iterations should be a positive "
                        "integer"))
    (if (> i 0)
        (cyanide-salted-hashed-value
         (funcall hash (format "%s%s" salt val)) salt (- i 1) hash)
      (format "%s%s" salt val))))

(defun cyanide-pw (&optional salt i hash-func)
  "Prompt user for password twice. If passwords do not match, return 'MISMATCH.

   else if SALT is non-nil, return the password hashed and salted for I
   iterations.

   else if SALT is nil, return the password.

   When hashing, use HASH-FUNC if HASH-FUNC is non-nil, otherwise use
   `cyanide-default-salt-hash-func'"
  (interactive)
  (let ((pw1 (read-passwd "symmetric gpg password: "))
        (pw2 (read-passwd "repeat password: ")))
    (if (equal pw1 pw2)
        (if salt
            (cyanide-salted-hashed-value pw1 salt i hash-func)
          pw1) ; else use literal password
      'MISMATCH))) ; else passwords did not match

(provide 'cyanide-crypto)
