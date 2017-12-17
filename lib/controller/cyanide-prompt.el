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

(require 'cyanide-get-one-by-slot "lib/controller/cyanide-get-one-by-slot")

(defun cyanide-prompt (prompt-func
                       prompt-str
                       prompt-names
                       collection
                       stringified-slot
                       equality-func
                       &optional
                       predicate
                       require-match
                       initial-input
                       hist
                       def
                       inherit-input-method)
  "Execute PROMPT-FUNC on obj from
       `cyanide-get-one-by-slot' while prompting the user
       with PROMPT-STR. Possible completions will come from
       PROMPT-NAMES. For documentation on PREDICATE,
       REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
       INHERIT-INPUT-METHOD args see `completing-read'."
  (progn
    (funcall prompt-func (cyanide-get-one-by-slot (completing-read
                                                   prompt-str
                                                   prompt-names
                                                   predicate
                                                   require-match
                                                   initial-input
                                                   hist
                                                   def
                                                   inherit-input-method)
                                                  collection
                                                  stringified-slot
                                                  equality-func))
    nil))

(provide 'cyanide-prompt)
