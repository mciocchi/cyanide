(require 'cyanide-get-one-by-slot)

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
