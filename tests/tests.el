(defun foccur-case-sensitive-test (func)
  "case | arg  | case-fold-search | func output | test output
        |      |                  |             | (nil = fail)
   -----+------+------------------+-------------+-------------
   1    | asdf | t                | t           | nil
   2    | asdF | t                | nil         | nil
   3    | asdf | nil              | nil         | nil
   4    | asdF | nil              | nil         | nil
   5    | asdf | t                | nil         | t  
   6    | asdF | t                | 3           | t  
   7    | asdf | nil              | t           | t  
   8    | asdF | nil              | 3           | t
"
  (let ((failed ""))
    (progn
      (let ((case-fold-search t))
        ;; fail case 1, pass case 5
        (let ((failed (if (funcall func "asdf")
                          (concat
                           failed
                           "foccur-case-sensitive-test [FAILED]: "
                           "False positive with case-fold-search t "
                           "and lowercase arg.\n"))))
          ;; fail case 2, pass case 6
          (let ((failed (if (not (funcall func "asdF"))
                            (concat
                             failed
                             "foccur-case-sensitive-test [FAILED]: "
                             "False negative with case-fold-search t "
                             "and uppercase arg.\n"))))
            ;; fail case 3, pass case 7
            (let ((case-fold-search nil))
              (let ((failed (if (not (funcall func "asdf"))
                                (concat
                                 failed
                                 "foccur-case-sensitive-test [FAILED]: "
                                 "False negative with case-fold-search nil "
                                 "and lowercase arg.\n"))))
                ;; fail case 4, pass case 8
                (let ((failed (if (not (funcall func "asdF"))
                                  (concat
                                   failed "foccur-case-sensitive-test "
                                   "[FAILED]: False negative with "
                                   "case-fold-search nil and "
                                   "uppercase arg.\n"))))
                  (if failed (print failed)
                    (print "foccur-case-sensitive-test [PASSED]\n")) ;; else
                  (if failed nil ;if failed return false
                    t)))))))))) ;else return true

;; Return true only if all tests pass.
;; New tests need to be added here.
(defun run-tests ()
  (let ((case-sensitive-test-result (foccur-case-sensitive-test
                                     'foccur-case-sensitive)))
    (and case-sensitive-test-result)))

(run-tests)
