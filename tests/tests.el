(defun foccur-case-sensitive-test (func)
  (let ((failed ""))
    (progn
      (let ((case-fold-search t))
        (if (funcall func "asdf")
            (concat
             failed
             "foccur-case-sensitive-test [FAILED]: "
             "False positive with case-fold-search t "
             "and lowercase arg.\n")))
      (let ((case-fold-search nil))
        (if (not (funcall func "asdf"))
            (concat
             failed
             "foccur-case-sensitive-test [FAILED]: "
             "False negative with case-fold-search nil "
             "and lowercase arg.\n")))
      (let ((case-fold-search t))
        (if (not (funcall func "asdF"))
            (concat
             failed
             "foccur-case-sensitive-test [FAILED]: "
             "False negative with case-fold-search t"
             "and uppercase arg.\n")))
      (let ((case-fold-search nil))
        (if (not (funcall func "asdF"))
            (concat
             failed
             "foccur-case-sensitive-test [FAILED]: "
             "False negative with case-fold-search nil "
             "and uppercase arg.\n")))
      failed)))

(defun run-tests ()
  (foccur-case-sensitive-test 'foccur-case-sensitive))
(run-tests)
