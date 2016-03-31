(defvar tests '(cyanide-case-sensitive-test
              ; fail-test
                cyanide-edges-test
                cyanide-frame-test)
  "All new tests need to be mapped here.")

;; check if test suite reports failiures correctly.
;; (defun fail-test ()
;;   (progn
;;     (print "fail-test [FAILED]")
;;     nil))

(defun cyanide-case-sensitive-test ()
  (cyanide-case-sensitive-test-1 'cyanide-case-sensitive))

(defun cyanide-case-sensitive-test-1 (func)
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
                           "cyanide-case-sensitive-test [FAILED]: "
                           "False positive with case-fold-search t "
                           "and lowercase arg.\n"))))
          ;; fail case 2, pass case 6
          (let ((failed (if (not (funcall func "asdF"))
                            (concat
                             failed
                             "cyanide-case-sensitive-test [FAILED]: "
                             "False negative with case-fold-search t "
                             "and uppercase arg.\n"))))
            ;; fail case 3, pass case 7
            (let ((case-fold-search nil))
              (let ((failed (if (not (funcall func "asdf"))
                                (concat
                                 failed
                                 "cyanide-case-sensitive-test [FAILED]: "
                                 "False negative with case-fold-search nil "
                                 "and lowercase arg.\n"))))
                ;; fail case 4, pass case 8
                (let ((failed (if (not (funcall func "asdF"))
                                  (concat
                                   failed "cyanide-case-sensitive-test "
                                   "[FAILED]: False negative with "
                                   "case-fold-search nil and "
                                   "uppercase arg.\n"))))
                  (if failed (print failed)
                    (print "cyanide-case-sensitive-test [PASSED]\n")) ;; else
                  (if failed nil ; if failed return false
                    t))))))))))  ; else return true

(defun cyanide-edges-test ()
  "Construct cyanide-edges object, check getters and setters."
  (let ((id (cl-gensym))
        (edge-list `(,(abs (random))
                     ,(abs (random))
                     ,(abs (random))
                     ,(abs (random)))))
    (let ((edges (cyanide-edge-builder edge-list)))
      (cyanide-edges-test-1 edges
                            edge-list))))

(defun cyanide-edges-test-1 (edges edge-list)
  (progn
    (cyanide-set-edges edges edge-list)
    (if (equal edge-list (cyanide-get-edges edges))
        (progn
          (print "cyanide-edges-test [PASSED]")
          t) ; if success return t
      (progn
        (print (concat "cyanide-edges-test [FAILED]: "
                       "set value != get value."))
        nil)))) ; else return nil

(defun cyanide-frame-test ()
  "Test constructor, getters, and setters for cyanide-frame."
  (let ((frame (cyanide-frame-builder (selected-frame))))
    (cyanide-set-frame frame (selected-frame))
    (if (eq (selected-frame) (cyanide-get-frame frame))
        (progn
          (print "cyanide-frame-test [PASSED]")
          t)     ; return t if passed.
      (progn
        (print "cyanide-frame-test [FAILED]")
        nil)))) ; else return nil.

(defun run-tests ()
  (let ((results (mapcar 'funcall
                         tests)))
    (if (eval (cons 'and results))
        "all tests passed."
        "some tests failed!")))

(run-tests)
