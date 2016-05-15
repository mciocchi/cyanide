(defvar legacy-tests '(cyanide-case-sensitive-test
                       ; fail-test
                       cyanide-edges-test
                       cyanide-frame-test)
  "All new tests need to be mapped here.")

(defun cyanide-test-executor (tests)
  (mapcar 'cyanide-test-executor-1 tests))

(defun cyanide-test-executor-1 (test)
    (let ((test-func (pop test))
          (test-arg (pop test))
          (true-func (pop test))
          (false-func (pop test)))
      (let ((test-name (format "%s" test-func)))
        (print (concat "cyanide-test-executor: " test-name))
        (let ((test-output (funcall test-func test-arg)))
          (if test-output
              (funcall true-func test-name)
            (funcall false-func test-name))))))

(defun proto-test-func (p-test-obj)
  (eq 1 (oref p-test-obj :number)))

(defclass proto-test-class ()
  ((number :initarg :number)))

(setq proto-test-obj
      (proto-test-class :number 1))

(defun cyanide-true-func (test-name)
  (progn
    (print (concat test-name " [PASSED]"))
    t))

(defun cyanide-false-func (test-name)
  (progn
    (print (concat test-name " [FAILED]"))
    nil))

(defun cyanide-tree-test (tree)
  (eq 'cyanide-tree
      (eieio-object-class tree)))

(defun cyanide-get-sub-treenodes-test (root-obj)
  (and
   (listp (cyanide-get-sub-treenodes root-obj))
   (eq 'cyanide-tree
       (eieio-object-class root-obj))
   (child-of-class-p (eieio-object-class
                      (car (cyanide-get-sub-treenodes root-obj)))
                     'cyanide-treenode)))

(defun cyanide-tree-tests (arg)
  (let ((root-obj (cyanide-tree-builder
                   (window-tree)
                   (abs (random))
                   (cyanide-frame-builder (selected-frame))
                   nil)))
    (let ((sub-obj (cyanide-tree-builder
                    (window-tree)
                    (abs (random))
                    (cyanide-frame-builder (selected-frame))
                    nil)))
      (cyanide-add-sub-treenode root-obj sub-obj)
      (cyanide-set-super-tree sub-obj root-obj)
      ; return true only if all tests do.
      (eval (cons 'and (cyanide-test-executor `((cyanide-tree-test
                                                 ,root-obj
                                                 cyanide-true-func
                                                 cyanide-false-func)

                                                (cyanide-get-sub-treenodes-test
                                                 ,root-obj
                                                 cyanide-true-func
                                                 cyanide-false-func)

                                                (cyanide-super-treenode-test
                                                 ,sub-obj
                                                 cyanide-true-func
                                                 cyanide-false-func)

                                                (cyanide-super-treenode-test-2
                                                 '(,sub-obj ,root-obj)
                                                 cyanide-true-func
                                                 cyanide-false-func)

                                                (cyanide-sub-treenode-test
                                                 '(,sub-obj ,root-obj)
                                                 cyanide-true-func
                                                 cyanide-false-func))))))))

(defun cyanide-super-treenode-test (tree-obj)
  (eq (eieio-object-class (cyanide-get-super-tree tree-obj))
      'cyanide-tree))

(defun cyanide-super-treenode-test-2 (args)
  "args: '(,sub-obj ,root-obj)"
  (progn (eq
                 (cyanide-get-super-tree (car (car (cdr args))))
                 (car (cdr (car (cdr args)))))))

(defun cyanide-sub-treenode-test (args)
  "args: '(,sub-obj ,root-obj)"
  (progn (eq
                 (car (car (cdr args)))
                 (car (cyanide-get-sub-treenodes
                       (car (cdr (car (cdr args)))))))))

(setq cyanide-menu-function-obj-1
      (cyanide-menu-function
       :func (lambda () (interactive) (print "executing mvn package"))
       :display-name "mvn package"))

(setq cyanide-menu-function-obj-2
      (cyanide-menu-function
       :func (lambda () (interactive) (print "executing mvn clean"))
       :display-name "mvn clean"))

(setq cyanide-menu-obj
      (cyanide-menu :display-name "CyanIDE Test Menu"
                    :members `(,cyanide-menu-function-obj-1
                               ,cyanide-menu-function-obj-2)))

(defun cyanide-menu-function-test (menu-function-obj)
  (eq 'cyanide-menu-function
      (eieio-object-class menu-function-obj)))

;; Note: check menu bar to make sure this renders in a visually appealing way.
(defun cyanide-menu-test (menu-obj)
  (cyanide-menu-render menu-obj 'cyanide-test-menu cyanide-mode-map)
  (eq 'cyanide-menu (eieio-object-class menu-obj)))

(setq tests
      `((proto-test-func
         ,(proto-test-class :number 1)
         cyanide-true-func
         cyanide-false-func)

        (cyanide-tree-tests
         nil
         cyanide-true-func
         cyanide-false-func)

        (cyanide-menu-function-test
         ,cyanide-menu-function-obj-1
         cyanide-true-func
         cyanide-false-func)

        (cyanide-menu-test
         ,cyanide-menu-obj
         cyanide-true-func
         cyanide-false-func)))

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
                         legacy-tests)))
    (if (eval (cons 'and results))
        "all tests passed."
      "some tests failed!")
    (cyanide-test-executor tests)))
