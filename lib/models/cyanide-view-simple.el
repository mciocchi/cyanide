(require 'cyanide-view)

(cl-defun cyanide-view-simple (&key id
                                    display-name
                                    load-hook
                                    teardown-hook
                                    description)
  "Wrapper for `cyanide-view' to automatically handle some common load-hook and
teardown-hook tasks."
  (let ((pre-configured-load-hook '((lambda ()
                                      (progn
                                        (setq frame-title-format
                                              (cyanide-project-and-views-frame-title))
                                        (if (bound-and-true-p cyanide-current-project)
                                            (cyanide-render-menu-with-tasks cyanide-current-project
                                                                            'cyanide-default-menu-with-tasks)
                                          (cyanide-menu-render (cyanide-get-one-by-slot 'cyanide-default-menu
                                                                                        cyanide-menu-item-collection
                                                                                        ":id"
                                                                                        'eq)
                                                               'cyanide-default-menu
                                                               cyanide-mode-map))
                                        (cd-proj-root)))))
        (pre-configured-teardown-hook '((lambda ()
                                          (progn
                                            (setq frame-title-format
                                                  (cyanide-project-and-views-frame-title)))))))

    (cyanide-view :id id
                  :display-name display-name
                  :load-hook (append pre-configured-load-hook
                                     load-hook)
                  :teardown-hook (append pre-configured-teardown-hook
                                         teardown-hook)
                  :description description)))

(provide 'cyanide-view-simple)
