(require 'cyanide-project)

(setq coll (makehash 'eq))

(setq test-collection
      (cyanide-hash-collection :id 'test-collection :implementation coll))

(setq test-project (cyanide-project :id 'test-project
                                    :display-name "Test Project"
                                    :collection test-collection))

(cyanide-add-to-collection test-project)

(cyanide-add-to-collection test-collection test-project)
