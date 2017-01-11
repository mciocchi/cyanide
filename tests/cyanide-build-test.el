(setq coll (makehash 'eq))

(setq test-collection
      (cyanide-hash-collection :id 'test-collection :implementation coll))

(cyanide-build-1 :constructor cyanide-project
                 :constructor-args '(:id 'test-project1
                                         :display-name "test project1"
                                         :collection test-collection))

(cyanide-build cyanide-project
               :id 'test-project2
               :display-name "test project2"
               :collection test-collection)
