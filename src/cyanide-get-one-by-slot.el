(defun cyanide-get-one-by-slot (sym
                                lst
                                stringified-slot
                                equality-func)
  "Return one obj from LST where SYM matches with
       EQUALITY-FUNC the value stored in STRINGIFIED-SLOT.
       Optimized lookup: return the first relevant result
        from the list and stop looking."
  (let ((obj nil)
        (i nil)
        (l lst)
        (slot (intern stringified-slot)))
    (while (and (eq nil obj)
                l)
      (setq i (pop l))
      (when (funcall equality-func (eval `(oref i ,slot)) sym)
        (setq obj i)))
    obj))

(provide 'cyanide-get-one-by-slot)
