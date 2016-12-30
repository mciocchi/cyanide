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

    (defun cyanide-tokenize-window-treenode (node)
      (if (windowp node)
          'window
        (if (booleanp node)
            'split-direction
          (let ((x (car node)))
            (let ((type (type-of x)))
              (if (equal 'window type)
                  'tree                    ; tree without properties
                (if (booleanp x)           ; else if
                    'tree
                  (if (integerp  x)
                      'edges
                    (if (booleanp (car x))
                        'tree
                      (error               ; else error- guard input
                       (concat "Invalid input to "
                               "cyanide-tokenize-window-treenode")))))))))))

    (defclass cyanide-treenode (cyanide-identifiable)
      ((position   :initarg :position
                   :initform -2
                   :type integer
                   :documentation
                   "Position of this cyanide-treenode inside of its
                   super-tree. -2 indicates that this value was not
                   initialized. -1 indicates that this treenode is a root and
                   therefore has no position because it has no super-tree.")
       (super-tree :initarg :super-tree
                   :initform []
                   :type vector
                   :documentation
                   "Object that refers to the super-tree of this treenode.")
       (frame      :initarg :frame
                   :initform []
                   :type vector
                   :documentation
                   "Object to encapsulate emacs frames.")
       (edges      :initarg :edges
                   :initform []
                   :type vector
                   :documentation
                   "Object to encapsulate window edge dimensions.
                    see `window-edges' for more information.")))

    (cl-defmethod cyanide-set-super-tree ((node cyanide-treenode)
                                          super-tree)
      (oset node :super-tree super-tree))

    (cl-defmethod cyanide-get-super-tree ((node cyanide-treenode))
      (oref node :super-tree))

    (cl-defmethod cyanide-set-position ((node cyanide-treenode)
                                        position)
      (oset node :position position))

    (cl-defmethod cyanide-set-frame ((node cyanide-treenode)
                                     frame)
      (oset node :frame frame))

    ;; if no super-tree, node is a root.
    (defun cyanide-parse-treenodes (nodes pos &optional super-tree)
      "Convert entire emacs `window-tree' into
       object-oriented constructs for CyanIDE."
      (when nodes  ; else break out
        (let ((node (car nodes))
              (nodes (cdr nodes))
              (pos (+ 1 pos)))
          (cyanide-parse-treenode node pos super-tree)
          (cyanide-parse-treenodes nodes pos super-tree))))

    (defun cyanide-parse-treenode (node pos &optional super-tree)
      "Convert individual nodes of emacs `window-tree' into
       object-oriented constructs for CyanIDE."
      ;; TO DO: need to pass frame into these constructors also.
      (let ((token (cyanide-tokenize-window-treenode node)))
        (if (eq 'split-direction token)
            (cyanide-parse-split-direction node pos super-tree) ; TO DO
          (if (eq 'edges token)
              (cyanide-parse-edges node pos super-tree)         ; TO DO
            (if (eq 'window token)
                (cyanide-parse-window node pos super-tree)      ; TO DO
              (if (eq 'tree token)
                  (cyanide-parse-tree node pos super-tree)   ; TO DO mostly done
                (error "cyanide-parse-treenode: unrecognized token")))))))

    (defun cyanide-parse-tree (tree pos frame super-tree)
      (cyanide-tree-builder tree pos frame super-tree))

    (defun cyanide-parse-window (tree pos super-tree)
      (cyanide-window-builder window pos super-tree))

    (defclass cyanide-window (cyanide-treenode)
      ((window-number :initarg :window-number
                      :initform 0
                      :type integer)
       (window   :initarg :window
                 :type window
                 :documentation "")
       (buffer   :initarg :buffer
                 :type buffer
                 :documentation ""))
      "CyanIDE class to encapsulate emacs windows.")

    (defun cyanide-window-builder (window pos super-tree)
      "Constructor for `cyanide-window'."
      (let ((position pos)
            (id (cl-gensym))
            (window-number (cyanide-window-number window))
            (buffer (window-buffer))
            (edge-left (car (window-edges window)))
            (edge-top (cadr (window-edges window)))
            (edge-right (car (cddr (window-edges window))))
            (edge-bottom (cadr (cddr (window-edges window)))))
        (let ((edges (cyanide-edge-builder `(,edge-left
                                             ,edge-top
                                             ,edge-right
                                             ,edge-bottom))))
          (let ((frame (cyanide-frame-builder (window-frame window))))
            (let ((win (cyanide-window
                        :window window
                        :id id
                        :position pos
                        :window-number window-number
                        :buffer buffer
                        :frame frame
                        :edges edges)))
              (add-to-list cyanide-treenodes win)
              (cyanide-add-sub-treenode super-tree win)
              (cyanide-set-super-tree win super-tree))))))

    (defclass cyanide-tree (cyanide-treenode)
      ((sub-treenodes :initarg :sub-treenodes
                      :initform '()
                      :type list
                      :documentation
                      "Collection containing sub-treenodes of this node.")
       (split-direction :initarg :split-direction
                        :type boolean))
      "CyanIDE class to encapsulate emacs window-tree.")

    (cl-defmethod cyanide-add-sub-treenode ((super-tree cyanide-tree)
                                            sub-treenode)
      (object-add-to-list super-tree :sub-treenodes sub-treenode))

    (cl-defmethod cyanide-remove-sub-treenode ((super-tree cyanide-tree)
                                               sub-treenode)
      (object-remove-from-list super-tree :sub-treenodes sub-treenode))

    (cl-defmethod cyanide-set-sub-treenodes ((tree cyanide-tree)
                                             (sub-treenodes list))
      (oset tree :sub-treenodes sub-treenodes))

    ;; TO DO    (cl-defmethod cyanide-get-sub-treenodes ((tree cyanide-tree))
    ;; TO DO      (cyanide-get-sub-treenodes tree)
    ;;          should return list instead of object.
    (cl-defmethod cyanide-get-sub-treenodes ((tree cyanide-tree))
      (oref tree :sub-treenodes))

    ;; TO DO:
    ;;       construct tree-obj without sub-treenodes
    ;;       handle root nodes
    (defun cyanide-tree-builder (tree pos frame-obj &optional super-tree)
      "Constructor for `cyanide-tree'. "
      (let ((id (cl-gensym)))
        (let ((tree-obj (cyanide-tree :id id)))
          (cyanide-set-frame tree-obj frame-obj)
          (cyanide-set-position tree-obj pos)
          ;; TO DO
          ;;          (cyanide-parse-treenodes tree pos tree-obj)
          tree-obj))) ; return tree-obj

    (defun cyanide-window-number (&optional win)
      "Return window number of an emacs window type."
      (let ((window-configuration-change-hook nil)
            (original-win (selected-window))
            (f (lambda (wn)
                 (string-to-int
                  (car
                   (split-string
                    (cadr
                     (split-string
                      (format "%s" wn) "window "))
                    " on "))))))
        (if win (funcall f win)                     ; if optional arg
          (funcall f (selected-window)))))          ; else use selected window

    (defun cyanide-window-list ()
      "Return an alist of (window-number . window) for each
       window in the current frame."
      (mapcar
       (lambda (w)
         `(,(cyanide-window-number w)
           ,(selected-window)))
       (window-list)))

    (defclass cyanide-edges ()
      ((id     :initarg :id
               :initform nil
               :type symbol)
       (left   :initarg :left
               :initform -1
               :type integer)
       (top    :initarg :top
               :initform -1
               :type integer)
       (right  :initarg :right
               :initform -1
               :type integer)
       (bottom :initarg :bottom
               :initform -1
               :type integer))
      "CyanIDE class to encapsulate window-edges.")

    (defun cyanide-edge-builder (edge-list)
      "Constructor for `cyanide-edges'."
      (let ((id (cl-gensym)))
        (let ((edge-obj (cyanide-edges :id id)))
          (cyanide-set-edges edge-obj edge-list)
          edge-obj))) ; return edge-obj

    (cl-defmethod cyanide-set-edge ((edges cyanide-edges)
                                    edge-name
                                    value)
      "Set the value of a `cyanide-window' edge by name.
       Valid names are :left :top :right or :bottom."
      (progn
        (when (not (memq edge-name '(:left :top :right :bottom)))
          (error (concat "Invalid edge name: " (format "%s" edge-name))))
        (when (not (eq value (abs value)))
          (error "Window dimensions cannot be negative"))
        (eval `(oset edges ,edge-name value))))

    (cl-defmethod cyanide-get-edge ((edges cyanide-edges)
                                    edge-name)
      "Get the value of a `cyanide-window' edge by name.
       Valid names are :left :top :right or :bottom."
      (progn
        (when (not (memq edge-name '(:left :top :right :bottom)))
          (error (concat "Invalid edge name: " (format "%s" edge-name))))
        (eval `(oref edges ,edge-name))))

    (cl-defmethod cyanide-set-edges ((edges cyanide-edges) new-edge-list)
      "Set multiple `cyanide-window' edge values at the same
       time. This can be used to easily parse
       `window-edges'."
      (progn
        (cyanide-set-edge edges :left (car new-edge-list))
        (cyanide-set-edge edges :top (cadr new-edge-list))
        (cyanide-set-edge edges :right (car (cddr new-edge-list)))
        (cyanide-set-edge edges :bottom (car (last new-edge-list)))))

    (cl-defmethod cyanide-get-edges ((edges cyanide-edges))
      "Get the value of all `cyanide-window' edges in a
       structure identical to `window-edges'."
      `(,(cyanide-get-edge edges :left)
        ,(cyanide-get-edge edges :top)
        ,(cyanide-get-edge edges :right)
        ,(cyanide-get-edge edges :bottom)))

    (defclass cyanide-frame (cyanide-identifiable)
      ((frame :initarg :frame
              :type frame))
      "CyanIDE class to encapsulate emacs frames.")

    (defun cyanide-frame-builder (frame)
      "Constructor for `cyanide-frame'."
      (let ((id (cl-gensym)))
        (cyanide-frame :id id :frame frame)))

    (cl-defmethod cyanide-get-frame ((frame cyanide-frame))
      (oref frame :frame))

    (cl-defmethod cyanide-set-frame ((frame cyanide-frame) value)
      (oset frame :frame value))

(provide 'cyanide-window-utils)
