(defun associative-list-add (lst key val)
  (cons (cons key val) lst))

(defun associative-list-get (lst key)
  (some (lambda (elm)
          (when (eq (car elm) key)
            (cdr elm)))
        lst))

(defun property-list-add (lst key val)
  (append (list key val) lst))

(defun property-list-get (lst key)
  (second (member key lst)))

(defun binary-tree-add (tree key val)
  (if tree
      (if (string< key (caar tree))
          (list (car tree) (binary-tree-add (second tree) key val) (third tree))
          (list (car tree) (second tree) (binary-tree-add (third tree) key val)))
      (list (cons key val) nil nil)))

(defun binary-tree-get (tree key)
  (when tree
    (if (eq (caar tree) key)
        (cdar tree)
        (if (string< key (caar tree))
            (binary-tree-get (second tree) key)
            (binary-tree-get (third tree) key)))))


