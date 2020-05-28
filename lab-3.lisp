(defun @map (func lst)
  (labels ((%map (lst)
             (when lst
               (cons (funcall func (car lst))
                     (%map (cdr lst))))))
    (%map lst)))

(defun filter (func lst)
  (labels ((%filter (lst)
             (when lst
               (let ((head (car lst))
                     (tail (cdr lst)))
                 (if (funcall func head)
                     (cons head (%filter tail))
                     (%filter tail))))))
    (%filter lst)))

(defun fold (func lst &optional (init nil init-p))
  (if lst
      (fold func (cdr lst) (if init-p
                               (funcall func init (car lst))
                               (car lst)))
      init))
