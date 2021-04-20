(defun rotate-list (xs &optional opposite)
  "A pure function that rotates the list."
  (if opposite
      (concatenate 'list (cdr xs) (list (car xs)))
      (concatenate 'list (last xs) (butlast xs))))

(defun permute-at (ring n)
  "A pure function that permutes the nth and the (n+1)th element
of RING."
  ;; ((0 1 2 3 4 5) 3) => (0 1 2 4 3 5)
  ;; ((0 1 2 3 4 5) 5) => (5 1 2 3 4 0)
  (when (and (listp ring)
             (not (null ring)))
    (let* ((l (length ring))
           (n (mod n l)))
      (when (>= l 2)
        (if (= n (- l 1))
            (concatenate 'list
                         (last ring)
                         (butlast (cdr ring))
                         (list (car ring)))
            (concatenate 'list
                         (subseq ring 0 n)
                         (list (nth (mod (+ n 1) l) ring))
                         (list (nth (mod (+ n 0) l) ring))
                         (subseq ring (+ n 2))))))))
