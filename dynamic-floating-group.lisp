;; TODO
;;
;; (X) make every function default at group = (current-group).
;; make every function check if input group is a dyn-float-group.
;;
;; ( ) Handle the case when windows are moving out and in to the
;; group. Currently, moving a window from dyn-float-group to a
;; tile-group causes error. Also, moving a window from another
;; group to a dyn-float-group causes an error.
;;
;; ( ) Free a window when it is controlled my mouse.
;;
;; ( ) Add a function to print the dyn-order (state) of the
;; (current) group, in order to make the development easier.
;;
;; ( ) Emacs drop-down window.

(in-package #:stumpwm)

(defstruct dyn-order :window :free)
;; (dyn-order-free (make-dyn-order :window 1 :free t))

(defclass dyn-float-group (float-group)
  ((dyn-order :initform nil
              :accessor dyn-float-group-dyn-order)))

(flet ((add-float-window (group window)
         ;; not sure if needed
         (change-class window 'float-window)
         (float-window-align window)
         (group-focus-window group window)))
  (defmethod group-add-window ((group dyn-float-group)
                               window
                               &key &allow-other-keys)
    (add-float-window group window)
    (nconc
     (dyn-float-group-dyn-order group)
     (list (make-dyn-order :window window
                           :free nil)))
    (sync-dyn-order group)
    (re-tile group)))

(defmethod group-delete-window ((group dyn-float-group)
                                (window float-window))
  (declare (ignore window))
  (%float-focus-next group)
  (sync-dyn-order group)
  (re-tile group))

(defun sync-dyn-order (&optional (group (current-group)))
  ;; Expect GROUP to be an instance of dyn-float-group

  ;; If window W is not in the dyn-order, make one for it and
  ;; push into dyn-order.
  (loop for w in (group-windows group)
        do (unless (member w (mapcar #'dyn-order-window
                                     (dyn-float-group-dyn-order group)))
             (push (make-dyn-order :window w :free nil)
                   (dyn-float-group-dyn-order group))))
  ;; If window W+ is not in the list of windows of GROUP, delete
  ;; W+ from the dyn-order.
  (loop for w+ in (dyn-float-group-dyn-order group)
        do (unless (member (dyn-order-window w+) (group-windows group))
             (deletef (dyn-float-group-dyn-order group) w+)))
  ;; Push free windows to the first elements in the list.
  (setf (dyn-float-group-dyn-order group)
        (concatenate 'list
                     (remove-if (lambda (dyno) (equal nil (dyn-order-free dyno)))
                                (dyn-float-group-dyn-order group))
                     (remove-if (lambda (dyno) (equal t (dyn-order-free dyno)))
                                (dyn-float-group-dyn-order group))))
  ;; Let the (group-windows group) respect the order of
  ;; (dyn-float-group-dyn-order group) FIXME Still need to know
  ;; how to trigger stumpwm to reorder the displayed windows with
  ;; respect to its window list.
  (setf (group-windows group)
        (mapcar #'dyn-order-window
                (dyn-float-group-dyn-order group))))

(defun current-window+ (&optional (group (current-group)))
  (let ((gcw (group-current-window group)))
    (find-if (lambda (x)
               (equal gcw (dyn-order-window x)))
             (dyn-float-group-dyn-order group))))

(defun current-window-position (&optional (group (current-group)))
  (position (current-window+ group)
            (dyn-float-group-dyn-order group)
            :test #'equal))

(defun free-all (&optional (group (current-group)))
  ;; alias: un-tile-all
  (loop for w+ in (dyn-float-group-dyn-order group)
        do (setf (dyn-order-free w+) t))
  (re-tile group))

(defun unfree-all (&optional (group (current-group)))
  ;; alias: tile-all
  (loop for w+ in (dyn-float-group-dyn-order group)
        do (setf (dyn-order-free w+) nil))
  (re-tile group))

(defun free-window (&optional (window (current-window))
                      (group (current-group)))
  (loop for w+ in (dyn-float-group-dyn-order group)
        if (equal window (dyn-order-window w+))
          do (setf (dyn-order-free w+) t))
  (re-tile group))

(defun unfree-window (&optional (window (current-window))
                        (group (current-group)))
  (loop for w+ in (dyn-float-group-dyn-order group)
        if (equal window (dyn-order-window w+))
          do (setf (dyn-order-free w+) nil))
  (re-tile group))

(defun unfloating-windows+ (&optional (group (current-group)))
  "Return the list of window+s whose :FREE slot is nil."
  (remove-if-not
   (lambda (w+) (eq (dyn-order-free w+) nil))
   (dyn-float-group-dyn-order group)))

(defun re-tile (&optional (group (current-group)))
  ;; FIXME respect modeline and boarder.. or even gap in the future
  (sync-dyn-order group)
  (let* ((cs (slot-value (current-screen) 'number))
         (sw (xlib:screen-width cs))
         (sh (xlib:screen-height cs))
         (wl (mapcar #'dyn-order-window (unfloating-windows+ group)))
         (N (length wl)))
    (case N
      (0 nil)
      (1 (float-window-move-resize
          (car wl)
          :x 0 :y 0 :width sw :height sh))
      (t (progn
           (float-window-move-resize
            (car wl)
            :x 0 :y 0 :width (round (/ sw 2)) :height sh)
           (loop for k from 1 to (- N 1)
                 do (float-window-move-resize
                     (nth k wl)
                     :x (round (/ sw 2))
                     :y (* (round (/ sh (- N 1)))
                           (- k 1))
                     :width (round (/ sw 2))
                     :height (round (/ sh (- N 1))))))))))

(defun rotate-list (xs &optional opposite)
  "Rotate the list immutably."
  (if opposite
      (concatenate 'list (cdr xs) (list (car xs)))
      (concatenate 'list (last xs) (butlast xs))))

(defun rotate-window-list (&optional (group (current-group)) opposite)
  (let ((dyno (dyn-float-group-dyn-order group)))
    ;; (setf dyno (rotate-list dyno)) ;; TODO Use symbol-microlet instead
    (setf (dyn-float-group-dyn-order group) (rotate-list dyno opposite)) ;; FIXME ugly workaround.
    (re-tile group)))

(defun permute-at (ring n)
  "Pure function that permutes the nth and the (n+1)th element of
RING."
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

(defun permute-window-list (&optional opposite
                              (group (current-group))
                              (n (current-window-position group)))
  (when opposite (setf n (- n 1)))
  (let ((dyno (dyn-float-group-dyn-order group)))
    ;; (setf dyno (rotate-list dyno)) ;; TODO Use symbol-microlet instead
    (setf (dyn-float-group-dyn-order group)
          (permute-at dyno n)) ;; FIXME ugly workaround.
    (re-tile group)))

(defun gnew-dyn-float (name)
  ;; TODO Turn this into a stumpwm command.
  (add-group (current-screen) name :type 'dyn-float-group))

(defun gnew-dyn-float-bg (name)
  ;; TODO Turn this into a stumpwm command.
  (add-group (current-screen) name :type 'dyn-float-group :background t))

;; For testing.
;; (setf test-group (gnew-dyn-float-bg "TEST"))
