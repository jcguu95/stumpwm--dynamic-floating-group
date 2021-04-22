(in-package #:stumpwm)

;; An augmented window (a window with another piece of info.)
(defstruct win+ :window :free)
;;   access example: (win+-free (make-win+ :window 1 :free t))

;; A dyn-order, or a dynamic order, is a list of win+.
(defclass dyn-float-group (float-group)
  ((dyn-order :initform nil :accessor dyn-float-group-dyn-order)))

(defun dyn-float-group-p (group)
  (eq (type-of group) 'dyn-float-group))

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
     (list (make-win+ :window window :free nil)))
    (re-tile group)))

(defmethod group-delete-window ((group dyn-float-group)
                                (window float-window))
  (declare (ignore window))
  (%float-focus-next group)
  (sync-dyn-order group)
  (re-tile group))

(defmethod group-button-press ((group dyn-float-group) button x y (window float-window))
  ;; Free the window if it's pressed at the boarder or with
  ;; *float-window-modifier*.
  (let ((xwin (window-xwin window)))
    (multiple-value-bind (relx rely same-screen-p child state-mask)
        (xlib:query-pointer (window-parent window))
      (declare (ignore same-screen-p child))
      (when (or
             (< x (xlib:drawable-x xwin))
             (> x (+ (xlib:drawable-width xwin)
                     (xlib:drawable-x xwin)))
             (< y (xlib:drawable-y xwin))
             (> y (+ (xlib:drawable-height xwin)
                     (xlib:drawable-y xwin)))
             (intersection (float-window-modifier)
                           (xlib:make-state-keys state-mask)))
        (free-window window group))))

  (call-next-method))


(defun sync-dyn-order (&optional (group (current-group)))
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      ;; If window W is not in the dyn-order, make one for it and
      ;; push into dyn-order.
      (progn
        (loop for w in (group-windows group)
              do (unless (member w (mapcar #'win+-window
                                           (dyn-float-group-dyn-order group)))
                   (push (make-win+ :window w :free nil)
                         (dyn-float-group-dyn-order group))))
        ;; If window W+ is not in the list of windows of GROUP, delete
        ;; W+ from the dyn-order.
        (loop for w+ in (dyn-float-group-dyn-order group)
              do (unless (member (win+-window w+) (group-windows group))
                   (deletef (dyn-float-group-dyn-order group) w+)))
        ;; Make the free windows on top of the stack.
        (setf (dyn-float-group-dyn-order group)
              (concatenate 'list
                           (remove-if (lambda (dyno) (equal nil (win+-free dyno)))
                                      (dyn-float-group-dyn-order group))
                           (remove-if (lambda (dyno) (equal t (win+-free dyno)))
                                      (dyn-float-group-dyn-order group))))
        ;; Let the (group-windows group) respect the order of
        ;; (dyn-float-group-dyn-order group)
        (setf (group-windows group)
              (mapcar #'win+-window
                      (dyn-float-group-dyn-order group))))))

(defun current-window+ (&optional (group (current-group)))
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (let ((gcw (group-current-window group)))
        (find-if (lambda (x)
                   (equal gcw (win+-window x)))
                 (dyn-float-group-dyn-order group)))))

(defun next-window+ (&optional (N 1) (group (current-group)))
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (let ((dyno (dyn-float-group-dyn-order group)))
        (nth (mod (+ N (position (current-window+ group) dyno)) (length dyno))
             dyno))))

(defcommand focus-next-window (&optional (N 1) (group (current-group))) ()
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (group-focus-window group (win+-window (next-Nth-window+ N group)))))

(defcommand focus-last-window (&optional (group (current-group))) ()
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (focus-next-window -1 group)))

(defun current-window-position (&optional (group (current-group)))
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (position (current-window+ group)
                (dyn-float-group-dyn-order group)
                :test #'equal)))

(defun free-all (&optional (group (current-group)))
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      ;; alias: un-tile-all
      (progn (loop for w+ in (dyn-float-group-dyn-order group)
                   do (setf (win+-free w+) t))
             (re-tile group))))

(defcommand unfree-all
    (&optional (group (current-group))) ()
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (progn
        ;; alias: tile-all
        (loop for w+ in (dyn-float-group-dyn-order group)
              do (setf (win+-free w+) nil))
        (re-tile group))))

(defun free-window (&optional (window (current-window))
                      (group (current-group)))
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (progn
        (loop for w+ in (dyn-float-group-dyn-order group)
              if (equal window (win+-window w+))
                do (setf (win+-free w+) t))
        (re-tile group))))

(defcommand unfree-window
    (&optional (window (current-window)) (group (current-group))) ()
    (if (not (dyn-float-group-p group))
        (error "GROUP must be of type DYN-FLOAT-GROUP.")
        (progn
          (symbol-macrolet ((dyno (dyn-float-group-dyn-order group)))
            (loop for w+ in dyno
                  do (when (equal window (win+-window w+))
                       (progn
                         (deletef dyno w+)
                         (setf (win+-free w+) nil)
                         (if (null dyno)
                             (setf dyno (list w+))
                             (push w+ (cdr (last dyno))))))))
          (re-tile group))))

(defun toggle-freeness-current-window (&optional (window (current-window))
                                         (group (current-group)))
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (progn
        (if (eq (win+-free (current-window+ group)) t)
            (unfree-window window group)
            (free-window window group))
        (re-tile group))))

(defun unfloating-windows+ (&optional (group (current-group)))
  "Return the list of window+s whose :FREE slot is nil."
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (remove-if-not
       (lambda (w+) (eq (win+-free w+) nil))
       (dyn-float-group-dyn-order group))))

(defun re-tile (&optional (group (current-group)))
  ;; FIXME respect modeline and boarder.. or even gap in the future
  ;; Waiting for the fix for a related issue for general floating group.
  ;; https://github.com/stumpwm/stumpwm/issues/864
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (progn
        (sync-dyn-order group)
        (let* ((cs (slot-value (current-screen) 'number))
               (sw (xlib:screen-width cs))
               (sh (xlib:screen-height cs))
               (wl (mapcar #'win+-window (unfloating-windows+ group)))
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
                           :height (round (/ sh (- N 1))))))))))))

(defcommand rotate-window-list
    (&optional (group (current-group)) opposite) ()
    (if (not (dyn-float-group-p group))
        (error "GROUP must be of type DYN-FLOAT-GROUP.")
        (flet ((rotate-list (xs &optional opposite)
                 "An adhoc pure function that rotates the list."
                 (if opposite
                     (concatenate 'list (cdr xs) (list (car xs)))
                     (concatenate 'list (last xs) (butlast xs)))))
          (symbol-macrolet ((dyno (dyn-float-group-dyn-order group)))
            (setf dyno (rotate-list dyno opposite))
            (re-tile group)))))

(defcommand permute-window-list
    ;; TODO Make 'opposite take + or - 1.
    (&optional opposite (group (current-group))
     (n (current-window-position group)))
    ()

  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (flet ((permute-at (ring n)
               "A pure function that permutes the nth and
the (n+1)th element of RING."
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
                                      (subseq ring (+ n 2)))))))))
        (progn
          (when opposite (setf n (- n 1)))
          (symbol-macrolet ((dyno (dyn-float-group-dyn-order group)))
            (setf dyno (permute-at dyno n))
            (re-tile group))))))

(defcommand gnew-dyn-float (name) ((:rest "Group Name: "))
  "Create a new dynamic floating group named NAME."
  (unless name (throw 'error :abort))
  (add-group (current-screen) name :type 'dyn-float-group))

(defcommand gnew-dyn-float-bg (name) ((:rest "Group Name: "))
  "Create a new dynamic floating group named NAME in the background."
  (unless name (throw 'error :abort))
  (add-group (current-screen) name :type 'dyn-float-group :background t))

;; For testing.
(setf test-group (gnew-dyn-float-bg "TEST"))

;; for development ease
(defcommand print-devel-stat () ()
  (echo (prin1-to-string
         (list (dyn-float-group-dyn-order (current-group))
               ""
               (group-windows (current-group))))))

;; (define-key *top-map* (stumpwm:kbd "s-j") "move-focus down")
;; (define-key *top-map* (stumpwm:kbd "s-h") "move-focus left")
;; (define-key *top-map* (stumpwm:kbd "s-k") "move-focus up")
;; (define-key *top-map* (stumpwm:kbd "s-l") "move-focus right")
;; "exchange-direction down" ;;
;; (group-focus-window group (first (group-windows group)))

(defcommand tmp-wrapper-s-j () ()
  (let ((cg (current-group)))
    (if (dyn-float-group-p cg)
        (focus-next-window)
        (call-interactively 'move-focus "down"))))
(define-key *top-map* (stumpwm:kbd "s-j") "tmp-wrapper-s-j")

(defcommand tmp-wrapper-s-k () ()
  (let ((cg (current-group)))
    (if (dyn-float-group-p cg)
        (focus-last-window)
        (call-interactively 'move-focus "up"))))
(define-key *top-map* (stumpwm:kbd "s-k") "tmp-wrapper-s-k")

(defcommand tmp-wrapper-s-J () ()
  (let ((cg (current-group)))
    (if (dyn-float-group-p cg)
        (permute-window-list)
        (call-interactively 'exchange-direction "down"))))
(define-key *top-map* (stumpwm:kbd "s-J") "tmp-wrapper-s-J")

(defcommand tmp-wrapper-s-K () ()
  (let ((cg (current-group)))
    (if (dyn-float-group-p cg)
        (permute-window-list t)
        (call-interactively 'exchange-direction "up"))))
(define-key *top-map* (stumpwm:kbd "s-K") "tmp-wrapper-s-K")

;; TODO
;; 1. s-H, s-L : (un)swap with master
;; 2. s-h, s-l : (un)focus on master
;; 3. s-+, s-- : toggle master width
;;
;; TODO I think I should make another name space (CL package for
;; the functions here.. many function names could easily collapse
;; with others.
