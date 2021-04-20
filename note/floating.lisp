;; Have a handy function to get all info of all windows in the
;; current group. Or even more, to be able to see the current state
;; of the whole group.



;; We have =window-display-height= but no =window-display-width=. ...
;; Well, =window-display-height= is broken. It doesn't really change
;; as I alter the height of my floating window.
(defun current-window-height () (slot-value (current-window) 'height))
(defun current-window-width () (slot-value (current-window) width))
;; How about #'display-current-window-info?


How to *really* change the width, height, x and y, of the current window?

(float-window-move-resize (current-window) :width 500)

;; <= I can use this to re-tile. But I have to find out what's the
;; space available for the current group. -- it's here
(xlib:screen-width (slot-value (current-screen) 'number))
(xlib:screen-height (slot-value (current-screen) 'number))




;; I want to switch focus to the next window in the current group easily.

(focus-next-window (current-group))
(focus-prev-window (current-group))
;; Have a way to switch to "full-screen" mode in a floating group.
;; In that case, all windows are full-screened. Make sure that I can
;; still switch focus to next/prev window easily. While
;; "full-screen" mode is off, all windows should be set back to
;; their original size.


;; emacs dropdown window -- my dream



;; The following is what (current-window) returns. The slot values
;; can be retrieved by, say, =(slot-value (current-window)
;; 'fullscreen)=.

;; #<FLOAT-WINDOW {10081A6513}>
;; --------------------
;; Class: #<STANDARD-CLASS STUMPWM::FLOAT-WINDOW>
;; --------------------
;;  Group slots by inheritance [ ]
;;  Sort slots alphabetically  [X]

;; All Slots:
;; [ ]  CLASS         = "Emacs"
;; [ ]  FULLSCREEN    = NIL
;; [ ]  GRAVITY       = NIL
;; [ ]  GROUP         = @1=#<FLOAT-GROUP {10081C41C3}>
;; [ ]  HEIGHT        = 474
;; [ ]  LAST-HEIGHT   = 474
;; [ ]  LAST-WIDTH    = 951
;; [ ]  LAST-X        = 132
;; [ ]  LAST-Y        = 212
;; [ ]  MARKED        = NIL
;; [ ]  NORMAL-HINTS  = #S(XLIB:WM-SIZE-HINTS :USER-SPECIFIED-POSITION-P NIL :USER-SPECIFIED-SIZE-P NIL :X NIL :Y NIL :WIDTH NIL :HEIGHT NIL :MIN-WIDTH 28 :MIN-HEIGHT 24 :MAX-WIDTH NIL :MAX-HEIGHT NIL :WIDTH-INC 1 :HEIGHT-INC 1 :MIN-ASPECT NIL :MAX-ASPECT NIL :BASE-WIDTH 28 :BASE-HEIGHT 24 :WIN-GRAVITY :NORTH-WEST :PROGRAM-SPECIFIED-POSITION-P NIL :PROGRAM-SPECIFIED-SIZE-P NIL)
;; [ ]  NUMBER        = 0
;; [ ]  PARENT        = #<XLIB:WINDOW :0 A00014>
;; [ ]  PLIST         = #<HASH-TABLE :TEST EQL :COUNT 0 {10083066D3}>
;; [ ]  RES           = "emacs"
;; [ ]  ROLE          = ""
;; [ ]  STATE         = 1
;; [ ]  TITLE         = "*sly-mrepl for sbcl* – Doom Emacs"
;; [ ]  TYPE          = :NORMAL
;; [ ]  UNMAP-IGNORES = 0
;; [ ]  USER-TITLE    = NIL
;; [ ]  WIDTH         = 951
;; [ ]  X             = 132
;; [ ]  XWIN          = #<XLIB:WINDOW :0 1800142>
;; [ ]  Y             = 212

;; test

(defun re-tile ()
  ;; Assuming boarder is very thin.
  (let* ((cs (slot-value (current-screen) 'number))
         (sw (xlib:screen-width cs))
         (sh (xlib:screen-height cs))
         (cg (current-group))
         (wl (group-windows cg))
         (N (length wl)))

    (float-window-move-resize (car wl) :x 0 :y 0 :width (round (/ sw 2)) :height sh)
    (loop for k from 1 to (- N 1)
          do (float-window-move-resize
              (nth k wl)
              :x (round (/ sw 2))
              :y (* (round (/ sh (- N 1)))
                    (- k 1))
              :width (round (/ sw 2))
              :height (round (/ sh (- N 1)))))
    ))
(re-tile)

(defun rotate-list (xs)
  "Purely rotate a list."
  (concatenate 'list (cdr xs) (list (car xs))))

(defun rotate-window-list ()
  ;; Broken
  (let (gw (group-windows (current-group)))
    (setf gw (rotate-list gw))
    (re-tile)))

