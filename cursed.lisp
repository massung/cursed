;;;; libcurses-style output-pane for LispWorks
;;;;
;;;; Copyright (c) 2012 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :cursed
  (:use :cl :capi :color)
  (:export
   #:cursed-pane

   ;; macros
   #:with-output-to-cursed-pane

   ;; accessors
   #:cursed-pane-pixmap
   #:cursed-pane-chars
   #:cursed-pane-chars-wide
   #:cursed-pane-chars-high
   #:cursed-pane-cursor-x
   #:cursed-pane-cursor-y
   #:cursed-pane-cursor-visible-p
   #:cursed-pane-scroll
   #:cursed-pane-clear
   #:cursed-pane-copy))

(in-package :cursed)

(defconstant +default-cursed-font-desc+ (gp:make-font-description :family "Courier New" :size 13.0)
  "The default font for all cursed panes.")

(defstruct (cursed-font-metrics (:conc-name cursed-font-))
  "Cached metrics for a font on a cursed pane."
  width height ascent)

(defclass cursed-pane (output-pane stream:fundamental-character-output-stream)
  ((pixmap  :initform nil :reader cursed-pane-pixmap)
   (chars   :initform nil :reader cursed-pane-chars)
   (metrics :initform nil :reader cursed-pane-font-metrics)

   ;; the size of the pane
   (chars-wide :initarg :chars-wide :initform 80 :reader cursed-pane-chars-wide)
   (chars-high :initarg :chars-high :initform 24 :reader cursed-pane-chars-high)

   ;; selected text
   (sel-start :initform nil :reader cursed-pane-selection-start)
   (sel-end   :initform nil :reader cursed-pane-selection-end)

   ;; non-nil if the cursor should be drawn
   (cursor-visible :initarg :cursor-visible :initform t :reader cursed-pane-cursor-visible-p)

   ;; x,y location of the cursor
   (cursor-x :initarg :cursor-x :initform 0 :accessor cursed-pane-cursor-x)
   (cursor-y :initarg :cursor-y :initform 0 :accessor cursed-pane-cursor-y))
  (:default-initargs
   :background :black
   :foreground :gray90
   :visible-border nil
   :draw-with-buffer t
   :visible-max-width t
   :visible-max-height t
   :font +default-cursed-font-desc+
   :create-callback 'create-cursed-pane
   :destroy-callback 'destroy-cursed-pane
   :resize-callback 'resize-cursed-pane
   :display-callback 'display-cursed-pane
   :input-model '(((:button-1 :press) click-cursed-pane)
                  ((:motion :button-1 :press) drag-cursed-pane))))

(defmacro with-output-to-cursed-pane ((pane &key x y foreground background) &body body)
  "Override current colors, force output."
  (let ((cx (gensym))
        (cy (gensym)))
    `(let ((*standard-output* ,pane))
       
       ;; reposition the cursor
       (lw:when-let (,cx ,x) (setf (cursed-pane-cursor-x *standard-output*) ,cx))
       (lw:when-let (,cy ,y) (setf (cursed-pane-cursor-y *standard-output*) ,cy))

       ;; temporarily set the foreground and background colors
       (gp:with-graphics-state ((slot-value ,pane 'pixmap)
                                ,@(when background `(:background ,background))
                                ,@(when foreground `(:foreground ,foreground)))
         (unwind-protect
             (progn ,@body)
           (force-output *standard-output*))))))

(defmethod create-cursed-pane ((pane cursed-pane))
  "Set the size of the pane and clear the output."
  (with-slots (metrics chars chars-wide chars-high)
      pane
    (multiple-value-bind (left top right bottom)
        (gp:get-character-extent pane #\W)

      ;; save off the metrics so we don't need to compute them all the time
      (setf metrics (make-cursed-font-metrics :width (- right left)
                                              :height (- bottom top 1)
                                              :ascent (gp:get-font-ascent pane)))

      ;; resize the pane using the hint table
      (set-hint-table pane (list :visible-min-width (* (cursed-font-width metrics) chars-wide)
                                 :visible-min-height (* (cursed-font-height metrics) chars-high)))

      ;; allocate an array of strings for all the characters (column major)
      (setf chars (make-string (* chars-wide chars-high) :initial-element #\space)))))

(defmethod destroy-cursed-pane ((pane cursed-pane))
  "Free memory used by the pane."
  (with-slots (pixmap)
      pane
    (when pixmap
      (gp:destroy-pixmap-port pixmap))))

(defmethod resize-cursed-pane ((pane cursed-pane) x y w h)
  "Create the pixmap for the pane, destroy any currently existing one."
  (declare (ignore x y))
  (with-slots (pixmap)
      pane

    ;; free the existing pixmaps
    (when pixmap
      (gp:destroy-pixmap-port pixmap))

    ;; create a new pixmap port to render all the characters to
    (let ((foreground (simple-pane-foreground pane))
          (background (simple-pane-background pane)))
      (setf pixmap (gp:create-pixmap-port pane w h :clear t :foreground foreground :background background))

      ;; set the font of the pixmap port
      (setf (gp:graphics-state-font (gp:get-graphics-state pixmap)) (simple-pane-font pane)))))

(defmethod display-cursed-pane ((pane cursed-pane) x y w h)
  "Redraw characters in the pane."
  (with-slots (metrics pixmap cursor-x cursor-y cursor-visible sel-start sel-end chars chars-wide)
      pane
    (when pixmap
      (gp:draw-image pane (gp:make-image-from-port pixmap) x y :from-x x :from-y y :to-width w :to-height h))

    ;; font metrics
    (let ((fw (cursed-font-width metrics))
          (fh (cursed-font-height metrics))
          (fa (cursed-font-ascent metrics)))

      ;; render the selected text
      (when (and sel-start sel-end)
        (gp:with-graphics-state (pane :foreground :color_highlighttext :background :color_highlight)
          (loop :with start := (min sel-start sel-end)
                :with end := (max sel-start sel-end)
                :with i := start
                :while (<= i end)
                :for y := (truncate i chars-wide)
                :for x := (- i (* y chars-wide))
                :for n := (min (- chars-wide x) (- (1+ end) i))
                :do (gp:draw-string pane chars (* x fw) (+ (* y fh) fa) :block t :start i :end (incf i n)))))

      ;; render the cursor over the text
      (when cursor-visible
        (gp:draw-rectangle pane (1+ (* cursor-x fw)) (+ (* cursor-y fh) fa) (- fw 2) 3 :filled t)))))

(defmethod click-cursed-pane ((pane cursed-pane) x y &optional drag)
  "Position the cursor from a click."
  (with-slots (metrics sel-start sel-end cursor-visible chars-wide chars-high)
      pane
    (when cursor-visible
      (let ((x (truncate x (cursed-font-width metrics)))
            (y (truncate y (cursed-font-height metrics))))

        ;; only update if within the pane
        (when (and (<= 0 x (1- chars-wide))
                   (<= 0 y (1- chars-high)))

          ;; update the selection
          (let ((pos (+ (* y chars-wide) x)))
            (if drag
                (setf sel-end pos)
              (setf sel-start pos sel-end nil)))

          ;; update the position in the stream
          (file-position pane (list x y)))))))

(defmethod drag-cursed-pane ((pane cursed-pane) x y)
  "Drag the cursor, extend the current selection."
  (click-cursed-pane pane x y t))

(defmethod cursed-pane-scroll ((pane cursed-pane))
  "Scroll all the characters on the pane. The cursor does not move."
  (with-slots (metrics pixmap chars chars-wide)
      pane
    (let ((w (gp:port-width pane))
          (h (gp:port-height pane)))
      (gp:draw-image pixmap (gp:make-image-from-port pixmap) 0 0 :from-y (1- (gp:get-font-height pixmap)))

      ;; clear the background of the bottom line
      (gp:draw-rectangle pixmap 0 h w (- (cursed-font-height metrics)) :filled t)

      ;; update the characters
      (let ((new-line (make-string chars-wide  :initial-element #\space)))
        (setf chars (concatenate 'string (subseq chars chars-wide) new-line)))

      ;; force redraw
      (gp:invalidate-rectangle pane))))

(defmethod cursed-pane-clear ((pane cursed-pane))
  "Clear the pane, same as clearing the output stream."
  (stream:stream-clear-output pane))

(defmethod cursed-pane-copy ((pane cursed-pane))
  "Copy the text from the cursed pane into the clipboard."
  (with-slots (chars sel-start sel-end chars-wide)
      pane
    (when (and sel-start sel-end)
      (let ((string (with-output-to-string (s)
                      (loop :with start := (min sel-start sel-end)
                            :with end := (max sel-start sel-end)
                            :with line := (truncate start chars-wide)
                            
                            ;; loop over each line, insert newlines
                            :for i :from start :below end
                            :for y := (truncate i chars-wide)
                            
                            ;; insert a newline?
                            :when (/= y line)
                            :do (progn
                                  (setf line y)
                                  (terpri s))
                            
                            ;; write the character
                            :do (princ (char chars i) s)))))
        (prog1
            string
          (set-clipboard pane nil string))))))

(defmethod (setf cursed-pane-cursor-visible-p) (visible-p (pane cursed-pane))
  "Change whether or not the cursor is visible."
  (with-slots (cursor-visible)
      pane
    (setf cursor-visible visible-p)

    ;; redraw the pane
    (gp:invalidate-rectangle pane)))

(defmethod stream:stream-file-position ((pane cursed-pane))
  "Return the current cursor position."
  (list (cursed-pane-cursor-x pane)
        (cursed-pane-cursor-y pane)))

(defmethod (setf stream:stream-file-position) (new-pos (pane cursed-pane))
  "Set the position of the cursor."
  (destructuring-bind (x y)
      new-pos
    (setf (cursed-pane-cursor-x pane) x
          (cursed-pane-cursor-y pane) y)

    ;; redraw when the cursor is visible
    (when (cursed-pane-cursor-visible-p pane)
      (gp:invalidate-rectangle pane))))

(defmethod stream:stream-clear-output ((pane cursed-pane))
  "Wipe the pixmap."
  (with-slots (pixmap chars chars-wide chars-high cursor-x cursor-y)
      pane
    ;; erase the pixmap
    (gp:clear-graphics-port pixmap)

    ;; wipe the character buffer
    (setf chars (make-string (* chars-wide chars-high) :initial-element #\space))

    ;; reset the cursor to the beginning
    (setf cursor-x 0
          cursor-y 0)

    ;; redraw
    (gp:invalidate-rectangle pane)))

(defmethod stream:stream-force-output ((pane cursed-pane))
  "Redraw, forcing output to take effect."
  (apply-in-pane-process pane #'gp:invalidate-rectangle pane))

(defmethod stream:stream-output-width ((pane cursed-pane))
  "Return the total number of columns."
  (cursed-pane-chars-wide pane))

(defmethod stream:stream-start-line-p ((pane cursed-pane))
  "T if at the start of a newline."
  (zerop (cursed-pane-cursor-x pane)))

(defmethod stream:stream-terpri ((pane cursed-pane))
  "Force a newline if not on one already."
  (with-slots (cursor-x cursor-y chars-high)
      pane
    (unless (zerop cursor-x)
      (if (= cursor-y (1- chars-high))
          (cursed-pane-scroll pane)
        (incf cursor-y))
      (setf cursor-x 0))))

(defmethod stream:stream-write-char ((pane cursed-pane) char)
  "Output a single character at the current cursor position."
  (with-slots (metrics pixmap chars (x cursor-x) (y cursor-y) chars-wide chars-high)
      pane
    (when (and (<= 0 x (1- chars-wide))
               (<= 0 y (1- chars-high)))
      (case char

        ;; newlines
        (#\return    (setf x 0))
        (#\linefeed  (terpri pane))
        
        ;; backup the cursor
        (#\backspace (setf x (max (1- x) 0)))
        
        ;; advance the cursor
        (#\tab       (when (>= (setf x (if (zerop (logand x 7))
                                           (+ x 8)
                                         (1- (logand 8 (+ x 7))))) chars-wide)
                       (terpri pane)))
        
        ;; all other characters are written
        (otherwise   (setf (aref chars (+ (* y chars-wide) x)) char)
                     
                     ;; render to the pixmap
                     (let ((fw (cursed-font-width metrics))
                           (fh (cursed-font-height metrics))
                           (fa (cursed-font-ascent metrics)))
                       (gp:draw-character pixmap char (* x fw) (+ (* y fh) fa) :block t))
                     
                     ;; advance the cursor (wrap lines)
                     (when (>= (incf x) chars-wide)
                       (terpri pane)))))))

(defmethod stream:stream-write-string ((pane cursed-pane) string &optional (start 0) (end (length string)))
  "Output a value at the current cursor position."
  (loop :for i :from start :below end :do (stream:stream-write-char pane (char string i))))
