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
   #:cursed-pane-clear))

(in-package :cursed)

(defconstant +default-cursed-font-desc+ (gp:make-font-description :family "Courier New" :size 14.0)
  "The default font for all cursed panes.")

(defclass cursed-pane (output-pane stream:fundamental-character-output-stream)
  ((pixmap :initform nil :reader cursed-pane-pixmap)
   (bg     :initform nil :reader cursed-pane-background)
   (chars  :initform nil :reader cursed-pane-chars)

   ;; the size of the pane
   (chars-wide :initarg :chars-wide :initform 80 :reader cursed-pane-chars-wide)
   (chars-high :initarg :chars-high :initform 24 :reader cursed-pane-chars-high)

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
   :font +default-cursed-font-desc+
   :create-callback 'create-cursed-pane
   :destroy-callback 'destroy-cursed-pane
   :resize-callback 'resize-cursed-pane
   :display-callback 'display-cursed-pane
   :input-model '()))

(defmacro with-output-to-cursed-pane ((pane &key x y foreground background) &body body)
  "Override current colors, force output."
  (let ((cx (gensym))
        (cy (gensym)))
    `(let ((*standard-output* ,pane))
       
       ;; reposition the cursor
       (lw:when-let (,cx ,x) (setf (cursed-pane-cursor-x *standard-output*) ,cx))
       (lw:when-let (,cy ,y) (setf (cursed-pane-cursor-y *standard-output*) ,cy))

       ;; temporarily set the foreground and background colors
       (gp:with-graphics-state ((slot-value ,pane 'bg) ,@(when background `(:foreground ,background)))
         (gp:with-graphics-state ((slot-value ,pane 'pixmap) ,@(when foreground `(:foreground ,foreground)))
           (unwind-protect
               (progn ,@body)
             (force-output *standard-output*)))))))

(defmethod create-cursed-pane ((pane cursed-pane))
  "Set the size of the pane and clear the output."
  (with-slots (chars chars-wide chars-high)
      pane
    (let ((char-width (gp:get-font-average-width pane))
          (char-height (gp:get-font-height pane)))
      (set-hint-table pane (list :visible-min-width (* char-width chars-wide)
                                 :visible-max-width (* char-width chars-wide)
                                 :visible-min-height (* char-height chars-high)
                                 :visible-max-height (* char-height chars-high))))

    ;; allocate an array of strings for all the characters (column major)
    (setf chars (make-string (* chars-wide chars-high) :initial-element #\space))))

(defmethod destroy-cursed-pane ((pane cursed-pane))
  "Free memory used by the pane."
  (with-slots (pixmap)
      pane
    (when pixmap
      (gp:destroy-pixmap-port pixmap))))

(defmethod resize-cursed-pane ((pane cursed-pane) x y w h)
  "Create the pixmap for the pane, destroy any currently existing one."
  (declare (ignore x y))
  (with-slots (pixmap bg)
      pane

    ;; free the existing pixmaps
    (when pixmap
      (gp:destroy-pixmap-port pixmap))
    (when bg
      (gp:destroy-pixmap-port bg))

    ;; create a new pixmap port to render all the characters to
    (let ((foreground (simple-pane-foreground pane))
          (background (simple-pane-background pane)))
      (setf bg (gp:create-pixmap-port pane w h :clear t :foreground background :background background)
            pixmap (gp:create-pixmap-port pane w h :clear t :foreground foreground :background :transparent))

      ;; set the font of the pixmap port
      (setf (gp:graphics-state-font (gp:get-graphics-state pixmap)) (simple-pane-font pane)))))

(defmethod display-cursed-pane ((pane cursed-pane) x y w h)
  "Redraw characters in the pane."
  (with-slots (bg pixmap cursor-x cursor-y cursor-visible)
      pane
    (when pixmap
      ;; render the background
      (gp:draw-image pane (gp:make-image-from-port bg) x y :from-x x :from-y y :to-width w :to-heigh h)

      ;; TODO: render the selection

      ;; render the text
      (gp:draw-image pane (gp:make-image-from-port pixmap) x y :from-x x :from-y y :to-width w :to-height h))

    ;; render the cursor over the text
    (when cursor-visible
      (let ((fw (gp:get-font-average-width pane))
            (fh (gp:get-font-height pane))
            (fa (gp:get-font-ascent pane)))
        (gp:draw-rectangle pane (* cursor-x fw) (+ (* cursor-y fh) fa) fw 3 :filled t)))))

(defmethod cursed-pane-scroll ((pane cursed-pane))
  "Scroll all the characters on the pane. The cursor does not move."
  (with-slots (bg pixmap chars chars-wide chars-high cursor-x)
      pane
    (let ((w (gp:port-width pane))
          (h (gp:port-height pane)))
      (gp:draw-image bg (gp:make-image-from-port bg) 0 0 :from-y (gp:get-font-height pane))

      ;; because the background is transparent we need to create a new pixmap to render to
      (let* ((gs (gp:get-graphics-state pixmap))
             (fg (gp:graphics-state-foreground gs))
             (np (gp:create-pixmap-port pane w h :clear t :foreground fg :background :transparent)))
        (gp:draw-image np (gp:make-image-from-port pixmap) 0 0 :from-y (gp:get-font-height pane))

        ;; set the font for the new pixmap
        (setf (gp:graphics-state-font (gp:get-graphics-state np)) (gp:graphics-state-font gs))

        ;; delete the old one and set it
        (gp:destroy-pixmap-port pixmap)

        ;; assign the new one
        (setf pixmap np))

      ;; clear the background of the bottom line
      (gp:draw-rectangle bg 0 h w (- (gp:get-font-height pane)) :filled t)

      ;; update the characters
      (let ((shifted-chars (subseq chars 0 (* chars-wide (1- chars-high))))
            (new-line (make-string chars-wide  :initial-element #\space)))
        (setf chars (concatenate 'string shifted-chars new-line)))

      ;; move the cursor to the beginning of the line
      (setf cursor-x 0)

      ;; force redraw
      (gp:invalidate-rectangle pane))))

(defmethod cursed-pane-clear ((pane cursed-pane))
  "Wipe the pane."
  (with-slots (bg pixmap chars cursor-x cursor-y chars-wide chars-high)
      pane
    (setf chars (make-string (* chars-wide chars-high) :initial-element #\space))

    ;; clear the pixmap port
    (let ((w (gp:port-width pane))
          (h (gp:port-height pane)))
      (gp:draw-rectangle bg 0 0 w h :filled t :foreground (simple-pane-background pane))
      (gp:draw-rectangle pixmap 0 0 w h :filled t :foreground (simple-pane-background pane)))

    ;; put the cursor back at the top
    (setf cursor-x 0
          cursor-y 0)

    ;; force redraw
    (gp:invalidate-rectangle pane)))

(defmethod (setf cursed-pane-cursor-visible-p) (visible-p (pane cursed-pane))
  "Change whether or not the cursor is visible."
  (with-slots (cursor-visible cursor-x cursor-y)
      pane
    (setf cursor-visible visible-p)

    ;; redraw the pane where the cursor is located
    (let ((fw (gp:get-font-average-width pane))
          (fh (gp:get-font-height pane)))
      (gp:invalidate-rectangle pane (* cursor-x fw) (* cursor-y fh) fw fh))))

(defmethod stream:stream-file-position ((pane cursed-pane))
  "Return the current cursor position."
  (list (cursed-pane-cursor-x pane)
        (cursed-pane-cursor-y pane)))

(defmethod (setf stream:stream-file-position) (new-pos (pane cursed-pane))
  "Set the position of the cursor."
  (destructuring-bind (x y)
      new-pos
    (setf (cursed-pane-cursor-x pane) x
          (cursed-pane-cursor-y pane) y)))

(defmethod stream:stream-clear-output ((pane cursed-pane))
  "Wipe the pixmap."
  (with-slots (bg pixmap chars chars-wide chars-high)
      pane
    (let ((w (gp:port-width pixmap))
          (h (gp:port-height pixmap)))
      (gp:draw-rectangle bg 0 0 w h :filled t)
      (gp:draw-rectangle pixmap 0 0 w h :filled t)

      ;; wipe the character buffer
      (setf chars (make-string (* chars-wide chars-high) :initial-element #\space)))))

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
  (with-slots (bg pixmap chars (x cursor-x) (y cursor-y) chars-wide)
      pane
    (case char
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
      (otherwise
       (setf (aref chars (+ (* y chars-wide) x)) char)

       ;; render to the pixmap
       (let ((fw (gp:get-font-average-width pane))
             (fh (gp:get-font-height pane))
             (fa (gp:get-font-ascent pane)))
         (gp:draw-rectangle bg (* x fw) (* y fh) fw fh :filled t)
         (gp:draw-character pixmap char (* x fw) (+ (* y fh) fa) :block t))

       ;; advance the cursor (wrap lines)
       (when (>= (incf x) chars-wide)
         (terpri pane))))))

(defmethod stream:stream-write-string ((pane cursed-pane) string &optional (start 0) (end (length string)))
  "Output a value at the current cursor position."
  (loop :for i :from start :below end :do (stream:stream-write-char pane (char string i))))
