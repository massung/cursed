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
   #:cursed-pane-selection-visible-p
   #:cursed-pane-selection-start
   #:cursed-pane-selection-end
   #:cursed-pane-scroll
   #:cursed-pane-clear
   #:cursed-pane-copy
   #:cursed-pane-paste))

(in-package :cursed)

(defstruct (cursed-font-metrics (:conc-name cursed-font-) (:constructor make-cursed-font (pane)))
  "Cached metrics for drawing."
  (width  (gp:get-font-width pane))
  (height (gp:get-font-height pane))
  (ascent (gp:get-font-ascent pane))
  (indent (- (gp:get-font-average-width pane)
             (gp:get-font-width pane))))

(defclass cursed-pane (output-pane stream:fundamental-character-output-stream)
  ((chars-wide     :initform 80  :reader cursed-pane-chars-wide          :initarg :chars-wide)
   (chars-high     :initform 24  :reader cursed-pane-chars-high          :initarg :chars-high)

   ;; optional visibility setings
   (sel-visible    :initform t   :reader cursed-pane-selection-visible-p :initarg :selection-visible)
   (cursor-visible :initform t   :reader cursed-pane-cursor-visible-p    :initarg :cursor-visible)

   ;; x,y location of the cursor
   (cursor-x       :initform 0   :reader cursed-pane-cursor-x            :initarg :cursor-x)
   (cursor-y       :initform 0   :reader cursed-pane-cursor-y            :initarg :cursor-y)

   ;; cached information
   (bg-pixmap      :initform nil :reader cursed-pane-bg-pixmap)
   (fg-pixmap      :initform nil :reader cursed-pane-fg-pixmap)
   (chars          :initform nil :reader cursed-pane-chars)
   (metrics        :initform nil :reader cursed-pane-font-metrics)

   ;; selected text
   (sel-start      :initform nil)
   (sel-end        :initform nil))
  (:default-initargs
   :background :black
   :foreground :gray90
   :draw-with-buffer t
   :visible-border nil
   :visible-max-width t
   :visible-max-height t
   :font (gp:make-font-description :family "Courier" :size 13.0)
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
       (gp:with-graphics-state ((slot-value ,pane 'bg-pixmap) ,@(when background `(:foreground ,background)))
         (gp:with-graphics-state ((slot-value ,pane 'fg-pixmap) ,@(when foreground `(:foreground ,foreground)))
           (unwind-protect
               (progn ,@body)
             (force-output *standard-output*)))))))

(defmethod create-cursed-pane ((pane cursed-pane))
  "Set the size of the pane and clear the output."
  (with-slots (metrics chars chars-wide chars-high)
      pane
    (setf metrics (make-cursed-font pane))

    ;; resize the pane using the hint table
    (set-hint-table pane (list :visible-min-width (* (cursed-font-width metrics) chars-wide)
                               :visible-min-height (* (cursed-font-height metrics) chars-high)))

    ;; allocate an array of strings for all the characters (column major)
    (setf chars (make-string (* chars-wide chars-high) :initial-element #\space))))

(defmethod destroy-cursed-pane ((pane cursed-pane))
  "Free memory used by the pane."
  (with-slots (bg-pixmap fg-pixmap)
      pane
    (when bg-pixmap
      (gp:destroy-pixmap-port bg-pixmap))
    (when fg-pixmap
      (gp:destroy-pixmap-port fg-pixmap))))

(defmethod resize-cursed-pane ((pane cursed-pane) x y w h)
  "Create the pixmap for the pane, destroy any currently existing one."
  (declare (ignore x y))
  (with-slots (bg-pixmap fg-pixmap)
      pane

    ;; free the existing pixmaps
    (when bg-pixmap
      (gp:destroy-pixmap-port bg-pixmap))
    (when fg-pixmap
      (gp:destroy-pixmap-port fg-pixmap))

    ;; create a new pixmap port to render all the characters to
    (let ((foreground (simple-pane-foreground pane))
          (background (simple-pane-background pane)))
      (setf bg-pixmap (gp:create-pixmap-port pane w h :clear t :foreground background :background background))
      (setf fg-pixmap (gp:create-pixmap-port pane w h :clear t :foreground foreground :background :transparent))

      ;; set the font of the foreground pixmap port
      (setf (gp:graphics-state-font (gp:get-graphics-state fg-pixmap)) (simple-pane-font pane)))))

(defmethod display-cursed-pane ((pane cursed-pane) x y w h)
  "Redraw characters in the pane."
  (with-slots (metrics bg-pixmap fg-pixmap cursor-x cursor-y cursor-visible sel-visible sel-start sel-end chars-wide)
      pane
    (let ((fw (cursed-font-width metrics))
          (fh (cursed-font-height metrics))
          (fa (cursed-font-ascent metrics)))

      ;; draw the background
      (when bg-pixmap
        (gp:draw-image pane (gp:make-image-from-port bg-pixmap) x y :from-x x :from-y y :to-width w :to-height h))

      ;; if the selection is visible, render it over the background
      (when (and sel-visible sel-start sel-end)
        (let* ((start (min sel-start sel-end))
               (end (max sel-start sel-end))

               ;; line and character positions
               (line-start (truncate start chars-wide))
               (line-end (truncate end chars-wide))
               (char-start (- start (* line-start chars-wide)))
               (char-end (- end (* line-end chars-wide)))

               ;; bounding area of selected text
               (x1 (* char-start fw))
               (x2 (* (1+ char-end) fw))
               (y1 (* line-start fh))
               (y2 (* (1+ line-end) fh)))
          (gp:with-graphics-state (pane :foreground :color_highlight)
            (if (= line-start line-end)
                (gp:draw-rectangle pane x1 y1 (- x2 x1) (- y2 y1) :filled t)
              (progn
                (gp:draw-rectangle pane x1 y1 (- (gp:port-width pane) x1) fh :filled t)
                (gp:draw-rectangle pane 0 (+ y1 fh) (gp:port-width pane) (- y2 y1 fh fh) :filled t)
                (gp:draw-rectangle pane 0 y2 x2 (- fh) :filled t))))))

      ;; draw the foreground
      (when fg-pixmap
        (gp:draw-image pane (gp:make-image-from-port fg-pixmap) x y :from-x x :from-y y :to-width w :to-height h))

      ;; render the cursor over the text
      (when cursor-visible
        (gp:draw-rectangle pane (1+ (* cursor-x fw)) (+ (* cursor-y fh) fa) (1- fw) 3 :filled t)))))

(defmethod click-cursed-pane ((pane cursed-pane) x y)
  "Position the cursor from a click."
  (declare (ignore x y))
  (with-slots (sel-start sel-end)
      pane
    (when (or sel-start sel-end)
      (setf sel-start nil sel-end nil)

      ;; redraw with no selection
      (gp:invalidate-rectangle pane))))

(defmethod drag-cursed-pane ((pane cursed-pane) x y)
  "Drag the cursor, extend the current selection."
  (with-slots (metrics sel-start sel-end chars-wide chars-high)
      pane
    (let ((x (truncate x (cursed-font-width metrics)))
          (y (truncate y (cursed-font-height metrics))))

      ;; only update if within the pane
      (when (and (<= 0 x (1- chars-wide))
                 (<= 0 y (1- chars-high)))

        ;; update the selection
        (setf sel-end (+ (* y chars-wide) x))

        ;; start need set too?
        (unless sel-start
          (setf sel-start sel-end))

        ;; redraw with the new selection
        (gp:invalidate-rectangle pane)))))

(defmethod (setf cursed-pane-selection-visible-p) (visible-p (pane cursed-pane))
  "Set whether or not the selection is visible."
  (setf (slot-value pane 'sel-visible) visible-p)

  ;; redraw with (or without) the selection
  (gp:invalidate-rectangle pane))

(defmethod cursed-pane-selection-start ((pane cursed-pane))
  "Return the (x y) coordinates where the selection starts or nil if not set."
  (with-slots (sel-start chars-wide)
      pane
    (and sel-start (let ((y (truncate sel-start chars-wide)))
                     (list (- sel-start (* y chars-wide)) y)))))

(defmethod (setf cursed-pane-selection-start) (pos (pane cursed-pane))
  "Set the selection start position. Can be NIL."
  (with-slots (sel-start chars-wide chars-high)
      pane
    (setf sel-start (when pos
                      (destructuring-bind (x y)
                          pos
                        (when (and (<= 0 x (1- chars-wide))
                                   (<= 0 y (1- chars-high)))
                          (+ (* y chars-wide) x)))))

    ;; redraw with the selection
    (gp:invalidate-rectangle pane)))
                      
(defmethod cursed-pane-selection-end ((pane cursed-pane))
  "Return the (x y) coordinates where the selection ends or nil if not set."
  (with-slots (sel-end chars-wide)
      pane
    (and sel-end (let ((y (truncate sel-end chars-wide)))
                   (list (- sel-end (* y chars-wide)) y)))))

(defmethod (setf cursed-pane-selection-end) (pos (pane cursed-pane))
  "Set the selection end position. Can be NIL."
  (with-slots (sel-end chars-wide chars-high)
      pane
    (setf sel-end (when pos
                    (destructuring-bind (x y)
                        pos
                      (when (and (<= 0 x (1- chars-wide))
                                 (<= 0 y (1- chars-high)))
                        (+ (* y chars-wide) x)))))
    
    ;; redraw with the selection
    (gp:invalidate-rectangle pane)))

(defmethod cursed-pane-scroll ((pane cursed-pane))
  "Scroll all the characters on the pane. The cursor does not move."
  (with-slots (metrics bg-pixmap fg-pixmap chars chars-wide)
      pane
    (let ((w (gp:port-width pane))
          (h (gp:port-height pane)))

      ;; scroll up and clear the bottom line
      (let ((fh (cursed-font-height metrics)))
        (gp:draw-image bg-pixmap (gp:make-image-from-port bg-pixmap) 0 0 :from-y (1- fh))
        (gp:draw-image fg-pixmap (gp:make-image-from-port fg-pixmap) 0 0 :from-y (1- fh))

        ;; clear the background of the bottom line
        (gp:draw-rectangle bg-pixmap 0 h w (- fh) :filled t :shape-mode :plain)
        (gp:draw-rectangle bg-pixmap 0 h w (- fh) :filled t :foreground :transparent :operation :copy))

      ;; update the characters
      (let ((new-line (make-string chars-wide :initial-element #\space)))
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
                            :for i :from start :to end
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

(defmethod cursed-pane-paste ((pane cursed-pane))
  "Paste the clipboard text into the pane."
  (write-string (clipboard pane :string) pane))

(defmethod (setf cursed-pane-cursor-visible-p) (visible-p (pane cursed-pane))
  "Change whether or not the cursor is visible."
  (with-slots (cursor-visible)
      pane
    (setf cursor-visible visible-p)

    ;; redraw the pane
    (gp:invalidate-rectangle pane)))

(defmethod (setf cursed-pane-cursor-x) (x (pane cursed-pane))
  "Redraw when positioning the cursor."
  (file-position pane (list x (cursed-pane-cursor-y pane))))

(defmethod (setf cursed-pane-cursor-y) (y (pane cursed-pane))
  "Redraw when positioning the cursor."
  (file-position pane (list (cursed-pane-cursor-x pane) y)))

(defmethod stream:stream-file-position ((pane cursed-pane))
  "Return the current cursor position."
  (list (cursed-pane-cursor-x pane)
        (cursed-pane-cursor-y pane)))

(defmethod (setf stream:stream-file-position) (new-pos (pane cursed-pane))
  "Set the position of the cursor."
  (destructuring-bind (x y)
      new-pos
    (setf (slot-value pane 'cursor-x) x
          (slot-value pane 'cursor-y) y)

    ;; redraw when the cursor is visible
    (when (cursed-pane-cursor-visible-p pane)
      (gp:invalidate-rectangle pane))))

(defmethod stream:stream-clear-output ((pane cursed-pane))
  "Wipe the pixmap."
  (with-slots (bg-pixmap fg-pixmap chars chars-wide chars-high cursor-x cursor-y)
      pane
    ;; erase the pixmaps
    (gp:clear-graphics-port bg-pixmap)
    (gp:clear-graphics-port fg-pixmap)

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
  (with-slots (metrics bg-pixmap fg-pixmap chars (x cursor-x) (y cursor-y) chars-wide chars-high)
      pane
    (when (and (<= 0 x (1- chars-wide))
               (<= 0 y (1- chars-high)))
      (case char

        ;; newlines
        (#\return    (setf x 0))
        (#\linefeed  (terpri pane))
        
        ;; backup the cursor
        (#\backspace (setf x (max (1- x) 0)))
        
        ;; advance the cursor to the next column marker
        (#\tab       (loop :for i :from x :below (+ (logand x 8) 8) :do (write-char #\space pane)))
        
        ;; all other characters are written
        (otherwise   (setf (aref chars (+ (* y chars-wide) x)) char)
                     
                     ;; render to the pixmap
                     (let ((fw (cursed-font-width metrics))
                           (fh (cursed-font-height metrics))
                           (fa (cursed-font-ascent metrics))
                           (fi (cursed-font-indent metrics)))
                       (gp:draw-rectangle bg-pixmap (* x fw) (* y fh) fw fh :filled t :shape-mode :plain)
                       (gp:draw-character fg-pixmap char (+ (* x fw) fi) (+ (* y fh) fa) :block nil))
                     
                     ;; advance the cursor (wrap lines)
                     (when (>= (incf x) chars-wide)
                       (terpri pane)))))))

(defmethod stream:stream-write-string ((pane cursed-pane) string &optional (start 0) (end (length string)))
  "Output a value at the current cursor position."
  (loop :for i :from start :below end :do (stream:stream-write-char pane (char string i))))
