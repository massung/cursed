;;;; libcurses output-pane for LispWorks
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

   ;; accessors
   #:cursed-pane-pixmap
   #:cursed-pane-chars
   #:cursed-pane-chars-wide
   #:cursed-pane-chars-high
   #:cursed-pane-cursor-x
   #:cursed-pane-cursor-y
   #:cursed-pane-cursor-visible-p))

(in-package :cursed)

(defclass cursed-pane (output-pane stream:fundamental-character-output-stream)
  ((pixmap :initform nil :reader cursed-pane-pixmap)
   (chars  :initform nil :reader cursed-pane-chars)

   ;; the size of the pane
   (chars-wide :initarg :chars-wide :initform 80 :reader cursed-pane-chars-wide)
   (chars-high :initarg :chars-high :initform 24 :reader cursed-pane-chars-high)

   ;; true if the cursor should be drawn
   (cursor-visible :initarg :cursor-visible :initform t :accessor cursed-pane-cursor-visible-p)

   ;; xy location of the cursor
   (cursor-x :initarg :cursor-x :initform 0 :accessor cursed-pane-cursor-x)
   (cursor-y :initarg :cursor-y :initform 0 :accessor cursed-pane-cursor-y))
  (:default-initargs
   :background :black
   :foreground :gray90
   :visible-border nil
   :font (gp:make-font-description :family "Courier" :size 12.0)
   :create-callback 'create-cursed-pane
   :destroy-callback 'destroy-cursed-pane
   :resize-callback 'resize-cursed-pane
   :display-callback 'display-cursed-pane))

(defmethod create-cursed-pane ((pane cursed-pane))
  "Set the size of the pane and clear the output."
  (with-slots (chars chars-wide chars-high)
      pane
    (set-hint-table pane (list :visible-min-width (list 'character chars-wide)
                               :visible-max-width (list 'character chars-wide)
                               :visible-min-height (list 'character chars-high)
                               :visible-max-height (list 'character chars-high)))

    ;; allocate an array of strings for all the characters (column major)
    (setf chars (make-array (list chars-wide chars-high) :initial-element #\space))))

(defmethod destroy-cursed-pane ((pane cursed-pane))
  "Free memory used by the pane."
  (with-slots (pixmap)
      pane
    (when pixmap
      (gp:destroy-pixmap-port pixmap))))

(defmethod resize-cursed-pane ((pane cursed-pane) x y w h)
  "Create the "
  (declare (ignore x y))
  (with-slots (pixmap)
      pane
    (when pixmap
      (gp:destroy-pixmap-port pixmap))
    (setf pixmap (gp:create-pixmap-port pane w h))))

(defmethod display-cursed-pane ((pane cursed-pane) x y w h)
  "Redraw characters in the pane."
  (with-slots (pixmap cursor-x cursor-y cursor-visible)
      pane
    (let ((cw (gp:get-font-average-width pane))
          (ch (gp:get-font-height pane))
          (fa (gp:get-font-ascent pane)))

      ;; render the display
      (when pixmap
        (gp:draw-image pane (gp:make-image-from-port pixmap) 0 0))

      ;; render the block cursor over them
      (when cursor-visible
        (gp:draw-rectangle pane (* cursor-x cw) (+ (* cursor-y ch) fa 1) cw 3 :filled t)))))

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
  (with-slots (pixmap)
      pane
    (let ((w (gp:port-width pixmap))
          (h (gp:port-height pixmap)))
      (apply-in-pane-process pane #'gp:draw-rectangle pixmap 0 0 w h :filled t))))

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
  (with-slots (cursor-x cursor-y)
      pane
    (unless (zerop cursor-x)
      (incf cursor-y)
      (setf cursor-x 0))))

(defmethod stream:stream-write-char ((pane cursed-pane) char)
  "Output a single character at the current cursor position."
  (with-slots (pixmap (x cursor-x) (y cursor-y) chars-wide chars-high)
      pane
    (flet ((render ()
             (let ((cw (gp:get-font-average-width pane))
                   (ch (gp:get-font-height pane))
                   (fa (gp:get-font-ascent pane)))
               (gp:draw-string pixmap (string char) (* x cw) (+ (* y ch) fa) :font (simple-pane-font pane))

               ;; advance the cursor (wrap lines)
               (when (= (incf x) chars-wide)
                 (when (= (incf y) chars-high)
                   (setf y 0))
                 (setf x 0)))))
      (apply-in-pane-process pane #'render))))

(defmethod stream:stream-write-string ((pane cursed-pane) string &optional (start 0) (end (length string)))
  "Output a value at the current cursor position."
  (with-slots (pixmap chars cursor-x cursor-y chars-wide chars-high)
      pane
    (flet ((render ()
             (let ((cw (gp:get-font-average-width pane))
                   (ch (gp:get-font-height pane))
                   (fa (gp:get-font-ascent pane)))
               (loop :with font := (simple-pane-font pane)
                     :with i := start

                     ;; stop when the entire string is consumed
                     :while (< i end)

                     ;; get the longest character string we can blit
                     :for n := (min (- end i) (- chars-wide cursor-x))
                     :for x := (* cursor-x cw)
                     :for y := (* cursor-y ch)

                     ;; render and update the index and cursor
                     :do (let ((cs (subseq string i (incf i n))))
                           (gp:draw-string pixmap cs x (+ y fa) :font font)

                           ;; advance the cursor
                           (when (= (incf cursor-x n) chars-wide)
                             (if (= (incf cursor-y 1) chars-high)
                                 (return nil)
                               (setf cursor-x 0))))))))
      (apply-in-pane-process pane #'render))))

(defmethod stream:stream-write-sequence ((pane cursed-pane) seq start end)
  "Write a sequence of things to the pane."
  (flet ((render ()
           (loop :for i :from start :below end :do (princ (aref seq i) pane))))
    (apply-in-pane-process pane #'render)))