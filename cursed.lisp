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
  (:use :cl :capi)
  (:export
   #:cursed-pane

   ;; accessors
   #:cursed-pane-chars
   #:cursed-pane-chars-wide
   #:cursed-pane-chars-high
   #:cursed-pane-cursor-x
   #:cursed-pane-cursor-y
   #:cursed-pane-cursor-visible-p

   ;; methods
   #:princ-to-pane
   ))

(in-package :cursed)

(defclass cursed-pane (output-pane)
  ((chars)

   ;; the size of the pane
   (chars-wide     :initarg :chars-wide     :initform 80 :reader cursed-pane-chars-wide)
   (chars-high     :initarg :chars-high     :initform 24 :reader cursed-pane-chars-high)

   ;; true if the cursor should be drawn
   (cursor-visible :initarg :cursor-visible :initform t  :accessor cursed-pane-cursor-visible-p)

   ;; xy location of the cursor
   (cursor-x       :initarg :cursor-x       :initform 0  :accessor cursed-pane-cursor-x)
   (cursor-y       :initarg :cursor-y       :initform 0  :accessor cursed-pane-cursor-y))
  (:default-initargs
   :background :black
   :foreground :gray90
   :font (gp:make-font-description :family "Courier" :size 12.0)
   :create-callback 'create-cursed-pane
   :display-callback 'display-cursed-pane))

(defstruct glyph (char " ") bg fg)

(defmethod create-cursed-pane ((pane cursed-pane))
  "Set the size of the pane and clear the output."
  (with-slots (chars chars-wide chars-high)
      pane
    (set-hint-table pane (list :visible-min-width (list 'character chars-wide)
                               :visible-max-width (list 'character chars-wide)
                               :visible-min-height (list 'character chars-high)
                               :visible-max-height (list 'character chars-high)))

    ;; allocate an array of strings for all the characters (column major)
    (setf chars (make-array (list chars-wide chars-high) :element-type 'glyph))

    ;; fill in the characters with default glyphs
    (dotimes (x chars-wide)
      (dotimes (y chars-high)
        (setf (aref chars x y) (make-glyph))))))

(defmethod display-cursed-pane ((pane cursed-pane) x y w h)
  "Redraw characters in the pane."
  (with-slots (chars cursor-x cursor-y cursor-visible)
      pane
    (let ((cw (gp:get-font-width pane))
          (ch (gp:get-font-height pane))
          (fd (gp:get-font-descent pane)))

      ;; erase the area that is to be redrawn
      (gp:draw-rectangle pane x y w h :filled t :foreground (simple-pane-background pane))

      ;; render the block cursor first
      (when cursor-visible
        (gp:draw-rectangle pane (* cursor-x cw) (* cursor-y ch) (1+ cw) (1+ ch) :filled t))

      ;; render all the characters in the area that needs redrawn
      (flet ((blit-glyph (x y &aux (glyph (aref chars x y)))
               (with-slots (char bg fg)
                   glyph
                 (when bg
                   (gp:draw-rectangle pane (* x cw) (* y ch) (1+ cw) (1+ ch) :filled t :foreground bg))
                 (gp:draw-string pane char (* x cw) (- (+ (* y ch) ch) fd) :foreground fg))))
        (loop :for i :from (truncate x cw) :below (truncate (+ x w) cw) :do
              (loop :for j :from (truncate y ch) :below (truncate (+ y h) ch) :do
                    (blit-glyph i j)))))))

(defmethod princ-to-pane (value (pane cursed-pane) &key background foreground)
  "Output a value at the current cursor position."
  (with-slots (chars cursor-x cursor-y chars-wide chars-high)
      pane
    (loop :for c :across (princ-to-string value)
          :do (let ((glyph (aref chars cursor-x cursor-y)))
                (setf (glyph-char glyph) (string c))

                ;; optional color settings
                (setf (glyph-bg glyph) background)
                (setf (glyph-fg glyph) foreground)

                ;; advance the cursor
                (when (>= (incf cursor-x) chars-wide)
                  (if (>= (incf cursor-y) chars-high)
                      (return nil)
                    (setf cursor-x 0)))))
    
    ;; TODO: limit the redraw to only what needs to be done
    (gp:invalidate-rectangle pane)))
