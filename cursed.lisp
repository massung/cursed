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
   :draw-with-buffer t
   :font (gp:make-font-description :family "Courier" :size 12.0)
   :create-callback 'create-cursed-pane
   :destroy-callback 'destroy-cursed-pane
   :resize-callback 'resize-cursed-pane
   :display-callback 'display-cursed-pane
   :input-model '()))

(defmethod create-cursed-pane ((pane cursed-pane))
  "Set the size of the pane and clear the output."
  (with-slots (chars chars-wide chars-high)
      pane
    (set-hint-table pane (list :visible-min-width (list 'character chars-wide)
                               :visible-max-width (list 'character chars-wide)
                               :visible-min-height (list 'character chars-high)
                               :visible-max-height (list 'character chars-high)))

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
  (with-slots (pixmap)
      pane
    (when pixmap
      (gp:destroy-pixmap-port pixmap))
    (setf pixmap (gp:create-pixmap-port pane w h :background (simple-pane-background pane) :clear t))))

(defmethod display-cursed-pane ((pane cursed-pane) x y w h)
  "Redraw characters in the pane."
  (with-slots (pixmap (x cursor-x) (y cursor-y) cursor-visible)
      pane
    (let ((fw (gp:get-font-average-width pane))
          (fh (gp:get-font-height pane))
          (fa (gp:get-font-ascent pane)))
      (when pixmap
        (gp:draw-image pane (gp:make-image-from-port pixmap) 0 0))
      
      ;; render the block cursor over them
      (when cursor-visible
        (gp:draw-rectangle pane (* x fw) (+ (* y fh) fa 1) fw 3 :filled t)))))

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
  (with-slots (pixmap chars chars-wide chars-high)
      pane
    (let ((w (gp:port-width pixmap))
          (h (gp:port-height pixmap)))
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
  (with-slots (cursor-x cursor-y)
      pane
    (unless (zerop cursor-x)
      (incf cursor-y)
      (setf cursor-x 0))))

(defmethod stream:stream-write-char ((pane cursed-pane) char)
  "Output a single character at the current cursor position."
  (with-slots (pixmap chars (x cursor-x) (y cursor-y) chars-wide chars-high)
      pane
    (let ((fw (gp:get-font-average-width pane))
          (fh (gp:get-font-height pane))
          (fa (gp:get-font-ascent pane)))
      (gp:draw-string pixmap (string char) (* x fw) (+ (* y fh) fa) :font (simple-pane-font pane))

      ;; bash the character buffer with the new char
      (setf (aref chars x y) char)

      ;; advance the cursor (wrap lines)
      (when (= (incf x) chars-wide)
        (when (= (incf y) chars-high)
          (setf y 0))
        (setf x 0)))))

(defmethod stream:stream-write-string ((pane cursed-pane) string &optional (start 0) (end (length string)))
  "Output a value at the current cursor position."
  (loop :for i :from start :below end :do (stream:stream-write-char pane (char string i))))
