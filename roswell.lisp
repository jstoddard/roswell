;;;;---------------------------------------------------------------------------
;;;; roswell.lisp
;;;; A game for the 2011 Spring Lisp Game Jam
;;;; Copyright (C) 2011 Jeremiah Stoddard
;;;;
;;;;     This program is free software: you can redistribute it and/or modify
;;;;     it under the terms of the GNU General Public License as published by
;;;;     the Free Software Foundation, either version 3 of the License, or
;;;;     (at your option) any later version.
;;;;
;;;;     This program is distributed in the hope that it will be useful,
;;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;     GNU General Public License for more details.
;;;;
;;;;     You should have received a copy of the GNU General Public License
;;;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;---------------------------------------------------------------------------

(in-package #:roswell)

(defparameter +data-directory+ "/home/jeremiah/src/lisp/roswell/")
(defparameter +screen-width+ 640)
(defparameter +screen-height+ 480)

;;; This class defines a game character. Both the player and the bad guys
;;; will be subclassed from this. The position and velocity variables are
;;; self-explanatory. direction is the direction in which the projectile
;;; will be sent if the character shoots, and also is used in determining
;;; how the character will be drawn. step-count determines the animation
;;; frame. hit-points determines how much damage the character can take
;;; before dying. sprite indicates which sprites to use for drawing the
;;; character.
(defclass game-char ()
  ((x-position :initarg :x-position :accessor x-position)
   (y-position :initarg :y-position :accessor y-position)
   (x-velocity :initarg :x-velocity :accessor x-velocity)
   (y-velocity :initart :y-velocity :accessor y-velocity)
   (direction :initarg :direction :accessor direction)
   (step-count :initarg :step-count :initform 0 :accessor step-count)
   (hit-points :initarg :hit-points :accessor hit-points)
   (sprite :initarg :sprite :accessor sprite)))

(defclass player (game-char)
  ((points :initarg :points :initform 0 :accessor points)
   (keys :initarg :keys :initform 0 :accessor keys)
   (max-hp :initarg :max-hp :initform 15 :accessor max-hp)))

;;; Game Loop
(defun main ()
  (sdl:with-init ()
    (sdl:window +screen-width+ +screen-height+
		:double-buffer t :title-caption "Roswell")
    (setf (sdl:frame-rate) 30)
    (sdl:initialise-default-font)
    ;; load our game assets
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
        (cond ((sdl:key= key :sdl-key-escape) (sdl:push-quit-event))))
      (:idle ()
       (sdl:update-display)))))
