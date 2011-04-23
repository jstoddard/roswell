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
(defparameter +gravity+ 2)

(defparameter *player-won* nil)

;;; These will hold the SDL surfaces containing tiles and sprites
(defparameter *characters* nil)
(defparameter *tiles* nil)

;;; Some utilities that may come in handy...
(defun word-wrap (string num-chars)
  "Add newlines to string to keep each line <= num-chars."
  (let ((string (remove #\Newline string)))
    (cond ((<= (length string) num-chars) string)
          (t
           (let ((string1 (subseq string 0 num-chars))
                 (string2 (subseq string num-chars)))
             (setf string1 (substitute #\Newline #\Space string1
                                       :count 1 :from-end t))
             (setf string2 (concatenate
                            'string
                            (subseq string1 (or (position #\Newline string1)
                                                num-chars))
                            string2))
             (setf string1 (subseq string1 0 (1+ (or (position #\Newline string1)
                                                     (1- num-chars)))))
             (concatenate 'string string1 (word-wrap string2 num-chars)))))))

;;; This class defines a game character. Both the player and the bad guys
;;; will be subclassed from this. The position and velocity variables are
;;; self-explanatory. direction is the direction in which the projectile
;;; will be sent if the character shoots, and also is used in determining
;;; how the character will be drawn. step-count determines the animation
;;; frame. hit-points determines how much damage the character can take
;;; before dying. sprite indicates which sprites to use for drawing the
;;; character.
(defparameter *player* nil) ; Instance of player representing the player

(defclass game-char ()
  ((x-position :initarg :x-position :accessor x-position)
   (y-position :initarg :y-position :accessor y-position)
   (x-velocity :initarg :x-velocity :accessor x-velocity)
   (y-velocity :initarg :y-velocity :accessor y-velocity)
   (direction :initarg :direction :accessor direction)
   (step-count :initarg :step-count :initform 0 :accessor step-count)
   (hit-points :initarg :hit-points :accessor hit-points)
   (sprite :initarg :sprite :accessor sprite)))

(defclass player (game-char)
  ((points :initarg :points :initform 0 :accessor points)
   (keys :initarg :keys :initform 0 :accessor keys)
   (max-hp :initarg :max-hp :initform 15 :accessor max-hp)))

;;; The level-map class contains the details of a map. The bg-map,
;;; obstacle-map, and object-map variables are two-dimensional arrays
;;; of map-width by map-height size.
(defparameter *current-level* nil) ; Map of current level
(defparameter *game-levels* nil) ; All maps that will be used in the game

(defclass level-map ()
  ((map-width :initarg :map-width :accessor map-width)
   (map-height :initarg :map-height :accessor map-height)
   (bg-map :initarg :bg-map :accessor bg-map)
   (obstacle-map :initarg :obstacle-map :accessor obstacle-map)
   (object-map :initarg :object-map :accessor object-map)
   (load-function :initarg :load-function :accessor load-function)))

(defun load-level (map-file)
  "Load the map given by map-file."
  (load (concatenate 'string +data-directory+ map-file)))

(defun add-level (map)
  "Append map to *game-levels*, creating a new map accessible to the game
via select-map."
 (setf *game-levels* (append *game-levels* (list map))))

(defun select-level (level)
  "Look up the map for the given level and set it as the current map. (i.e.
set *current-level* to that map)."
  (setf *current-level* (nth level *game-levels*))
  (if *current-level*
      (funcall (load-function *current-level*))
      (setf *player-won* t)))

;;; Drawing functions
(defun draw-tile (tile-set tile-number x y)
  "Draw the tile given by tile-number to the display surface at x, y.
tile-number should be an integer between 0 and 299, inclusive."
  (multiple-value-bind (tile-y tile-x) (floor tile-number 20)
    (sdl:set-cell (sdl:rectangle :x (* tile-x 32)
				 :y (* tile-y 32)
				 :w 32 :h 32)
		  :surface tile-set)
    (sdl:draw-surface-at tile-set (sdl:point :x x :y y))))

(defun draw-map ()
  "Place tiles from the map on the display surface so that player is centered
on the screen."
  (let ((top-x (- (x-position *player*) (floor +screen-width+ 2)))
	(top-y (- (y-position *player*) (floor +screen-height+ 2)))
	start-x start-y tile-x tile-y)
    (sdl:clear-display sdl:*black*)
    (setf start-x (- 0 (mod top-x 32)))
    (setf start-y (- 0 (mod top-y 32)))
    (loop for x upfrom start-x by 32
       when (>= x +screen-width+) return nil
       do (loop for y upfrom start-y by 32
	     when (>= y +screen-height+) return nil
	     when (and (>= (+ top-x x) 0) (>= (+ top-y y) 0))
	     do (progn
		  (setf tile-x (floor (+ top-x x) 32)
			tile-y (floor (+ top-y y) 32))
		  (when (and (< tile-x (map-width *current-level*))
			     (< tile-y (map-height *current-level*)))		    
		    (draw-tile *tiles* (aref (bg-map *current-level*)
					     tile-x tile-y) x y)
		    (when (<= 0 (aref (obstacle-map *current-level*) tile-x tile-y))
		      (draw-tile *tiles* (aref (obstacle-map *current-level*)
					       tile-x tile-y) x y))
		    (when (<= 0 (aref (object-map *current-level*) tile-x tile-y))
		      (draw-tile *tiles* (aref (object-map *current-level*)
					       tile-x tile-y) x y))))))))

(defun draw-player ()
  "Draw player at center of screen."
  (let ((sprite (+ (sprite *player*) (ash (step-count *player*) -4))))
    (case (direction *player*)
      (right nil)
      (left (setf sprite (+ sprite 4)))
      (up (setf sprite (+ sprite 8)))
      (down (setf sprite (+ sprite 12))))
    (draw-tile *characters* (+ sprite 20)
	       (- (ash +screen-width+ -1) 16)
	       (- (ash +screen-height+ -1) 16))
    (draw-tile *characters* sprite
	       (- (ash +screen-width+ -1) 16)
	       (- (ash +screen-height+ -1) 48))))

;;; Functions to load game assets
(defun load-assets ()
  "Load media (images, sound) that will be used by the game."
  (setf *characters* (sdl:convert-to-display-format
		      :surface (sdl:load-image
				(concatenate 'string +data-directory+
					     "media/characters.png")
				:image-type :png)
		      :pixel-alpha t))
  (setf *tiles*
	(sdl:convert-to-display-format
	 :surface (sdl:load-image
		   (concatenate 'string +data-directory+ "media/tiles.png")
		   :image-type :png)
	 :pixel-alpha t)))

;;; Clear things out for new game, etc.
(defun clear-game ()
  "Sets game state globals back to their original values..."
  (setf *game-levels* nil)
  (setf *current-level* nil)
  (setf *player-won* nil))

;;; Game Loop
(defun main ()
  (clear-game)
  (setf *player* (make-instance 'player
				:x-position 1 :y-position 1
				:direction 'right
				:hit-points 15
				:sprite 0))
  (sdl:with-init ()
    (sdl:window +screen-width+ +screen-height+
		:double-buffer t :title-caption "Roswell")
    (setf (sdl:frame-rate) 30)
    (sdl:initialise-default-font)
    ;; load our game assets
    (load-assets)
    (load-level "roswell.map")
    (select-level 0)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
        (cond ((sdl:key= key :sdl-key-escape) (sdl:push-quit-event))))
      (:idle ()
       (draw-map)
       (draw-player)
       (sdl:update-display)))))
