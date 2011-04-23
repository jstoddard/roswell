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
(defparameter *game-counter* 0) ;; for animation & other timing needs

;;; These will hold the SDL surfaces containing tiles and sprites
(defparameter *characters* nil)
(defparameter *tiles* nil)
(defparameter *projectile* nil)

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

(declaim (inline get-collision-tiles))
(defun get-collision-tiles (x y &optional (x-bbox 13) (y-bbox 15))
  "Return the tiles that a character in position (x,y) would collide with,
assuming a 26x26 bounding box."
  (remove-duplicates
   (list
    (list (ash (- x x-bbox) -5) (ash (- y y-bbox) -5))
    (list (ash (+ x x-bbox) -5) (ash (- y y-bbox) -5))
    (list (ash (+ x x-bbox) -5) (ash (+ y y-bbox) -5))
    (list (ash (- x x-bbox) -5) (ash (+ y y-bbox) -5)))
   :test #'equal))

;;; This class defines a game character. Both the player and the bad guys
;;; will be subclassed from this. The position and velocity variables are
;;; self-explanatory. direction is the direction in which the projectile
;;; will be sent if the character shoots, and also is used in determining
;;; how the character will be drawn. step-count determines the animation
;;; frame. hit-points determines how much damage the character can take
;;; before dying. sprite indicates which sprites to use for drawing the
;;; character.
(defparameter *player* nil) ; Instance of player representing the player
(defparameter *enemies* nil) ; List containing instances of all enemies

(defclass game-char ()
  ((x-position :initarg :x-position :accessor x-position)
   (y-position :initarg :y-position :accessor y-position)
   (x-velocity :initarg :x-velocity :initform 0 :accessor x-velocity)
   (y-velocity :initarg :y-velocity :initform 0 :accessor y-velocity)
   (direction :initarg :direction :initform 'right :accessor direction)
   (step-count :initarg :step-count :initform 0 :accessor step-count)
   (hit-points :initarg :hit-points :accessor hit-points)
   (sprite :initarg :sprite :accessor sprite)))

(defmethod collisionp ((character game-char))
  (cond ((or (>= (+ (x-position character) 16) (ash (map-width *current-level*) 5))
	     (< (- (x-position character) 16) 0)
	     (>= (+ (y-position character) 16) (ash (map-height *current-level*) 5))
	     (< (- (y-position character) 16) 0)) t)
	((and (not (position nil 
			     (mapcar #'(lambda (x)
					 (= -1 (aref (obstacle-map *current-level*)
						     (first x) (second x))))
				     (get-collision-tiles
				      (x-position character)
				      (y-position character))))))
	 nil)
	(t t)))

(defmethod move ((character game-char) direction)
  (case direction
    (up (decf (y-position character) 2)
	(when (collisionp character)
	  (incf (y-position character) 2)
	  (setf (y-velocity character) 0)))
    (down (incf (y-position character) 2)
	  (when (collisionp character)
	    (decf (y-position character) 2)
	    (setf (y-velocity character) 0)))
    (left (setf (direction character) 'left)
	  (decf (x-position character) 2)
	  (when (collisionp character)
	    (incf (x-position character) 2)
	    (setf (x-velocity character) 0)))
    (right (setf (direction character) 'right)
	   (incf (x-position character) 2)
	   (when (collisionp character)
	     (decf (x-position character) 2)
	     (setf (x-velocity character) 0))))
  (when (or (eq direction 'left) (eq direction 'right))
    (setf (step-count character) (mod (1+ (step-count character)) 32))))

(defmethod shoot ((character game-char) &optional (sprite 2))
  (let ((x (x-position character)) (y (- (y-position character) 10)))
    (cond ((eq (direction character) 'up) (decf y 38))
	  ((eq (direction character) 'down) (incf y 16))
	  ((eq (direction character) 'left) (decf x 16))
	  ((eq (direction character) 'right) (incf x 16)))
    (make-instance 'projectile :x-position x :y-position y
		   :direction (direction character) :sprite sprite)))

(defclass player (game-char)
  ((points :initarg :points :initform 0 :accessor points)
   (keys :initarg :keys :initform 0 :accessor keys)
   (max-hp :initarg :max-hp :initform 15 :accessor max-hp)))

(defmethod jump ((player player))
  (when	(and (position nil 
		       (mapcar #'(lambda (x)
				   (= -1 (aref (obstacle-map *current-level*)
					       (first x) (second x))))
			       (get-collision-tiles
				(x-position player)
				(y-position player) 1 16))))
    (setf (y-velocity player) -20)))

(defmethod shoot ((player player)
		  &optional (sprite (if (or (eq (direction player) 'up)
					    (eq (direction player) 'down))
					1 0)))
  (call-next-method player sprite))

;;; The projectile class contains details of a bullet or a laser beam.
(defparameter *projectiles* nil)

(defclass projectile ()
  ((x-position :initarg :x-position :accessor x-position)
   (y-position :initarg :y-position :accessor y-position)
   (direction :initarg :direction :accessor direction)
   (ttl :accessor ttl)
   (sprite :initarg :sprite :initform 2 :accessor sprite)))

(defmethod initialize-instance :after ((projectile projectile) &rest rest)
  (declare (ignore rest))
  (case (direction projectile)
    (up (setf (ttl projectile) (floor (+ (ash +screen-height+ -1)
					 (- (y-position projectile)
					    (y-position *player*))) 6)))
    (down (setf (ttl projectile) (floor (+ (ash +screen-height+ -1)
					   (- (y-position *player*)
					      (y-position projectile))) 6)))
    (left (setf (ttl projectile) (floor (+ (ash +screen-width+ -1)
					   (- (x-position projectile)
					      (x-position *player*))) 6)))
    (right (setf (ttl projectile) (floor (+ (ash +screen-width+ -1)
					    (- (x-position *player*)
					       (x-position projectile))) 6))))
  (when (null (ttl projectile)) (setf (ttl projectile) 0))
  (push projectile *projectiles*))

(defmethod detect-projectile-collision ((projectile projectile))
  (dolist (enemy *enemies*)
    (when (and (> (x-position projectile) (- (x-position enemy) 16))
	       (< (x-position projectile) (+ (x-position enemy) 16))
	       (> (y-position projectile) (- (y-position enemy) 16))
	       (< (y-position projectile) (+ (y-position enemy) 16)))
      (decf (hit-points enemy))
      (setf (ttl projectile) 0)))
  (when (and (> (x-position projectile) (- (x-position *player*) 16))
	     (< (x-position projectile) (+ (x-position *player*) 16))
	     (> (y-position projectile) (- (y-position *player*) 16))
	     (< (y-position projectile) (+ (y-position *player*) 16)))
    (decf (hit-points *player*))
    (setf (ttl projectile) 0)))

(defun update-projectiles ()
  "Loop through the projectiles, updating position, looking for collisions, and
finally remove old and collided projectiles."
  (dolist (projectile *projectiles*)
    (decf (ttl projectile))
    (cond ((eq (direction projectile) 'up)
	   (decf (y-position projectile) 6))
	  ((eq (direction projectile) 'down)
	   (incf (y-position projectile) 6))
	  ((eq (direction projectile) 'left)
	   (decf (x-position projectile) 6))
	  ((eq (direction projectile) 'right)
	   (incf (x-position projectile) 6)))
    (detect-projectile-collision projectile)
    (draw-tile *projectile* (sprite projectile)
	       (+ (- (x-position projectile) 16 
		     (x-position *player*))
		  (ash +screen-width+ -1))
	       (+ (- (y-position projectile) 16
		     (y-position *player*))
		  (ash +screen-height+ -1))))
  (setf *projectiles* (remove-if-not #'(lambda (projectile)
					 (> (ttl projectile) 0))
				     *projectiles*)))

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
  (let ((sprite (+ (sprite *player*) (ash (step-count *player*) -3))))
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
	 :pixel-alpha t))
  (setf *projectile* (sdl:convert-to-display-format
		      :surface (sdl:load-image
				(concatenate 'string +data-directory+
					     "media/projectiles.png")
				:image-type :ping)
		      :pixel-alpha t)))

;;; Logic functions -- things that will be applied every run through
;;; the game loop.
(defun apply-gravity ()
  "Adjust player and enemy y-velocity for the effects of gravity."
  (incf (y-velocity *player*) +gravity+))

(defun move-characters ()
  "Move the game characters according to their velocities."
  (when (> (x-velocity *player*) 0)
    (dotimes (i (x-velocity *player*)) (move *player* 'right)))
  (when (< (x-velocity *player*) 0)
    (dotimes (i (- 0 (x-velocity *player*))) (move *player* 'left)))
  (when (> (y-velocity *player*) 0)
    (dotimes (i (y-velocity *player*)) (move *player* 'down)))
  (when (< (y-velocity *player*) 0)
    (dotimes (i (- 0 (y-velocity *player*))) (move *player* 'up))))

;;; Clear things out for new game, etc.
(defun clear-game ()
  "Sets game state globals back to their original values..."
  (setf *game-levels* nil)
  (setf *current-level* nil)
  (setf *projectiles* nil)
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
       (setf *game-counter* (mod (1+ *game-counter*) 128))
       (apply-gravity)
       (setf (x-velocity *player*) 0)
       (when (sdl:get-key-state :sdl-key-left) (setf (x-velocity *player*) -2))
       (when (sdl:get-key-state :sdl-key-right) (setf (x-velocity *player*) 2))
       (when (or
	       (sdl:get-key-state :sdl-key-lctrl)
	       (sdl:get-key-state :sdl-key-rctrl))
	 (jump *player*))
       (when (and (= 0 (mod *game-counter* 8))
		  (or
		   (sdl:get-key-state :sdl-key-lalt)
		   (sdl:get-key-state :sdl-key-ralt)))
	 (shoot *player*))
       (move-characters)
       (draw-map)
       (draw-player)
       (update-projectiles)
       (sdl:update-display)))))
