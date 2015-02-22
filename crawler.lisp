(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 0) (compilation-speed 0) (safety 0) (debug 0)))
  (asdf:load-system :sdl2kit)
  (asdf:load-system :cl-jpeg)
  (asdf:load-system :png-read)
  (asdf:load-system :static-vectors)
  ;;(asdf:load-system :cl-opengl)
  )

(defparameter width 160)
(defparameter height 120)

(defparameter window-width (* width 4))
(defparameter window-height (* height 4))

(defparameter *num-frames* 0)
(defparameter *last-ticks* 0)
(defparameter *now-ticks* 0)
(defparameter *diff-ticks* 0)
(defparameter *image* 0)

(defstruct (image :conc-name) data width height depth pitch)

(declaim (inline getOffset))
(defun getOffset (x y w)
  (declare (type fixnum x y w))
  (the fixnum (+ (* y w) x)))


(defun gl-ortho-setup (&key (width 500) (height 500))
  "Set up 1:1 pixel ortho matrix"
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:ortho 0 width height 0 -1 1))

;;stolen form https://github.com/froggey/Mezzano/blob/master/gui/desktop.lisp - thanks mate!
(defun load-png (path)
  (ignore-errors
    (let* ((png (png-read:read-png-file path))
           (data (png-read:image-data png))
	   (size (array-dimension data 2))
           (width (png-read:width png))
           (height (png-read:height png))
           (array (make-array (* height width) :element-type '(unsigned-byte 32))))
      (ecase (png-read:colour-type png)
        (:truecolor-alpha
         (dotimes (y height)
           (dotimes (x width)
             (setf (aref array (+ (* y width) x))
		   (logior
		    (ash (aref data x y 0) 16)
		    (ash (aref data x y 1) 8)
		    (aref data x y 2)
		    (ash (aref data x y 3) 24))))))
        (:truecolor
         (dotimes (y height)
           (dotimes (x width)
             (setf (aref array (+ (* y width) x)) (logior
						   (ash (aref data x y 0) 16)
						   (ash (aref data x y 1) 8)
						   (aref data x y 2)
						   (ash #xFF 24)))))))
      (make-image :data array :width width :height height :depth size :pitch (* width size)))))

(defun render (renderer window-width window-height width height image)

  (setf *last-ticks* (sdl2:get-ticks))

  (let* ((tex (sdl2:create-texture renderer :argb8888 :streaming window-width window-height))
	 (xd 0.0)
	 (yd 0.0)
	 (z 0.0)
	 (xx 0.0)
	 (yy 0.0)
	 (val 0)
	 (brightness .09)
	 (ticks (sdl2:get-ticks))
	 (eye (+ (* (sin (/ ticks 666)) 2) (* (cos (/ ticks 666)) 2)))
	 (size 35)
	 (src-rect (sdl2:make-rect 0 0 width height))
	 (dest-rect (sdl2:make-rect 0 0 window-width window-height))
	 (pixels (static-vectors:make-static-vector (* width height) :element-type '(unsigned-byte 32) :initial-element 0))
    	 (zbuffer (make-array (* width height) :element-type 'float :initial-element 10000.0)))

    (if (>= *diff-ticks* 1000)
	(progn
	  (format t "frames = ~D ~%" *num-frames*)    
	  (setf *num-frames* 0)
	  (setf *diff-ticks* 0)
	  (finish-output))
	(setf *num-frames* (+ *num-frames* 1)))
    
    (dotimes (y height)

       (setf yd (/ (- (+ y 0.5) (/ height 2.0)) height))
      
       (setf z (/ (+ size eye) yd))
       (if (< yd 0)
       	  (setf z (/ (- size eye) (* yd -1))))
      
      (dotimes (x width)
       	(setf xd (* (/ (- x (/ width 2)) height) z))
	
 	(setf xx (logand (floor xd) (- (width image) 1)))
 	(setf yy (logand (floor z) (- (height image) 1)))

       	(setf (aref zbuffer (getOffset x y width)) (floor (* z brightness)))
       	(setf (aref pixels (getOffset x y width)) (aref (data image) (getOffset xx yy (width image)))))

    
    (post-process pixels zbuffer width height)
    (sdl2:update-texture tex (static-vectors:static-vector-pointer pixels) :rect src-rect :width (* width 4))
    (sdl2:render-copy renderer tex :source-rect src-rect :dest-rect dest-rect)
    (sdl2:render-present renderer)
    (sdl2:destroy-texture tex)
    (static-vectors:free-static-vector pixels))
  (setf *now-ticks* (sdl2:get-ticks))
  (setf *diff-ticks* (+ (- *now-ticks* *last-ticks*) *diff-ticks*)))

(defun post-process (pixels zbuffer width height)
  (let ((col 0)
	(a 0)
	(r 0)
	(g 0)
	(b 0)
	(brightness 0)
	(res 0))
    (declare (type fixnum a r g b brightness res col))
    
    (dotimes (i (* width height))

      (setf col (aref pixels i))

      (setf a (logand #xff (ash col -24)))
      (setf r (logand #xff (ash col -16)))
      (setf g (logand #xff (ash col -8)))
      (setf b (logand #xff col))
      
      (setf brightness (- 255 (round (aref zbuffer i))))
      
      (if (< brightness 0)
	  (setf brightness 0))

      (setf r (round (/ (* r brightness) 255)))
      (setf g (round (/ (* g brightness) 255)))
      (setf b (round (/ (* b brightness) 255)))

      (setf res (logior
		 (ash a 24)
		 (ash r 16)
		 (ash g 8)
		 b))
      
      (setf (aref pixels i) res))))


(defun basic-test ()
  (setf *image* (load-png #p"~/Dropbox/mario.png"))
  (sdl2:with-init (:everything)
    (multiple-value-bind (window renderer)
	(sdl2:create-window-and-renderer window-width window-height '(:shown<))
      ;;   (sdl2:create-window-and-renderer window-width window-height '(:shown :opengl:VSYNC :accelerated))
      ;; (sdl2:with-gl-context (gl window)
      ;;   (sdl2:gl-make-current window gl)
      ;;   (gl:enable :texture-2d)
      ;;   (gl-ortho-setup :width window-width :height window-height)
      
      ;; main loop
      (format t "Beginning main loop.~%")
      (finish-output)
      (sdl2:with-event-loop (:method :poll)
	(:keydown
	 (:keysym keysym)
	 (let ((scancode (sdl2:scancode-value keysym))
	       (sym (sdl2:sym-value keysym))
	       (mod-value (sdl2:mod-value keysym)))
	   (cond
	     ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
	     ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
	     ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))
	   (format t "Key sym: ~a, code: ~a, mod: ~a~%"
		   sym
		   scancode
		   mod-value)))
	
	(:keyup
	 (:keysym keysym)
	 (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	   (sdl2:push-event :quit)))
	
	(:mousemotion
	 (:x x :y y :xrel xrel :yrel yrel :state state)
	 (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
		 x xrel y yrel state))
	(:idle
	 ()
	 (render renderer window-width window-height width height *image*)
	 )
	
	(:quit () t))
      (sdl2:destroy-renderer renderer)
      (sdl2:destroy-window window))))
;;)

#-clozure
(sdl2:make-this-thread-main (lambda () (basic-test)))
(basic-test)
