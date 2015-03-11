(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 0) (compilation-speed 0) (safety 0) (debug 0)))
  (asdf:load-system :sdl2kit)
  (asdf:load-system :png-read)
  (asdf:load-system :static-vectors)
;  (asdf:load-system :cl-opengl)
  )

(defvar *base-pathname* (or *load-truename* *compile-file-truename*))

(defun asset-path (file)
  (format t "loading file ~A~%"(merge-pathnames file *base-pathname*))
  (merge-pathnames file *base-pathname*))


(defparameter *state* 0)

(defparameter width 160)
(defparameter height 120)

(defparameter half-height (/ height 2))
(defparameter half-width (/ width 2))

(defparameter window-width (* width 4))
(defparameter window-height (* height 4))

(defparameter *num-frames* 0)
(defparameter *last-ticks* 0)
(defparameter *diff-ticks* 0)

(defparameter *xpos* 0)
(defparameter *ypos* 0)

(defparameter *step-x* 3)
(defparameter *step-y* 3)
(defparameter *light* 6)
(defparameter *image* 0)
(defparameter *phi* 0)
(defstruct (image :conc-name) data width height depth pitch)

(defparameter *pi/180* 0.017453292519943295d0)

(defparameter yd-values 0)

(defparameter *xrel-old* 0)
(defparameter *xrel-new* 0)

(defparameter *yrel-old* 0)
(defparameter *yrel-new* 0)

(setf yd-values (make-array (list height) :element-type 'float))
(dotimes (y height)
  (setf (aref yd-values y) (/ (- (+ y 0.5) half-height) height)))

(defparameter xd-values 0)

(setf xd-values (make-array (list width) :element-type 'float))
(dotimes (x width)
  (setf (aref xd-values x) (/ (- x half-width) height)))



(declaim (inline getOffset))
(defun getOffset (x y w)
  (declare (type fixnum x y w))
  (the fixnum (+ (* y w) x)))


;; (defun gl-ortho-setup (&key (width 500) (height 500))
;;   "Set up 1:1 pixel ortho matrix"
;;   (gl:viewport 0 0 width height)
;;   (gl:matrix-mode :projection)
;;   (gl:ortho 0 width height 0 -1 1))

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

(defun render (renderer window-width window-height width height image state)


  ;; (if (= *phi* 360)
  ;;     (setf *phi* 0))
  
  ;; (setf *phi* (+ *phi* 1))
	
  (if (>= *diff-ticks* 1000) 
      (progn

	(format t "frames = ~D phi = ~D ~%" *num-frames* *phi*)    
	(setf *num-frames* 0)
	(setf *diff-ticks* 0)
	(finish-output)))
  
  (if (= state 0)
      (progn
  (let* ((tex (sdl2:create-texture renderer :argb8888 :streaming window-width window-height))
	 (xd 0.0)
	 (yd 0.0)
	 (z 0)
	 (xx 0.0)
	 (yy 0.0)
	 (x1 0)
	 (y1 0)
	 (a 0)
	 (brightness 1)
	 (ticks *last-ticks*)
;	 (eye (sin (* ticks 100)))
	 (eye -5)
	 (size 16)
	 (src-rect (sdl2:make-rect 0 0 width height))
	 (dest-rect (sdl2:make-rect 0 0 window-width window-height))
	 (pixels (static-vectors:make-static-vector (* width height) :element-type '(unsigned-byte 32) :initial-element 0))
	 (zbuffer (static-vectors:make-static-vector (* width height) :element-type '(unsigned-byte 32) :initial-element 0))
	 (offset 0))

    (dotimes (y height)

      ;;(setf yd (/ (- (+ y 0.5) half-height) height))
      (setf yd (aref yd-values y))
      
      (setf z (/ (+ size eye) yd))
      (if (< yd 0)
       	  (setf z   (/ (- size eye) (- yd))))
      
      (dotimes (x width)

	;;(setf xd (+ (* (/ (- x half-width) height) z) (/ ticks 10)))
	;;division by 10 from http://electronics.stackexchange.com/questions/12618/fastest-way-to-get-integer-mod-10-and-integer-divide-10
;	(setf xd (+ (* (aref xd-values x) z) (ash (* ticks 205) -11)))
	(setf xd (+ (* (aref xd-values x) z)))

	(setf a (/ (* *phi* pi) 180))
	
	(setf x1 (- (* xd (cos a)) (* z (sin a)))) 
	(setf y1 (+ (* xd (sin a)) (* z (cos a)))) 
	
 	(setf xx (logand (truncate (+ x1 *xpos*)) (- (width image) 1)))
 	(setf yy (logand (truncate (+ y1 *ypos*)) (- (height image) 1)))

	(setf offset (getOffset x y width))
       	(setf (aref zbuffer offset) (truncate (* z brightness)))
       	(setf (aref pixels offset) (aref (data image) (getOffset xx yy (width image))))))

    
    (post-process pixels zbuffer width height)
    (sdl2:update-texture tex (static-vectors:static-vector-pointer pixels) :rect src-rect :width (* width 4))
    (sdl2:render-copy renderer tex :source-rect src-rect :dest-rect dest-rect)
    (sdl2:render-present renderer)
    (sdl2:destroy-texture tex)
    (sdl2:free-rect src-rect)
    (sdl2:free-rect dest-rect)
    (static-vectors:free-static-vector zbuffer)
    (static-vectors:free-static-vector pixels))))
  (setf *diff-ticks* (+ (- (sdl2:get-ticks) *last-ticks*) *diff-ticks*))
  (setf *num-frames* (+ *num-frames* 1))
  (setf *last-ticks* (sdl2:get-ticks)))

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
      
      (setf brightness (round (/ 20000 (* (aref zbuffer i) *light*))))
      ;(setf brightness (- 255 (aref zbuffer i)))
      
      (if (< brightness 0)
	  (setf brightness 0))

      (setf r (the fixnum (ash (* r brightness) -8)))
      (setf g (the fixnum (ash (* g brightness) -8)))
      (setf b (the fixnum  (ash (* b brightness) -8)))

      (setf res (logior
		 (ash a 24)
		 (ash r 16)
		 (ash g 8)
		 b))
      
      (setf (aref pixels i) res))))

(defun handle-mouse (x y xrel yrel state)
  
  (setf *xrel-new* (- xrel *xrel-old*))
  (setf *xrel-old* xrel)

  (setf *phi* (- x)))
  
  
  ;;  (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
  ;;	  x xrel y yrel state)


(defun handle-inputs (keysym)
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
    (sdl2:push-event :quit))
  
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-p)
    (setf *state* 1))
  
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-o)
    (setf *state* 0))

  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
    (setf *xpos* (- *xpos* *step-x*)))
  
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d)    
    (setf *xpos* (+ *xpos* *step-x*)))

  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
    (setf *ypos* (+ *ypos* *step-y*)))

  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
    (setf *ypos* (- *ypos* *step-y*)))

  
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-k)    
    (setf *light* (- *light* 1))
    (if (< *light* 0)
	(setf *light* 1)))
  
  
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-l)    
    (setf *light* (+ *light* 1))
    (if (< *light* 0)
	(setf *light* 1))))


(defun basic-test ()
  (setf *image* (load-png (asset-path #p"floor-tile.png")))
  (sdl2:with-init (:everything)
    (multiple-value-bind (window renderer)
	(sdl2:create-window-and-renderer window-width window-height '(:shown))
      ;;   (sdl2:create-window-and-renderer window-width window-height '(:shown :opengl))
      ;; (sdl2:with-gl-context (gl window)
      ;;   (sdl2:gl-make-current window gl)
      ;;   (gl:enable :texture-2d)
      ;;   (gl-ortho-setup :width window-width :height window-height)

;	(gl-set-attr 'attr value)
      ;; main loop
      (format t "Beginning main loop.~%")
      (finish-output)
      (sdl2:with-event-loop (:method :poll)
	(:keydown
	 (:keysym keysym)
	 (handle-inputs keysym))
	;;  (let ((scancode (sdl2:scancode-value keysym))
	;;        (sym (sdl2:sym-value keysym))
	;;        (mod-value (sdl2:mod-value keysym)))
	;;    (cond
	;;      ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
	;;      ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
	;;      ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))
	;;    (format t "Key sym: ~a, code: ~a, mod: ~a~%"
	;; 	   sym
	;; 	   scancode
	;; 	   mod-value)))
	
	;; (:keyup
	;;  (:keysym keysym))	 
	
	(:mousemotion
	 (:x x :y y :xrel xrel :yrel yrel :state state)
	 (handle-mouse x y xrel yrel state))
	
	(:idle
	 ()
	 (render renderer window-width window-height width height *image* *state*))
	
	(:quit () t))
      (sdl2:destroy-renderer renderer)
      (sdl2:destroy-window window))))
;)

#+sbcl
(sdl2:make-this-thread-main (lambda () (basic-test)))
#+clozure
(basic-test)
