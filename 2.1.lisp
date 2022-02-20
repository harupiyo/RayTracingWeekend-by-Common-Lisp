(load "utils")

(defun render-ppm (width height &optional (stream nil))
  (format stream "P3~%~A ~A~%255~%" width height)
  (loop :for y :from (1- height) :downto 0
	:do (loop :for x :from 0 :to (1- width)
		  :do (format stream "~A ~A ~A~%"
			      (nearest-1 (* 255 (/ x (1- width)))) ; R
			      (nearest-1 (* 255 (/ y (1- height)))); G
			      (nearest-1 (* 255 0.25))) ; B
		  )))

(defparameter image-width 256)
(defparameter image-height 256)

(with-open-file
    (stream "test.ppm" :direction :output :if-exists :supersede)
  (render-ppm image-width image-height stream))
