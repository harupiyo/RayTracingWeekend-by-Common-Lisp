; loop cheat sheet
; https://www.unixuser.org/~euske/doc/cl/loop.html

(loop :for x :from 0 :to 10
	collect x)
1 
'(1 2 3) 

四捨五入
# 2.1. The PPM Image Format

https://lisphub.jp/common-lisp/cookbook/index.cgi?%E5%9B%9B%E6%8D%A8%E4%BA%94%E5%85%A5%E3%81%99%E3%82%8B
(defun nearest-1 (number) (values (floor (+ 0.5 number))))

(defparameter image-width 256)
(defparameter image-height 256)

(loop :for x :from 0 :to 255
      :for y :from 0 :to 255
      :with r = (nearest-1 (* 256 (/ y (- image-width 1))))
      :with g = (nearest-1 (* 256 (/ y (- image-height 1))))
      :with b = (nearest-1 (* 256 0.25))
      collect (list r g b))

;; https://lisphub.jp/common-lisp/cookbook/index.cgi?loop
;; 複数のforを指定できますが、入れ子になるのではなく並列して処理されます。 最初に終端にきたところでループは終わります。
;; (loop for i in '(1 2 3)
;;       for j in '(4 5)
;;       do (print (cons i j)))
;; ;-> (1 . 4) 
;;     (2 . 5)

(loop :for x :from 0 :to 255
      :with r = nil
      :with g = nil
      :with b = nil
      :collect (loop :for y :from 0 :to 255
		     :do (setf r (nearest-1 (* 256 (/ y (- image-width 1))))
			       g (nearest-1 (* 256 (/ y (- image-height 1))))
			       b (nearest-1 (* 256 0.25)))
		     :collect (list r g b)))

(defun render-ppm (width height &optional (stream nil))
  (format stream "P3~%~A ~A ~A~%" width height 255)
  (loop :for x :from 0 :to 255
	:do (loop :for y :from 0 :to 255
		  :do (format stream "~A ~A ~A~%"
			      ;; R
			      (nearest-1 (* 256 (/ y (- width 1))))
			      ;; G
			      (nearest-1 (* 256 (/ y (- height 1))))
			      ;; B
			      (nearest-1 (* 256 0.25)))
		  )))
(render-ppm image-width image-height t)

(with-open-file
    (stream "test.ppm" :direction :output :if-exists :supersede)
  (render-ppm image-width image-height stream))

