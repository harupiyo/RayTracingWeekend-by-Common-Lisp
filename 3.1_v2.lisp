;;; 3.1.lisp で説明しているコメントは省いてある

;;; ハマったところ
;;; initarg にはkeyword が使えるが、accessor にはキーワードを指定してはいけない
(defclass vec3 ()
  ((x :initarg :x :initform 0.0 :accessor x)
   (y :initarg :y :initform 0.0 :accessor y)
   (z :initarg :z :initform 0.0 :accessor z)))

(defclass point3 (vec3) ())
(defclass color (vec3) ())

(defvar foo (make-instance 'vec3 :x 1.0 :y 2.1 :z 3.2))
(defvar bar (make-instance 'vec3 :x 2.4 :y 3.5 :z 4.6))

(defmethod +plus ((v1 vec3) (v2 vec3))
  (make-instance
   'vec3
   :x (+ (x v1) (x v2))
   :y (+ (y v1) (y v2))
   :z (+ (z v1) (z v2))))

(+plus foo bar)

(defmethod +plus ((v1 vec3) (n number))
  (make-instance
   'vec3
   :x (+ (x v1) n)
   :y (+ (y v1) n)
   :z (+ (z v1) n)))

#|
TODO こういうとき、どうしたらいいかな？

あとで単行演算子の-minus を加える必要が出て追加
(defgeneric -minus (v1 &optional v2))

(defmethod -minus ((v vec3))
  (make-instance 'vec3
		 :x (- (x v))
		 :y (- (y v))
		 :z (- (z v)))) => 引数が足らないとエラー

1. アリティの変化するメソッドが書きたいが、できるか？
https://www.google.com/search?client=firefox-b-d&q=common+lisp+method+arity
https://stackoverflow.com/questions/15465138/find-functions-arity-in-common-lisp

2. 他のやり方はあるか？
|#
;; ↑ができないので、逆ベクトル(inverse vector)メソッドを用意した
;; https://ejje.weblio.jp/content/%E9%80%86%E3%83%99%E3%82%AF%E3%83%88%E3%83%AB
(defmethod -inverse ((v vec3))
  (make-instance 'vec3
		 :x (- (x v))
		 :y (- (y v))
		 :z (- (z v))))

(defmethod -minus ((v1 vec3) (v2 vec3))
  (make-instance
   'vec3
   :x (- (x v1) (x v2))
   :y (- (y v1) (y v2))
   :z (- (z v1) (z v2))))

(defmethod -minus ((v1 vec3) (n number))
  (make-instance
   'vec3
   :x (- (x v1) n)
   :y (- (y v1) n)
   :z (- (z v1) n)))

; https://en.wikipedia.org/wiki/Scalar_multiplication
(defmethod *scalar ((v vec3) (n number))
  (make-instance
   'vec3
   :x (* (x v) n)
   :y (* (y v) n)
   :z (* (z v) n)))

(*scalar foo 3.0)

(defmethod /div ((v vec3) (n number))
  (*scalar v (/ 1 n)))

(/div foo 3.0)

; norm = length. ユークリッドノルム = eucledean-norm
(defmethod euclidean-norm ((v vec3))
  (sqrt (vlength v)))

(defmethod vlength ((v vec3))
   (+ (expt (x v) 2)
      (expt (y v) 2)
      (expt (z v) 2)))

;; inner-product
;; https://ja.wikipedia.org/wiki/%E5%86%85%E7%A9%8D

;;; ここからは3.1_v2.lisp で追加したところ
;;; Vec3 Utility Functions
;;; https://raytracing.github.io/books/RayTracingInOneWeekend.html#thevec3class/vec3utilityfunctions

(defmethod vec3print ((v vec3))
  (format nil "x:~A y:~A z:~A~%" (x v) (y v) (z v)))

(vec3print foo)

(defmethod output ((v vec3))
  (format nil "~A ~A ~A" (x v) (y v) (z v)))

(output foo)

(defmethod *mul ((v1 vec3) (v2 vec3))
  (make-instance
   'vec3
   :x (* (x v1) (x v2))
   :y (* (y v1) (y v2))
   :z (* (z v1) (z v2))))
	
(output (*mul foo bar))

;; inner-product(=dot-product)
(defmethod *dot ((u vec3) (v vec3))
  (+ (* (x u) (x v))
     (* (y u) (y v))
     (* (z u) (z v))))

;; outer-product(=cross-product)
(defmethod *cross ((u vec3) (v vec3))
  (make-instance
   'vec3
   :x (-
       (* (y u) (z v))
       (* (z u) (y v)))
   :y (-
       (* (z u) (x v))
       (* (x u) (z v)))
   :y (-
       (* (x u) (y v))
       (* (y u) (x v)))))

(defmethod unit ((v vec3))
  (/div v (vlength v)))

;;; 3.3. Color Utilitiys

(load "utils")

(defmethod output ((c color))
  (format nil "~a ~a ~a"
	  (nearest-1 (* 255.999 (x c)))
	  (nearest-1 (* 255.999 (y c)))
	  (nearest-1 (* 255.999 (z c)))))

(defvar baz (make-instance 'color :x 20.0 :y 10.5 :z 30.2))
(output baz)

;;; Listing7: Final code for the [2.1.lisp]

(defun render-ppm (width height &optional (stream t))
  (format stream "P3~%~A ~A~%255~%" width height)
  (loop :for y :from (1- height) :downto 0
	:do (loop :for x :from 0 :to (1- width)
		  :do (format stream "~a~%"
			      (output
			       (make-instance
				'color
				:x (/ x (1- width))
				:y (/ y (1- height))
				:z 0.25))))))

(render-ppm 2 2 t)

;; (with-open-file
;;     (stream "test.ppm" :direction :output :if-exists :supersede)
;;   (render-ppm 256 256 stream)) ; OK!

;;; MEMO Emacs では.ppm ファイルをそのまま開ける！(inline 画像が出る)
