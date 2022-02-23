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

;; 説明しているコメントは省いてある

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

#|
あとでoutput に２つめの引数を追加したくなった。
すでに定義されているメソッドが暗黙的に作るdefgeneric と異なるアリティのメソッドは定義できないので、一旦削除する必要がある

1. find-method で探し、remove-method で削除する
https://stackoverflow.com/questions/57424177/remove-one-method-from-a-generic-function
https://lisphub.jp/common-lisp/cookbook/index.cgi?%E5%AE%9A%E7%BE%A9%E3%81%97%E3%81%9F%E3%83%A1%E3%82%BD%E3%83%83%E3%83%89%E3%82%92%E5%89%8A%E9%99%A4%E3%81%99%E3%82%8B%E3%81%AB%E3%81%AF
CL-USER> (find-method #'output () (list (find-class 'vec3)))
#<STANDARD-METHOD COMMON-LISP-USER::OUTPUT (VEC3) {100494D543}>
CL-USER> (find-method #'output () (list (find-class 'color)))
#<STANDARD-METHOD COMMON-LISP-USER::OUTPUT (COLOR) {1004DE6EF3}>

2. defgeneric を明示すると、このスペックに合わない引数を削除するように聞いてくる
これを利用する

(defgeneric output (vec3 &optional stream))

New lambda-list (VEC3 STREAM) is incompatible with existing methods of #<STANDARD-GENERIC-FUNCTION COMMON-LISP-USER::OUTPUT (2)>.
Old lambda-list (V)
   [Condition of type SIMPLE-ERROR]

Restarts:
 0: [CONTINUE] Remove all methods.    <---- これを選ぶ
|#

;; MEMO stream にはデフォルト値nil を指定したいのだが、このためには&optional を指定しなければならない。
;; そうしなければ(A B) のB はクラス指定子とみなされてしまう。
(defgeneric output (vec3 &optional stream))
(defmethod output ((v vec3) &optional (stream nil))
  (format stream "~A ~A ~A" (x v) (y v) (z v)))

(assert (string= "1.0 2.1 3.2" (output foo)))
(assert (eq nil (output foo t)))

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

(defun floor-color (c)
  "ppm ファイルにおける色は0-255 までなので、万一超えていたら255に丸める
   具体的にはcolor のx,y,x のいずれかが1 のときに256になる
   テスト目的で(make-instance 'color :x 1 :y 1 :z 1)
   のように表記したいのでこのフィルターを挿入した "
  (if (> c 255) 255 c))

(defmethod output ((c color) &optional (stream nil))
  (format stream "~a ~a ~a"
	  (floor-color (nearest-1 (* 255.999 (x c))))
	  (floor-color (nearest-1 (* 255.999 (y c))))
	  (floor-color (nearest-1 (* 255.999 (z c))))))

;; output メソッドが型ごとに適切なものを呼ばれているかを確認
(defvar baz1 (make-instance 'vec3 :x 0.25 :y 0.105 :z 0.3))
(defvar baz2 (make-instance 'color :x 0.25 :y 0.105 :z 0.3))
(assert (string= "0.25 0.105 0.3" (output baz1)))
(assert (string= "64 27 77" (output baz2)))

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

;; (render-ppm 2 2 t)

(with-open-file
     (stream "test.ppm" :direction :output :if-exists :supersede)
   (render-ppm 256 256 stream)) ; OK!

;;; MEMO Emacs では.ppm ファイルをそのまま開ける！(inline 画像が出る)
