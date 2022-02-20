;;; 4. Rays, a Simple Camera, and Background
;;; 4.1 The ray Class

(load "utils")
(load "3.1_v2.lisp")

(defclass ray ()
  ;; ここで型は指定できないのかな？
  ((origin :initarg :origin
	   :initform (make-instance 'point3 :x 0.0 :y 0.0 :z 0.0)
	   :accessor origin)
   (direction :initarg :direction
	      :initform (make-instance 'vec3 :x 0.0 :y 0.0 :z 0.0)
	      :accessor direction)))

(defmethod at ((r ray) (n number))
  (*mul (origin r) (+plus (direction r) n) ))

(let ((foo (make-instance
	    'ray
	    :origin (make-instance 'point3  :x 1.2 :y 10.1 :z -3.56)
	    :direction (make-instance 'vec3 :x 3.2 :y 0.5  :z 56))))
  (output (at foo 100) nil))

;;; 4.2. Sending Rays Into the Scene

(defconstant +unit-vector+ (make-instance 'vec3 :x 1 :y 1 :z 1))
(defconstant +zero-vector+ (make-instance 'vec3 :x 0 :y 0 :z 0))
(defconstant +unit-color+  (make-instance 'color :x 0.5 :y 0.7 :z 1.0))

;; MEMO 本にはメソッドごとのテスト結果の値が載っていず、まちがっていたときのデバッグがとても大変。インクリメンタルに進められる教科書になっていることがとても重要だと実感。
(defmethod color ((r ray))
  (let* ((n (* (+ (y (unit (direction r))) 1.0) 0.5))
	 (vec (*mul (*scalar +unit-vector+ (- 1.0 n))
		    (*scalar +unit-color+ n))))
    ;; coerce type vec3 to color
    (make-instance 'color :x (x vec) :y (y vec) :z (z vec))))

;;; draing facilities

;; 以前defconstant したものと、スロット名がかぶるので削除するにはこう
;; (makunbound 'width)
;; (makunbound 'height)

#|
;; 継承先でアクセッサの名前を変えることはできるか？vec2(x,y) をsize(width,height) のように
;; 試験
(defclass test ()
  ((x :initarg :x :accessor :x)))
(defclass test2 (test)
  ((x :initarg :test :accessor :test)))
(defclass test3 (test) ())
(defmethod p ((s test))
  (print (x s)))
(setq hoge (make-instance 'test2 :test 'hello))
(setq fuga (make-instance 'test3 :x 'lisp))
(typep hoge 'test2) ; => t
(typep hoge 'test)  ; => t
(p hoge) ; => There is no applicable method
(p fuga) ; => There is no applicable method
;; あれれ？p にtest2 やtest3は渡せないのか？
TODO
|#

;; いくつかのwidth,height を扱うのでまとめてみた
(defclass wh ()
  ((width  :initarg :width  :accessor width)
   (height :initarg :height :accessor height)))

;;; Screen
(defconstant +aspect-ratio+ (/ 16 9))
(defconstant +screen+
  (make-instance 'wh
		 :width 400
		 :height (/ 400 +aspect-ratio+))) 

;;; Camera
#|
# defclass camera にまつわる考察

(defclass camera ()
  "試作品"
  ((viewport :initform (make-instance
                          'wh
			    :width (/ aspect-ratio 2.0) ; A. height を指定したい
		            :height 2.0)
	     :accessor viewport)
   (focal-length :initform 1.0
		 :accessor  focal-length)
   (origin :initform (make-instance 'point3 :x 0 :y 0 :z 0)
	   :accessor :origin)))

# defclass の中ではlet* のようにclass 内のスロットの値を使うことができない

1.したがって、A. 地点やhorizontal やvertical については別メソッドを用意
(defmethod horizontal ((c camera))
  (make-instance 'vec3 :x (width (viewport c)) :y 0 :z 0))
(defmethod vertical ((c camera))
  (make-instance 'vec3 :x 0 :y (height (viewport c)) :z 0))
   
2. instance 生成(make-instance) の後処理(:around)もできる

Common Lisp Recipes 13-11. Extending and Modifying CLOS より

(defclass modifying-class (standard-class) ())
;; SBCL だけでよければ不要のはず
;; (defmethod closer-mop:validate-superclass
;;     ((class modifying-class) (superclass standard-class)) t)
(defmethod make-instance :around ((class logged-class) &key)
  (let ((new-object (call-next-method)))
    (push (format nil "~A created at ~A."
		  new-object (get-universal-time))
	  (creation-log class))
    new-object))

これを見ると、make-instance に:around をつけるとき、型の指定はclass である必要があるとわかる。したがってMOP が必要だ

3. いや、let の中でdefclass もできるのでは？
マクロだから変数展開をしてくれないかな？ = レキシカルスコープは使えないかな？

(let* ((h 2))
  (defclass camera ()
    ((viewport :initform (make-instance 'wh :width (/ aspect-ratio h)
					    :height h)
	       :accessor viewport))))
(width (viewport (make-instance 'camera))) ; => できるね！
|#


;; defclass camera facility
(let* ((h 2)
       (fcl-lngth 1)
       (vw (/ +aspect-ratio+ h))
       (orgn +zero-vector+)
       (hrzntl (make-instance 'vec3 :x vw :y 0 :z 0))
       (vrtcl (make-instance 'vec3 :x 0 :y h :z 0)))
  (defclass camera ()
    ((viewport :initform (make-instance 'wh :width vw :height h)
	       :accessor viewport)
     (focal-length :initform fcl-lngth
		   :accessor  focal-length)
     (origin :initform orgn
	     :accessor origin)
     (horizontal :initform hrzntl
		 :accessor horizontal)
     (vertical :initform vrtcl
	       :accessor vertical)
     (lower-left-corner :initform
			(-minus
			 (-minus (-minus orgn (/div hrzntl  2))
				 (/div vrtcl 2))
			 (make-instance 'vec3
					:x 0
					:y 0
					:z fcl-lngth))
			:accessor lower-left-corner))))

;; MEMO defvar は二回目以降は初期化してくれないので、defclass camera の定義を変えた場合におかしな挙動に困ることになる
;; TODO よいレシピはあるかな？defparameter を使えばいいかな？それともsetq とともに使うとよいかな？
(defvar *camera* (make-instance 'camera))

;;; Render

;; MEMO 定数screen があると、仮引数にこの名前が使えない
;;      したがって定数の場合には耳あてしておくのが無難
(defun pixel-color-at (x y screen camera)
  (let ((u (/ x (1- (width screen))))
	(v (/ y (1- (height screen)))))
    (color (make-instance
	    'ray
	    :origin (origin camera)
	    :direction
	    ;;
	    ;; lower-left-corner + u*horizontal + v*vertical - origin
	    ;;
	    ;; 以下はベクトル(vec3)
	    ;; - lower-left-corner
	    ;; - horizontal, vertical
	    ;; - origin
	    (;-minus
	     ; TODO
	     -minus
	     (+plus (lower-left-corner *camera*)
		    (+plus (*scalar (horizontal *camera*) u)
			   (*scalar (vertical *camera*) v)))
	     (origin camera))))))

;; when binding SB-KERNEL::Y
;;    [Condition of type TYPE-ERROR]The value
;;   #<VEC3 {100400E893}>
;; is not of type
;;   NUMBER
;; when binding SB-KERNEL::Y
;;    [Condition of type TYPE-ERROR]

(defun render-ppm (screen camera &optional (stream t))
  (format stream "P3~%~A ~A~%255~%" (width screen) (height screen))
  (loop :for y :from (1- (height screen)) :downto 0
	:do (loop :for x :from 0 :to (1- (width screen))
		  :do (format stream "~a~%"
			      (output
			       (pixel-color-at x y screen camera))))))

#|
おかしい。マイナス値が出てくる。マイナスの明るさのピクセルなんてない。
デバッグできた。
-minus を使っているのはpixel-color-at だけでなく、color メソッドもそうだった。
TODO MEMO trace 機能への要望は、trace した関数がどこから呼び出されたのかのcaller を表示してほしいというものだ。
(render-ppm (make-instance 'wh :width 2 :height 2) *camera* t)
(trace -minus)
|#

(render-ppm (make-instance 'wh :width 2 :height 2) *camera* t)
(render-ppm (make-instance 'wh :width 400 :height 200) *camera* t)
(render-ppm (make-instance 'wh :width 400 :height 200) *camera* t)
(render-ppm +screen+ *camera* t)

(with-open-file
    (stream "test.ppm" :direction :output :if-exists :supersede)
  (render-ppm +screen+ *camera* stream)) ; OK!

;;; MEMO Emacs では.ppm ファイルをそのまま開ける！(inline 画像が出る)
