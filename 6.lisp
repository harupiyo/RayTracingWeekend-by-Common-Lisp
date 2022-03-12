(load "5.lisp")

;;; 6. Surface Normals and Multiple Objects

;;; 6.1 Shading with Surface Normals

(defmethod hit ((shape sphere) (r ray))
  (let* ((oc (-minus (origin r) (center shape)))
	 (a (*dot (direction r)
		  (direction r)))          ; a is a scalar
	 (b (* 2 (*dot oc (direction r)))) ; b is a scalar
	 (c (- (*dot oc oc)
	       (expt (radius shape) 2)))   ; c is a scalar too
	 ;; discriminant: 判別式
	 ;; 二次方程式の判別式(b^2-4ac)を計算
	 ;; < 0 ... 当たっていない
	 ;; = 0 ... １点で当たっている（接している）
	 ;; > 0 ... 2点で当たっている（貫いている）"
	 (discriminant (- (* b b) (* 4 a c))))
    (if (< discriminant 0)
	;; 当たっていない時
	-1
	;; 当たった時は二次方程式の解の公式で解を返す
	(/ (- (- b) (sqrt discriminant))
	   (* 2 a)))))

(defconstant +sphere+
  (make-instance 'sphere
		 :center
		 (make-instance 'vec3 :x 0 :y 0 :z -1)
		 :radius
		 0.5))

(defmethod color ((r ray))
  (let* ((h (hit +sphere+ r))
	 (color
	   (if (> h 0)
	       ;; 球表面と光線が交錯した地点での法線(normal)ベクターを得る
	       ;; Figure 5:
	       ;; 球の中心点 - 接点 = 接点における法線ベクター
	       (let* ((normal (unit (-minus
				     (at r h) ;; 光線と球表面との接点
				     (center +sphere+)))))
		 ;; 得られた法線ベクターを色表現に起こす
		 (*scalar (make-instance 'vec3
					 :x (1+ (x normal))
					 :y (1+ (y normal))
					 :z (1+ (z normal)))
			  0.5))
	       ;; 光線が球と交差していない場合
	       (let ((n (* 0.5 (+ (y (unit (direction r))) 1.0))))
		 (+plus (*scalar +unit-vector+ (- 1.0 n))
			(*scalar +unit-color+ n))))))
    ;; coerce type vec3 to color
    (make-instance 'color :x (x color) :y (y color) :z (z color))))


;; test
;; (render-ppm (make-instance 'wh :width 2 :height 2) *camera* t)

;; ;; なぜか赤色にしかならない
;; (defgeneric color (r))
;; (defmethod color ((r ray)) (make-instance 'color :x 0 :y 0 :z 0))
;; (render-ppm (make-instance 'wh :width 2 :height 2) *camera* t)
;; ;; あ、そうか！5.lisp の(defmethod color :around) があるせいだ！
;; ;; :around を削除しよう
(remove-method #'color (find-method #'color '(:around) (list 'ray)))

(with-open-file
    (stream "test.ppm" :direction :output :if-exists :supersede)
  (render-ppm +screen+ *camera* stream))

;;; 6.2 Simplifying the Ray-Shpere Intersection Code

(defmethod hit ((shape sphere) (r ray))
  (let* ((oc (-minus (origin r) (center shape)))
	 (a (length-squared (direction r)))
	 (harf-b (*dot oc (direction r)))  ; harf-b is a scalar
	 (c (- (length-squared oc)
	       (expt (radius shape) 2)))   ; c is a scalar too
	 ;; discriminant: 判別式
	 ;; 二次方程式の判別式(b^2-4ac)を計算
	 ;; < 0 ... 当たっていない
	 ;; = 0 ... １点で当たっている（接している）
	 ;; > 0 ... 2点で当たっている（貫いている）"
	 (discriminant (- (expt harf-b 2) (* a c))))
    (if (< discriminant 0)
	;; 当たっていない時
	-1
	;; 当たった時は二次方程式の解の公式で解を返す
	(/ (- (- harf-b) (sqrt discriminant))
	   a))))

(with-open-file
    (stream "test.ppm" :direction :output :if-exists :supersede)
  (render-ppm +screen+ *camera* stream))

;;; 6.3. An Abstruction for Hittable Objects
;; ここからしばらく汎用化のリファクタリングが続き、動作確認ができないので辛い節

(defclass hittable ()
  ((point  :initarg :point  :accessor point  :documentation "point-of-intersection")
   (normal :initarg :normal :accessor normal :documentation "a normal vector")
   ;; 3DCG の世界ではt という変数があるのだが、Common Lispt では予約語なので使えない
   ;; そこでt が何かを調べてみた。
   ;; https://ja.wikipedia.org/wiki/%E7%B7%9A%E5%BD%A2%E8%A3%9C%E9%96%93
   ;; 線形補間におけるtime(秒)のことだった
   ;; ところがtime も予約語だった!
   ;; t-time にするとtea time に見えるし、t-as-time だと長過ぎる.
   ;; t@li は苦肉の策. @ があると特別なシンボルに見えてしまうかもしれないが、
   ;; Lisperなら単なるシンボルだと読んでくれるはず。
   ;; 念の為にドキュメントを付けておくことにする。
   (t@li :initarg :t@li     :accessor t@li   :documentation "time(sec) of linear-interpolation"))
  (:documentation "光線との当たり判定を持つ = 3D空間上の存在 = 物体"))

(defclass sphere (hittable)
  ((center :initarg :center :accessor center)
   (radius :initarg :radius :accessor radius)))

;; インターフェースの定義し直し = 過去のdefmethod の削除を兼ねている
(defgeneric hit (3dobj ray t-min t-max))

(defmethod hit ((3dobj sphere) (ray ray) t-min t-max)
  ;; discriminant: 判別式の計算
  ;; 二次方程式の判別式(b^2-4ac)を計算
  ;; < 0 ... 当たっていない
  ;; = 0 ... １点で当たっている（接している）
  ;; > 0 ... 2点で当たっている（貫いている）"
  (let* ((oc (-minus (origin ray) (center 3dobj)))
	 (a (length-squared (direction ray)))
	 (harf-b (*dot oc (direction ray)))  ; harf-b is a scalar
	 (c (- (length-squared oc)
	       (expt (radius 3dobj) 2)))   ; c is a scalar too
	 (discriminant (- (expt harf-b 2) (* a c))))
    (if (< discriminant 0)
	;; 当たっていない時
	nil
	;; 当たった時は二次方程式の解の公式で解を計算し、
	(let* ((sqrt-discriminant (sqrt discriminant))
	       (root (/ (- (- harf-b) sqrt-discriminant) a))) ; ここまでは以前と同じ
	  ;; t-min - t-max の範囲にあるかチェックし、
	  (when (or (< root t-min)
		    (< t-max root))
	    ;; -b±sqrt(b^2-4ac) のもう片方にセットし直し、
	    ;; (計算が + に転じていることに注意)
	    (setq root (/ (+ (- harf-b) sqrt-discriminant) a))
	    ;; もう一度範囲内かどうかをチェック
	    (when (or (< root t-min)
		      (< t-max root))
	      ;; それでだめならnil.
	      (return-from hit nil)))
	  ;; ここに来たものは、time の範囲内かつ、視点に近い方の交差点である
	  (let* ((p (at ray root)) 
		 (n (/ (-minus p (center 3dobj))
		       (radius 3dobj))))
	    ;; その情報をインスタンスとして返す.(本のような副作用はなし)
	    ;; これは呼び出し側にT を返すことでもある.
	    (make-instance 'hittable
			   :t@li root
			   :point p
			   :normal n))))))

;;; 6.4. Front Faces Versus Back Faces

(defclass hittable ()
  ((point  :initarg :point  :accessor point  :documentation "point-of-intersection")
   (normal :initarg :normal :accessor normal :documentation "a normal vector")
   ;; 3DCG の世界ではt という変数があるのだが、Common Lispt では予約語なので使えない
   ;; そこでt が何かを調べてみた。
   ;; https://ja.wikipedia.org/wiki/%E7%B7%9A%E5%BD%A2%E8%A3%9C%E9%96%93
   ;; 線形補間におけるtime(秒)のことだった
   ;; ところがtime も予約語だった!
   ;; t-time にするとtea time に見えるし、t-as-time だと長過ぎる.
   ;; t@li は苦肉の策. @ があると特別なシンボルに見えてしまうかもしれないが、
   ;; Lisperなら単なるシンボルだと読んでくれるはず。
   ;; 念の為にドキュメントを付けておくことにする。
   (t@li :initarg :t@li     :accessor t@li   :documentation "time(sec) of linear-interpolation")
   (front-face :initarg :front-face :accessor front-face :documentation "視点から見て表か裏か"))
  (:documentation "光線との当たり判定を持つ = 3D空間上の存在 = 物体"))

(defmethod front-face? ((ray ray) (outward-normal vec3))
  "光線の差す向きと物体の法線ベクトルから、物体の表裏を判定.
   内積をとって0より大きければ表とわかる"
  (< (*dot (direction ray) outward-normal)
     0))
			   
(defmethod hit ((3dobj sphere) (ray ray) t-min t-max)
  ;; すぐ手前のhit の更新. 変更点のみコメントする.
  (let* ((oc (-minus (origin ray) (center 3dobj)))
	 (a (length-squared (direction ray)))
	 (harf-b (*dot oc (direction ray)))
	 (c (- (length-squared oc)
	       (expt (radius 3dobj) 2)))
	 (discriminant (- (expt harf-b 2) (* a c))))
    (if (< discriminant 0)
	nil
	(let* ((sqrt-discriminant (sqrt discriminant))
	       (root (/ (- (- harf-b) sqrt-discriminant) a)))
	  (when (or (< root t-min)
		    (< t-max root))
	    (setq root (/ (+ (- harf-b) sqrt-discriminant) a))
	    (when (or (< root t-min)
		      (< t-max root))
	      (return-from hit nil)))
	  (let* ((p (at ray root)) 
		 (n (/div (-minus p (center 3dobj))
		       (radius 3dobj)))
		 (outward-normal (/div (-minus p (center 3dobj))
				       (radius 3dobj)))
		 (front-face? (front-face? ray outward-normal)))
	    (make-instance 'hittable
			   :t@li root
			   :point p
			   :front-face front-face?
			   :normal (if front-face? outward-normal (- outward-normal))))))))

;;; 6.5. A List of Hittable Objects

(defvar *hittable-list* nil)

(defgeneric hit-anything? (obj-list ray t-min t-max))
(defmethod hit-anything? (*hittable-list* (ray ray) t-min t-max)
  (loop :with closest := t-max
	:with hit-anything := nil
	:for 3dobj :in *hittable-list*
	:do (let ((hittable? (hit 3dobj ray t-min closest)))
	      (when hittable?
		(setf hit-anything hittable?
		      closest (t@li hittable?))))
	:finally (return hit-anything)))

;;; 6.6. Some New C++ Features

;; GC を持つCommon Lisp には不要な話をしているように見える

;;; 6.7. Common Constants and Utility Functions

(defconstant +infinity+ most-positive-double-float)

(defun degrees->radians (degree)
  (/ (* degree PI) 180))

;;; これまでの描画ロジックを6.3から6.7にかけての抽象レイヤーを使って書き直す

(defgeneric color (ray hittable))

(defmethod color ((ray ray) (world list))
  (let* (
	 ;; world にある全ての3D-OBJ から、光線と交差する単一のオブジェクトに刈り込んでいる
	 (hit? (hit-anything? world ray 0 +infinity+))
	 ;; 特定されたオブジェクトに対しての色の計算
	 (vec3
	   (if hit?
	       (*scalar (+plus (normal hit?) +unit-vector+) 0.5)
	       (let* ((unit (unit (direction ray)))
		      (t@li (* (1+ (y unit)) 0.5)))
		 (+plus (*scalar +unit-vector+ (- 1.0 t@li))
			(*scalar +unit-color+ t@li))))))
    (make-instance 'color :x (x vec3) :y (y vec3) :z (z vec3))))

(setq *hittable-list*
      (list
       (make-instance 'sphere
		      :center (make-instance 'vec3 :x 0 :y 0 :z -1)
		      :radius 0.5)
       (make-instance 'sphere
		      :center (make-instance 'vec3 :x 0 :y -100.5 :z -1)
		      :radius 100)))

;; from 4.1.lisp
(defun pixel-color-at (x y screen camera)
  (let ((u (/ x (1- (width screen))))
	(v (/ y (1- (height screen)))))
    (color (make-instance
	    'ray
	    :origin (origin camera)
	    :direction
	    (-minus
	     (+plus (lower-left-corner camera)
		    (+plus (*scalar (horizontal camera) u)
			   (*scalar (vertical camera) v)))
	     (origin camera)))
	   *hittable-list*))) ; added

;; from 4.1.lisp
#|
(defun render-ppm (screen camera &optional (stream t))
  (format stream "P3~%~A ~A~%255~%" (width screen) (height screen))
  (loop :for y :from (1- (height screen)) :downto 0
	:do (loop :for x :from 0 :to (1- (width screen))
		  :do (format stream "~a~%"
			      (output
			       (pixel-color-at x y screen camera))))))
|#

(with-open-file
    (stream "test.ppm" :direction :output :if-exists :supersede)
  (render-ppm +screen+ *camera* stream))
