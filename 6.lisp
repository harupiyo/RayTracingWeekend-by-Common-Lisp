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
