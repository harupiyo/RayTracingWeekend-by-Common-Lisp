(load "4.1.lisp")

;;; 5. Adding a Sphere

(defclass sphere ()
  ((center :initarg :center :accessor center)
   (radius :initarg :radius :accessor radius)))

(defmethod hit ((shape sphere) (r ray))
  "Calculate discriminat for quadratic.
  二次方程式の解の公式における、判別式(b^2-4ac)を計算
   < 0 ... 当たっていない
   = 0 ... １点で当たっている（接している）
   > 0 ... 2点で当たっている（貫いている）"
  (let* ((oc (-minus (origin r) (center shape)))
	 (a (*dot (direction r)
		  (direction r)))          ; a is a scalar
	 (b (* 2 (*dot oc (direction r)))) ; b is a scalar
	 (c (- (*dot oc oc)
	       (expt (radius shape) 2)))   ; c is a scalar too
	 (discriminant (- (* b b) (* 4 a c))))
    (> discriminant 0)))
  
;; Adding Flavor: 4.1.lisp's color
;;
;; :around
;; http://www.nct9.ne.jp/m_hiroi/clisp/clisp07.html
;;
;; 当初、ここには4.1.lisp の定義をまるまる持ってきて
;; 前処理を追加したバージョンとして上書きしていた。
;; ところが、もとにしたコードに不具合があって両方治す羽目になったときに、
;; あっ、こここそが :around の使いどころだぞと気づいた。
;;
(defmethod color :around ((r ray))
  (if (hit (make-instance 'sphere
			  :center
			  (make-instance 'vec3 :x 0 :y 0 :z -1)
			  :radius
			  0.5)
	   r)
    (make-instance 'color :x 1 :y 0 :z 0)
    (call-next-method)))
  
(with-open-file
    (stream "test.ppm" :direction :output :if-exists :supersede)
  (render-ppm +screen+ *camera* stream)) ; OK!
