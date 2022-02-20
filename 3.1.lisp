;;; The vec3 Class
;;; https://raytracing.github.io/books/RayTracingInOneWeekend.html#thevec3class

(defstruct vec3 x y z)
(defparameter vec (make-vec3 :x 1.0 :y 2.0 :z 3.0)) 
(vec3-x vec)

; defmethod
; https://programgenjin.hatenablog.com/entry/2019/03/12/102620

; + is used
; (defmethod + ((v1 vec3) (v2 vec3))
;   'hello)
; (inspect '+)
; the symbol + at #x00000000502452A0
;    0 NAME -----------> a simple-string (1) "+"
;    1 PACKAGE --------> the COMMON-LISP package
;    2 VALUE ----------> the symbol NIL
;    3 FUNCTION -------> NIL
;    4 PLIST ----------> the symbol NIL
; not fn, value. then it may be a macro?
;
; (macroexpand '(+ 1 2)) => (+ 1 2) ; NO! not a macro.
;
; (defmethod + ((v1 vec3) (v2 vec3))
;  'hello)
; でもSLDB上で上書きできるみたい。そういう選択肢が出る。

(defmethod plus ((v1 vec3) (v2 vec3))
  (make-vec3 :x (+ (vec3-x v1) (vec3-x v2))
	     :y (+ (vec3-y v1) (vec3-y v2))
	     :z (+ (vec3-z v1) (vec3-z v2))))

(plus vec (make-vec3 :x 3.0 :y 5.0 :z 1.2))

; https://en.wikipedia.org/wiki/Scalar_multiplication
(defmethod scalar-mul ((v1 vec3) (n float))
  (make-vec3 :x (* (vec3-x v1) n)
	     :y (* (vec3-y v1) n)
	     :z (* (vec3-z v1) n)))

(scalar-mul vec 3.0)
;(scalar-mul vec 3) ; NO APPLICABLE METHOD. OK!

(defmethod div ((v1 vec3) (n float))
  (mul v1 (/ 1 n)))

(div vec 3.0)

; norm = length. ユークリッドノルム = eucledean-norm
; https://ja.wikipedia.org/wiki/%E3%83%8E%E3%83%AB%E3%83%A0
; expt
; https://lisphub.jp/common-lisp/cookbook/index.cgi?x%E3%81%AEn%E4%B9%97%EF%BC%88%E3%81%B9%E3%81%8D%E4%B9%97%E3%83%BB%E7%B4%AF%E4%B9%97%EF%BC%89%E3%82%92%E8%A8%88%E7%AE%97%E3%81%99%E3%82%8B
; sqrt
; https://lisphub.jp/common-lisp/cookbook/index.cgi?%E5%B9%B3%E6%96%B9%E6%A0%B9%E3%82%92%E6%B1%82%E3%82%81%E3%82%8B
(defmethod euclidean-norm ((v1 vec3))
  (sqrt 
   (+ (expt (vec3-x v1) 2)
      (expt (vec3-y v1) 2)
      (expt (vec3-z v1) 2))))

;; inner-product
;; https://ja.wikipedia.org/wiki/%E5%86%85%E7%A9%8D

;;; ここで、vec3 にエイリアスでpoint,color が必要なことがわかった。それならばdefstruct よりもdefclass がいいね。書き直す。

;;; 3.1_v2.lisp に移る

;;; Vec3 Utility Functions
;;; https://raytracing.github.io/books/RayTracingInOneWeekend.html#thevec3class/vec3utilityfunctions

(defclass vec3 ()
  ((x :initarg :x :initform 0.0 :accessor :x)
   (y :initarg :x :initform 0.0 :accessor :y)
   (z :initarg :x :initform 0.0 :accessor :z)))
