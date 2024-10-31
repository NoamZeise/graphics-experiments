(ql:quickload :trivia)
(ql:quickload :canim)

(defun convert (x)
  (floor (* x 255)))

(defun to-colour (c)
  (trivia:match
   c
   ((list r g b)
    (canim:make-colour
     (convert r) (convert g) (convert b) 255))))

(defun interp (v a b)
  (+ (* v a) (* (- 1 v) b)))

(defun interp-col (v a b)
  (loop for x in a for y in b collecting
	(interp v x y)))

(defun smooth (v a b)
  (if (= b a)
      (if (< v a) 0 1)
    (let ((c (alexandria:clamp (/ (- v a) (- b a)) 0.0 1.0)))
      (* c c (- 3 (* 2 c))))))

(canim:make-im
 "xtoon.png" 300 300
 (canim:make-pos :x 0 :y 0 :scale 1)
 :pixel-fn
 #'(lambda (x y _)
     (let* ((warm '(0.78 0.37 0.08))
	    (cold '(0.04 0.235 0.39))
	    (step (expt (- 1 ( / (expt y 0.5) 1)) 2.2))
	    (int (smooth x (- 0.5 step) (+ 0.5 step))))
       (to-colour (interp-col int warm cold)))))
