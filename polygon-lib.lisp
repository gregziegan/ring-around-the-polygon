(load "svg")
(load "math_interpreter")

(defun get-all-points (ht)
  (let ((lst (gethash 0 ht)))
    (progn
      (loop for i
          from 1
          until (not (gethash i ht))
          do () (loop for e
                      in (gethash i ht)
                      do ()
                      (nconc lst (list e))))
      lst)))

(defmacro translate-pts (pt-lst env)
  `(loop for pts
         in ,pt-lst
         collect (cons (evaluate (car pts) ,env) (evaluate (cadr pts) ,env))))

(defun edge-transform (vertices mutation-lst)
  (let ((edges (make-edges (append vertices (list (car vertices))))))
   (progn
      (loop for mutation
            in mutation-lst
            do () (setf (gethash (car mutation) edges) 
                        (insert-mutation (cdr mutation) (gethash (car mutation) edges))))
      edges)))

(defun insert-mutation (mutation-type edge) 
  (let ((mutation-pts (interpret-mutation edge mutation-type)))
    (list (car edge) (first mutation-pts) (second mutation-pts) (third mutation-pts) (cadr edge))))

(defun get-indices (test lst)
  (loop
    for e in lst 
    and i from 0
    when (apply test (list i))
        collect e))

(defun make-edges (vertices)
  (let ((starts (get-indices #'evenp vertices))
        (ends (get-indices #'oddp vertices))
        (edges (make-hash-table)))
    (progn
      (loop for pt1 in starts
            for pt2 in ends
            for i from 0
            do () (setf (gethash i edges) (list pt1 pt2)))
      edges)))

(defun interpret-mutation (edge mutation-type)
  (let* ((pt1 (car edge)) (pt2 (cadr edge))
         (midpoint (calc-midpoint pt1 pt2))
         (distance (/ (calc-distance pt1 pt2) 5))
         (slope (calc-slope pt1 pt2)))
    (list 
      (calc-midpoint pt1 midpoint)
      (get-mutation (calc-mutation-pts midpoint distance slope) mutation-type)
      (calc-midpoint pt2 midpoint))))

(defun calc-mutation-pts (pt dst slope)
  (let* ((pp-slope (* (/ 1 slope) -1))
         (theta (atan pp-slope))
         (x (car pt)) (y (cdr pt))
         (dx (* dst (cos theta)))
         (dy (* dst (sin theta))))
    (list (cons (+ x dx) (+ y dy)) (cons (- x dx) (- y dy)))))

(defun get-mutation (mut-pts mutation-type)
  (let* ((distances (list (calc-distance '(0 . 0) (car mut-pts)) 
                          (calc-distance '(0 . 0) (cadr mut-pts))))
         (bump (nth (position (apply #'max distances) distances) mut-pts))
         (divot (nth (position (apply #'min distances) distances) mut-pts)))
    (if (eq mutation-type 1)
      bump
      divot)))

(defun calc-midpoint (pt1 pt2)
  (let ((x1 (car pt1)) (y1 (cdr pt1))
        (x2 (car pt2)) (y2 (cdr pt2)))
    (cons (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))

(defun calc-distance (pt1 pt2)
  (let ((x1 (car pt1)) (y1 (cdr pt1))
        (x2 (car pt2)) (y2 (cdr pt2)))
    (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))

(defun calc-slope (pt1 pt2)
  (let ((x1 (car pt1)) (y1 (cdr pt1))
        (x2 (car pt2)) (y2 (cdr pt2)))
    (/ (- y2 y1) (- x2 x1))))

(defun find-mutation (edge-num pt-lst)
  (cond
    ((eq (car pt-lst) '()) nil)
    ((eq edge-num (caar pt-lst)) (cadr pt-lst))
    (t (find-mutation edge-num (cdr pt-lst)))))
