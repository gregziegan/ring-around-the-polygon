(load "polygon-lib")

(defparameter *hexagon-scale* 40)
(defparameter *ngon-scale* 40)

(defun draw-polyhexagon (x y col)
  (labels ((calc-pt (pt)
            (cons (+ x (* *hexagon-scale* (car pt)))
                  (+ y (* *hexagon-scale* (cdr pt)))))
           (f (pol col)
              (polygon (mapcar #'calc-pt pol) col)))
    (f (polyhexagonmutant-points)
       col)))

(defun draw-mutantpoly (x y col)
  (labels ((calc-pt (pt)
            (cons (+ x (* *hexagon-scale* (car pt)))
                  (+ y (* *hexagon-scale* (cdr pt)))))
           (f (pol col)
              (polygon (mapcar #'calc-pt pol) col)))
    (f (get-all-points (polyhexagonmutant-points))
       col)))

(defun polyhexagon-points ()
  (let* ((c 1.0)
        (a (/ c 2))
        (b (* c (sin (deg-to-rad 60)))))
    (translate-pts 
      '((0 (4 * b)) ((1/2 * c) (5 * b)) ((3/2 * c) (5 * b))
        ((c * 2) (6 * b)) ((3 * c) (6 * b)) ((7/2 * c) (5 * b))
        ((9/2 * c) (5 * b)) ((5 * c) (4 * b)) ((9/2 * c) (3 * b))
        ((5 * c) (2 * b)) ((9/2 * c) b) ((7/2 * c) b) ((3 * c) 0)
        ((2 * c) 0) ((3/2 * c) b) ((1/2 * c) b) (0 (2 * b)) ((1/2 * c) (3 * b)))
      (list (list 'a a) (list 'b b) (list 'c c))))) 

(defun polyhexagonmutant-points ()
  (let* ((c 1.0)
         (a (/ c 2))
         (b (* c (sin (deg-to-rad 60)))))
    (edge-transform 
      (translate-pts  '((0 (4 * b)) ((1/2 * c) (5 * b)) ((3/2 * c) (5 * b))
                       ((c * 2) (6 * b)) ((3 * c) (6 * b)) ((7/2 * c) (5 * b))
                       ((9/2 * c) (5 * b)) ((5 * c) (4 * b)) ((9/2 * c) (3 * b))
                       ((5 * c) (2 * b)) ((9/2 * c) b) ((7/2 * c) b) ((3 * c) 0)
                       ((2 * c) 0) ((3/2 * c) b) ((1/2 * c) b) (0 (2 * b)) ((1/2 * c) (3 * b)))
                       (list (list 'a a) (list 'b b) (list 'c c)))
      '((0 . 1)))))

(defun draw-ngon (x y col declarations env pt-lst)
  (labels ((calc-pt (pt)
                    (cons (+ x (* *ngon-scale* (car pt)))
                          (+ y (* *ngon-scale* (cdr pt)))))
           (f (pol col)
              (polygon (mapcar #'calc-pt pol) col)))
    (f (ngon-points declarations env pt-lst)
       col)))

(defun ngon-points (declarations env pt-lst)
    (translate-pts pt-lst (generate-let declarations env)))

(defmacro generate-let (declarations env)
  `(mapcar (lambda (var) (evaluate var ,env)) ,declarations))  

(defun save (fname thunk)
  (with-open-file (*standard-output*
                    (concatenate 'string fname ".html")
                    :direction :output
                    :if-exists :supersede)
    (tag html ()
      (tag svg ()
        (funcall thunk)))))
