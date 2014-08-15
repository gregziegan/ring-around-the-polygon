;;; Originally made to make my life easier with representing simple math for polygon points in a non-polish prefix notation
;;; I say originally because now I'm liking polish prefix

(defun evaluate (expr env)
    (cond
      ((numberp expr) expr)
      ((atom expr) (lookup-value expr env)) 
      ((eq (operator expr) '*) (* (evaluate (operand1 expr) env) (evaluate (operand2 expr) env)))
      ((eq (operator expr) '/) (/ (evaluate (operand1 expr) env) (evaluate (operand2 expr) env)))
      ((eq (operator expr) '+) (+ (evaluate (operand1 expr) env) (evaluate (operand2 expr) env)))
      ((eq (operator expr) '-) (- (evaluate (operand1 expr) env) (evaluate (operand2 expr) env)))
      ((eq (operator expr) '%) (mod (evaluate (operand1 expr) env) (evaluate (operand2 expr) env)))
      ((eq (operator expr) '^) (expt (evaluate (operand1 expr) env) (evaluate (operand2 expr) env)))
      ((eq (operator expr) '=) (list (operand1 expr) (evaluate (operand2 expr) env)))))

(defun lookup-value (var env)
  (cadr (assoc var env)))

(defun operator (expr)
  (cadr expr))

(defun operand1 (expr)
  (car expr))

(defun operand2 (expr)
  (caddr expr))

(defun deg-to-rad (degrees) (* pi (/ degrees 180.0))) 
