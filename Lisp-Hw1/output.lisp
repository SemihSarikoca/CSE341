(declaim (ftype (function (integer integer) integer) sum))
(defun sum (a b)
(let ((c (+ a b)))
c
))
(defun main ()
(let* ((x 10)
(y 20))
(setf result (sum x y))
(if (> result 25) (progn
(format t "Result is greater than 25~%")
(setf x 5)
))
(loop for i = 0
then (incf i)
while (< i 10) do (progn
(format t "~A~%" i)
))
0
))
(main)
