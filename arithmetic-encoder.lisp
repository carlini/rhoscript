(defun point-in-range (point low high)
  (and
   (> point low)
   (< point high)))

(defun arithmetic-encode (symbols weighted-possibles)
  (labels ((pick-point-in (low high)
	     (assert (< low high))
	     (let ((point (/ 1 2))
		   (step (/ 1 2))
		   (midpoint (/ (+ low high) 2))
		   (emit nil))
	       (loop while (not (and (point-in-range (+ point step) low high)
				     (point-in-range (- point step) low high))) do
		    (if (point-in-range midpoint point (+ point step))
			(progn
			  (setf point (+ point (/ step 2)))
			  (push 1 emit))
			(progn
			  (setf point (- point (/ step 2)))
			  (push 0 emit)))
		    (setf step (/ step 2)))
	       emit)))
;    (print "setup done")
  (let ((low 0)
	(high 1))
    (loop for element in symbols for weighted-possible in weighted-possibles do
;	 (print 'iter)
;	 (print element)
;	 (print weighted-possible)
;	 (print (assoc element weighted-possible :test 'equalp))
	 (assert (< low high))
	 (let ((total-weight (loop for el in weighted-possible sum (cadr el)))
	       (lower-weight (loop for el in weighted-possible 
				while (not (equalp (car el) element))
				sum (cadr el)))
	       (this-weight (cadr (assoc element weighted-possible :test 'equalp))))
;	   (print "dolet")
;	   (format t "~%ENCODE WEIGHTS ~a and ~a~%" total-weight lower-weight)
	   
	   (let* ((distance-outer (- high low))
		  (new-low-offset (* (/ lower-weight total-weight) distance-outer))
		  (new-high-offset (* (/ this-weight total-weight) distance-outer)))
;	     (print "sdf")
	     (setf low (+ low new-low-offset))
	     (setf high (+ low new-high-offset))
	     (format t "~%After ~a we have ~a ~a." element low high)
;	     (print "qqq")
	     )))
;    (print "done step 1")
    (pick-point-in low high))))

(defun arithmetic-decode-preprocess (number)
  (let ((real-number 0)
	(step (/ 1 2)))
    (loop for el in (reverse number) do
	 (if (= el 1)
	     (setf real-number (+ real-number step)))
	 (setf step (/ step 2)))
    (setf real-number (+ real-number step))
    real-number))
    
(defun arithmetic-decode (number weighted-possible)
  (let ((total-weight (loop for el in weighted-possible sum (cadr el))))
;    (format t "~%FULL WEIGHT ~a~%" total-weight)
	(let ((it 0) (sumsofar 0))
	  (loop
	     for el in weighted-possible 
	     while (< sumsofar number) do
	       (setf sumsofar (+ sumsofar (/ (cadr el) total-weight)))
	       (incf it))
	  (decf it)
	  (let ((decoding (nth it weighted-possible)))
	    (format t "~%Decoded to ~a" decoding)
;	    (format t "number now ~a" number)
	    (setf number (- number 
			    (- sumsofar (/ (cadr decoding) total-weight))))
;	    (format t " number then ~a" number)
	    (setf number (/ number
			    (/ (cadr decoding) total-weight)))
;	    (format t "number finally ~a~%" number)
	    (cons number (car decoding))))))

(defun arithmetic-decode-loop (number weighted-possible)
  (let* ((elt (arithmetic-decode number weighted-possible))
	 (number (car elt))
	 (res (cdr elt)))
    (if (not (eq res (caar (last weighted-possible))))
	(cons res (arithmetic-decode-loop number weighted-possible))
	(list res))))

;(trace arithmetic-decode)
;(trace arithmetic-decode-loop)

;(let ((x (list '((1 60) (2 20) (3 10) (4 10)))))
;  (setf (cdr x) x)
;  (let* ((e (arithmetic-encode (loop for i from 1 to 2 append '(1 3 1 3 1 3 2 2 2 4)) x))
;	 (d (arithmetic-decode-loop (arithmetic-decode-preprocess e) (car x))))
;    (print e)
;    (print d)))

;(loop for i from 1 to 5 collect 2)
