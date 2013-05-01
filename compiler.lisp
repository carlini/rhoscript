(defun new-array () (make-array 0 :adjustable t :fill-pointer 0))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *commands* nil)
  (defparameter *command-info* nil)

  (defmacro n-times (n &body body)
    `(loop for i from 1 to ,n collect ,@body))
  (defun with-char (x c)
    (intern (concatenate 'string
			 c
			 (symbol-name x))))
  (defun with-under (x) (with-char x "_"))
  (defmacro commands (&body body)
    (real-commands body))
  (defun real-commands (body)
    (labels ((gen-cmd (kind name args res cmd)
	       (declare (ignore kind))
	       (declare (ignore res))
	       `(,(with-under name) 
		  ,(mapcar #'cadr args) ,@cmd)))
      
      `(defun make-commands ()
	 (setf *commands*
	       ',(mapcar (lambda (x)
			  (gen-cmd (car x) (cadr x) (caddr x) (cadddr x) (cddddr x)))
			body))
	 (setf *command-info*
	       ',(mapcar (lambda (x)
			   (list (cadr x) (mapcar #'car (caddr x)) (cadddr x)))
			 body))))))

(commands
  (cmd add ((int a) (int b)) (int)
       (+ a b))
  (cmd sub ((int a) (int b)) (int)
       (- a b))
  (cmd call ((fun f)) ()
       (funcall f))
  (cmd range ((int n)) (list)
       (let ((arr (make-array n :adjustable t)))
	 (loop for n from 0 to (- n 1) do (setf (aref arr n) n))
	 arr))
  (cmd map ((fun fn) (list l)) (list)
       (let* ((result (new-array))
	     (i 0)
	     (cons-cell (cons result nil)))
	 (setf (cdr cons-cell)
	       (lambda ()
		 (if (< (1+ i) (length l))
		     (progn
		       (push (aref l i) stack)
		       (funcall fn)
		       (vector-push-extend (pop stack) result)
		       (incf i))
		     (setf (cdr cons-cell) nil))))
	 cons-cell))
  (cmd force ((list l)) (list)
       (loop while (not (null (cdr l))) do
	    (funcall (cdr l)))
       (car l))
  (cmd call-n-times ((fun fn) (int n)) ()
       (loop for i from 0 to (1- n) do (funcall fn))))

(defun parse (commands)
  (let ((command-names (mapcar #'car *command-info*)))
    `(lambda ()
       ,@(mapcar
	  (lambda (cmd)
	    (cond
	      ((listp cmd)
	       `(push ,(parse cmd) stack))
	      ((and (symbolp cmd) (member cmd command-names))
	       (let* ((full-command (assoc cmd *command-info*))
		      (args (cadr full-command))
		      (results (caddr full-command)))
		 (let ((the-command (cons (with-under cmd) (n-times (length args) 
							      '(pop stack)))))
		   (cond
		     ((= (length results) 0)
		      the-command)
		     ((= (length results) 1)
		      `(push ,the-command stack))
		     (t
		      `(setq stack (append ,the-command stack)))))))
	      (t
	       `(push ,cmd stack))))
	  commands))))

(defun do-compile (commands)
  (make-commands)
  `(let ((stack '())
	 (restore-stack '()))
     (declare (ignore restore-stack))
     (labels ,*commands*
       (funcall ,(parse commands))
       stack)))

;(print (do-compile '(10000 range (1 add) map force)))

; 3.3 seconds
; 8.7b cycles
(time 
 (eval (do-compile '(5 100000000 (3 add) call-n-times))))


;; (funcall
;;  (lambda (inc outc)
;;    `(lambda ,(loop for i from 0 to (1- inc) collect 
;; 		  (intern (concatenate 'string "ARG" (write-to-string i))))
;;       arg0))
;;  3 4)
