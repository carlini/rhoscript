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

(defmacro wrap-fn (&body b)
  `(lambda (nret &rest args)
     (setq stack (append args stack))
     ,@b
     (case nret
       (0 nil)
       (1 (pop stack))
       (2 (list (pop stack) (pop stack)))
       (otherwise (loop for i from 1 to nret collect (pop stack))))))

(commands
  (cmd dup () (type)
       (car stack))
  (cmd add ((int a) (int b)) (int)
       (+ a b))
  (cmd sub ((int a) (int b)) (int)
       (- a b))
  (cmd call ((fun f)) ()
       (funcall f 0))
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
		 (if (< i (length l))
		     (progn
		       (vector-push-extend (funcall fn 1 (aref l i)) result)
		       (incf i))
		     (setf (cdr cons-cell) nil))))
	 cons-cell))
  (cmd force ((list l)) (list)
       (loop while (not (null (cdr l))) do
	    (funcall (cdr l)))
       (car l))
  (cmd call-n-times ((fun fn) (int n)) ()
       (loop for i from 0 to (1- n) do (funcall fn 0))))

(defun parse (commands)
  (let ((command-names (mapcar #'car *command-info*)))
    `(wrap-fn
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
	 commands)
       )))

(defun do-compile (commands)
  (make-commands)
  `(let ((stack '())
	 (restore-stack '()))
     (declare (ignore restore-stack))
     (labels ,*commands*
       (funcall ,(parse commands) 0)
       stack)))

(defun compress-code (input)
  (make-commands)
  (let ((bindings '((dup a) (add b) (sub c) (call b) (range d) (map c) (force b) (call-n-times c))))
    (labels ((deeprepl (x)
	       (if (listp x)
		   (mapcar #'deeprepl x)
		   (let ((r (cadr (assoc x bindings))))
		     (if r r x)))))
      (deeprepl input))))

(compress-code '(100 range 1000 ((1 add) map force) call-n-times))

;(time (eval (do-compile '(100 range 1000 ((1 add) map force) call-n-times))))
(eval (do-compile '(5 dup add)))
