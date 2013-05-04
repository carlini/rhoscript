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

(defvar *count* 0)

(defun sym ()
  (intern (concatenate 'string
		       "FN"
		       (write-to-string (incf *count*)))))

(defun tree-assoc (tr list)
  (if (null list)
      nil
      (if (tree-equal tr (car (car list)))
	  (car list)
	  (tree-assoc tr (cdr list)))))


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
		       (let ((res (funcall fn 1 (aref l i))))
			 (vector-push-extend res result))
		       (incf i))
		     (setf (cdr cons-cell) nil))))
	 cons-cell))
  (cmd force ((list l)) (list)
       (loop while (not (null (cdr l))) do
	    (funcall (cdr l)))
       (car l))
  (cmd call-n-times ((fun fn) (int n)) ()
       (loop for i from 0 to (1- n) do (funcall fn 0))))

(defvar stack nil)
(defvar *defined-vars* nil)
;(defvar *function-definitions* nil)

(defun compress-code (input)
  (make-commands)
  (let ((bindings '((dup a) (add b) (sub c) (call b) (range d) (map c) (force b) (call-n-times c))))
    (labels ((deeprepl (x)
	       (if (listp x)
		   (mapcar #'deeprepl x)
		   (let ((r (cadr (assoc x bindings))))
		     (if r r x)))))
      (deeprepl input))))

(defun group-together (bindings)
  (let ((res '()))
    (loop for item in bindings do
	 (if (not (assoc (car item) res))
	     (push item res)
	     (push (cadr item) (cdr (assoc (car item) res)))))
    res))

(defun typeof (el)
  (cond
    ((numberp el) 'int)
    ((listp el) 'list)
    ((vectorp el) 'list)
    ((functionp el) 'fun)
    (t (error `(this is very bad ,el)))))

(defun is-prefix (shorter longer)
  (if (null shorter) 
      t
      (if (null longer)
	  nil
	  (and (eq (car shorter) (car longer))
	       (is-prefix (cdr shorter) (cdr longer))))))

(defun do-decode (possible-commands types)
  (let ((possible 
	 (remove-if-not
	  (lambda (x)
	    (is-prefix (cadr (assoc x *command-info*)) types))
	  (cdr possible-commands))))
    (print possible-commands)
    (print types)
    (print possible)
    (assert (= (length possible) 1))
    (car possible)))

(defun do-compile-2 (commands)
  (setf stack nil)
  (setf *defined-vars* nil)
  (make-commands)
  `(let ((stack '())
	 (restore-stack '()))
     (declare (ignore restore-stack))
     (labels ,*commands*
       ,(cons 'progn (uncompress-pass-2 (uncompress-pass-1 commands)))
       (funcall #'fn1 0)
       stack)))

;; Turns the compressed input program to a set of defuns referencing each other.
(defun uncompress-pass-1 (program)
  (let ((defuns '())
	(fnid 0))
    (defun helper (input)
      (incf fnid)
      (let ((myid (intern (concatenate 'string "FN" (write-to-string fnid)))))
	(push
	 (list
	  myid
	  (mapcar
	   (lambda (cmd)
	     (cond
	       ((listp cmd)
		(list 'fun (helper cmd)))
	       ((numberp cmd)
		(list 'int cmd))
	       (t
		(list 'builtin cmd))))
	   input))
	 defuns)
	myid))
    (helper program)
    defuns))

;; Wraps everything with the lisp defuns so everything actually runs
(defun uncompress-pass-2 (defuns)
  (mapcar
   (lambda (input)
     (let ((fnid (car input))
	   (body (cadr input)))
       `(let ((first t))
	  (defun ,fnid (nret &rest args)
	    (if first
		(let* ((fn-body (uncompress-pass-3 ',body (append args stack)))
		       (fn-lambda (eval `(labels ,*commands* ,fn-body))))
		  (print '("Doing a decode for fn" ,fnid))
		  (setf first nil)
		  (defun ,(with-under fnid) (args)
		    (apply fn-lambda args))
		  (apply fn-lambda (cons nret args)))
		(,(with-under fnid) (cons nret args)))))))
   defuns))

;; Returns a lambda which runs the full program given by the input
(defun uncompress-pass-3 (input stack)
  (let* ((types (mapcar #'typeof stack))
	 (bindings '((dup a) (add b) (sub c) (call b) (range d) (map c) (force b) (call-n-times c)))
 	 (inverse-bindings (group-together (mapcar #'reverse bindings))))
    `(lambda (nret &rest args)
       (setq stack (append args stack))
       ,@(mapcar
	 (lambda (cmd)
	   (case (car cmd)
	     (int
	      (push 'int types)
	      `(push ,(cadr cmd) stack))
	     (fun
	      (push 'fun types)
	      `(push #',(cadr cmd) stack))
	     (builtin
	      (let* ((decoded (do-decode (assoc (cadr cmd) inverse-bindings) types))
		     (full (assoc decoded *command-info*))
		     (args (cadr full))
		     (results (caddr full)))
		(dotimes (i (length args)) (pop types))
		(setf types (append results types))
		(let ((the-command `(,(with-under decoded) ,@(n-times (length args) '(pop stack)))))
		  (cond
		    ((= (length results) 0)
		     the-command)
		    ((= (length results) 1)
		     `(push ,the-command stack))
		    (t
		     `(setq stack (append ,the-command stack)))))))))
	 input)
	      
       (case nret
	 (0 nil)
	 (1 (pop stack))
	 (2 (list (pop stack) (pop stack)))
	 (otherwise (loop for i from 1 to nret collect (pop stack)))))))
       

(let ((comp (compress-code '(100 range 100000 ((2 add) map force) call-n-times))))
;  (print comp)
;(let ((comp (compress-code '(20 range (1 (add) call) map force))))
;(let ((comp (compress-code '(20 range (1 add) map force))))
;(let ((comp (compress-code '(2 (((1 add) call) call) call))))
;(let ((comp (compress-code '(2 3 add))))
;  (print (uncompress-pass-2
;	  (uncompress-pass-1 comp))))
  (time (eval (do-compile-2 comp))))


;(time (eval (do-compile '(100 range 1000 ((1 add) map force) call-n-times))))
;(eval (do-compile '(5 dup add)))
