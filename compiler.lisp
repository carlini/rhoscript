(defun new-array () (make-array 0 :adjustable t :fill-pointer 0))

(defun to-array (list)
  (let ((result (new-array)))
    (loop for item in list do
	 (vector-push-extend item result))
    result))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *commands* nil)
  (defparameter *command-info* nil)

  (defmacro n-times (n &body body)
    `(loop for i from 1 to ,n collect ,@body))

  (defun ir-to-lisp (input)
    (mapcar
     (lambda (decoded)
       (case (car decoded)
	 (int
	  `(push ,(cadr decoded) stack))
	 (fun
	  `(push #',(cadr decoded) stack))
	 (builtin
	  (let* ((full (assoc (cadr decoded) *command-info*))
		 (args (cadr full))
		 (results (caddr full)))
	    (let ((the-command `(,(with-under (cadr decoded)) ,@(n-times (length args) '(pop stack)))))
	      (cond
		((= (length results) 0)
		 the-command)
		((= (length results) 1)
		 `(push ,the-command stack))
		(t
		 `(setq stack (append ,the-command stack)))))))))
     input))

;  (ir-to-lisp '((int 1) (builtin add)))

  (defun with-types (input)
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

  (defun with-char (x c)
    (intern (concatenate 'string
			 c
			 (symbol-name x))))
  (defun with-under (x) (with-char x "_"))
  (defmacro commands (&body body)
    (real-commands body))
  (defun real-commands (body)
    (labels ((gen-cmd (kind name args res cmd)
	       (declare (ignore res))
	       (case kind
		 (cmd
		  `(,(with-under name) 
		     ,(mapcar #'cadr args) ,@cmd))
		 (mak
		  `(,(with-under name) (&rest args)
		     (setf stack (append args stack))
		     ,@(ir-to-lisp (with-types cmd))
		     (pop stack)))))
	     (get-args (inp)
	       (case (car inp)
		 (cmd
		  (mapcar #'car (caddr inp)))
		 (mak
		  (caddr inp)))))
      
      (let ((*command-info*
	     (mapcar (lambda (x)
			 (list (cadr x) (get-args x) (cadddr x)))
		       body)))
	  `(defun make-commands ()
	     (setf *commands*
		   ',(mapcar
		      (lambda (x) 
			(gen-cmd (car x) (cadr x) (caddr x) (cadddr x) (cddddr x)))
		      body))
	     (setf *command-info*
		   ',(mapcar (lambda (x)
			       (list (cadr x) (get-args x) (cadddr x)))
			     body)))))))

(defvar null-symbol (gensym))

(defun list-get (list n)
  (if (< n (length (car list)))
      (aref (car list) n)
      (progn
	(loop while (and (<= (length (car list)) n) (not (null (cdr list)))) do
	     (funcall (cdr list)))
	(if (< n (length (car list)))
	    (aref (car list) n)
	    null-symbol))))

(defmacro with-forced (input output &body run)
  `(let ((,output (car ,input)))
     (loop while (not (null (cdr ,input))) do
	  (funcall (cdr ,input)))
     ,@run))

(defmacro list-to-list-iter (list-name &body body)
  `(let* ((result (new-array))
	  (index 0)
	  (cons-cell (cons result nil)))
     ,@(if (assoc 'initially body)
	   (cdr (assoc 'initially body)))
     (setf (cdr cons-cell)
	   (lambda ()
	     (let ((each (list-get ,list-name index)))
	       (if (eq null-symbol each)
		   (setf (cdr cons-cell) nil)
		   (progn
		    ,@(cdr (assoc 'next body))
		    (incf index))))))
     cons-cell))

(commands
  (cmd dup ((type other)) (type type)
       (list other other))
  (cmd add ((int a) (int b)) (int)
       (+ a b))
  (cmd subtract ((int a) (int b)) (int)
       (- a b))
  (mak inc (int) (int)
       1 add)
  (cmd sub ((int a) (int b)) (int)
       (- a b))
  (cmd call ((fun f)) ()
       (funcall f 0))
  (cmd range ((int n)) (list)
       (let ((arr (make-array n :adjustable t)))
	 (loop for n from 0 to (- n 1) do (setf (aref arr n) n))
	 (cons arr nil)))
  (cmd sum ((list l)) (int)
       (with-forced l list
	 (loop for el across list sum el)))
  (cmd min ((list l)) (int)
       (with-forced l list
	 (loop for el across list minimize el)))
  (cmd max ((list l)) (int)
       (with-forced l list
	 (loop for el across list maximize el)))
  (cmd map ((fun fn) (list l)) (list)
       (list-to-list-iter l
	 (next
	  (vector-push-extend (funcall fn 1 each) result))))
  (cmd length ((list l)) (int)
       (with-forced l list
	 (length list)))
  (cmd get ((int i) (list l)) (type)
       (list-get l i))
  (cmd prefixes ((list l)) (list)
       (list-to-list-iter l
	 (initially
	  (vector-push-extend (new-array) result))
	 (next
	  (let ((tmp (new-array)))
	    (loop for j from 0 to index do
		 (vector-push-extend (aref (car l) j) tmp))
	    (vector-push-extend tmp result)))))
  (cmd permutations ((list l)) (list)
       (with-forced l list
	 (labels ((permute (list)
		    (if (<= (length list) 1)
			(list list)
			(loop for i from 0 to (1- (length list)) append
			     (mapcar (lambda (x) (cons (nth i list) x))
				     (permute (without i list))))))
		  (without (i list)
		    (if (= i 0)
			(cdr list)
			(cons (car list) (without (1- i) (cdr list))))))
	   (list (to-array (mapcar (lambda (x) (list (to-array x)))
				   (permute (coerce list 'list))))))))
  (cmd force ((list l)) (list)
       (with-forced l list
	 l))
  (cmd force-1 ((list l)) (list)
       (if (not (null (cdr l)))
	   (funcall (cdr l)))
       l)
  (cmd call-n-times ((fun fn) (int n)) ()
       (loop for i from 0 to (1- n) do (funcall fn 0))))

(defvar stack nil)

(defvar *bindings* nil)

(defun compress-code (input)
  (make-commands)
  (let ((buckets (loop for i from 0 to 20 collect nil)))
    (labels ((fits-in (type bucket)
	       (every 
		(lambda (x) 
		  (not (or (is-subset-of type (cdr x)) 
			   (is-subset-of (cdr x) type))))
		bucket))
	     (add-to-first (cmd type)
	       (let ((free (loop for b in buckets for i from 0 to (length buckets) when (fits-in type b) collect i)))
		 (push (cons cmd type) (nth (car free) buckets)))))
      (loop for x in *command-info* do
	   (add-to-first (car x) (cadr x)))
      (let* ((letters '(lower-a lower-b lower-c lower-d lower-e lower-f lower-g 
			lower-h lower-i lower-j lower-k lower-l lower-m lower-n 
			lower-o lower-p lower-q lower-r lower-s lower-t lower-u 
			lower-v lower-w lower-x lower-y lower-z upper-a upper-b 
			upper-c upper-d upper-e upper-f upper-g upper-h upper-i 
			upper-j upper-k upper-l upper-m upper-n upper-o upper-p 
			upper-q upper-r upper-s upper-t upper-u upper-v upper-w 
			upper-x upper-y upper-z 0 1 2 3 4 5 6 7 8 9 0 backtick 
			tilde bang at hash dollar percent carrot and star lpar 
			rpar minus underscore plus equals space tab newline 
			semicolon colon comma period slash question quote 
			doublequote bar backslash)))
	(setf *bindings*
	     (loop for i from 0 to (1- (length buckets)) append
		  (mapcar (lambda (x) (list (car x) (nth i letters))) (nth i buckets)))))
      (labels ((deeprepl (x)
		 (if (listp x)
		     (mapcar #'deeprepl x)
		     (let ((r (cadr (assoc x *bindings*))))
		       (if r r x)))))
	(deeprepl input)))))
      
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

(defun is-subset-of (a b)
  (cond
    ((null a) t)
    ((null b) t)
    ((or (equal (car b) 'type) (equal (car a) (car b)))
     (is-subset-of (cdr a) (cdr b)))
    (t nil)))


(defun is-prefix (shorter longer)
  (is-subset-of (subseq longer 0 (length longer)) shorter))

(defun do-decode (possible-commands types)
  (let ((possible 
	 (remove-if-not
	  (lambda (x)
	    (is-prefix (cadr (assoc x *command-info*)) types))
	  (cdr possible-commands))))
    (assert (<= (length possible) 1))
    (car possible)))

(defun do-compile (commands)
  (setf stack nil)
  (make-commands)
  `(let ((stack '())
	 (restore-stack '()))
     (declare (ignore restore-stack))
     (labels ,*commands*
       ,(cons 'progn (create-defuns (parse-and-split commands)))
       (funcall #'fn1 0)
       stack)))

;; Turns the compressed input program to a set of defuns referencing each other.
(defun parse-and-split (program)
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
(defun create-defuns (defuns)
  (mapcar
   (lambda (input)
     (let ((fnid (car input))
	   (body (cadr input)))
       `(let ((first t))
	  (defun ,fnid (nret &rest args)
	    (if first
		(let* ((fn-body (uncompress-body ',body (append args stack)))
		       (fn-lambda (eval `(labels ,*commands* ,fn-body))))
		  (setf first nil)
		  (defun ,(with-under fnid) (args)
		    (apply fn-lambda args))
		  (apply fn-lambda (cons nret args)))
		(,(with-under fnid) (cons nret args)))))))
   defuns))

;; Returns a lambda which runs the full program given by the input
(defun uncompress-body (input stack)
  (let* ((types (mapcar #'typeof stack))
 	 (inverse-bindings (group-together (mapcar #'reverse *bindings*))))
    `(lambda (nret &rest args)
       (setq stack (append args stack))
       ,@(ir-to-lisp
	  (mapcar
	   (lambda (cmd)
	     (case (car cmd)
	       (int
		(push 'int types)
		cmd)
	       (fun
		(push 'fun types)
		cmd)
	       (builtin
		(let* ((decoded (do-decode (assoc (cadr cmd) inverse-bindings) types))
		       (full (assoc decoded *command-info*))
		       (args (cadr full))
		       (results (caddr full)))
		  (case decoded
		    (dup
		     (push (car types) types)
		     '(builtin dup))
		    (otherwise
		     (dotimes (i (length args)) (pop types))
		     (setf types (append results types))
		     (list 'builtin decoded)))))))
	   input))
	      
       (case nret
	 (0 nil)
	 (1 (pop stack))
	 (2 (list (pop stack) (pop stack)))
	 (otherwise (loop for i from 1 to nret collect (pop stack)))))))

;(let ((comp (compress-code '(1000 range 10000 ((2 add) map force) call-n-times))))
;  (print comp)
;(let ((comp (compress-code '(20 range (1 (add) call) map force))))
;(let ((comp (compress-code '(20 range (1 add) map (2 add) map force))))
;(let ((comp (compress-code '(2 (((1 add) call) call) call))))
;(let ((comp (compress-code '(2 dup add))))
;(let ((comp (compress-code '(1000000 range (inc) map (2 add) map force))))
(let ((comp (compress-code '(5 range (1 add) map prefixes force))))
  (print (eval (do-compile comp))))



;(time (eval (do-compile '(100 range 1000 ((1 add) map force) call-n-times))))
;(eval (do-compile '(5 dup add)))
