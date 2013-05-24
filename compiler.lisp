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
	 (list
	  `(push '(,(cadr decoded)) stack))
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
  (let ((name (gensym)))
    `(let ((,name ,input))
       (let ((,output (car ,name)))
	 (loop while (not (null (cdr ,name))) do
	      (funcall (cdr ,name)))
	 ,@run))))

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
		   (progn
		    ,@(if (assoc 'finally body)
			  (cdr (assoc 'finally body)))
		    (setf (cdr cons-cell) nil))
		   (progn
		    ,@(cdr (assoc 'next body))
		    (incf index))))))
     cons-cell))

(defmacro creating-new-list (&body body)
  `(let* ((result (new-array))
	  (cons-cell (cons result nil)))
     ,@(if (assoc 'initially body)
	   (cdr (assoc 'initially body)))
     (setf (cdr cons-cell)
	   (lambda ()
	     (if (not (progn ,@(cdr (assoc 'next body))))
		 (progn
		   ,@(if (assoc 'finally body)
			 (cdr (assoc 'finall body)))
		   (setf (cdr cons-cell) nil)))))
     cons-cell))

(commands
  (cmd dup ((type other)) (type type)
       "Duplicates the top element of the stack."
       (list other other))
  (cmd swap ((type a) (type b)) (type type)
       "Swap the order of the top two elements on the stack."
       (list b a))
  (cmd eq ((type a) (type b)) (bool)
       (equalp a b))
  (cmd neq ((type a) (type b)) (bool)
       (not (equalp a b)))
  (cmd drop ((type a)) ()
       (declare (ignore a)))
  (cmd print () ()
       (format t "Stack dump:~%")
       (loop for el in stack for i from 0 do
	    (format t "   ~a. ~a~%" i el))
       (format t "~%"))
  (cmd rot ((type a) (type b) (type c)) (type type type)
       (list b c a))
  (cmd unrot ((type a) (type b) (type c)) (type type type)
       (list c a b))
  (cmd or ((bool a) (bool b)) (bool)
       (or a b))
  (cmd add ((int a) (int b)) (int)
       (+ a b))
  (cmd multiply ((int a) (int b)) (int)
       (* a b))
  (cmd subtract ((int a) (int b)) (int)
       (- a b))
  (cmd abs ((int a)) (int)
       (abs a))
  (mak inc (int) (int)
       1 add)
;  (mak inc-all (list) (list)
;       (inc) map)
  (cmd arg-a () (type)
       (car (car argument-restore-stack)))
  (cmd arg-b () (type)
       (cadr (car argument-restore-stack)))
  (cmd arg-c () (type)
       (caddr (car argument-restore-stack)))
  (cmd arg-d () (type)
       (cadddr (car argument-restore-stack)))
  (cmd sub ((int a) (int b)) (int)
       (- a b))
  (cmd call ((fun f)) ()
       (funcall f 0))
  (cmd explode ((list l)) ()
       (with-forced l list
	 (loop for x across (reverse list) do
	      (push x stack))))
  (cmd range ((int n)) (list)
       (let ((arr (make-array n :adjustable t)))
	 (loop for n from 0 to (- n 1) do (setf (aref arr n) n))
	 (list arr)))
  (cmd with-index ((list l)) (list)
       (list-to-list-iter l
	 (next
	  (vector-push-extend (list (to-array (list index each))) result))))
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
  (cmd any ((list l)) (list)
       (not (loop for i from 0 until (eq (list-get l i) null-symbol) never (list-get l i))))
  (cmd all ((list l)) (list)
       (loop for i from 0 until (eq (list-get l i) null-symbol) always (list-get l i)))
  (cmd zip ((list a) (list b)) (list)
       (let ((i 0))
	 (creating-new-list
	  (next
	   (let ((e1 (list-get a i)) (e2 (list-get b i)))
	     (if (or (eq e1 null-symbol) (eq e2 null-symbol))
		 nil
		 (progn
		   (vector-push-extend (to-array (list e1 e2)) result)
		   (incf i))))))))
  (cmd fold ((fun fn) (type init) (list l)) (type)
       (with-forced l list
	 (loop for el across list do
	      (setf init (funcall fn 1 init el)))
	 init))
  (cmd length ((list l)) (int)
       (with-forced l list
	 (length list)))
  (cmd get ((int i) (list l)) (type)
       (list-get l i))
  (cmd concatenate ((list a) (list b)) (list)
       (let ((res (new-array)))
	 (with-forced b list
	   (loop for j from 0 to (1- (length list)) do
		(vector-push-extend (aref list j) res)))
	 (with-forced a list
	   (loop for j from 0 to (1- (length list)) do
		(vector-push-extend (aref list j) res)))
	 (list res)))
  (cmd prefixes ((list l)) (list)
       (list-to-list-iter l
	 (initially
	  (vector-push-extend (list (new-array)) result))
	 (next
	  (vector-push-extend (list (subseq (car l) 0 (1+ index))) result))))
  (cmd flatten ((list l)) (list)
       (list-to-list-iter l
	 (next
	  (with-forced each each-list
	    (loop for el across each-list do
		 (vector-push-extend el result))))))
  (cmd uniq ((list l)) (list)
       (let ((seen (make-hash-table :test 'equalp)))
	 (list-to-list-iter l
	   (next
	    (if (not (gethash each seen))
		(progn
		  (setf (gethash each seen) t)
		  (vector-push-extend each result)))))))
	  
  (cmd suffixes ((list l)) (list)
       (with-forced l list
	 (let ((length (length list)))
	   (list-to-list-iter l
	     (next
	      (vector-push-extend (list (subseq (car l) index length)) result))
	     (finally
	      (vector-push-extend (list (new-array)) result))))))
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
	 list
	 l))
  (cmd force-1 ((list l)) (list)
       (if (not (null (cdr l)))
	   (funcall (cdr l)))
       l)
  (cmd call-n-times ((fun fn) (int n)) ()
       (loop for i from 0 to (1- n) do (funcall fn 0))))

(defvar stack nil)
(defvar restore-stack nil)
(defvar argument-restore-stack nil)

(defvar *bindings* nil)

(defun compress-code (input)
  (make-commands)
  (let ((buckets (loop for i from 0 to 70 collect nil)))
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
			upper-x upper-y upper-z n0 n1 n2 n3 n4 n5 n6 n7 n8 n9 n0 
			backtick tilde bang at hash dollar percent carrot and star 
			lpar rpar minus underscore plus equals space tab newline 
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
    ((eq (type-of el) 'boolean) 'bool)
    ((eq (type-of el) 'null) 'bool)
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
;  (print possible-commands)
;  (print types)
  (assert possible-commands)
  (let ((possible 
	 (remove-if-not
	  (lambda (x)
;	    (print (list (assoc x *command-info*) (is-prefix (cadr (assoc x *command-info*)) types)))
	    (is-prefix (cadr (assoc x *command-info*)) types))
	  (cdr possible-commands))))
    (assert (<= (length possible) 1))
    (car possible)))

(defun do-compile (commands)
  (setf stack nil)
  (setf restore-stack nil)
  (setf argument-restore-stack nil)
  (make-commands)
  `(let ((stack '())
	 (argument-restore-stack '())
	 (restore-stack '()))
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
	       ((vectorp cmd)
		(list 'list cmd))
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
     (let* ((fnid (car input))
	    (body (cadr input))
	    (block-kinds nil))
       (loop while (and (eq (caar body) 'builtin)
			(eq (char (symbol-name (cadr (car body))) 0) #\*)) do
	    (push (cadr (car body)) block-kinds)
	    (setf body (cdr body)))
       `(let ((first t))
	  (defun ,fnid (nret &rest args)
	    (if first
		(let* ((fn-body (uncompress-body ',fnid ',block-kinds ',body (append args stack)))
		       (fn-lambda (eval (list 'labels *commands* fn-body))))
		  (setf first nil)
		  (print (list ',fnid fn-body))
		  (defun ,(with-under fnid) (args)
		    (apply fn-lambda args))
		  (apply fn-lambda (cons nret args)))
		(,(with-under fnid) (cons nret args)))))))
   defuns))

;; Returns a lambda which runs the full program given by the input
(defun uncompress-body (fnid block-kinds input stack)
;  (if (not (eq fnid 'fn1)) (push '*restoring block-kinds))
  (let* ((types (mapcar #'typeof stack))
 	 (inverse-bindings (group-together (mapcar #'reverse *bindings*)))
	 (lambda-part nil)
	 (defun-part nil))
    (setf lambda-part
	  `(lambda (nret &rest args)
	     ,@(if (member '*restoring block-kinds)
		   '((push stack restore-stack)))
	     (setq stack (append args stack))
	     ,@(if (member '*exploding block-kinds)
		   '((with-forced (pop stack) list 
		       (loop for x across (reverse list) do
			    (push x stack)))))
	     ,@(if (not (member '*no-arguments block-kinds))
		   '((push stack argument-restore-stack)))
	     ,@(ir-to-lisp
		(let ((commands nil)
		      (cont t))
;		  (print "LOOP TIME")
		  (loop for cmd in input for i from 0 to (length input) while cont do
;		       (print "ITERATION")
;		       (print cmd)
;		       (print types)
		       (case (car cmd)
			 (int
			  (push 'int types)
			  (push cmd commands))
			 (fun
			  (push 'fun types)
			  (push cmd commands))
			 (list
			  (push 'list types)
			  (push cmd commands))
			 (builtin
			  (let* ((decoded (do-decode (assoc (cadr cmd) inverse-bindings) types))
				 (full (assoc decoded *command-info*))
				 (args (cadr full))
				 (results (caddr full)))
;			    (print "DECODED")
;			    (print decoded)
			    (if decoded
				(case decoded
				  (dup
				   (push (car types) types)
				   (push '(builtin dup) commands))
				  (swap
				   (let ((a (pop types)) (b (pop types)))
				     (push a types)
				     (push b types))
				   (push '(builtin swap) commands))
				  (otherwise
				   (dotimes (i (length args)) (pop types))
				   (setf types (append results types))
				   (push (list 'builtin decoded) commands)))
				(progn
				  (let ((new-name (intern (concatenate 'string 
								       (symbol-name fnid) "P"))))
				    (setf cont nil)
				    (push (list 'fun new-name) commands)
				    (push '(builtin call) commands)
				    (setf defun-part 
					  (car 
					   (create-defuns 
					    (list (list new-name (cons '(builtin *no-arguments)
								       (subseq input i)))))))
;				    (print "DO ABORT CODE")
				    )))))))
;		  (print `(and we know that i is ,i with up to ,(length input)))
		  (reverse commands)))

	     (let ((answer 
		    (case nret
		      (0 nil)
		      (1 (pop stack))
		      (2 (list (pop stack) (pop stack)))
		      (otherwise (loop for i from 1 to nret collect (pop stack))))))
	       ,@(if (not (member '*no-arguments block-kinds))
		     '((pop argument-restore-stack)))
	       ,@(if (member '*restoring block-kinds)
		     '((setf stack (pop restore-stack))))
	       answer)))
;    (print defun-part)
    (if defun-part
	`(progn
	   ,defun-part
	   ,lambda-part)
	lambda-part)))

(defun run (text)
  (eval (do-compile (compress-code text))))
(defun rrun (text)
  (print (do-compile (compress-code text))))

;(time (loop for i across (caar (run '(8 range permutations (with-index force dup (*exploding *restoring unrot (*exploding *restoring arg-c arg-a eq arg-c arg-a subtract abs arg-d arg-b subtract abs neq or) map force all) map force all) map force))) count i))

(run '(4 range permutations (with-index force dup (*exploding *restoring unrot (*exploding *restoring arg-c arg-a eq arg-c arg-a subtract abs arg-d arg-b subtract abs neq or) map force all) map force all) map force))

;(progn
;  (time (run '(8 range permutations (with-index force dup (*exploding *restoring) map force) map force)))   nil)
