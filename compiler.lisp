(defun new-array () (make-array 0 :adjustable t :fill-pointer 0))

(defun to-array (list)
  (let ((result (new-array)))
    (loop for item in list do
	 (vector-push-extend item result))
    result))

(set-macro-character #\] (get-macro-character #\)))
(set-macro-character #\[
		     (lambda (stream char)
		       (declare (ignore char))
		       `(run ',(read-delimited-list #\] stream t))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *commands* nil)
  (defparameter *full-commands* nil)
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
    (setf *full-commands* body)
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

;(defun create-docstring

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
       "Compares the top two elements of the stack for equality."
       (equalp a b))
  (cmd neq ((type a) (type b)) (bool)
       "Compares the top two elements of the stack for inequality."
       (not (equalp a b)))
  (cmd gt ((int a) (int b)) (bool)
       "Checks if the first argument is larger than the second."
       (not (> a b)))
  (cmd gte ((int a) (int b)) (bool)
       "Checks if the first argument is larger than or equal to the second."
       (not (>= a b)))
  (cmd drop ((type a)) ()
       "Removes the top element of the stack."
       (declare (ignore a)))
  (cmd print () ()
       (format t "Stack dump:~%")
       (loop for el in stack for i from 0 do
	    (format t "   ~a. ~a~%" i el))
       (format t "~%"))
  (cmd rot ((type a) (type b) (type c)) (type type type)
       "Rotates the top three elements: A B C -> B C A"
       (list b c a))
  (cmd unrot ((type a) (type b) (type c)) (type type type)
       "Inverted rotate of the top three elements: A B C -> C A B"
       (list c a b))
  (cmd or ((bool a) (bool b)) (bool)
       "Logical or of the top two elements of the stack."
       (or a b))
  (cmd add ((int a) (int b)) (int)
       (+ a b))
  (cmd multiply ((int a) (int b)) (int)
       (* a b))
  (cmd subtract ((int a) (int b)) (int)
       (- b a))
  (cmd divide ((int a) (int b)) (int)
       (floor (/ b a)))
  (cmd mod ((int a) (int b)) (int)
       (mod b a))
  (cmd abs ((int a)) (int)
       (abs a))
  (cmd even ((int a)) (bool)
       (evenp a))
  (cmd odd ((int a)) (bool)
       (oddp a))
  (mak inc (int) (int)
       1 add)
;  (mak inc-all (list) (list)
;       (inc) map)
  (cmd arg-a () (type)
       "Pushes the top element of the stack at the time of the last
        context establishment to the stack."
       (car (car argument-restore-stack)))
  (cmd arg-b () (type)
       "Pushes the second to top element of the stack at the time of the last
        context establishment to the stack."
       (cadr (car argument-restore-stack)))
  (cmd arg-c () (type)
       "Pushes the third to top element of the stack at the time of the last
        context establishment to the stack."
       (caddr (car argument-restore-stack)))
  (cmd arg-d () (type)
       "Pushes the forth to top element of the stack at the time of the last
        context establishment to the stack."
       (cadddr (car argument-restore-stack)))
  (cmd call ((fun f)) ()
       "Takes a function off the stack and runs it."
       (funcall f 0 nil))
  (cmd explode ((list l)) ()
       "Pushes each element of a list on to the stack; the head becomes the top."
       (with-forced l list
	 (loop for x across (reverse list) do
	      (push x stack))))
  (cmd implode ((int i)) (list)
       (let ((a (new-array)))
	 (loop for j from 1 to i do
	      (vector-push-extend (pop stack) a))
	 (list a)))
  (cmd range ((int n)) (list)
       "Generates a list of numbers from 0 to n."
       (let ((arr (make-array n :adjustable t)))
	 (loop for n from 0 to (- n 1) do (setf (aref arr n) n))
	 (list arr)))
  (cmd with-index ((list l)) (list)
       "Returns a new list, where each element is a list of the index and previous element."
       (list-to-list-iter l
	 (next
	  (vector-push-extend (list (to-array (list index each))) result))))
  (cmd outer ((list a) (list b)) (list)
       (list-to-list-iter a
	 (next
;	  (print each)
	  (let ((tmp each))
	    (vector-push-extend
	     (list-to-list-iter b
	       (next
		(vector-push-extend (list (to-array (list tmp each))) result)))
	     result)))))
  (cmd sum ((list l)) (int)
       "Computes the sum of a list."
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
	  (vector-push-extend (funcall fn 1 (list each)) result))))
  (cmd filter ((fun fn) (list l)) (list)
       (list-to-list-iter l
	 (next
	  (if (funcall fn 1 (list each))
	      (vector-push-extend each result)))))
  (cmd sort ((list l)) (list)
       (with-forced l list
	 (list (sort list #'<))))
  (cmd reverse ((list l)) (list)
       (with-forced l list
	 (list (reverse list))))
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
		   (vector-push-extend (list (to-array (list e1 e2))) result)
		   (incf i))))))))
  (cmd fold ((fun fn) (type init) (list l)) (type)
       (with-forced l list
	 (loop for el across list do
	      (setf init (funcall fn 1 (list init el))))
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
  (cmd ite ((fun a) (fun b) (bool case)) ()
       (if case
	   (funcall a 0 nil)
	   (funcall b 0 nil)))
  (cmd if ((fun a) (bool case)) ()
       (if case
	   (funcall a 0 nil)))
  (cmd do-while ((fun fn)) ()
       (let ((cont t))
	 (loop while cont do
	      (setf cont (funcall fn 1 nil)))))
  (cmd call-n-times ((fun fn) (int n)) ()
       (loop for i from 0 to (1- n) do (funcall fn 0 nil))))

(defvar stack nil)
(defvar restore-stack nil)
(defvar argument-restore-stack nil)

(defvar *bindings* nil)
      
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

(defun do-decode (command-abbrv types)
  (if (member 'type types) nil
      (let* ((possible-commands 
	      (mapcar #'car 
		      (remove-if-not 
		       (lambda (x) (is-subset-of types (cadr x))) *command-info*)))
	     (as-str (symbol-name command-abbrv))
	     (index (parse-integer (subseq as-str (1+ (position #\- as-str)) (length as-str)))))
	(nth index possible-commands))))
(defun run-compiled (commands)
  (setf stack nil)
  (setf restore-stack nil)
  (setf argument-restore-stack nil)
  (make-commands)
  `(let ((stack '())
	 (argument-restore-stack '())
	 (restore-stack '()))
     (labels ,*commands*
       ,(cons 'progn (create-defuns (parse-and-split commands)))
       (funcall #'fn1 0 nil)
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

(defvar *c1* 0)
(defvar *c2* 0)

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
	  (defun ,fnid (nret args)
	    (if first
		(let* ((fn-body (uncompress-body ',fnid ',block-kinds ',body 
						 (append args stack) #'decompress))
		       (fn-lambda (eval (list 'labels *commands* fn-body))))
		  (setf first nil)
		  (setf (symbol-function ',fnid) fn-lambda)
;		  (print (list 'setf (list 'symbol-function ',fnid) fn-body))
		  (setf (symbol-function ',(with-under fnid)) fn-lambda)
		  (funcall fn-lambda nret args))
		(progn
		  (incf *c2*)
		  (,(with-under fnid) nret args)))))))
   defuns))

(defun decompress (input types inverse-bindings fnid) 
  (declare (ignore input types inverse-bindings fnid))
  (assert nil))

(defvar *compiled-code* nil)

(defun do-encode (real-command types)
  (let ((possible-commands 
	 (mapcar #'car 
		 (remove-if-not 
		  (lambda (x) (is-subset-of types (cadr x))) *command-info*))))
;    (format t "~%TAKING ~a BITS" (log (length possible-commands)))
    (position real-command possible-commands)))

(defun commands-to-commands (input types inverse-bindings fnid)
  (declare (ignore inverse-bindings))
  (let ((new-name (intern (concatenate 'string
				       (symbol-name fnid) "P"))))
    (if (eq (caar input) 'builtin)
	(progn
;	  (print (list (car input)
;		       (cadr (nth (- (length input) 2) input))
;		       (cadr (nth (- (length input) 1) input))
;		       (do-encode (cadr (car input)) types)))

	  (push (list 
		 (cadr (nth (- (length input) 2) input))
		 (cadr (nth (- (length input) 1) input))
		 (do-encode (cadr (car input)) types))
		*compiled-code*)))
    (incf (cadr (nth (- (length input) 1) input)))
    (if (cdddr input)
	(cons (list (car input)
		    `(fun ,new-name)
		    '(builtin call))
	      (car (create-defuns
		    (list (list new-name (cons '(builtin *no-arguments)
					       (cdr input)))))))
	(list (list (car input))))))

(defun compressed-to-commands (input types inverse-bindings fnid)
  (let ((commands nil)
	(cont t)
	(defun-part nil))
    (loop for cmd in input for i from 0 to (length input) while cont do
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
	    (let* ((decoded (do-decode (cadr cmd) types))
		   (full (assoc decoded *command-info*))
		   (args (cadr full))
		   (results (caddr full)))
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
		    (assert (not (= i 0)))
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
		      )))))))
    (cons (reverse commands)
	  defun-part)))

;; Returns a lambda which runs the full program given by the input
(defun uncompress-body (fnid block-kinds input stack decompressor)
;  (if (not (eq fnid 'fn1)) (push '*restoring block-kinds))
  (let* ((types (mapcar #'typeof stack))
 	 (inverse-bindings (group-together (mapcar #'reverse *bindings*)))
	 (uncompressed-code (funcall decompressor input types inverse-bindings fnid))
	 (lambda-part (ir-to-lisp (car uncompressed-code)))
	 (defun-part (cdr uncompressed-code))
	 (final-part
	  `(lambda (nret args)
	     (incf *c1*)
	     ,@(if (member '*restoring block-kinds)
		   '((push stack restore-stack)))
	     (setq stack (append args stack))
	     ,@(if (member '*exploding block-kinds)
		   '((with-forced (pop stack) list 
		       (loop for x across (reverse list) do
			    (push x stack)))))
	     ,@(if (not (member '*no-arguments block-kinds))
		   '((push stack argument-restore-stack)))

	     ,@lambda-part

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
	       answer))))
    (if defun-part
	`(progn
	   ,defun-part
	   ,final-part)
	final-part)))

(defun compile-by-running (input)
  (let ((id 0))
    (labels ((tag-input (in)
	       (if (listp in)
		   (append (mapcar #'tag-input in)
			   (list (intern (concatenate 'string 
						      "COMPILE-TAG-" 
						      (write-to-string (incf id))))
				 0))
		   in))
	     (do-replace (tagged replace-with)
	       (if (listp tagged)
		   (let ((kind (nth (- (length tagged) 2) tagged))
			 (new-tagged (mapcar (lambda (x) (do-replace x replace-with)) tagged))
;			 (new-tagged tagged)
			 (skip 0))
		     (loop for elt in new-tagged while
			  (and (eq (type-of elt) 'symbol)
			       (eq (char (symbol-name elt) 0) #\*)) do
;			  (print "think about *")
;			  (print elt)
			  (incf skip))

		     (loop for elt in replace-with if (eq (car elt) kind) do
;			  (print "DO A REPLACE")
;			  (print elt)
;			  (print (nth (+ (cadr elt) skip) new-tagged))
			  (setf (nth (+ (cadr elt) skip) new-tagged) 
				(intern (concatenate 'string "BUILTIN-" (write-to-string (caddr elt))))))
;		     (print "TAGGED IS THIS")
;		     (print new-tagged)
		     new-tagged)
		   tagged))
	     (drop-last (tree)
	       (if (listp tree)
		   (mapcar #'drop-last (subseq tree 0 (- (length tree) 2)))
		   tree)))
      (let* ((tagged (tag-input input))
	     (*compiled-code* nil))
	(setf (symbol-function 'decompress) #'commands-to-commands)
	(print tagged)
	(eval (run-compiled tagged))
	(print (drop-last (do-replace tagged *compiled-code*)))))))

(defun run (i) 
  (let ((compiled (compile-by-running i)))
    (print compiled)
    (setf (symbol-function 'decompress) #'compressed-to-commands)
    (eval (run-compiled compiled))))


;(defun run (text)
;  (eval (run-compiled (compile-by-running text))))

;(time (loop for i across (caar (run '(8 range permutations (with-index force dup (*exploding *restoring unrot (*exploding *restoring arg-c arg-a eq arg-c arg-a subtract abs arg-d arg-b subtract abs neq or) map force all) map force all) map force))) count i))

;(run '(4 range permutations (with-index force dup (*exploding *restoring unrot (*exploding *restoring arg-c arg-a eq arg-c arg-a subtract abs arg-d arg-b subtract abs neq or) map force all) map force all) map force))

;(time
;  [8 range permutations (with-index dup outer flatten (flatten) map (*exploding *restoring arg-c eq arg-c arg-a subtract abs arg-d arg-b subtract abs neq or) map all) filter length])

;(time
; [5 range 0 (add) fold])

;(time
; [1 2 3 add])

; d c abs(a-c) 0=abs(a-c) arg-b

; TODO this shouldn't break ...
; after a non-stack-restoring block, have to find some way of always
; re-evaluating what's on the stack. This means any builtin that takes
; a function as an argument. map, fold, anything.
;(run '(5 range ((1) call add) map force))
;(run '(5 range ((1 add) call) map force))

;(run '(5 range (1 add) map force))

;(progn
;  (time (run '(8 range permutations (with-index force dup (*exploding *restoring) map force) map force)))   nil)
