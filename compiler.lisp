(defparameter *times* (make-hash-table))

(defmacro mydefun (name args &body body)
  (let ((sym (gensym)) (sym2 (gensym)))
    `(defun ,name ,args
       (let ((,sym2 (get-internal-real-time)))
	 (let ((,sym
		(progn
		  ,@body)))
	   (if (gethash ',name *times*)
	       (setf (gethash ',name *times*) (+ (gethash ',name *times*) 
						(- (get-internal-real-time) ,sym2)))
	       (setf (gethash ',name *times*) (- (get-internal-real-time) ,sym2)))
;	   (format t "~%Function call ~a took ~a." ',name (- (get-internal-real-time) ,sym2))
	   ,sym)))))

(defun new-array () (make-array 0 :adjustable t :fill-pointer 0))

(defun to-array (list)
  (let ((result (new-array)))
    (loop for item in list do
	 (vector-push-extend item result))
    result))

(defmacro ignore-redefun (&body code)
  `(locally
       (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
     (handler-bind
	 (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
       ,@code
       )))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\] (get-macro-character #\)))
  (set-macro-character #\[
		       (lambda (stream char)
			 (declare (ignore char))
			 `(run ',(read-delimited-list #\] stream t))))

  (defparameter *commands* nil)
  (defparameter *full-commands* nil)
  (defparameter *command-info* nil)

  (defmacro n-times (n &body body)
    `(loop for i from 1 to ,n collect ,@body))

  (defun with-char (x c)
    (intern (concatenate 'string
			 c
			 (symbol-name x))))
  (defun with-under (x) (with-char x "_"))

  ;; The handler for the commands macro. Creates the *commands* and
  ;; the *commands-info* lists with the correct information.
  (defun real-commands (body)
    (labels ((gen-cmd (kind name args res cmd)
	       (declare (ignore res))
	       (case kind
		 (cmd
		  `(,(with-under name) 
		     ,(mapcar #'cadr args) ,@cmd))
	       ))
	     (get-args (inp)
	       (case (car inp)
		 (cmd
		  (mapcar #'car (caddr inp)))
		 (mak
		  (caddr inp))))
	     (notes-of (x)
	       (loop for i from 4 while (symbolp (nth i x)) collect (nth i x))))
      
      `(defun make-commands ()
	 (setf *full-commands* ',body)
	 (setf *commands*
	       ',(mapcar
		  (lambda (x) 
		    (gen-cmd (car x) (cadr x) (caddr x) (cadddr x) (last x)))
		  body))
	 (setf *command-info*
	       ',(mapcar (lambda (x)
			   (list (cadr x) (get-args x) (cadddr x)
				 (member :messy (notes-of x))))
			 body)))))
  (defmacro commands (&body body)
    (real-commands body)))

;; This is the symbol which list-get returns when the list is empty.
;; It needs to be a fresh symbol so lists can contain nil.
(defvar null-symbol (gensym))

;; Get the nth element of some lazy list, evaluating as required.
;; Returns either the element or null-symbol.
(defun list-get (list n)
  (if (< n (length (car list)))
      (aref (car list) n)
      (progn
	(loop while (and (<= (length (car list)) n) (not (null (cdr list)))) do
	     (funcall (cdr list)))
	(if (< n (length (car list)))
	    (aref (car list) n)
	    null-symbol))))

;; A macro to force the evaluation of a list and do something with the result.
(defmacro with-forced (input output &body run)
  (let ((name (gensym)))
    `(let ((,name ,input))
       (let ((,output (car ,name)))
	 (loop while (not (null (cdr ,name))) do
	      (funcall (cdr ,name)))
	 ,@run))))

;; A macro to create a new list.
;; There are three cases:
;;   - initially: code to run before the iteration.
;;   - next: how to handle each element of the original list.
;;   - finally: code to run after the end of the list.
(defmacro creating-new-listq (&body body)
  `(let ((result (new-array))
	 (continue t))
     ,@(cdr (assoc 'initially body))
     (loop while continue do
	  (if (not (progn ,@(cdr (assoc 'next body))))
	      (setf continue nil)))
     ,@(cdr (assoc 'finally body))
     (list result)))
(defmacro creating-new-list (&body body)
  `(let* ((result (new-array))
  	  (cons-cell (cons result nil)))
     ,@(if (assoc 'initially body)
  	   (cdr (assoc 'initially body)))
     (setf (cdr cons-cell)
	   (let ((stack-copy stack))
	     (lambda ()
	       (let ((argument-restore-stack (list stack-copy)))
		 (if (not (progn ,@(cdr (assoc 'next body))))
		     (progn
		       ,@(if (assoc 'finally body)
			     (cdr (assoc 'finall body)))
		       (setf (cdr cons-cell) nil)))))))
     cons-cell))

;; A macro which creates a new list based off of a previous list.
(defmacro list-to-list-iter-q (list-name &body body)
  `(let ((result (new-array)))
     ,@(cdr (assoc 'initially body))
     (loop for index from 0 for each across (car ,list-name) do
	  ,@(cdr (assoc 'next body)))
     ,@(cdr (assoc 'finally body))
     (list result)))
(defmacro list-to-list-iter (list-name &body body)
  `(let* ((result (new-array))
  	  (index 0)
  	  (cons-cell (cons result nil)))
     ,@(if (assoc 'initially body)
  	   (cdr (assoc 'initially body)))
     (setf (cdr cons-cell)
	   (let ((stack-copy stack))
	     (lambda ()
	       (let ((argument-restore-stack (list stack-copy)))
		 (let ((each (list-get ,list-name index)))
		   (if (eq null-symbol each)
		       (progn
			 ,@(if (assoc 'finally body)
			       (cdr (assoc 'finally body)))
			 (setf (cdr cons-cell) nil))
		       (progn
			 ,@(cdr (assoc 'next body))
			 (incf index))))))))
     cons-cell))

(defmacro save-arguments (&body body)
  `(let ((argument-restore-stack (cons stack argument-restore-stack)))
     ,@body))

(commands
; type
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
  (cmd drop ((type a)) ()
       "Removes the top element of the stack."
       (declare (ignore a)))
  (cmd print () ()
       (progn
	 (print "Stack dump:~%")
	 (loop for el in stack for i from 0 do
	      (format t "   ~a. ~a~%" i el))
	 (format t "~%")))
  (cmd p ((type a)) ()
       (print a))
  (cmd pp () ()
       (print stack))
  (cmd rot ((type a) (type b) (type c)) (type type type)
       "Rotates the top three elements: A B C -> B C A"
       (list b c a))
  (cmd unrot ((type a) (type b) (type c)) (type type type)
       "Inverted rotate of the top three elements: A B C -> C A B"
       (list c a b))
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

; int
  (cmd pick ((int a)) (type)
       (nth a stack))
  (cmd or ((bool a) (bool b)) (bool)
       "Logical or of the top two elements of the stack."
       (or a b))
  (cmd and ((bool a) (bool b)) (bool)
       "Logical and of the top two elements of the stack."
       (and a b))
  (cmd gte ((int a) (int b)) (bool)
       "Checks if the first argument is larger than the second."
       (not (> a b)))
  (cmd gt ((int a) (int b)) (bool)
       "Checks if the first argument is larger than or equal to the second."
       (not (>= a b)))
  (cmd add ((int a) (int b)) (int)
       "Adds the top two elements onf the stack."
       (+ a b))
  (cmd inc ((int a)) (int)
       "Increments the top element of the stack."
       (+ a 1))
  (cmd dec ((int a)) (int)
       "Decrements the top element of the stack."
       (- a 1))
  (cmd multiply ((int a) (int b)) (int)
       "Multiplies the top two elements onf the stack."
       (* a b))
  (cmd subtract ((int a) (int b)) (int)
       "Subtracts from the second-to-top by the top of the stak."
       (- b a))
  (cmd divide ((int a) (int b)) (int)
       "Divides from the second-to-top by the top of the stak."
       (floor (/ b a)))
  (cmd mod ((int a) (int b)) (int)
       "Computes the remainder of the second-to-top when divided by the top of the stak."
       (mod b a))
  (cmd abs ((int a)) (int)
       "Computes the absolute value of the top of the stack."
       (abs a))
  (cmd even ((int a)) (bool)
       "Tests if the top of the stack is an even number."
       (evenp a))
  (cmd odd ((int a)) (bool)
       "Tests if the top of the stack is an odd number."
       (oddp a))
  (cmd gcd ((int a) (int b)) (int)
       (let ((tmp 0))
	 (loop while (and (> a 0) (> b 0)) do
	      (setf tmp a)
	      (setf a (mod b a))
	      (setf b tmp))
	 b))
  (cmd implode ((int count)) (list) :messy
       "Pops the top count elements off the stack and makes a list out of them."
       (let ((a (new-array)))
	 (loop for j from 1 to count do
	      (vector-push-extend (pop stack) a))
	 (list a)))
  (cmd range ((int n)) (list)
       "Generates a list of numbers from 0 (inclusive) to n (exclusive)."
       (let ((arr (make-array n :adjustable t)))
	 (loop for n from 0 to (- n 1) do (setf (aref arr n) n))
	 (list arr)))
  (cmd range-from-1 ((int n)) (list)
       "Generates a list of numbers from 1 (inclusive) to n (exclusive)."
       (let ((arr (make-array n :adjustable t)))
	 (loop for n from 1 to n do (setf (aref arr (1- n)) n))
	 (list arr)))
  (cmd get ((int i) (list l)) (type)
       "Indexes in to a list."
       ; TODO negative index?
       (list-get l i))
  (cmd substr ((int start) (int end) (list l)) (type)
       "Indexes in to a list."
       ; TODO negative index?
       (let ((arr (new-array)))
	 (loop for i from start to (1- end) do
	      (vector-push-extend (list-get l i) arr))
	 (list arr)))
  (cmd combinations ((int size) (list l)) (list)
       (with-forced l list
	 (labels ((combine (lst sz)
		    (if (eq sz 0)
			(list nil)
			(reduce #'append
				(maplist 
				 (lambda (x) 
				   (mapcar (lambda (y) (cons (car x) y))
					   (combine (cdr x) (1- sz))))
				 lst)))))
	   (list (to-array (mapcar (lambda (x) (list (to-array x)))
				   (combine (coerce list 'list) size)))))))

; list
  (cmd explode ((list l)) () :messy
       "Pushes each element of a list on to the stack; the head of the list
        becomes the top of the stack."
       (with-forced l list
	 (loop for x across (reverse list) do
	      (push x stack))))
  (cmd outer ((list a) (list b)) (list)
       "Creates a new list which contains all pairs of elements in the two input lists."
       (list-to-list-iter a
	 (next
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
       "Find the smallest element of a list."
       (with-forced l list
	 (loop for el across list minimize el)))
  (cmd max ((list l)) (int)
       "Find the largest element of a list."
       (with-forced l list
	 (loop for el across list maximize el)))
  (cmd with-index ((list l)) (list)
       "Returns a new list, where each element is a list of the index and list's element."
       (list-to-list-iter l
	 (next
	  (vector-push-extend (list (to-array (list index each))) result))))
  (cmd sort ((list l)) (list)
       "Sorts the elements of a list."
       (with-forced l list
	 (list (sort list #'<))))
  (cmd reverse ((list l)) (list)
       "Reverses a list."
       (with-forced l list
	 (list (reverse list))))
  (cmd first ((list l)) (type)
       "Get the first element of a list."
       (list-get l 0))
  (cmd butfirst ((list l)) (list)
       "Get the first element of a list."
       (list-to-list-iter l
	 (next
	  (if (not (eq index 0))
	      (vector-push-extend each result)))))
  (cmd set-minus ((list takeaway) (list given)) (list)
       (with-forced takeaway forced-takeaway-arr
	 (let ((forced-takeaway (coerce forced-takeaway-arr 'list)))
	   (list-to-list-iter given
	     (next
	      (if (not (member each forced-takeaway))
		  (vector-push-extend each result)))))))
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
  (cmd transpose ((list l)) (list)
       (let ((index 0))
	 (creating-new-list
	   (next
;	    (format t "~%ON OUTER INDEX ~a ~a" index (list-get l index))
;	    (format t "~%       then ~a" (list-get (list-get l index) 0))
	    (when (not (eq (list-get (list-get l 0) index) null-symbol))
	      (let ((jindex 0)
		    (iindex index))
		(vector-push-extend
		 (creating-new-list
		   (next
		    (let ((tmp (list-get l jindex)))
		      (incf jindex)
		      (when (not (eq tmp null-symbol))
			(vector-push-extend (list-get tmp iindex) result)))))
		 result))
	      (incf index))))))
	       
  (cmd length ((list l)) (int)
       (with-forced l list
	 (length list)))
  (cmd concatenate ((list a) (list b)) (list)
       ;; fixmeinfinite
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
       ;; fixmeinfinite
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
       ;; better to not force and present in reversed order?
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
;  (cmd force-1 ((list l)) (list)
;       (if (not (null (cdr l)))
;	   (funcall (cdr l)))
;       l)

; fun
  (cmd call ((fun f)) ()
       "Takes a function off the stack and runs it."
       (save-arguments
	(funcall f 0 nil)))
  (cmd map ((fun fn) (list l)) (list)
       "Maps a function over a list. Each element of the new list is the function applied
        to the old element."
       (list-to-list-iter l
	   (next
	    (vector-push-extend (funcall fn 1 (list each)) result))))
  (cmd filter ((fun fn) (list l)) (list)
       "Returns a new list where only elements where the function returns true are retained."
       (list-to-list-iter l
	 (next
	  (if (funcall fn 1 (list each))
	      (vector-push-extend each result)))))
  (cmd reduce ((fun fn) (list l)) (type)
       (let ((init (list-get l 0)))
	 (with-forced l list
	   (loop for el across list for i from 0 if (not (eq i 0)) do
		(setf init (funcall fn 1 (list el init))))
	   init)))
  (cmd fold ((fun fn) (type init) (list l)) (type)
       (with-forced l list
	 (loop for el across list do
	      (setf init (funcall fn 1 (list el init))))
	 init))
  (cmd uniq-by ((fun fn) (list l)) (list)
       (let ((seen nil))
	 (list-to-list-iter l
	   (next
	    (when (not (some (lambda (x) x) 
			     (mapcar (lambda (x) (funcall fn 1 (list x each))) seen)))
		(vector-push-extend each result)
		(push each seen))))))
  (cmd ite ((fun a) (fun b) (bool case)) ()
       (save-arguments
	(if case
	    (funcall a 0 nil)
	    (funcall b 0 nil))))
  (cmd if ((fun a) (bool case)) ()
       (save-arguments
	(if case
	    (funcall a 0 nil))))
  (cmd do-while ((fun fn)) ()
       (save-arguments
	(let ((cont t))
	  (loop while cont do
	       (setf cont (funcall fn 1 nil))))))
  (cmd call-n-times ((fun fn) (int n)) ()
       "Runs the top function n times."
       (save-arguments
	(loop for i from 0 to (1- n) do (funcall fn 0 nil)))))


;; These are the stacks we're going to work with. They need to be
;; dynamically scoped so that lisp doesn't complain when we (eval) code.
(defvar stack nil)
(defvar restore-stack nil)
(defvar argument-restore-stack nil)

;; Compute the type of an element that is on the stack.
(defun typeof (el)
  (cond
    ((numberp el) 'int)
    ((eq (type-of el) 'boolean) 'bool)
    ((eq (type-of el) 'null) 'bool)
    ((listp el) 'list)
    ((vectorp el) 'list)
    ((functionp el) 'fun)
    ((symbolp el) 'builtin)
    (t (error `(this is very bad ,el)))))

;; Returns true iff a is a subset of the type of b.
;; For example, (int int) is a subset of (type int), but
;; (int type) is not a subset of (int int).
(defun is-subset-of (a b)
  (cond
    ((null a) t)
    ((null b) t)
    ((or (equal (car a) 'abort) (equal (car b) 'abort)) nil)
    ((or (equal (car b) 'type) (equal (car a) (car b)))
     (is-subset-of (cdr a) (cdr b)))
    (t nil)))

(defun is-prefix (shorter longer)
  (is-subset-of (subseq longer 0 (length longer)) shorter))

;; Turns the tagged input program to a set of defuns referencing each other.
;; This is only useful compilation running pass; not for the actual running pass,
;; since when we are running we don't know the tree structure ahead of time.
(defun parse-and-split (program)
  (let ((defuns '())
	(fnid 0))
    (labels ((helper (input)
	       (incf fnid)
	       (let ((myid (intern (concatenate 'string "FN-" (write-to-string fnid)))))
		 (push
		  (list
		   myid
		   (mapcar
		    (lambda (cmd)
		      (if (eq (car cmd) 'fun)
			  (list 'fun (helper cmd))
			  cmd))
		    (cadr input)))
		  defuns)
		 myid)))
      (helper program)
      defuns)))

;; When we need a fresh function name, we use this.
(defvar *fun-count* 0)
(defun fresh-fun-name ()
  (intern (concatenate 'string "DYN-FN-" (write-to-string (incf *fun-count*)))))

;; Create a set of lisp defuns that actually let the code run. Each defun
;; initially just uncompresses the code it contains, and then from then on
;; when called actually executes the code.
(defun create-defuns (defuns &optional (block-kinds nil))
  (mapcar
   (lambda (input)
     (let* ((fnid (car input))
	    (body (cadr input))
	    (more-block-kinds nil))
       (loop while (eq (caar body) 'fun-modifier) do
	    (push (cadr (car body)) more-block-kinds)
	    (setf body (cdr body)))
       `(let ((first t))
	  (labels ((keep-only (commands body)
		     (let ((refed (referenced-commands body)))
		       (remove-if-not 
			(lambda (x)
			  (member (car x) refed))
			commands)))
		   (referenced-commands (body)
		     (if (listp body)
			 (cons (car body) (reduce #'append 
						  (mapcar #'referenced-commands 
							  (cdr body)))))))
	    (ignore-redefun
	      (defun ,fnid (nret args)
		(if first
		    (let* ((fn-body (uncompress-body ',fnid ',(append block-kinds more-block-kinds)
						     ',body (append args stack) #'decompress))
			   (fn-lambda (eval (list 'labels (keep-only *commands* fn-body) fn-body))))
		      (setf first nil)
		      (setf (symbol-function ',fnid) fn-lambda)
		      (setf (symbol-function ',(with-under fnid)) fn-lambda)
		      (funcall fn-lambda nret args))
		    (funcall #',(with-under fnid) nret args))))))))
   defuns))

;; A generic decompressor which does nothing.
(defun decompress (input types fnid) 
  (declare (ignore input types fnid))
  (assert nil))

;; When we're doing the compilation run, this is where we put all of the
;; actual mapping of uncompressed code -> compressed code.
(defvar *compiled-code* nil)

;; Creates an encoding of the uncompressed command to be used to compress it later.
;; Uses the stack information to figure out how to encode it.
(defun do-encode (real-command types)
  (let ((possible-commands 
	 (mapcar #'car 
		 (remove-if-not 
		  (lambda (x) (is-subset-of types (cadr x))) *command-info*))))
;    (format t "~%TAKING ~a BITS" (log (length possible-commands)))
;    (format t "~%LOOKUP ~a ~a " real-command possible-commands)
    (list (cons (position real-command possible-commands) (length possible-commands)))))

;; A "decompressor" used in compilation run. 
;; It turns commands in to commands, but first inspects the stack  and uses this
;; information to encode the commands.
(defun commands-to-commands (input types fnid)
  (let ((new-name (intern (concatenate 'string
				       (symbol-name fnid) "P"))))
    (if (eq (caar input) 'builtin)
	(progn
	  (push (append
		 (car (last input))
		 (do-encode (cadr (car input)) types))
		*compiled-code*)))
    (incf (caddr (car (last input))))
    (if (cddr input)
	(cons (list (car input)
		    `(fun ,new-name)
		    '(builtin call))
	      (car (create-defuns
		    (list (list new-name (cons '(fun-modifier *no-arguments)
					       (cdr input)))))))
	(list (list (car input))))))

;; An actual decoder. It returns the command to actually run given stack information
;; and the value of the command.
(defun do-decode (command-abbrv types)
  (if (member 'type types) nil
      (let* ((possible-commands 
	      (mapcar #'car 
		      (remove-if-not 
		       (lambda (x) (is-subset-of types (cadr x))) *command-info*))))
;	(format t "~%Decoded ~a to ~a" command-abbrv (nth (car command-abbrv) possible-commands))
	(nth (car command-abbrv) possible-commands))))

;; Given the big blob of compressed data, extracts the next single token.
;; The blob of compressed data can be represented any way, as long as it is tagged
;; with 'decode-me. A precondition of this is that there actually exists more data.
(defun extract-next-token (input types)
  (format t "~%INPUT TO NEXT TOKEN ~a~%" input)
  (let* ((not-yet-decoded (cdar (last input)))
	 (possible-commands
	      (mapcar #'car 
		      (remove-if-not 
		       (lambda (x) (is-subset-of types (cadr x))) *command-info*)))
	 (count (length possible-commands))
	 (weights `(,@(loop for i from 0 to 255 collect `((compressed-function ,i) ,(/ count 256)))
		    ,@(loop for i from 0 to 9 collect `((int ,i) ,(/ count 10)))
		    ,@(loop for i from 0 to (1- count) 
			 collect `((builtin (,i . ,count)) 7))
		    ((eof) ,count)))
	 (next-block (arithmetic-decode (car not-yet-decoded) weights))
	 (next-input (car next-block))
	 (next-token (cdr next-block)))
    (format t "~% nt ~a ni ~a" next-token next-input)
    (print not-yet-decoded)
    (case (car next-token)
      (compressed-function
       (let ((resulting-bits nil))
	 (loop for i from 1 to (cadr next-token) do
	      (let* ((both-parts (arithmetic-decode next-input '((0 1) (1 1))))
		     (input-part (car both-parts))
		     (bit-part (cdr both-parts)))
		(setf next-input input-part)
		(push bit-part resulting-bits)))
	 (setf (car not-yet-decoded) next-input)
	 (format t "~%BITS ARE ~a~%" (reverse resulting-bits))
	 (print next-input)
	 (list 'fun-as-list 
	       (list (list 'decode-me 
;			  (cdr (loop for i from 1 to (cadr next-token) collect 
;				    (pop (car not-yet-decoded))))))
			   (arithmetic-decode-preprocess (reverse resulting-bits))))
;	      (caddr next-token))
	     nil)))
;       (pop not-yet-decoded))
      (builtin
       (let ((cmd (do-decode (cadr next-token) types)))
	 (when cmd
	   (setf (car not-yet-decoded) next-input)
;	   (pop (car not-yet-decoded))
	   (list 'builtin cmd))))
      (eof
       (format t "IT IS EOF")
;       (assert nil)
       nil)
      (otherwise
       (setf (car not-yet-decoded) next-input)
       next-token))))

;; Checks if there is actually any remaining work to do, and if there is
;; returns a 'decode-me containing it.
(defun remaining-work (input)
  (format t "~%TESTREM~a" (cadr (car (last input))))
   (when (equalp 'x (arithmetic-decode (cadr (car (last input))) '((x 9) (end 1))))
;    (format t "We have some work left to do ~a.~%" input)
    input))

;; Decodes a blob of compressed data to some commands by lookking at the stack.
;; Uses very basic static analysis to decode as many commands at once as possible.
;; It stops running the analysis on several cases:
;;   - A command can return many different types of arguments (e.g., a list get).
;;   - A function was just used which operations on a non-stack-restoring block.
;;   - A builtin was called that is known to mess up the stack (e.g., explode).
;; If it can't convert the compressed input to decompressed data all at once, it
;; pushes on to the stack a continuation representing the rest of the work to do,
;; and then pushes the "call" builtin.
(defun compressed-to-commands (input types fnid)
  (let ((commands nil)
	(cont t)
	(defun-part nil))
    (loop while cont do
	 (let ((cmd 
		(if (not (member 'abort types)) 
		    (extract-next-token input types))))
	   (if cmd
	       (case (car cmd)
		 (int
		  (push 'int types)
		  (push cmd commands))
		 (fun
		  (push 'fun types)
		  (push cmd commands))
		 (fun-as-list
		  (push 'fun types)
		  (push cmd commands))
		 (list
		  (push 'list types)
		  (push cmd commands))
		 (builtin
		  (let* ((decoded (cadr cmd))
			 (full (assoc decoded *command-info*))
			 (args (cadr full))
			 (results (caddr full)))
		  (case (cadr cmd)
		    (dup
		     (push (car types) types)
		     (push '(builtin dup) commands))
		    (swap
		     (let ((a (pop types)) (b (pop types)))
		       (push a types)
		       (push b types))
		     (push '(builtin swap) commands))
		    (otherwise
		     (if (or (member 'fun (loop for i from 1 to (length args) collect (pop types)))
			     (nth 3 full))
			 (push 'abort types)
			 (setf types (append results types)))
		     (push (list 'builtin decoded) commands))))))
	       (if (remaining-work input)
		 (let ((new-name (intern (concatenate 'string 
						      (symbol-name fnid) "P"))))
		   (setf cont nil)
		   (push (list 'fun new-name) commands)
		   (push '(builtin call) commands)
		   (setf defun-part 
			 (car 
			  (create-defuns
			   (list (list new-name
				       (remaining-work input)))
			   '(*no-arguments)))))
		   (setf cont nil)))))
    (cons (reverse commands)
	  defun-part)))

;; Convert the IR to some actual lisp code.
;; The input is a flat list of tagged data, and the output is a pair:
;;   - car: a list of lisp code that can be placed in a progn block
;;   - cdr: a list of new functions to define before running the body
(defun ir-to-lisp (input)
  (let ((defuns nil))
    (cons
     (mapcar
      (lambda (decoded)
	(case (car decoded)
	  (int
	   `(push ,(cadr decoded) stack))
	  (fun
	   `(push #',(cadr decoded) stack))
	  (fun-as-list
	   (let ((id (fresh-fun-name)))
	     (push
	      (car (create-defuns (list (cons id (list (cadr decoded)))) (caddr decoded))) 
	      defuns)
	     `(push #',id stack)))
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
      input)
     defuns)))


;; Returns a lambda which runs the full program given by the input.
;; We may need to define some new functions either because ir-to-lisp
;; told us to, or because compressed-to-commands told us to.
;; Before we do anything, we check if the block is a special type which
;; modifies the stack. If it is, we make the operation.
(defun uncompress-body (fnid block-kinds input stack decompressor)
;  (if (not (eq fnid 'fn-1)) (push '*restoring block-kinds))
;  (format t "DO I DO IT? ~a!!!" (member '*exploding block-kinds))
  (if (member '*exploding block-kinds)
      (with-forced (pop stack) list 
	(loop for x across (reverse list) do
	     (push x stack))))

;  (format t "~%STAK IS NOW ~a" stack)
;  (format t "KIND IS ~a" block-kinds)

  (let* ((types (mapcar #'typeof stack))
	 (uncompressed-code (funcall decompressor input types fnid))
	 (defun-part (cdr uncompressed-code))
	 (ir-part (ir-to-lisp (car uncompressed-code)))
	 (lambda-part (car ir-part))
	 (defun-part-2 (cdr ir-part))
	 (final-part
	  `(lambda (nret args)
	     ,@(if (member '*restoring block-kinds)
		   '((push stack restore-stack)))
	     (setq stack (append args stack))
	     ,@(if (member '*exploding block-kinds)
		   '((with-forced (pop stack) list 
		       (loop for x across (reverse list) do
			    (push x stack)))))
	     ,@(if (not (member '*no-arguments block-kinds))
		   `((push (append 
			    ,(if (member '*exploding block-kinds)
				 `(coerce (with-forced (car args) list list) 'list)
				 `args)
			    (car argument-restore-stack)) argument-restore-stack))
		   `((pop argument-restore-stack)))

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
	(if defun-part-2
	    `(progn
	       ,defun-part
	       ,@defun-part-2
	       ,final-part)
	    `(progn
	       ,defun-part
	       ,final-part))
	(if defun-part-2
	    `(progn
	       ,@defun-part-2
	       ,final-part)
	    final-part))))

;; Returns the full expanded lisp code which runs some commands.
(defun run-compiled (commands)
  (setf stack nil)
  (setf restore-stack nil)
  (setf argument-restore-stack nil)
  (make-commands)
  `(let ((stack '())
	 (argument-restore-stack '())
	 (restore-stack '()))
     ,(cons 'progn (create-defuns (parse-and-split commands)))
     (funcall #'fn-1 0 nil)
     stack))

;; Turn a nested list of commands in to a set of bytes which are very
;; dense, yet still represent the same information.
;;   1. Flatten out the data.
(defun list-to-bytes (list)
  (format t "~%AAAAAA ~a" list)
  (labels ((prepare (function-tree)
	     (let ((modifiers 
		    (loop while (eq (car (car function-tree)) 'fun-modifier)
		       collect (cadr (pop function-tree)))))
	       (append
		(reduce #'append
			(mapcar
			 (lambda (x)
			   (if (eq (car x) 'fun)
			       (let ((encoded (arithmetic-encode-body (cadr x))))
				 `((compressed-function ,(length encoded))
				   ,@(mapcar (lambda (x) `(single-bit ,x)) encoded)))
			       (list x)))
			 function-tree))
		(list '(eof)))))
;		     ,@(reduce #'append (mapcar #'make-flat (cadr tree)))
;		     (fun-end)))
	   (arithmetic-encode-body (function)
	     (format t "~%QQQQ~a" function)
	     (let* ((flat-function (prepare function)))
	       (format t "~%BBBBBB ~a~%" flat-function)
	       (arithmetic-encode
		flat-function
		(append
		 (loop for elt in flat-function collect
		      (case (car elt)
			(int
			 `((funthing 10) 
			   ,@(loop for i from 0 to 9 collect `((int ,i) 1)) 
			   (rest 70)
			   ((eof) 10)))
			(compressed-function
			 `(,@(loop for i from 0 to 255 collect
				  `((compressed-function ,i) 1))
			     (other-stuff ,(* 256 9))))
			(single-bit
			 `(((single-bit 0) 1) ((single-bit 1) 1)))
			(builtin
			 (let ((count (cdadr elt)))
			   `((functhing ,count) (integers ,count)
			     ,@(loop for i from 0 to (1- count) collect
				    `((builtin (,i . ,count)) 7))
			     ((eof) ,count))))
			(eof
			 `(((stuff) 9) ((eof) 1))))))))))

      (arithmetic-encode-body (cadr list))))


;; From the compressed bytes, return a list of how to run them.
(defun bytes-to-list (bytes)
  (let* ((encoded (arithmetic-decode-preprocess bytes)))
;	 (first-block (arithmetic-decode encoded '(((fun-start ()) 1/2) (fun-end 1/2) (rest 9))))
;	 (encoded-rest (car first-block))
;	 (decoded (cdr first-block)))
;    (print decoded)
;    (assert (equalp decoded '(fun-start ())))
    (list 'fun (list (list 'decode-me encoded)))))

;; Converts the high-level language to commands tagged by their type.
(defun add-types-to-user-input (input)
  (if (listp input)
      (list 'fun (mapcar #'add-types-to-user-input input))
      (if (and (symbolp input) 
	       (eq (char (symbol-name input) 0) #\*))
	  (list 'fun-modifier input)
	  (list (typeof input) input))))

;; Compiles some input to the dense form.
;; Compilation requires actually running the program so we can inspect,
;; at run time, the types on the stack. This allows us to get much better
;; compression of the builtins.
;; Also returns the answer we got from here, to make sure it's the same as
;; the answer we get when we actually run the program.
(defun compile-by-running (input)
  (let ((id 0))
    (labels ((tag-input (in)
	       (if (eq (car in) 'fun)
		   (list 'fun (append (mapcar #'tag-input (cadr in))
				      (list (list 'compile-tag (incf id) 0))))
		   in))
	     (do-replace (tagged replace-with)
	       (if (eq (car tagged) 'fun)
		   (let ((kind (butlast (car (last (cadr tagged)))))
			 (new-tagged (list 'fun
					   (mapcar (lambda (x) (do-replace x replace-with)) 
						   (cadr tagged))))
			 (skip 0))
		     (loop for elt in (cadr new-tagged) while
			  (eq (car elt) 'fun-modifier) do
			  (incf skip))

		     (loop for elt in replace-with if (equalp (subseq elt 0 2) kind) do
			  (setf (cadr (nth (+ (caddr elt) skip) (cadr new-tagged)))
				(cadddr elt)))
		     new-tagged)
		   tagged))
	     (drop-last (tree)
	       (if (eq (car tree) 'fun)
		   (list 'fun (mapcar #'drop-last (butlast (cadr tree))))
		   tree)))
      (let* ((with-types (add-types-to-user-input input))
	     (tagged (tag-input with-types))	     
	     (*compiled-code* nil))
	(setf (symbol-function 'decompress) #'commands-to-commands)
	(let ((answer (mapcar #'repl-print (eval (run-compiled tagged)))))
	  (cons answer (list-to-bytes (drop-last (do-replace tagged *compiled-code*)))))))))

(defun runq (i)
  (print (compile-by-running i)))

;; Run the program by first compiling it, then running it.
(defun run (i) 
  (format t "~%Run the program ~a" i)
  (let* ((answer-and-compiled (compile-by-running i))
	 (answer (car answer-and-compiled))
	 (compiled (cdr answer-and-compiled))
	 (*fun-count* 0))
    (format t "~%The compiled version is ~a of size ~a ~a~%" compiled (length compiled) 
	    (/ (length compiled) 8.0))
;    (print answer)
    (setf (symbol-function 'decompress) #'compressed-to-commands)
    (let ((run-answer (mapcar #'repl-print (time (eval (run-compiled (bytes-to-list compiled)))))))
      (format t "~%Ans1: ~a;~%Ans2: ~a" answer run-answer)
      (assert (equalp answer run-answer))
      run-answer)))

(defun repl-print (x)
  (case (typeof x)
    (int x)
    (bool 
     (if x 'true 'false))
    (fun 'fun)
    (list
;     (print x)
     (with-forced x list
;       (print (aref list 0))
       (loop for i across list collect (repl-print i))))))

;; (defun golf-print (stack mode)
;;   (if (eq mode 'smart)
;;       (cond
;; 	((typeof (car stack) 'list)
;; 	 (top-list-by-newlines))
;; 	((typeof (car stack) 'int)
;; 	 ; implode until no longer an int
;; 	 (push (loop for while (and stack (eq (typeof (car stack)) 'int) )
;; 		  collect (pop stack)) stack)
;; 	 (top-list-by-spaces)))))

;; (defun read-string (string mode)
;;   (if (eq mode 'smart)
;;       ;; try spliting by newlines if possible. 
;;       (let* ((strs (split-by-newlines))
;; 	     (mapcar #'split-by-spaces strs))
;; 	)))
	
      
;(run '(5 range (1 add) map))  
(run '((5) 6))


;(with-open-file (stream "/tmp/pipe")
;    (loop for line = (read-line stream nil) until (null line) do
;	  (compile-by-running (read-from-string (concatenate 'string "(" line " pp)")))))
;	  (print line)))
;	  (if (and (not (eq line :end)) (not (equalp line "")))
;	      (let ((to-run (read-from-string line)))
;		(print to-run)))))

;(time
;[5 range prefixes force 5 range suffixes force zip (*exploding concatenate) map (sum) map force])

;(time
;[2 range (1 subtract) map force (*exploding add) call])


;(defun run (text)
;  (eval (run-compiled (compile-by-running text))))

;(time (loop for i across (caar (run '(8 range permutations (with-index force dup (*exploding *restoring unrot (*exploding *restoring arg-c arg-a eq arg-c arg-a subtract abs arg-d arg-b subtract abs neq or) map force all) map force all) map force))) count i))

;(run '(4 range permutations (with-index force dup (*exploding *restoring unrot (*exploding *restoring arg-c arg-a eq arg-c arg-a subtract abs arg-d arg-b subtract abs neq or) map force all) map force all) map force))

;(time
;  [8 range permutations (with-index dup outer flatten (flatten) map (*exploding *restoring arg-c eq arg-c arg-a subtract abs arg-d arg-b subtract abs neq or) map all) filter length])

;(time [8 range permutations (with-index dup outer flatten (flatten) map (*exploding *restoring arg-c eq arg-c arg-a subtract abs arg-d arg-b subtract abs neq or) map all) filter length])
;(time
; [5 range 0 (add) fold])

;(time
; [3 (2 (4) call-n-times) call-n-times])

; d c abs(a-c) 0=abs(a-c) arg-b

;(run '(5 range (1 add) map force))


;[2 range (*exploding add) call]

;(progn
;  (time (run '(8 range permutations (with-index force dup (*exploding *restoring) map force) map force)))   nil)

;(progn
;  [1 dup range permutations (with-index (*exploding add arg-a arg-b subtract 2 implode) map transpose (*restoring uniq length eq) map all) filter length])

;(time
; [5 dup range permutations (with-index dup (*exploding add) map uniq length arg-b eq swap (*exploding subtract) map uniq length arg-b eq and) filter force length])


;(let* ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)) 
;       (stream (sb-bsd-sockets:socket-make-stream socket :input t :output t))) 
;  (sockint::bind (sb-bsd-sockets:socket-file-descriptor socket) "/tmp/q" 5))

;; (with-open-file (stream "/tmp/pipe")
;;   (let ((result nil))
;;     (loop while t do
;; 	 (let ((chr (read-char-no-hang stream)))
;; ;	   (print chr)
;; 	   (when chr
;; 	     (if (eq chr #\~)
;; 		 (when (remove-if (lambda (x) (or (eq x #\ ) (eq x #\Newline))) result)
;; 		   (print (read-from-string (concatenate 'string (reverse result) nil)))
;; 		   (setf result nil))
;; 		 (push chr result)))))))


;;[(dup 3 mod subtract dup 3 add swap) #((#(0 0 4 0 0 0 0 0 8)) (#(0 9 8 3 0 0 7 0 0)) (#(5 1 0 7 0 9 0 0 4)) (#(0 0 0 5 0 2 0 0 0)) (#(0 5 0 0 0 0 0 6 0)) (#(4 0 0 6 0 1 0 0 7)) (#(7 0 0 4 0 6 0 8 2)) (#(0 0 5 9 0 0 3 4 0)) (#(8 0 0 0 0 0 9 0 0))) 9 range dup outer flatten (*restoring *exploding drop drop arg-a get arg-c transpose arg-b get concatenate arg-b arg-d call arg-c (*restoring rot substr) map force arg-a arg-d call substr flatten rot drop drop concatenate uniq (*restoring 0 neq) filter length) map force]

;(run '(5 range dup zip dup (*exploding *restoring arg-a arg-a add) map 1 swap force))

; 
;; ..4.....8
;; .983..7..
;; 51.7.9..4
;; ...5.2...
;; .5.....6.
;; 4..6.1..7
;; 7..4.6.82
;; ..59..34.
;; 8.....9..
