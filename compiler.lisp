(load "arithmetic-encoder.lisp")

(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)


(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defstruct (type-list 
	     (:conc-name "TYPE-LIST-"))
  (array nil)
  (kind 'list)
  (generator nil))

(defun new-array () (make-array 20 :adjustable t :fill-pointer 0))

(defun to-array (list)
  (if (stringp list)
    (make-type-list :array (map 'vector #'char-code list) :kind 'string)
    (make-type-list :array (coerce list 'vector))))

(defun as-string (list)
  (setf (type-list-kind list) 'string)
  list)

(defun to-string (list)
  (with-forced list _
    (if (numberp (aref (type-list-array list) 0)) ;;;xxfixme
	(map 'string #'code-char (type-list-array list))
	(format nil "~{~A~^~%~}"
		(loop for e across (type-list-array list) 
		   collect (to-string e))))))

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

  (defstruct (command-info
	       (:conc-name "COMMAND-"))
    (name nil)
    (bindings nil)
    (args nil)
    (res nil)
    (notes nil))
  (defun find-command-by-name (name infos)
    (loop for el in infos if (eq (command-name el) name) return el))

  (defmacro n-times (n &body body)
    `(loop for i from 1 to ,n collect ,@body))

  (defun with-char (x c)
    (intern (concatenate 'string
			 c
			 (symbol-name x))))
  (defun with-under (x) (with-char x "_"))
  (defun without-under (x)
    (intern (subseq (symbol-name x) 1)))

  ;; The handler for the commands macro. Creates the *commands* and
  ;; the *commands-info* lists with the correct information.
  (defun real-commands (body)
    (labels ((gen-cmd (kind name bindings args res cmd)
	       (declare (ignore bindings res))
	       (case kind
		 (cmd
		  (list (with-under name) 
			(mapcar #'cadr args)
			`(progn
;			   (format t "~%i am running ~a with args ~a and ~a~%"
;				   ',name
;				   ',(loop for el in args collect (car el))
;				   ,(cons 'list (loop for el in args collect (cadr el))))
			   ,@(loop for el in args when (eq (car el) 'fun) collect
				  `(setf ,(cadr el) (fun-on-stack-fun ,(cadr el))))
			   (let ((res ,@cmd))
;			     (format t "and returning ~a~%" res)
			     res))))
	       ))
	     (get-args (inp)
	       (case (car inp)
		 (cmd
		  (mapcar #'car (fourth inp)))
		 (mak
		  (caddr inp))))
	     (notes-of (x)
	       (loop for i from 5 while (and (symbolp (nth i x)) (not (null (nth i x))))
		  collect (nth i x)))
	     (repl-it (x)
	       (let ((expand '((fun (either fun-nonrestoring fun-restoring))
			       (anylist (either list string)))))
		 (if (listp x)
		     (mapcar #'repl-it x)
		     (if (assoc x expand) (second (assoc x expand)) x)))))
      
      `(defun make-commands ()
	 (setf *full-commands* ',body)
	 (setf *commands*
	       ',(mapcar
		  (lambda (x) 
		    (gen-cmd (first x) (second x) (third x) (fourth x) (fifth x) (last x)))
		  body))
	 (setf *command-info*
	       ,(cons 'list
		      (mapcar (lambda (x)
				`(make-command-info :name ',(second x)
						    :bindings ',(repl-it (third x))
						    :args ',(repl-it (get-args x))
						    :res ',(fifth x)
						    :notes ',(member :messy (notes-of x))))
			      body))))))
  (defmacro commands (&body body)
    (real-commands body)))

;; This is the symbol which list-get returns when the list is empty.
;; It needs to be a fresh symbol so lists can contain nil.
(defvar null-symbol (gensym))

;; Get the nth element of some lazy list, evaluating as required.
;; Returns either the element or null-symbol.
(defun list-get (list n)
  (if (< n (length (type-list-array list)))
      (aref (type-list-array list) n)
      (progn
	(loop while (and (<= (length (type-list-array list)) n) 
			 (not (null (type-list-generator list)))) do
	     (funcall (type-list-generator list)))
	(if (< n (length (type-list-array list)))
	    (aref (type-list-array list) n)
	    null-symbol))))

;; A macro to force the evaluation of a list and do something with the result.
(defmacro with-forced (input output &body run)
  (let ((name (gensym)))
    (if (eq output '_)
	`(let ((,name ,input))
	   (loop while (not (null (type-list-generator ,name))) do
		(funcall (type-list-generator ,name)))
	   ,@run)
	`(let ((,name ,input))
	   (let ((,output (type-list-array ,name)))
	     (loop while (not (null (type-list-generator ,name))) do
		  (funcall (type-list-generator ,name)))
	     ,@run)))))

(defmacro save-arguments (&body body)
  `(let ((state (make-state :base-stack stack)))
     ,@body))


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
  	  (the-type-list (make-type-list :array result)))
     ,@(if (assoc 'initially body)
  	   (cdr (assoc 'initially body)))
     (setf (type-list-generator the-type-list)
	   (lambda ()
	     (if (not (progn ,@(cdr (assoc 'next body))))
		 (progn
		   ,@(if (assoc 'finally body)
			 (cdr (assoc 'finall body)))
		   (setf (type-list-generator the-type-list) nil)))))
     the-type-list))

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
  	  (the-type-list (make-type-list :array result)))
     ,@(if (assoc 'initially body)
  	   (cdr (assoc 'initially body)))
     (setf (type-list-generator the-type-list)
;	   (let ((stack-copy stack))
;	   (save-arguments
	     (lambda ()
;	       (let ((argument-restore-stack (list stack-copy)))
;		 (format t "Set ARS 2 ~A~%" argument-restore-stack)
		 (let ((each (list-get ,list-name index)))
		   (if (eq null-symbol each)
		       (progn
			 ,@(if (assoc 'finally body)
			       (cdr (assoc 'finally body)))
			 (setf (type-list-generator the-type-list) nil))
		       (progn
			 ,@(cdr (assoc 'next body))
			 (incf index))))))
     the-type-list))

(commands
; type
  (cmd unsure () () () :messy
       "does nothing"
       42)
  (cmd dup ((:a type)) ((:a other)) (:a :a)
       "Duplicates the top element of the stack."
       (list other other))
  (cmd dup-top-two ((:a type) (:b type)) ((:a other1) (:b other2)) (:a :b :a :b)
       "Duplicates the top two elements of the stack."
       (list other1 other2 other1 other2))
  (cmd swap ((:a type) (:b type)) ((:a a) (:b b)) (:b :a)
       "Swap the order of the top two elements on the stack."
       (list b a))
  (cmd eq () ((type a) (type b)) (bool)
       "Compares the top two elements of the stack for equality."
       (equalp a b))
  (cmd neq () ((type a) (type b)) (bool)
       "Compares the top two elements of the stack for inequality."
       (not (equalp a b)))
  (cmd drop () ((type q)) ()
       "Removes the top element of the stack."
       ;(declare (ignore a)))
       q)
  (cmd print () () ()
       "Pretty-print the enture stack; usefully only for debugging."
       (progn
	 (format t "Stack dump:~%")
	 (loop for el in stack for i from 0 do
	      (format t "   ~a. ~a~%" i el))
	 (format t "~%")))
  (cmd rot ((:a type) (:b type) (:c type)) ((:a a) (:b b) (:c c)) (:b :c :a)
;  (cmd rot () ((type a) (type b) (type c)) (type type type)
       "Rotates the top three elements: A B C -> B C A"
       (list b c a))
  (cmd unrot ((:a type) (:b type) (:c type)) ((:a a) (:b b) (:c c)) (:c :a :b)
;  (cmd unrot () ((type a) (type b) (type c)) (type type type)
       "Inverted rotate of the top three elements: A B C -> C A B"
       (list c a b))
  (cmd arg-a () () (type)
       "Pushes the top element of the stack, at the time of the last
        context establishment, to the stack."
       (car (car argument-restore-stack)))
  (cmd arg-b () () (type)
       "Pushes the second to top element of the stack, at the time of the last
        context establishment, to the stack."
       (cadr (car argument-restore-stack)))
  (cmd arg-c () () (type)
       "Pushes the third to top element of the stack, at the time of the last
        context establishment, to the stack."
       (caddr (car argument-restore-stack)))
  (cmd arg-d () () (type)
       "Pushes the forth to top element of the stack, at the time of the last
        context establishment, to the stack."
       (cadddr (car argument-restore-stack)))
;  (cmd pair () ((type a) (type b)) (type)
;       (list (to-array (list a b))))
  (cmd forever () ((type arg)) (list)
       "Create an infinite list containing a single element."
       (creating-new-list
	 (next
	  (vector-push-extend arg result))))
  (cmd naturals () () (list)
       "Return a sequence of all the natural numbers (0, 1, 2, 3...)."
       (let ((i 0))
	 (creating-new-list
	   (next
	    (vector-push-extend i result)
	    (incf i)))))
  (cmd box () ((type arg)) (list)
       "Place an element in a list containing only itself."
       (creating-new-list
	 (initially
	  (vector-push-extend arg result))))
  (cmd cons () ((type arg) (list l)) (list)
       "Add an item to the front of a list."
       (list-to-list-iter l
	 (initially
	  (vector-push-extend arg result))
	 (next
	  (vector-push-extend each result))))
  (cmd member () ((type arg) (list l)) (bool)
       "Test if an item is a member of a list."
       (with-forced l list
	 (loop for el across list do
	      (if (equalp el arg)
		  (return t)))))
  (cmd index-of () ((type arg) (list l)) (int)
       "Find the index of an item in a list, or -1 if not present."
       (with-forced l list
	 (block out
	   (loop for el across list for count from 0 do
		(if (equalp el arg)
		    (return-from out count)))
	   -1)))
  
; int
  (cmd pick () ((int n)) (type)
       "Take the nth element of the stack and duplicate it on the top of the stack."
       (nth n stack))
  (cmd or () ((bool a) (bool b)) (bool)
       "Logical or of the top two elements of the stack."
       (or a b))
  (cmd not () ((bool a)) (bool)
       "Logical or of the top two elements of the stack."
       (not a))
  (cmd and () ((bool a) (bool b)) (bool)
       "Logical and of the top two elements of the stack."
       (and a b))
  (cmd gte () ((int a) (int b)) (bool)
       "Checks if the first argument is larger than the second."
       (not (> a b)))
  (cmd gt () ((int a) (int b)) (bool)
       "Checks if the first argument is larger than or equal to the second."
       (not (>= a b)))
  (cmd add () ((int a) (int b)) (int)
       "Adds the top two elements onf the stack."
       (+ a b))
  (cmd negate () ((int a)) (int)
       "Negate the top element of the stack."
       (- 0 a))
  (cmd inc () ((int a)) (int)
       "Increments the top element of the stack."
       (+ a 1))
  (cmd dec () ((int a)) (int)
       "Decrements the top element of the stack."
       (- a 1))
  (cmd multiply () ((int a) (int b)) (int)
       "Multiplies the top two elements onf the stack."
       (* a b))
  (cmd subtract () ((int a) (int b)) (int)
       "Subtracts from the second-to-top by the top of the stak."
       (- b a))
  (cmd swapsubtract () ((int a) (int b)) (int)
       "Subtracts from the top by the second-to-top of the stak."
       (- a b))
  (cmd divide () ((int a) (int b)) (int)
       "Divides from the second-to-top by the top of the stak."
       (floor (/ b a)))
  (cmd swapdivide () ((int a) (int b)) (int)
       "Divides from the top by the second-to-top of the stak."
       (floor ( a b)))
  (cmd pow () ((int a) (int b)) (int)
       "Multiplies the top two elements onf the stack."
       (expt b a))
  (cmd mod () ((int a) (int b)) (int)
       "Computes the remainder of the second-to-top when divided by the top of the stak."
       (mod b a))
  (cmd abs () ((int a)) (int)
       "Computes the absolute value of the top of the stack."
       (abs a))
  (cmd zero () ((int a)) (bool)
       "Test if a number is zero."
       (= a 0))
  (cmd even () ((int a)) (bool)
       "Tests if the top of the stack is an even number."
       (evenp a))
  (cmd odd () ((int a)) (bool)
       "Tests if the top of the stack is an odd number."
       (oddp a))
  (cmd gcd () ((int a) (int b)) (int)
       "Compute the greatest common divisor of two integers."
       (let ((tmp 0))
	 (loop while (and (> a 0) (> b 0)) do
	      (setf tmp a)
	      (setf a (mod b a))
	      (setf b tmp))
	 b))
  (cmd take ((:a anylist)) ((int a) (:a l)) (:a)
       "Take the first n elements of a list."
       (let ((index 0))
	 (creating-new-list
	   (next
	    (if (<= (incf index) a)
		(vector-push-extend (list-get l (1- index)) result))))))
  (cmd implode () ((int count)) (list) :messy
       "Pops the top count elements off the stack and makes a list out of them."
       (let ((a (new-array)))
	 (loop for j from 1 to count do
	      (vector-push-extend (pop stack) a))
	 (make-type-list :array a)))
  (cmd range () ((int n)) (list)
       "Generates a list of numbers from 0 (inclusive) to n (exclusive)."
       (let ((arr (make-array n :adjustable t)))
	 (loop for n from 0 to (- n 1) do (setf (aref arr n) n))
	 (make-type-list :array arr)))
  (cmd range-from-1 () ((int n)) (list)
       "Generates a list of numbers from 1 (inclusive) to n (exclusive)."
       (let ((arr (make-array n :adjustable t)))
	 (loop for n from 1 to n do (setf (aref arr (1- n)) n))
	 (make-type-list :array arr)))
  (cmd range-from-to () ((int a) (int b)) (list)
       "Generates a list of numbers from 1 (inclusive) to n (exclusive)."
       (let ((arr (make-array (- a b) :adjustable t)))
	 (loop for n from b to (1- a) do (setf (aref arr (- n b)) n))
	 (make-type-list :array arr)))
  (cmd get () ((int i) (list l)) (type)
       "Indexes in to a list."
       ; TODO negative index?
       (list-get l i))
  (cmd substr () ((int start) (int end) (list l)) (list)
       "Returns a subsequence of the elemtns of a list."
       ; TODO negative index?
       (let ((arr (new-array)))
	 (loop for i from start to (1- end) do
	      (vector-push-extend (list-get l i) arr))
	 (make-type-list :array arr)))
  (cmd combinations () ((int size) (list l)) (list)
       "Compute all of the combinations of a list."
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
	   (to-array 
	    (mapcar (lambda (x) (to-array x))
		    (combine (coerce list 'list) size))))))
  (cmd int-to-str-base-10 () ((int x)) (string)
       (to-array (write-to-string x :base 10)))
  (cmd int-to-str-base-16 () ((int x)) (string)
       (to-array (write-to-string x :base 16)))
  (cmd int-to-str-base-2 () ((int x)) (string)
       (to-array (write-to-string x :base 2)))

; list
  (cmd list-to-string () ((list l)) (string)
       (labels ((l2s (inp)
		  (with-forced inp r
		    (make-type-list :kind 'string
				    :array
				    (if (and (> (length r) 0) (not (numberp (aref r 0))))
					(map 'vector #'l2s r)
					r)))))
	 (l2s l)))
  (cmd string-to-list () ((string s)) (list)
       (with-forced s _
	 (make-type-list :array (type-list-array s) 
			 :kind 'list)))
  (cmd explode () ((anylist l)) () :messy
       "Pushes each element of a list on to the stack; the head of the list
        becomes the top of the stack."
       (with-forced l list
	 (loop for x across (reverse list) do
	      (push x stack))))
  (cmd outer () ((list a) (list b)) (list)
       "Creates a new list which contains all pairs of elements in the two input lists."
       (list-to-list-iter a
	 (next
	  (let ((tmp each))
	    (vector-push-extend
	     (list-to-list-iter b
	       (next
		(vector-push-extend (to-array (list tmp each)) result)))
	     result)))))
  (cmd sum () ((list l)) (int)
       "Computes the sum of a list."
       (with-forced l list
	 (loop for el across list sum el)))
  (cmd min () ((list l)) (int)
       "Find the smallest element of a list."
       (with-forced l list
	 (loop for el across list minimize el)))
  (cmd max () ((list l)) (int)
       "Find the largest element of a list."
       (with-forced l list
	 (loop for el across list maximize el)))
  (cmd arg-min () ((list l)) (int)
       "Find the smallest element of a list."
       (let ((best (list-get l 0)))
	 (with-forced l list
	   (loop for el in
		(loop for el across list for ii from 0 collect
		     (if (<= el best)
			 (progn
			   (setf best el)
			   ii)
			 -1))
		maximize el))))
  (cmd arg-max () ((list l)) (int)
       "Find the smallest element of a list."
       (let ((best (list-get l 0)))
	 (with-forced l list
	   (loop for el in
		(loop for el across list for ii from 0 collect
		     (if (>= el best)
			 (progn
			   (setf best el)
			   ii)
			 -1))
		maximize el))))
  (cmd with-index () ((list l)) (list)
       "Returns a new list, where each element is a list of the index and list's element."
       (list-to-list-iter l
	 (next
	  (vector-push-extend (to-array (list index each)) result))))
  (cmd sort () ((list l)) (list)
       "Sorts the elements of a list."
       (with-forced l list
	 (list (sort list #'<))))
  (cmd reverse ((:a anylist)) ((:a l)) (:a)
       "Reverses a list."
       (with-forced l list
	 (to-array (reverse list))))
  (cmd first-and-rest ((:a anylist)) ((:a l)) (type :a)
       (list
	(list-get l 0)
	(list-to-list-iter l
	  (next
	   (if (not (eq index 0))
	       (vector-push-extend each result))))))
  (cmd first () ((anylist l)) (type)
       "Get the first element of a list."
       (list-get l 0))
  (cmd rest ((:a anylist)) ((:a l)) (:a)
       "Get every element other than the first element of a list."
       (list-to-list-iter l
	 (next
	  (if (not (eq index 0))
	      (vector-push-extend each result)))))
  (cmd last () ((anylist l)) (type)
       "Get the first element of a list."
       (with-forced l lst
	 (list-get l (1- (length lst)))))
  (cmd butlast ((:a anylist)) ((:a l)) (:a)
       "Get every element other than the first element of a list."
       (list-to-list-iter l
	 (next
	  (if (not (eq (list-get l (1+ index)) null-symbol))
	      (vector-push-extend each result)))))
  (cmd set-minus () ((list takeaway) (list given)) (list)
       "The set difference of two lists; all of the elements in the second list, execpt
        for those which occur in the first list."
       (with-forced takeaway forced-takeaway-arr
	 (let ((forced-takeaway (coerce forced-takeaway-arr 'list)))
	   (list-to-list-iter given
	     (next
	      (if (not (member each forced-takeaway))
		  (vector-push-extend each result)))))))
  (cmd any () ((list l)) (bool)
       "Tests if any of the elements in a list are true."
       (not (loop for i from 0 until (eq (list-get l i) null-symbol) never (list-get l i))))
  (cmd all () ((list l)) (bool)
       "Tests if all of the elements in a list are true."
       (loop for i from 0 until (eq (list-get l i) null-symbol) always (list-get l i)))
  (cmd zip () ((list a) (list b)) (list)
       "Takes two lists and forms a new list, pairing up elements together."
       (let ((i 0))
	 (creating-new-list
	  (next
	   (let ((e1 (list-get a i)) (e2 (list-get b i)))
	     (if (or (eq e1 null-symbol) (eq e2 null-symbol))
		 nil
		 (progn
		   (vector-push-extend (to-array (list e1 e2)) result)
		   (incf i))))))))
  (cmd transpose ((:a anylist)) ((:a l)) (:a)
       "Takes a multi-dimensional list and reverses the order of the first and second axis.
       That is, if 'some_list i get j get' is the same as 'some_list transpose j get i get'."
       (let ((index 0))
	 (creating-new-list
	   (next
	    (when (not (eq (list-get (list-get l 0) index) null-symbol))
	      (let ((jindex 0)
		    (iindex index))
		(vector-push-extend
		 (funcall (if (eq (type-list-kind l) 'stringq) #'as-string (lambda (x) x))
		  (creating-new-list
		    (next
		     (let ((tmp (list-get l jindex)))
		       (incf jindex)
		       (when (not (eq tmp null-symbol))
			 (vector-push-extend (list-get tmp iindex) result))))))
		 result))
	      (incf index))))))
	       
  (cmd length () ((anylist l)) (int)
       "Compute the length of a list."
       (with-forced l list
	 (length list)))
  (cmd concatenate ((:a anylist)) ((:a a) (:a b)) (:a)
       "Concatenate two lists."
       ;; fixmeinfinite
       (let ((res (new-array)))
	 (with-forced b list
	   (loop for j from 0 to (1- (length list)) do
		(vector-push-extend (aref list j) res)))
	 (with-forced a list
	   (loop for j from 0 to (1- (length list)) do
		(vector-push-extend (aref list j) res)))
	 (make-type-list :array res)))
  (cmd prefixes ((:a anylist)) ((:a l)) (:a)
       "Compute all of the prefixes of a list."
       (list-to-list-iter l
	 (initially
	  (vector-push-extend (make-type-list :array (new-array)) result))
	 (next
	  (vector-push-extend (make-type-list :array (subseq (type-list-array l) 0 (1+ index))) result))))
  (cmd flatten () ((list l)) (list)
       "Flatten a n-dimensional list to an (n-1)-dimensional list."
       ;; fixmeinfinite
       (list-to-list-iter l
	 (next
	  (with-forced each each-list
	    (loop for el across each-list do
		 (vector-push-extend el result))))))
  (cmd uniq () ((list l)) (list)
       "Return only the unique elements of a list, order-preserved."
       (let ((seen (make-hash-table :test 'equalp)))
	 (list-to-list-iter l
	   (next
	    (if (not (gethash each seen))
		(progn
		  (setf (gethash each seen) t)
		  (vector-push-extend each result)))))))
	  
  (cmd suffixes ((:a anylist)) ((:a l)) (:a)
       "Compute all of the suffixes of a list."
       ;; better to not force and present in reversed order?
       (with-forced l list
	 (let ((length (length list)))
	   (list-to-list-iter l
	     (next
	      (vector-push-extend (make-type-list :array (subseq (type-list-array l) index length)) result))
	     (finally
	      (vector-push-extend (make-type-list :array (new-array)) result))))))
  (cmd permutations ((:a anylist)) ((:a l)) (:a)
       "Compute all of the permutations of a list."
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
	   (to-array (mapcar (lambda (x) (to-array x))
			     (permute (coerce list 'list)))))))
  (cmd join ((:a anylist)) ((list joinon) (list l)) (list)
       (list-to-list-iter l
	 (next
	  (loop for e across (with-forced each ea ea) do 
	       (vector-push-extend e result))
	  (if (not (eq (list-get l (1+ index)) null-symbol))
	      (loop for e across (with-forced joinon j j) do 
	       (vector-push-extend e result))))))
	      
  (cmd force () ((list l)) (list)
       "Force a list to be evaluated completely."
       (with-forced l list
	 list
	 l))

; string
  (cmd puts () ((string s)) ()
       (with-forced s str
	 (format t "~a" (map 'string #'code-char str))))
  (cmd str-to-int-base-10 () ((string s)) (int)
       (parse-integer (to-string s)))
  (cmd split-by-whitespace () ((string s)) (list)
       (with-forced s _
	 (to-array 
	  (mapcar #'to-array
		  (cl-ppcre:split "\\W+" (to-string s))))))
  (cmd split-by-newlines () ((string s)) (list)
       (with-forced s _
	 (to-array 
	  (mapcar #'to-array
		  (cl-ppcre:split "[\\r\\n]" (to-string s))))))
  (cmd split-by-newlines-to-str () ((string s)) (string)
       (with-forced s _
	 (as-string
	  (to-array
	   (mapcar #'to-array
		   (cl-ppcre:split "[\\r\\n]" (to-string s)))))))
  (cmd split-by-spaces () ((string s)) (list)
       (with-forced s _
	 (to-array 
	  (mapcar #'to-array
		  (cl-ppcre:split "[ \\t]+" (to-string s))))))
  (cmd uppercase-string () ((string s)) (string)
       (with-forced s _
	 (to-array (string-upcase (to-string s)))))
  (cmd lowercase-string () ((string s)) (string)
       (with-forced s _
	 (to-array (string-downcase (to-string s)))))
  (cmd capitalize-string () ((string s)) (string)
       (with-forced s _
	 (to-array (string-capitalize (to-string s)))))
  (cmd split () ((string split-at) (string longstr)) (list)
       (with-forced longstr _
	 (with-forced split-at _
	   (to-array 
	    (mapcar #'to-array
		    (cl-ppcre:split (list :sequence (to-string split-at)) 
				    (to-string longstr)))))))
  (cmd simple-replace () ((string replace-with) (string replace-this) (string replace-in)) (string)
       (with-forced replace-in _
	 (with-forced replace-with _
	   (with-forced replace-this _
	     (as-string
	      (to-array 
	       (cl-ppcre:regex-replace-all (list :sequence (to-string replace-this))
					   (to-string replace-in)
					   (to-string replace-with))))))))
  (cmd strip () ((string s)) (list)
       (with-forced s _
	 (to-array
	  (cl-ppcre:regex-replace "^\\W*(\\w|\\w.*\\w)\\W*$" (to-string s) "\\1"))))

; fun
  (cmd call () ((fun f)) ()
       "Takes a function off the stack and runs it."
       (save-arguments
	 (funcall f state 0 nil)))
  (cmd call-with-return () ((fun f)) (type)
       "Takes a function off the stack and runs it, returning the top element of the stack."
       (save-arguments
	 (funcall f state 1 nil)))
  (cmd call-with-arg-and-return () ((fun f) (type arg)) (type)
       "Takes a function off the stack and runs it with the second-to-top element of the stack, returning the top element of the stack."
       (save-arguments
	 (funcall f state 1 (list arg))))
  (cmd map ((:a anylist)) ((fun fn) (:a l)) (:a)
       "Maps a function over a list. Each element of the new list is the function applied
        to the old element."
       (save-arguments
       (list-to-list-iter l
	   (next
	    (vector-push-extend (funcall fn state 1 (list each)) result)))))
  (cmd keep-maxes-by () ((fun fn) (list l)) (list)
       "Keep only the largest elements of a list as decided by a functions."
       (save-arguments
	 (let ((best (funcall fn state 1 (list (list-get l 0))))
	       (result nil))
	   (with-forced l list
	     (loop for el across list do
		  (let ((value (funcall fn state 1 (list el))))
;		    (format t "is ~a ~a~%" el value)
		    (when (> value best)
			(setf best value)
			(setf result nil))
		    (if (>= value best)
			(push el result)))))
	   (to-array (reverse result)))))
  (cmd filter () ((fun fn) (list l)) (list)
       "Returns a new list where only elements where the function returns true are retained."
       (save-arguments
       (list-to-list-iter l
	 (next
	  (if (funcall fn state 1 (list each))
	      (vector-push-extend each result))))))
  (cmd reduce () ((fun fn) (list l)) (type)
       "Returns a single element which is the result of calling a function on successive
        elements of a list."
       (save-arguments
       (let ((init (list-get l 0)))
	 (with-forced l list
	   (loop for el across list for i from 0 if (not (eq i 0)) do
		(setf init (funcall fn state 1 (list el init))))
	   init))))
  (cmd fold () ((fun fn) (type init) (list l)) (type)
       "Identical to reduce, but specifying a different initial value. This is a left-fold."
       (save-arguments
	 (with-forced l list
	   (loop for el across list do
		(setf init (funcall fn state 1 (list el init))))
	   init)))
  (cmd uniq-by () ((fun fn) (list l)) (list)
       "Returns a new list of only the unique elements, using some other predicate than equality."
       (save-arguments
       (let ((seen nil))
	 (list-to-list-iter l
	   (next
	    (when (not (some (lambda (x) x) 
			     (mapcar (lambda (x) (funcall fn state 1 (list x each))) seen)))
		(vector-push-extend each result)
		(push each seen)))))))
  (cmd tabulate-forever () ((fun fn)) (list)
       "Create a sequence obtained by calling a function on the integers from 0"
       (let ((number 0))
	 (save-arguments
	   (creating-new-list
	     (next
	      (vector-push-extend (funcall fn state 1 (list number)))
	      (incf number))))))
  (cmd tabulate () ((fun fn) (int upto)) (list)
       "Create a sequence obtained by calling a function on the integers from 0"
       (let ((number 0))
	 (save-arguments
	   (creating-new-list
	     (next
	      (when (< number upto)
		(vector-push-extend (funcall fn state 1 (list number)))
		(incf number)))))))
  (cmd partition () ((fun fn) (list l)) (list)
       (let ((seen (make-hash-table :test 'equalp))
	     (res (new-array)))
	 (save-arguments
	   (with-forced l list
	     (loop for el across list do
		  (let ((val (funcall fn state 1 (list el))))
		    (if (not (gethash val seen))
			(setf (gethash val seen) (new-array)))
		    (vector-push-extend el (gethash val seen))))))
;	 (print seen)
	 (maphash (lambda (key value) (vector-push-extend (list value) res)) seen)
	 (make-type-list :array res)))
;  (cmd unreduce () ((fun fn) (type something)) (list)
  (cmd fixpoint ((:a type)) ((fun a) (:a start)) (:a)
       (let ((seen (make-hash-table :test 'equalp)))
	 (save-arguments
	   (loop while (not (gethash start seen)) do
		(setf (gethash start seen) t)
		(setf start (funcall a state 1 (list start)))))
	 start))
  (cmd ite () ((fun a) (fun b) (bool case)) ()
       "Run one of two functions based on if the next element on the stack is true or not."
       (save-arguments
	(if case
	    (funcall a state 0 nil)
	    (funcall b state 0 nil))))
  (cmd if () ((fun a) (bool case)) ()
       "Run a function if the top of the stack is true."
       (save-arguments
	(if case
	    (funcall a state 0 nil))))
  (cmd do-while () ((fun fn)) ()
       "Run a function as long as it returns true from the top of the stack."
       (save-arguments
	(let ((cont t))
	  (loop while cont do
	       (setf cont (funcall fn state 1 nil))))))
  (cmd call-n-times () ((fun fn) (int n)) ()
       "Runs the top function n times."
       (save-arguments
	(loop for i from 0 to (1- n) do (funcall fn state 0 nil)))))


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
;    ((listp el) 'list)
;    ((vectorp el) 'list)
    ((eq (type-of el) 'type-list) 
     (type-list-kind el))
;    ((functionp el) 'fun)
    ((eq (type-of el) 'fun-on-stack)
     (if (fun-on-stack-is-restoring el) 'fun-restoring 'fun-nonrestoring))
    ((symbolp el) 'builtin)
    (t (error `(this is very bad ,el)))))

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
			  (list 'fun (helper cmd) (third cmd))
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

;; When we eval our defuns, we wrap it with only the *commands* which are actually
;; used; otherwise the evals become very slow.
(defun keep-only (commands body)
  (labels ((referenced-commands (body)
	     (if (listp body)
		 (if (listp (car body))
		     (reduce #'append (mapcar #'referenced-commands body))
		     (cons (car body) (reduce #'append 
					      (mapcar #'referenced-commands 
						      (cdr body))))))))
    
    (let ((refed (referenced-commands body)))
      (remove-if-not 
       (lambda (x)
	 (member (car x) refed))
       commands))))

;; This is where we keep track of the uncompressed resulting lisp code from
;; the low level binary data, and also where we keep track of the resulting
;; high level lisp code.
(defvar *final-defuns* nil)
(defvar *final-highlevel* nil)

(defun log-uncompressed (name defun highlevel)
  (push (list name defun) *final-defuns*)
  (push (list name highlevel) *final-highlevel*))

;; Create a set of lisp defuns that actually let the code run. Each defun
;; initially just uncompresses the code it contains, and then from then on
;; when called actually executes the code.
(defun create-defuns (defuns &optional (block-kinds nil))
  (mapcar
   (lambda (input)
     (let* ((fnid (car input))
	    (body (cadr input))
	    (more-block-kinds nil))
       (if (eq fnid 'fn-1)
	   (push '*non-restoring more-block-kinds))
       (loop while (eq (caar body) 'fun-modifier) do
	    (push (cadr (car body)) more-block-kinds)
	    (setf body (cdr body)))
       `(let ((first t))
	  (defun ,fnid (qstate nret args)
	    (if first
		(let* ((fn-body (uncompress-body ',fnid ',(append block-kinds more-block-kinds)
						 ',body nret args stack qstate #'decompress))
		       (fn-lambda (eval (list 'labels (keep-only *commands* fn-body) 
					      fn-body))))
		  (setf first nil)
		  (setf (symbol-function ',fnid) fn-lambda)
		  (setf (symbol-function ',(with-under fnid)) fn-lambda)
		  (funcall fn-lambda qstate nret args))
		(funcall #',(with-under fnid) qstate nret args))))))
   defuns))

;; A generic decompressor which does nothing.
(defun decompress (input types argtypes fnid) 
  (declare (ignore input types argtypes fnid))
  (assert nil))


;; When we're doing the compilation run, this is where we put all of the
;; actual mapping of uncompressed code -> compressed code.
(defvar *compiled-code* nil)

;; Do the actual unification for the type matching (below).
(defun unify (one-type match-with bindings)
  (let ((o-match-with match-with))
;	(match-)
    (let ((found (assoc match-with bindings)))
      (when found
	(setf bindings (remove found bindings))
	(setf match-with (second found))))
    (cond
      ((equal match-with 'type) 
       (cons (list o-match-with one-type) bindings))
      ((equal match-with one-type) 
       (cons (list o-match-with one-type) bindings))
      ((and (listp match-with)
	    (equal (car match-with) 'either))
       (or (unify one-type (second match-with)
		  (cons (list o-match-with (second match-with)) bindings))
	   (unify one-type (third match-with)
		  (cons (list o-match-with (third match-with)) bindings)))))))

;; We want to match a type description against something actually on the stack.
;; For example, the argument (type) matches the stack value (int).
;; But more specifically, if we declare :a to be a type, then the arguments
;;  (:a :a) will match (int int) but not (int list).
(defun matches-type-description (realtypes bindings arguments)
  (labels ((fixup (b)
	     (loop for e in b if (not (eq (car e) 'type)) collect e)))
    
    (cond
      ((null arguments)
       (list bindings))
      ((null realtypes)
       nil)
      (t 
       (let ((unification (unify (car realtypes) (car arguments) 
				 (if bindings bindings
				     '((type type))))))
	 (if unification 
	     (matches-type-description (cdr realtypes)
				       (fixup unification)
				       (cdr arguments))))))))

;(MATCHES-TYPE-DESCRIPTION '(STRING)
;			  '((:A ANYLIST))
;			  '(:A))

(defun result-from-type-description (results bindings)
  (mapcar (lambda (x)
	    (let ((find (assoc x bindings)))
	      (if find (second find) x)))
	  results))

(defun command-rewriter (command full args)
  (let ((extra-stuff nil))
    (if full
	(let ((desc (car (matches-type-description args
						   (command-bindings full) 
						   (command-args full)))))
;	  (format t "okay so ~a ~a~%" (result-from-type-description (command-res full) desc)
;		  (member 'fun-nonrestoring args))
	  (if (and (equal (result-from-type-description (command-res full) desc) '(list))
		   (member 'fun-nonrestoring args))
	      (push '(builtin force) extra-stuff))
;	  (format t "AAA ~a ~a ~a ~%" args desc 
;		  (result-from-type-description (command-res full) desc))
	  (let ((has-str (result-from-type-description (command-res full) desc)))
	    (when (and (member 'string has-str) (not (member 'string (command-res full))))
	      (assert (<= (position 'string has-str) 1))
	      (when (eq (position 'string has-str) 0)
		(push '(builtin list-to-string) extra-stuff))
	      (when (eq (position 'string has-str) 1)
		(push '(builtin swap) extra-stuff)
		(push '(builtin list-to-string) extra-stuff)
		(push '(builtin swap) extra-stuff))))))
    (cons command extra-stuff)))


;(matches-type-description '(int) '((:a int)) '(:a))

(defun find-possible-commands (types)
    (mapcar #'command-name
	    (remove-if-not 
	     (lambda (x) 
	       (matches-type-description types
					 (command-bindings x)
					 (command-args x)))
	     *command-info*)))

;; Creates an encoding of the uncompressed command to be used to compress it later.
;; Uses the stack information to figure out how to encode it.
(defun do-encode (real-command types)
  (let ((possible-commands 
	 (find-possible-commands types)))
;    (format t "~%TAKING ~a BITS" (log (length possible-commands)))
;    (format t "~%LOOKUP ~a ~a " real-command possible-commands)
    (when (not (position real-command possible-commands))
      (error (format nil "The command ~a does not exist for types ~a" real-command types)))
    (list (cons (position real-command possible-commands) (length possible-commands)))))

;; A "decompressor" used in compilation run. 
;; It turns commands in to commands, but first inspects the stack  and uses this
;; information to encode the commands.
(defun commands-to-commands (input types argtypes fnid)
  (declare (ignore argtypes))
  (let* ((new-name (intern (concatenate 'string
					(symbol-name fnid) "P")))
	 (full-command (find-command-by-name (cadr (car input)) *command-info*))
	 (inputs (if full-command
		     (loop for el in (command-args full-command) for arg in types 
			collect arg))))
    (if (eq (caar input) 'builtin)
	(progn
	  (push (append
		 (car (last input))
		 (do-encode (cadr (car input)) types))
		*compiled-code*)))
    (incf (caddr (car (last input))))
    (if (cddr input)
	(cons (append (command-rewriter (car input) full-command inputs)
		    `((fun ,new-name (*non-restoring))
		      (builtin call)))
	      (car (create-defuns
		    (list (list new-name (append '((fun-modifier *no-arguments)
						   (fun-modifier *non-restoring))
					       (cdr input)))))))
	(list (command-rewriter (car input) full-command inputs)))))

;; An actual decoder. It returns the command to actually run given stack information
;; and the value of the command.
(defun do-decode (command-abbrv types)
  (if (member 'type types) nil
      (let* ((possible-commands 
	      (find-possible-commands types)))
;	(format t "~%Decoded ~a to ~a" command-abbrv (nth (car command-abbrv) possible-commands))
	(nth (car command-abbrv) possible-commands))))

(defun make-function-weights (&optional (ct 1))
    (let* ((valids '(((*non-restoring) 8) (() 16) ((*exploding) 2)
		     ((*non-restoring *exploding) 1)))
	   (total (loop for el in valids sum (second el))))
      (loop for el in valids
	 collect `((fun ,(car el)) ,(/ (* ct (second el)) total)))))

;; Given the big blob of compressed data, extracts the next single token.
;; The blob of compressed data can be represented any way, as long as it is tagged
;; with 'decode-me. A precondition of this is that there actually exists more data.
(defun extract-next-token (input types)
  (setf input (caar input))

  (if (has-more-data input)
  (let* ((possible-commands (find-possible-commands types))
	 (count (length possible-commands))
	 (weights `(,@(make-function-weights count)
		    (it-is-an-integer ,count)
		    ,@(loop for i from 0 to (1- count) 
			 collect `((builtin (,i . ,count)) 8))))
	 (next-token (uncompress-once-from input weights)))
    (when (eq next-token 'it-is-an-integer)
	(use-up-data input)
	(setf next-token (list 'int (uncompress-once-from input '(:geometric-mode 1/2)))))
    (when (eq (car next-token) 'fun)
	(use-up-data input)
	(setf next-token (list 'compressed-function
			       (uncompress-once-from input '(:geometric-mode 9/10))
			       (second next-token)))) ; *restoring, or *exploding, or other
    (case (car next-token)
      (compressed-function
       (use-up-data input)
       (let ((resulting-bits (arithmetic-extract-n-bits input (second next-token))))
	 (list 'fun-as-list
	       (make-compressed-data resulting-bits)
	       (third next-token))))
      (builtin
       (let ((cmd (do-decode (cadr next-token) types)))
	 (if cmd
	     (progn
	       (use-up-data input)
	       (list 'builtin cmd)))))
      (int
       (use-up-data input)
       (let ((resulting-bits (arithmetic-extract-n-bits input (+ 2 (second next-token)))))
	 (list 'int (arithmetic-decode-integer resulting-bits))))
      (otherwise
       (error "oh noes"))))
  nil))

;; Checks if there is actually any remaining work to do, and if there is
;; returns a 'decode-me containing it.
(defun remaining-work (input)
  (if (has-more-data (caar input))
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
(defun compressed-to-commands (input types argstack-types fnid)
  (let ((commands nil)
	(cont t)
	(defun-part nil))
;    (format t "input data is ~a~%" (get-compressed-data (caar input)))
    (loop while cont do
	 (let* ((cmd (if (not (member 'abort types)) 
			 (extract-next-token input types))))
	   (if cmd
	       (case (car cmd)
		 (int
		  (push 'int types)
		  (push cmd commands))
		 (fun
;		  (format t "AND PUSH FUN2 ~a~%" cmd)
		  (push 'fun types)
		  (push cmd commands))
		 (fun-as-list
;		  (format t "AND PUSH FUN1 ~a ~a~%" (third cmd) (member '*restoring (third cmd)))
		  (push (if (member '*non-restoring (third cmd))
			    'fun-nonrestoring
			    'fun-restoring)
			types)
;		  (print types)
		  (push cmd commands))
		 (list
		  (push 'list types)
		  (push cmd commands))
		 (builtin
		  (let* ((decoded (cadr cmd))
			 (full (find-command-by-name decoded *command-info*))
			 (args (command-args full))
			 (matching 
			  (car (matches-type-description types (command-bindings full) args)))
			 (results (mapcar (lambda (x) 
					    (if (assoc x matching)
						(second (assoc x matching))
						x))
					  (command-res full))))
;		    (format t "match ~a ~a ~a ~a~%" full args matching results)
		    
		  (case decoded
		    (arg-a
		     (push (first argstack-types) types)
		     (push '(builtin arg-a) commands))
		    (arg-b
		     (push (second argstack-types) types)
		     (push '(builtin arg-b) commands))
		    (arg-c
		     (push (third argstack-types) types)
		     (push '(builtin arg-c) commands))
		    (arg-d
		     (push (fourth argstack-types) types)
		     (push '(builtin arg-d) commands))
		    (otherwise
		     (let ((the-types (loop for i from 1 to (length args) collect (pop types))))
;		       (format t "THETYPES ~a~%" the-types)
		       (setf commands (append (reverse (command-rewriter (list 'builtin decoded)
									 full
									 the-types))
					      commands))
		       (if (or (member 'fun-nonrestoring the-types)
			       (member 'fun the-types) ;; this should never happen
			       (member :messy (command-notes full)))
			   (progn
;			     (format t "DOABORT ~a ~a ~a~%" full the-types results)
;			     (if (equal results '(list))
;				 (push '(builtin force) commands))
			     (push 'abort types))
			   (setf types (append results types)))))))))
	       (if (remaining-work input)
		 (let* ((new-name (intern (concatenate 'string 
						       (symbol-name fnid) "P"))))
;			(new-name-maker (intern (concatenate 'string 
;							     (symbol-name new-name) "-MAKER"))))
		   (setf cont nil)
		   (push (list 'fun new-name '(*non-restoring)) commands)
		   (push '(builtin call) commands)
		   ;; (push `(verbatim 
		   ;; 	   (progn
		   ;; 	     (funcall #',new-name-maker)
		   ;; 	     (funcall #',new-name)))
		   ;; 	 commands)
		   ;; (setf defun-part
		   ;; 	 `(defun ,new-name-maker ()
		   ;; 	    (format t "it is being run ~a~%" stack)
		   ;; 	    (let* ((uncompressed-code (compressed-to-commands ',input ',restore-args (mapcar #'typeof stack) ',new-name))
		   ;; 		   (defun-part (cdr uncompressed-code))
		   ;; 		   (ir-part (ir-to-lisp (car uncompressed-code)))
		   ;; 		   (lambda-part (lisp-optimize (car ir-part)))
		   ;; 		   (defun-part-2 (cdr ir-part))
		   ;; 		   (real-program
		   ;; 		    (if defun-part 
		   ;; 			(if defun-part-2
		   ;; 			    `(progn
		   ;; 			       ,defun-part
		   ;; 			       ,@defun-part-2
		   ;; 			       ,@lambda-part)
		   ;; 			    `(progn
		   ;; 			       ,defun-part
		   ;; 			       ,@lambda-part))
		   ;; 			(if defun-part-2
		   ;; 			    `(progn
		   ;; 			       ,@defun-part-2
		   ;; 			       ,@lambda-part)
		   ;; 			    `(progn
		   ;; 			       ,@lambda-part)))))
			      
		   ;; 	      (format t "the program continuation is ~a~%" real-program)
		   ;; 	      (eval (list 'defun ',new-name nil 
		   ;; 			   (list 'labels (keep-only *commands* real-program) 
		   ;; 				 real-program))))))
		   (setf defun-part 
			 (car 
			  (create-defuns
			   (list (list new-name
				       (remaining-work input)))
			   '(*no-arguments *non-restoring)))))
		   (setf cont nil)))))
;    (format t "~%COMMANDS IS ~a~%"  (reverse commands))
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
	  (verbatim
	   (cadr decoded))
	  (int
	   `(push ,(cadr decoded) stack))
	  (fun
;	   (error 'hi 'bye)
	   (format t "~%we're making the function on the stack ~a~%" decoded)
	   (assert (eq (length decoded) 3))
	   `(push (make-fun-on-stack 
		   :fun #',(cadr decoded) 
		   :is-restoring ',(null (find '*non-restoring (caddr decoded)))) stack))
	  (fun-as-list
	   (assert (eq (length decoded) 3))
	   (format t "~%2we're making the function on the stack ~a~%" decoded)
	   (let ((id (fresh-fun-name)))
	     (push
	      (car (create-defuns (list (cons id (list (list (list (cadr decoded)))))) 
				  (caddr decoded))) 
	      defuns)
	     `(push (make-fun-on-stack 
		     :fun #',id 
		     :is-restoring ,(null (find '*non-restoring (caddr decoded)))) stack)))
	  (list
	   `(push '(,(cadr decoded)) stack))
	  (builtin
	   (let* ((full (find-command-by-name (cadr decoded) *command-info*))
		  (args (command-args full))
		  (results (command-res full)))
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

(defun lisp-optimize (code)
;  (format t "code is before optimize ~a~%" code)
  (let ((curstack nil)
	(result nil))
    (labels ((maybe-replace (cmd)
	       (if (and (listp cmd) (find-command-by-name (without-under (car cmd)) *command-info*))
		   (cons (car cmd)
			 (loop for el in (cdr cmd) collect
			      (if curstack
				  (pop curstack)
				  '(pop stack))))
		   cmd)))
      (loop for el in code do
;	   (format t "it is ~a and ~a~%" el (eq (car el) 'push))
	   (if (eq (car el) 'push)
	       (progn
		 (push (maybe-replace (second el)) curstack))
	       (progn
;		 (format t "dodump1 ~a~%" curstack)
		 (setf result (append (loop for e in curstack collect `(push ,e stack)) result))
		 (setf curstack nil)
		 (push el result)))))
;    (format t "dodump2 ~a~%" curstack)
    (setf result (append (loop for e in curstack collect `(push ,e stack)) result))
    (reverse result)))
    

(defun correct-stack (kinds args stack state)
;  (print 123123)
;  (print (list args stack))
;  (print kinds)
  (let* ((first-pass
	  (if (member '*non-restoring kinds)
	      (append args stack)
	      (append args (state-base-stack state))))
	 (second-pass
	  (if (member '*exploding kinds)
	     (append (coerce (with-forced (car first-pass) list list) 'list)
		     (cdr first-pass))
	     first-pass)))
;    (format t "passes are ~a ~a~%" first-pass second-pass)
    second-pass))

(defstruct (state
	     (:conc-name "STATE-"))
  (base-stack nil))

(defstruct (fun-on-stack
	     (:conc-name "FUN-ON-STACK-"))
  (fun nil)
  (is-restoring 'restoring))

;; Returns a lambda which runs the full program given by the input.
;; We may need to define some new functions either because ir-to-lisp
;; told us to, or because compressed-to-commands told us to.
;; Before we do anything, we check if the block is a special type which
;; modifies the stack. If it is, we make the operation.
(defun uncompress-body (fnid block-kinds input nret args stack state decompressor)
  (let* ((correct-stack (correct-stack block-kinds args stack state))
	 (types (mapcar #'typeof correct-stack))
	 (argument-types (if (member '*no-arguments block-kinds)
			     (mapcar #'typeof (car argument-restore-stack))
			     types))
	 (uncompressed-code (funcall decompressor input types argument-types fnid))
	 (defun-part (cdr uncompressed-code))
	 (ir-part (ir-to-lisp (car uncompressed-code)))
	 (lambda-part (lisp-optimize (car ir-part)))
	 (defun-part-2 (cdr ir-part))
	 (final-part
	  `(lambda (state nret args)
;	     (declare (ignore nret))
	     (assert (equal nret ,nret)) ;; if this fails, called with different nret
;	     (format t "sdfsdfsdf ~a~%" (not (member '*non-restoring ',block-kinds)) )
	     ,@(if (not (member '*non-restoring block-kinds))
		   '((push stack restore-stack)))
	     
	     (let ((corrected (correct-stack ',block-kinds args stack state)))
	       (setf stack corrected)
	       ,@(if (not (member '*no-arguments block-kinds))
		     `((let ((argument-restore-stack (list corrected)))
			 ,@lambda-part))
		     lambda-part))

;            ---------------------------------	    
;            THIS CODE BELOW MIGHT BE REQUIRED
;                     DO NOT DELETE
;            ---------------------------------	    
;	     (let ((res (case nret
;			  (0 nil)
;			  (1 (pop stack))
;			  (2 (list (pop stack) (pop stack)))
;			  (otherwise (loop for i from 1 to nret collect (pop stack))))))
	     (let ((res ,(case nret
			       (0 'nil)
			       (1 '(pop stack))
			       (2 (cons 'list (loop for i from 1 to nret collect '(pop stack)))))))
	       ,@(if (not (member '*non-restoring block-kinds))
		     `((setf stack (pop restore-stack))))
	       res))))
    (log-uncompressed fnid final-part uncompressed-code)
    
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
(defun run-compiled (commands init-stack)
  (make-commands)
  `(let ((stack ',init-stack)
	 (restore-stack nil)
	 (argument-restore-stack '()))
     ,(cons 'progn (create-defuns (parse-and-split commands)))
     (funcall #'fn-1 (make-state) 0 nil)
     (mapcar #'repl-print stack)))

;; Turn a nested list of commands in to a set of bits which are very
;; dense, yet still represent the same information.
;;   1. Arithmetic encode each function.
;;   2. Prefix the function by the bits representing a function, and its length.
;;   3. Add the bits of the function to the outer function.
;;   4. Recurse.
(defun list-to-bits (list)
;  (format t "~%AAAAAA ~a" list)
  (labels ((prepare (function-tree)
;	     (print function-tree)
	       (append
		(reduce #'append
			(mapcar
			 (lambda (x)
			   (case (car x)
			     (fun
			      (let ((encoded (arithmetic-encode-body 
					      (subseq (cadr x) 
						      (length (get-modifiers (cadr x)))))))
				`((fun ,(get-modifiers (cadr x)))
				  (geometric-number ,(length encoded) 9/10)
				  ,@(mapcar (lambda (x) `(single-bit ,x)) encoded))))
			      (int
			       (multiple-value-bind (n bits) (arithmetic-encode-integer (cadr x))
				 `((int)
				   (geometric-number ,n 1/2)
				   ,@(mapcar (lambda (x) `(single-bit ,x)) bits))))
			      (otherwise
			       (list x))))
			 function-tree))))
	   (get-modifiers (body)
		    (loop while (eq (car (car body)) 'fun-modifier)
		       collect (cadr (pop body))))
;		     ,@(reduce #'append (mapcar #'make-flat (cadr tree)))
;		     (fun-end)))
	   (arithmetic-encode-body (function)
;	     (format t "~%QQQQ~a" function)
	     (let* ((flat-function (prepare function)))
;	       (format t "~%BBBBBB ~a~%" flat-function)
	       (arithmetic-encode
		(mapcar (lambda (x) (if (eq (car x) 'geometric-number) (cadr x) x))
			flat-function)
		(append
		 (loop for elt in flat-function collect
		      (case (car elt)
			(int
			 `(((funs) 1) ((int) 1) ((other) 8)))
			(fun
;			 (format t "asdfasdfadsf ~a ~a" elt (make-function-weights))
			   `(,@(make-function-weights)
			       ((other) 9)))
			(geometric-number
			 `(:geometric-mode ,(third elt)))
			(single-bit
			 `(((single-bit 0) 1) ((single-bit 1) 1)))
			(builtin
			 (let ((count (cdadr elt)))
			   `((functhing ,count) (integers ,count)
			     ,@(loop for i from 0 to (1- count) collect
				    `((builtin (,i . ,count)) 8))))))))))))

      (arithmetic-encode-body (cadr list))))

(defun bits-to-bytes (bits)
  (let ((end-marked (append bits '(1))))
    (loop while (not (eq (mod (length end-marked) 8) 0)) do
	 (setf end-marked (append end-marked '(0))))
    (subseq (write-to-string 
	     (parse-integer (format nil "11111111~{~a~}" end-marked) :radix 2) :base 16)
	    2)))

(defun bytes-to-bits (bytes)
  (let* ((hex-digits '((0 0 0 0) (0 0 0 1) (0 0 1 0) (0 0 1 1)
		       (0 1 0 0) (0 1 0 1) (0 1 1 0) (0 1 1 1)
		       (1 0 0 0) (1 0 0 1) (1 0 1 0) (1 0 1 1)
		       (1 1 0 0) (1 1 0 1) (1 1 1 0) (1 1 1 1)))
	 (bits (loop for el across bytes append 
		   (nth (parse-integer (coerce (list el) 'string) :radix 16)  hex-digits))))
    (loop while (eq (car (last bits)) 0) do
	 (setf bits (butlast bits)))
    (butlast bits)))

;; From the compressed bits, return a list of how to run them.
(defun bits-to-list (bits)
  (let* ((encoded (make-compressed-data bits)))
    (list 'fun (list (list encoded)))))

;; Converts the high-level language to commands tagged by their type.
(defun add-types-to-user-input (input)
  (if (listp input)
      (let ((deeper (mapcar #'add-types-to-user-input input)))
	(list 'fun deeper (loop for e in deeper 
			     while (eq (car e) 'fun-modifier)
			     collect (cadr e))))
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
(defun compile-by-running (input init-stack)
  (let ((id 0))
    (labels ((tag-input (in)
	       (if (eq (car in) 'fun)
		   (list 'fun (append (mapcar #'tag-input (cadr in))
				      (list (list 'compile-tag (incf id) 0)))
			 (caddr in))
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
	(print tagged)
	(setf (symbol-function 'decompress) #'commands-to-commands)
	(let ((answer (eval (run-compiled tagged init-stack))))
	  (cons answer 
		(bits-to-bytes 
		 (list-to-bits 
		  (drop-last (do-replace tagged *compiled-code*))))))))))

(defun run-bits (bits &optional (init-stack nil))
    (setf (symbol-function 'decompress) #'compressed-to-commands)
    (setf *final-defuns* nil)
    (setf *final-highlevel* nil)
    (time (eval (run-compiled (bits-to-list bits) init-stack))))

(defun run-bytes (bytes &optional (init-stack nil))
  (run-bits (bytes-to-bits bytes) init-stack))

;; Run the program by first compiling it, then running it.
(defun run (i &optional (init-stack nil))
  (labels ((fix-init (e)
	     (cond
	       ((numberp e)
		e)
	       ((stringp e)
		(to-array e))
	       ((vectorp e)
		(to-array (loop for a across e collect (fix-init a)))))))
    (setf init-stack (mapcar #'fix-init init-stack)))
;  (ignore-redefun
  (format t "~%Run the program ~a" i)
  (let* ((answer-and-compiled (compile-by-running i init-stack))
	 (answer (car answer-and-compiled))
	 (compiled (cdr answer-and-compiled))
	 (*fun-count* 0))
    (format t "~%The compiled version is ~a of size ~a~%" compiled (/ (length compiled) 2))
;    (print answer)
    (let ((run-answer (run-bytes compiled init-stack)))
      (format t "~%Ans1: ~a;~%Ans2: ~a" answer run-answer)
      (assert (equalp answer run-answer))
      run-answer)))

(defun make-lisp-from-final (&optional (init-stack nil))
  (let ((body (loop for el in *final-defuns* collect
		   `(setf (symbol-function ',(car el)) ,(cadr el)))))
  `(labels ((fix-init (e)
	     (cond
	       ((numberp e)
		e)
	       ((stringp e)
		(make-type-list :array e :kind 'string))
	       ((vectorp e)
		(to-array (loop for a across e collect (fix-init a)))))))
       (let ((init-stack (mapcar #'fix-init ',init-stack)))
	 (labels ,(keep-only *commands* body)
	   (let ((stack init-stack)
		 (restore-stack nil)
		 (argument-restore-stack nil))
	     ,@body
	     (funcall #'fn-1 (make-state) 0 nil)
	     (mapcar #'repl-print stack)))))))

(defun repl-print (x)
  (case (typeof x)
    (int x)
    (bool 
     (if x 'true 'false))
;    (fun 'fun)
    (string
     (with-forced x _
       (cons 'string (to-string x))))
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
	
(defun run-it-now ()
  (handler-bind ((style-warning #'muffle-warning))
    (with-timeout 1
      (let ((code (compile-by-running 
		   (read-from-string (read-line))
		   nil)))
	(format t "Stack dump:~%")
	(loop for el in (car code) for i from 0 do
	     (format t "   ~a. ~a~%" i el))
	(format t "~%")
	(format t "Compiled Code (~a bytes): ~a~%" (/ (length (cdr code)) 2) (cdr code))))))

;(sb-ext:save-lisp-and-die "compiler" :executable t :purify t :toplevel 'run-it-now)
      
;(time
;[5 range prefixes force 5 range suffixes force zip (*exploding concatenate) map (sum) map force])

;(time
;[2 range (1 subtract) map force (*exploding add) call])

;(time (loop for i across (caar (run '(8 range permutations (with-index force dup (*exploding *restoring unrot (*exploding *restoring arg-c arg-a eq arg-c arg-a subtract abs arg-d arg-b subtract abs neq or) map force all) map force all) map force))) count i))

;(run '(4 range permutations (with-index force dup (*exploding *restoring unrot (*exploding *restoring arg-c arg-a eq arg-c arg-a subtract abs arg-d arg-b subtract abs neq or) map force all) map force all) map force))

;(time
;  [8 range permutations (with-index dup outer flatten (flatten) map (*exploding *restoring arg-c eq arg-c arg-a subtract abs arg-d arg-b subtract abs neq or) map all) filter length])

;(time [8 range permutations (with-index dup outer flatten (flatten) map (*exploding *restoring arg-c eq arg-c arg-a subtract abs arg-d arg-b subtract abs neq or) map all) filter length])

;(progn
;  (time (run '(8 range permutations (with-index force dup (*exploding *restoring) map force) map force)))   nil)

;(progn
;  [1 dup range permutations (with-index (*exploding add arg-a arg-b subtract 2 implode) map transpose (*restoring uniq length eq) map all) filter length])

;(time
; [5 dup range permutations (with-index dup (*exploding add) map uniq length arg-b eq swap (*exploding subtract) map uniq length arg-b eq and) filter force length])


;(run '(5 dup range permutations (with-index dup (*exploding add) map uniq length arg-b eq swap (*exploding subtract) map uniq length arg-b eq and) filter force length))

;; (run '((dup 3 mod subtract dup 3 add swap) swap 9 range dup outer flatten (*restoring *exploding drop drop arg-a get arg-c transpose arg-b get concatenate arg-b arg-d call arg-c (*restoring rot substr) map force arg-a arg-d call substr flatten rot drop drop concatenate uniq (*restoring 0 neq) filter length) map force) '((#((#(0 0 4 0 0 0 0 0 8)) (#(0 9 8 3 0 0 7 0 0)) (#(5 1 0 7 0 9 0 0 4)) (#(0 0 0 5 0 2 0 0 0)) (#(0 5 0 0 0 0 0 6 0)) (#(4 0 0 6 0 1 0 0 7)) (#(7 0 0 4 0 6 0 8 2)) (#(0 0 5 9 0 0 3 4 0)) (#(8 0 0 0 0 0 9 0 0))))))

;(run '(5 range dup zip dup (*restoring *exploding arg-a arg-a add) map 1 swap force))

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


; 1.28
;(run '(200 (*restoring (dup 3 mod subtract dup 3 add swap) swap 9 range dup outer flatten (*restoring *exploding drop drop arg-a get arg-c transpose arg-b get concatenate arg-b arg-d call arg-c (*restoring rot substr) map force arg-a arg-d call substr flatten rot drop drop concatenate uniq (*restoring 0 neq) filter length) map force) call-n-times) '(#(#(0 0 4 0 0 0 0 0 8) #(0 9 8 3 0 0 7 0 0) #(5 1 0 7 0 9 0 0 4) #(0 0 0 5 0 2 0 0 0) #(0 5 0 0 0 0 0 6 0) #(4 0 0 6 0 1 0 0 7) #(7 0 0 4 0 6 0 8 2) #(0 0 5 9 0 0 3 4 0) #(8 0 0 0 0 0 9 0 0))))
;(test-with-stack "Partial sudoku" '(6 5 5 7 5 8 5 4 4 7 7 7 7 4 7 5 6 6 6 6 6 7 6 8 7 7 7 5 5 4 8 4 5 6 6 6 5 5 4 8 4 5 5 5 6 6 6 6 8 6 7 6 5 6 6 8 6 8 6 7 7 7 7 6 7 6 6 5 7 7 7 7 5 5 5 7 4 6 6 6 6)
;    (#(#(0 0 4 0 0 0 0 0 8) #(0 9 8 3 0 0 7 0 0) #(5 1 0 7 0 9 0 0 4) #(0 0 0 5 0 2 0 0 0) #(0 5 0 0 0 0 0 6 0) #(4 0 0 6 0 1 0 0 7) #(7 0 0 4 0 6 0 8 2) #(0 0 5 9 0 0 3 4 0) #(8 0 0 0 0 0 9 0 0)))
;  (dup 3 mod subtract dup 3 add swap) swap 9 range dup outer flatten (*restoring *exploding drop drop arg-a get arg-c transpose arg-b get concatenate arg-b arg-d call arg-c (*restoring rot substr) map force arg-a arg-d call substr flatten rot drop drop concatenate uniq (*restoring 0 neq) filter length) map force)

;.323
;; (run '((*restoring
;; 	(dup 3 mod subtract dup 3 add swap) swap ; thing to take n -> [n-n%3, n+3-n%3]
;; 	9 range dup outer flatten  ; create all paris of ints
;; 	(*restoring *exploding swap drop get ; get one row
;; 	 arg-c transpose arg-b get ; get one colum
;; 	 concatenate ; append them
;; 	arg-b arg-d call arg-c (*restoring rot substr) map ; get one 3 wide stripe
;; 	 arg-a arg-d call substr ; get the 3x3 box
;; 	 flatten rot drop drop concatenate ; add it on to the list
;; 	 uniq ; take only unique elements
;; 	 9 range swap set-minus
;; 	 (*restoring 0 neq) filter force ; remove 0s
;; 	 ) 
;; 	map force) ; function which returns the valid moves
;;        (*restoring call-with-return (*restoring length) map arg-min print)
;;        call-with-return)
;;      '((#((#(0 0 4 0 0 0 0 0 8)) (#(0 9 8 3 0 0 7 0 0)) (#(5 1 0 7 0 9 0 0 4)) (#(0 0 0 5 0 2 0 0 0)) (#(0 5 0 0 0 0 0 6 0)) (#(4 0 0 6 0 1 0 0 7)) (#(7 0 0 4 0 6 0 8 2)) (#(0 0 5 9 0 0 3 4 0)) (#(8 0 0 0 0 0 9 0 0))))))

;(run '(dup range permutations (*restoring with-index dup (*restoring *exploding add) map uniq swap (*restoring *exploding subtract) map uniq concatenate length arg-b dup add eq) filter length) '(8))


;(print (make-lisp-from-final '(8)))
;(print *final-defuns*)
;(print *final-highlevel*)

;(run '(range permutations (*restoring with-index (*restoring sum) map uniq arg-a with-index (*restoring *exploding subtract) map uniq concatenate length) keep-maxes-by) '(6))


;(let ((inp '(0 0 0 0 0 0 1 1 1 0 0 1 0 0 0 1 1 1 0 1 1 1 1 0 1 1 1 1 0 0 0 1 0 1 0 1
;       1 1 0 0 0 0 0 1 1 1 1 0 0 0 1 0 0 0 1 1 0 0 0 1 0 0 1 1 1 0 0 0)))
;  (assert (equal (bytes-to-bits (bits-to-bytes inp)) inp)))

;(run '(split-by-spaces) '("hiq there how are you"))

;(run '(5 range (0 neq) map))

;(print (make-lisp-from-final '(8)))

;(run '(length 3 add) '("hi there"))

;(print (make-lisp-from-final '("hi there")))

;(run '(5 range (48 add) map list-to-string puts))

;(run '(simple-replace) '("q" "a" "aa"))

;(run '(split-by-newlines-to-str first-and-rest) (list (format nil "a~%b~%c~%d")))
;; (run '(split-by-newlines-to-str first-and-rest string-to-list 32 forever zip transpose list-to-string explode unrot (*restoring rot simple-replace reverse arg-c arg-b simple-replace split-by-newlines-to-str transpose) fixpoint) (list (format nil "lisp
;; plispbxrarfh
;; sccolispeofn
;; glnnkrfuelvq
;; fmnuptdnfivc
;; mbpsiljvosqi
;; rpyakjzuhpwu
;; hskrivpzwxoy
;; tithyncyetwh
;; ilwwelispylf
;; iamyebtllzcf
;; epsilbykvvxo
;; fbghrhxnrwhw")))


;(run '(


