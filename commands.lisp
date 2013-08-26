;;;; commands.lisp -- provide all of the commands for the compiler

;;;; Copyright (C) 2013, Nicholas Carlini <nicholas@carlini.com>.
;;;;
;;;; This file is part of rhoScript.
;;;;
;;;; rhoScript is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; rhoScript is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with rhoScript.  If not, see <http://www.gnu.org/licenses/>.


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *commands* nil)
  (defparameter *full-commands* nil)
  (defparameter *command-info* nil)

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
      
      (let ((the-commands
	     (mapcar
	      (lambda (x) 
		(gen-cmd (first x) (second x) (third x) (fourth x) (fifth x) (last x)))
	      body)))
	`(defun make-commands ()
	   (setf *full-commands* ',body)
	   (setf *commands* ',the-commands)
	   (setf *command-info*
		 ,(cons 'list
			(mapcar (lambda (x)
				  `(make-command-info :name ',(second x)
						      :bindings ',(repl-it (third x))
						      :untouched-bindings ',(third x)
						      :args ',(repl-it (get-args x))
						      :untouched-args ',(get-args x)
						      :res ',(fifth x)
						      :notes ',(member :messy (notes-of x))))
				body)))
	   ,(cons 'progn
		  (loop for a in the-commands collect
		       (cons 'defun a)))))))
  (defmacro commands (&body body)
    (real-commands body)))


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
	     (lambda ()
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


(defmacro save-arguments (&body body)
  `(let ((state (make-state :base-stack stack)))
     ,@body))

(defun magic-read (s &optional (outer-flatten t) (inner-flatten t) (read-item t) (touch-lines t))
  (let* ((lines (cl-ppcre:split "[\\r\\n]" s))
	 (parsed nil))
    (setf parsed
	  (mapcar 
	   (lambda (x)  (cl-ppcre:split "[\\W+]" x))
	   lines))
    (if (and touch-lines
	     (every (lambda (x) (eq (length x) (length (car parsed))))
		    parsed))
	(progn
	  (if (and read-item
		   (every (lambda (x) x) (loop for row in parsed for e in row 
					    collect (cl-ppcre:scan "^[0-9]+$" e))))
	      (setf parsed (loop for row in parsed collect
				(loop for e in row collect
				     (parse-integer e)))))
	  (setf parsed
		(mapcar (if (and (= (length (car parsed)) 1) inner-flatten)
			    #'car 
			    #'to-array)
			parsed)))
	(setf parsed lines))
    
    (if (and (= (length lines) 1) outer-flatten)
	(car parsed)
	(to-array
	 parsed))))

(commands
; type
  (cmd unsure () () () :messy
       "Do nothing."
       42)
  (cmd dup ((:a type)) ((:a other)) (:a :a)
       "Duplicate the top element of the stack."
       (list other other))
  (cmd dup-top-two ((:a type) (:b type)) ((:a other1) (:b other2)) (:a :b :a :b)
       "Duplicate the top two elements of the stack."
       (list other1 other2 other1 other2))
  (cmd swap ((:a type) (:b type)) ((:a a) (:b b)) (:b :a)
       "Swap the order of the top two elements on the stack."
       (list b a))
  (cmd eq () ((type a) (type b)) (bool)
       "Compare the top two elements of the stack for equality."
       (equalp a b))
  (cmd neq () ((type a) (type b)) (bool)
       "Compare the top two elements of the stack for inequality."
       (not (equalp a b)))
  (cmd drop () ((type q)) ()
       "Remove the top element of the stack."
       ;(declare (ignore a)))
       q)
  (cmd print () () ()
       "Pretty-print the entire stack; useful only for debugging."
       (progn
	 (format t "Stack dump:~%")
	 (loop for el in stack for i from 0 do
	      (format t "   ~a. ~a~%" i el))
	 (format t "~%")))
  (cmd rot ((:a type) (:b type) (:c type)) ((:a a) (:b b) (:c c)) (:b :c :a)
       "Rotate the top three elements: A B C -> B C A"
       (list b c a))
  (cmd unrot ((:a type) (:b type) (:c type)) ((:a a) (:b b) (:c c)) (:c :a :b)
       "Rotate the top three elements in the reverse direction: A B C -> C A B"
       (list c a b))
  (cmd arg-a () () (type)
       "Push the top element of the stack, at the time of the last
        context establishment, to the stack."
       (car (car argument-restore-stack)))
  (cmd arg-b () () (type)
       "Push the second to top element of the stack, at the time of the last
        context establishment, to the stack."
       (cadr (car argument-restore-stack)))
  (cmd arg-c () () (type)
       "Push the third to top element of the stack, at the time of the last
        context establishment, to the stack."
       (caddr (car argument-restore-stack)))
  (cmd arg-d () () (type)
       "Push the fourth to top element of the stack, at the time of the last
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
  (cmd cons ((:a anylist)) ((type arg) (:a l)) (:a)
       "Add an item to the front of a list."
       (list-to-list-iter l
	 (initially
	  (vector-push-extend arg result))
	 (next
	  (vector-push-extend each result))))
  (cmd rcons ((:a anylist)) ((type arg) (:a l)) (:a)
       "Add an item to the end of a list."
       (list-to-list-iter l
	 (next
	  (vector-push-extend each result))
	 (finally
	  (vector-push-extend arg result))))
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
       "Logical not of the top element of the stack."
       (not a))
  (cmd and () ((bool a) (bool b)) (bool)
       "Logical and of the top two elements of the stack."
       (and a b))
  (cmd gte () ((int a) (int b)) (bool)
       "Check if the first argument is larger than the second."
       (>= b a))
  (cmd gt () ((int a) (int b)) (bool)
       "Check if the first argument is larger than or equal to the second."
       (> b a))
  (cmd lt () ((int a) (int b)) (bool)
       "Check if the first argument is less than the second."
       (< b a))
  (cmd lte () ((int a) (int b)) (bool)
       "Check if the first argument is less than or equal to the second."
       (<= b a))
  (cmd add () ((int a) (int b)) (int)
       "Add the top two elements of the stack."
       (+ a b))
  (cmd negate () ((int a)) (int)
       "Negate the top element of the stack."
       (- 0 a))
  (cmd inc () ((int a)) (int)
       "Increment the top element of the stack."
       (+ a 1))
  (cmd dec () ((int a)) (int)
       "Decrement the top element of the stack."
       (- a 1))
  (cmd multiply () ((int a) (int b)) (int)
       "Multiply the top two elements of the stack."
       (* a b))
  (cmd subtract () ((int a) (int b)) (int)
       "Subtract from the second-to-top by the top of the stack."
       (- b a))
  (cmd swapsubtract () ((int a) (int b)) (int)
       "Subtract from the top by the second-to-top of the stack."
       (- a b))
  (cmd divide () ((int a) (int b)) (int)
       "Divide from the second-to-top by the top of the stack."
       (floor (/ b a)))
  (cmd swapdivide () ((int a) (int b)) (int)
       "Divide from the top by the second-to-top of the stack."
       (floor (/ a b)))
  (cmd pow () ((int a) (int b)) (int)
       "Return the second-to-top element to the power of the top element."
       (expt b a))
  (cmd square () ((int a)) (int)
       "Square the top element of the stack."
       (* a a))
  (cmd mod () ((int a) (int b)) (int)
       "Compute the remainder of the second-to-top when divided by the top of the stack."
       (mod b a))
  (cmd divides () ((int a) (int b)) (bool)
       "Test if the top element divides the second-to-top element"
       (= (mod b a) 0))
  (cmd abs () ((int a)) (int)
       "Compute the absolute value of the top of the stack."
       (abs a))
  (cmd zero () ((int a)) (bool)
       "Test if a number is zero."
       (= a 0))
  (cmd even () ((int a)) (bool)
       "Test if the top of the stack is an even number."
       (evenp a))
  (cmd odd () ((int a)) (bool)
       "Test if the top of the stack is an odd number."
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
  (cmd untake ((:a anylist)) ((int a) (:a l)) (:a)
       "Take all but the first n elements of a list."
       (let ((index 0))
	 (creating-new-list
	   (next
	    (if (> (incf index) a)
		(vector-push-extend (list-get l (1- index)) result))))))
  (cmd implode () ((int count)) (list) :messy
       "Pop the top count elements off the stack and make a list out of them."
       (let ((a (new-array)))
	 (loop for j from 1 to count do
	      (vector-push-extend (pop stack) a))
	 (make-type-list :array a)))
  (cmd range () ((int n)) (list)
       "Generate a list of numbers from 0 (inclusive) to n (exclusive)."
       (let ((arr (make-array n :adjustable t)))
	 (loop for n from 0 to (- n 1) do (setf (aref arr n) n))
	 (make-type-list :array arr)))
  (cmd range-from-1 () ((int n)) (list)
       "Generate a list of numbers from 1 (inclusive) to n (exclusive)."
       (let ((arr (make-array n :adjustable t)))
	 (loop for n from 1 to n do (setf (aref arr (1- n)) n))
	 (make-type-list :array arr)))
  (cmd range-from-to () ((int a) (int b)) (list)
       "Generate a list of numbers from the first number (inclusive) to the second number (exclusive)."
       (let ((arr (make-array (- a b) :adjustable t)))
	 (loop for n from b to (1- a) do (setf (aref arr (- n b)) n))
	 (make-type-list :array arr)))
  (cmd get () ((int i) (list l)) (type)
       "Get the nth element of a list."
       ; TODO negative index?
       (list-get l i))
  (cmd substr () ((int start) (int end) (list l)) (list)
       "Return a subsequence of the elements of a list."
       ; TODO negative index?
       (let ((arr (new-array)))
	 (loop for i from start to (1- end) do
	      (vector-push-extend (list-get l i) arr))
	 (make-type-list :array arr)))
  (cmd combinations ((:a anylist)) ((int size) (:a l)) (:a)
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
       "Convert an integer to a string, in base 10."
       (to-array (write-to-string x :base 10)))
  (cmd int-to-str-base-16 () ((int x)) (string)
       "Convert an integer to a string, in base 16."
       (to-array (write-to-string x :base 16)))
  (cmd int-to-str-base-2 () ((int x)) (string)
       "Convert an integer to a string, in base 12."
       (to-array (write-to-string x :base 2)))
  (cmd int-to-str-base-2 () ((int base) (int x)) (string)
       "Convert an integer to a string, in a specified base."
       (to-array (write-to-string x :base base)))

; list
  (cmd list-to-string () ((list l)) (string)
       "Convert a list of bytes to a string."
       (labels ((l2s (inp)
		  (with-forced inp r
		    (make-type-list :kind 'string
				    :array
				    (if (and (> (length r) 0) (not (numberp (aref r 0))))
					(map 'vector #'l2s r)
					r)))))
	 (l2s l)))
  (cmd string-to-list () ((string s)) (list)
       "Convert a string to a list of bytes."
       (with-forced s _
	 (make-type-list :array (type-list-array s) 
			 :kind 'list)))
  (cmd explode () ((anylist l)) () :messy
       "Push each element of a list onto the stack; the head of the list
        will become the top of the stack."
       (with-forced l list
	 (loop for x across (reverse list) do
	      (push x stack))))
  (cmd outer () ((list a) (list b)) (list)
       "Create a new list which contains all pairs of elements in the two input lists."
       (list-to-list-iter a
	 (next
	  (let ((tmp each))
	    (vector-push-extend
	     (list-to-list-iter b
	       (next
		(vector-push-extend (to-array (list tmp each)) result)))
	     result)))))
  (cmd sum () ((list l)) (int)
       "Compute the sum of a list."
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
       "Return a new list, where each element is a list of the index and the corresponding element."
       (list-to-list-iter l
	 (next
	  (vector-push-extend (to-array (list index each)) result))))
  (cmd sort () ((list l)) (list)
       "Sort the elements of a list."
       (with-forced l list
	 (sort list #'<)))
  (cmd reverse ((:a anylist)) ((:a l)) (:a)
       "Reverse a list."
       (with-forced l list
	 (to-array (reverse list))))
  (cmd first-and-rest ((:a anylist)) ((:a l)) (type :a)
       "Push both the first element of a list and the remaining list to the stack."
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
       "The set difference of two lists: all of the elements in the second list, except
        for those which occur in the first list."
       (with-forced takeaway forced-takeaway-arr
	 (let ((forced-takeaway (coerce forced-takeaway-arr 'list)))
	   (list-to-list-iter given
	     (next
	      (if (not (member each forced-takeaway))
		  (vector-push-extend each result)))))))
  (cmd any () ((list l)) (bool)
       "Test if any of the elements in a list are true."
       (not (loop for i from 0 until (eq (list-get l i) null-symbol) never (list-get l i))))
  (cmd all () ((anylist l)) (bool)
       "Test if all of the elements in a list are true."
       (loop for i from 0 until (eq (list-get l i) null-symbol) always (list-get l i)))
  (cmd zip ((:a anylist)) ((:a a) (:a b)) (:a)
       "Take two lists and form a new list, pairing up elements together."
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
       "Take a multi-dimensional list and reverse the order of the first and second axes.
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
  (cmd rotations ((:a anylist)) ((:a l)) (:a)
       "Compute all of the rotations of a list."
       (with-forced l list
	 (labels ((rotate (list i)
		    (append (subseq list i (length list))
			     (subseq list 0 i)))
		  (rotations (list)
		    (to-array
		     (loop for i from 0 to (1- (length list)) collect
			  (to-array (rotate list i))))))
	   (rotations (coerce list 'list)))))
  (cmd join ((:a anylist)) ((:a joinon) (:a l)) (:a)
       "Flatten out a list, placing a second list in between each sublist."
       (list-to-list-iter l
	 (next
	  (loop for e across (with-forced each ea ea) do 
	       (vector-push-extend e result))
	  (if (not (eq (list-get l (1+ index)) null-symbol))
	      (loop for e across (with-forced joinon j j) do 
	       (vector-push-extend e result))))))
  (cmd intersperse ((:a anylist)) ((type item) (:a l)) (:a)
       "Intersperse an item in a list."
       (list-to-list-iter l
	 (next
	  (vector-push-extend each result)
	  (if (not (eq (list-get l (1+ index)) null-symbol))
	      (vector-push-extend item result)))))
  (cmd force () ((list l)) (list)
       "Force a list to be evaluated completely."
       (with-forced l list
	 list
	 l))

; string
  (cmd puts () ((string s)) ()
       "Print out the contents of a string."
       (with-forced s str
	 (format t "~a" (map 'string #'code-char str))))
  (cmd str-to-int-base-10 () ((string s)) (int)
       "Convert a string to an integer, treating the string as base 10."
       (parse-integer (to-string s)))
  (cmd magic-read () ((string s)) (type)
       "Attempt to read the string in to the appropriate data structures. Try hard to do
 \"the right thing\" whenever possible. Types are entirely data-dependent, and this function
 might behave incorrectly on some inputs."
       (magic-read (to-string s)))
  (cmd magic-read-kind () ((int how) (string s)) (type)
       "Read the string into the appropriate data structures, as directed by the flags.
A 1 in the low bit indicates that if there is only one line, flatten the array once.
A 1 in the second bit indicates that if there is a line with only one word, flatten that array.
A 1 in the third bit indicates words should not attempt to be parsed to their kind if possible.
A 1 in the fourth bit indicates that lines should not attempt to be read at all."
       (labels ((zero (x) (= x 0)))
	 (magic-read (to-string s) (not (zero (logand how 1))) (not (zero (logand how 2)))
		     (zero (logand how 4)) (zero (logand how 8)))))
	     
  (cmd split-by-whitespace () ((string s)) (list)
       "Split a string to a list of strings, splitting on any whitespace."
       (with-forced s _
	 (to-array 
	  (mapcar #'to-array
		  (cl-ppcre:split "\\W+" (to-string s))))))
  (cmd split-by-newlines () ((string s)) (list)
       "Split a string to a list of strings, splitting on newlines."
       (with-forced s _
	 (to-array 
	  (mapcar #'to-array
		  (cl-ppcre:split "[\\r\\n]" (to-string s))))))
  (cmd split-by-newlines-to-str () ((string s)) (string)
       "Split a string to a multi-level string, splitting on newlines."
       (with-forced s _
	 (as-string
	  (to-array
	   (mapcar #'to-array
		   (cl-ppcre:split "[\\r\\n]" (to-string s)))))))
  (cmd split-by-spaces () ((string s)) (list)
       "Split a string to a list of strings, splitting on spaces."
       (with-forced s _
	 (to-array 
	  (mapcar #'to-array
		  (cl-ppcre:split "[ \\t]+" (to-string s))))))
  (cmd uppercase-string () ((string s)) (string)
       "Force every character in a string to upper case."
       (with-forced s _
	 (to-array (string-upcase (to-string s)))))
  (cmd lowercase-string () ((string s)) (string)
       "Force every character in a string to lower case."
       (with-forced s _
	 (to-array (string-downcase (to-string s)))))
  (cmd capitalize-string () ((string s)) (string)
       "Make a string capitalized: every word starts with a capital letter, all other letters 
are lower case."
       (with-forced s _
	 (to-array (string-capitalize (to-string s)))))
  (cmd split () ((string split-at) (string longstr)) (list)
       "Split a string by occurrences of a specific string."
       (with-forced longstr _
	 (with-forced split-at _
	   (to-array 
	    (mapcar #'to-array
		    (cl-ppcre:split (list :sequence (to-string split-at)) 
				    (to-string longstr)))))))
  (cmd simple-replace () ((string replace-with) (string replace-this) (string replace-in)) (string)
       "Replace one string with another string for all occurrences of a third string."
       (with-forced replace-in _
	 (with-forced replace-with _
	   (with-forced replace-this _
	     (as-string
	      (to-array 
	       (cl-ppcre:regex-replace-all (list :sequence (to-string replace-this))
					   (to-string replace-in)
					   (to-string replace-with))))))))
  (cmd strip () ((string s)) (list)
       "Strip a string, removing whitespace around the outside."
       (with-forced s _
	 (to-array
	  (cl-ppcre:regex-replace "^\\W*(\\w|\\w.*\\w)\\W*$" (to-string s) "\\1"))))

; fun
  (cmd call () ((fun f)) ()
       "Take a function off the stack and run it."
       (save-arguments
	 (funcall f state 0 nil)))
  (cmd call-with-return () ((fun f)) (type)
       "Take a function off the stack and run it, returning the top element of the stack."
       (save-arguments
	 (funcall f state 1 nil)))
  (cmd call-with-arg-and-return () ((fun f) (type arg)) (type)
       "Take a function off the stack and run it with the second-to-top element of the 
stack, returning the top element of the stack."
       (save-arguments
	 (funcall f state 1 (list arg))))
  (cmd map ((:a anylist)) ((fun fn) (:a l)) (:a)
       "Map a function over a list. Each element of the new list is the function applied
        to the old element."
       (save-arguments
       (list-to-list-iter l
	   (next
	    (vector-push-extend (funcall fn state 1 (list each)) result)))))
  (cmd keep-maxes-by () ((fun fn) (list l)) (list)
       "Keep only the largest elements of a list as decided by a function."
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
  (cmd filter ((:a anylist)) ((fun fn) (:a l)) (:a)
       "Return a new list where only elements for which the function returns true are retained."
       (save-arguments
       (list-to-list-iter l
	 (next
	  (if (funcall fn state 1 (list each))
	      (vector-push-extend each result))))))
  (cmd reduce () ((fun fn) (list l)) (type)
       "Return a single element which is the result of calling a function on successive
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
       "Return a new list of only the unique elements, using some other predicate than equality."
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
	      (vector-push-extend (funcall fn state 1 (list number)) result)
	      (incf number))))))
  (cmd tabulate () ((fun fn) (int upto)) (list)
       "Create a sequence obtained by calling a function on the integers from 0 up to a number"
       (let ((number 0))
	 (save-arguments
	   (creating-new-list
	     (next
	      (when (< number upto)
		(vector-push-extend (funcall fn state 1 (list number)) result)
		(incf number)))))))
  (cmd partition () ((fun fn) (list l)) (list)
       "Partition a list to a list of lists, where each element of each list has the same value 
when the function is applied to it."
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
	 (maphash (lambda (key value) 
		    (declare (ignore key)) 
		    (vector-push-extend (list value) res)) seen)
	 (make-type-list :array res)))
;  (cmd unreduce () ((fun fn) (type something)) (list)
  (cmd fixpoint ((:a type)) ((fun a) (:a start)) (:a)
       "Compute the fixedpoint of applying a function to an item, terminating when the function 
returns the same a value for the second time, not necessarily consecutively."
       (let ((seen (make-hash-table :test 'equalp)))
	 (save-arguments
	   (loop while (not (gethash start seen)) do
		(setf (gethash start seen) t)
		(setf start (funcall a state 1 (list start)))))
	 start))
  (cmd fixpoint-with-history ((:a type)) ((fun a) (:a start)) (:a)
       "Compute the fixedpoint of applying a function to an item, terminating when the function 
returns the same a value for the second time, not necessarily consecutively. Return the
full sequence generated."
       (let ((seen (make-hash-table :test 'equalp))
	     (res (new-array)))
	 (save-arguments
	   (loop while (not (gethash start seen)) do
		(vector-push-extend start res)
		(setf (gethash start seen) t)
		(setf start (funcall a state 1 (list start)))))
	 (make-type-list :array res)))
  (cmd ite () ((fun a) (fun b) (bool case)) ()
       "Run one of two functions based on whether the next element on the stack is true or not."
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
       "Run the top function n times."
       (save-arguments
	(loop for i from 0 to (1- n) do (funcall fn state 0 nil)))))
