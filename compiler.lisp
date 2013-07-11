;;;; compiler.lisp -- the base rhoScript compiler

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
  (declaim (optimize (debug 3) (speed 0) (safety 3)))

  ; (load "~/quicklisp/setup.lisp")
  ; (ql:quickload :cl-ppcre)

  ;; We include some packages first.
  (require :cl-ppcre)
  (load "arithmetic-encoder.lisp")
  (load "auxiliary.lisp")

  ;; These are the stacks we're going to work with. They need to be
  ;; dynamically scoped so that lisp doesn't complain when we (eval) code.
  (defvar stack nil)
  (defvar restore-stack nil)
  (defvar argument-restore-stack nil)
  
  ;; Now load up all the commands
  (load "commands.lisp")

  ;; And let you run commands with []
  (set-macro-character #\] (get-macro-character #\)))
  (set-macro-character #\[
		       (lambda (stream char)
			 (declare (ignore char))
			 `(run ',(read-delimited-list #\] stream t)))))


;; This is where we keep track of the uncompressed resulting lisp code from
;; the low level binary data, and also where we keep track of the resulting
;; high level code.
(defvar *final-defuns* nil)
(defvar *final-highlevel* nil)
(defun log-uncompressed (name defun highlevel)
  (push (list name defun) *final-defuns*)
  (push (list name highlevel) *final-highlevel*))

;; When we need a fresh function name, we use this.
(defvar *fun-count* 0)
(defun fresh-fun-name ()
  (intern (concatenate 'string "DYN-FN-" (write-to-string (incf *fun-count*)))))

;; When we're doing the compilation run, this is where we put all of the
;; actual mapping of uncompressed code -> compressed code.
(defvar *compiled-code* nil)


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
		       (fn-lambda (eval fn-body)))
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

(defun result-from-type-description (results bindings)
  (mapcar (lambda (x)
	    (let ((find (assoc x bindings)))
	      (if find (second find) x)))
	  results))

;; Some commands need to be rewritten at runtime.
;; We force list operations when they are being called with a non-stack-restoring function.
;; Also functions returning both strings and lists are coerced to the correct type.
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
(defun commands-to-commands-q (input types argstack-types fnid)
  (declare (ignore argstack-types))
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
    (if (cddr input) ;; there is at least one non-informational token
	(cons (append (command-rewriter (car input) full-command inputs)
		    `((fun ,new-name (*non-restoring))
		      (builtin call)))
	      (car (create-defuns
		    (list (list new-name (cdr input)))
		    '(*no-arguments *non-restoring))))
	(list (command-rewriter (car input) full-command inputs)))))
(defun commands-to-commands (input types argstack-types fnid)
  (let ((commands nil)
	(cont t)
	(defun-part nil))
    (loop while (and cont (cdr input)) do
	 (let* ((cmd (if (not (or (member 'abort types)
				  (member 'type types)))
			 (pop input))))
	   (if cmd
	       (case (car cmd)
		 (int
		  (push 'int types)
		  (push cmd commands))
		 (fun
;		  (format t "AND PUSH FUN2 ~a~%" cmd)
		  (assert (= (length cmd) 3))
		  (push (if (member '*non-restoring (third cmd))
			    'fun-nonrestoring
			    'fun-restoring)
			types)
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
		  (push (append
			 (car (last input))
			 (do-encode (cadr cmd) types))
			*compiled-code*)
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
	       (if (cdr input)
		 (let* ((new-name (intern (concatenate 'string 
						       (symbol-name fnid) "P"))))
		   (setf cont nil)
		   (push (list 'fun new-name '(*non-restoring)) commands)
		   (push '(builtin call) commands)
		   (setf defun-part 
			 (car 
			  (create-defuns
			   (list (list new-name input))
			   '(*no-arguments *non-restoring)))))
		   (setf cont nil)))
	   (when cmd
;	     (format t "~% IT IS HERE ~A ~A ~A ~A~%" cmd input (last input) (car (last input)))
	     (incf (caddr (car (last input)))))))
;    (format t "~%COMMANDS IS ~a~%"  (reverse commands))
    (cons (reverse commands)
	  defun-part)))


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

;; Checks if there is actually any remaining work to do.
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
		   (setf cont nil)
		   (push (list 'fun new-name '(*non-restoring)) commands)
		   (push '(builtin call) commands)
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
;	   (format t "~%we're making the function on the stack ~a~%" decoded)
	   (assert (eq (length decoded) 3))
	   `(push (make-fun-on-stack 
		   :fun #',(cadr decoded) 
		   :is-restoring ',(null (find '*non-restoring (caddr decoded)))) stack))
	  (fun-as-list
	   (assert (eq (length decoded) 3))
;	   (format t "~%2we're making the function on the stack ~a~%" decoded)
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
  (let* ((first-pass
	  (if (member '*non-restoring kinds)
	      (append args stack)
	      (append args (state-base-stack state))))
	 (second-pass
	  (if (member '*exploding kinds)
;	     (append (coerce (with-forced (car first-pass) list list) 'list)
;		     (cdr first-pass))
	      (with-forced (car first-pass) list
		(setf first-pass (cdr first-pass))
		(loop for i from (1- (length list)) downto 0 do
		     (push (aref list i) first-pass))
		first-pass)
	     first-pass)))
;    (format t "passes are ~a ~a~%" first-pass second-pass)
    second-pass))

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
     (if (or (eq (typeof (car stack)) 'fun-restoring)
	     (eq (typeof (car stack)) 'fun-nonrestoring))
	 (push (_map (pop stack) (pop stack)) stack))
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
	(setf (symbol-function 'decompress) #'commands-to-commands)
	(let ((answer (time (eval (run-compiled tagged init-stack)))))
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

(defun run-it-now ()
  (handler-bind ((style-warning #'muffle-warning))
    (handler-case  
	(with-timeout 1
	  (let ((code (compile-by-running 
		       (read-from-string (read-line))
		       nil)))
	    (format t "Stack dump:~%")
	    (loop for el in (car code) for i from 0 do
		 (format t "   ~a. ~a~%" i el))
	    (format t "~%")
	    (format t "Compiled Code (~a bytes): ~a~%" (/ (length (cdr code)) 2) (cdr code))))
       (timeout (c)
	 (declare (ignore c))
	 (format t "Timeout Error: Function took too long to complete.~%"))
       (SIMPLE-ERROR (c)  
	 (print c)))))

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



(run '(5 dup range permutations (with-index dup (*exploding add) map uniq length arg-b eq swap (*exploding subtract) map uniq length arg-b eq and) filter length))
