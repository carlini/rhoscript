;;;; auxiliary.lisp -- provides several useful functions and structs

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


(defstruct (type-list 
	     (:conc-name "TYPE-LIST-"))
  (array nil)
  (kind 'list)
  (generator nil))


(defstruct (state
	     (:conc-name "STATE-"))
  (base-stack nil))


(defstruct (fun-on-stack
	     (:conc-name "FUN-ON-STACK-"))
  (fun nil)
  (is-restoring 'restoring))


(defstruct (command-info
	     (:conc-name "COMMAND-"))
  (name nil)
  (untouched-bindings nil)
  (bindings nil)
  (args nil)
  (untouched-args nil)
  (res nil)
  (notes nil))


;; This is the symbol which list-get returns when the list is empty.
;; It needs to be a fresh symbol so lists can contain nil.
(defvar null-symbol (gensym))


(defun new-array () (make-array 20 :adjustable t :fill-pointer 0))

(defun to-array (list)
  (if (stringp list)
    (make-type-list :array (map 'vector #'char-code list) :kind 'string)
    (make-type-list :array (coerce list 'vector))))

(defun as-string (list)
  (setf (type-list-kind list) 'string)
  list)

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
