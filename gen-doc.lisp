(defun find-doc (code)
  (loop for i from 4 while (symbolp (nth i code)) 
     finally (return (if (stringp (nth i code)) (nth i code) ""))))

(defun to-doc (cmd)
  (let* ((cmd-name (command-name cmd))
	 (pretty-args-1
	  (format nil "~{~s~^, ~} -> ~{~s~^, ~}" (command-untouched-args cmd) (command-res cmd)))
	 (pretty-args
	  (if (command-bindings cmd)
	      (format nil
		      "~{~{~s~^ ~}~^, ~} => ~a"
		      (command-untouched-bindings cmd) pretty-args-1)
	      pretty-args-1))
	 (code (assoc cmd-name (mapcar #'cdr *full-commands*)))
	 (docstring (find-doc code)))
    (format t "
	  <div class=\"command\">
	    <div class=\"command-name\">~a :: ~a 
	      <label class=\"showme\" for=\"cmd-~a\"><span class=\"button\"><a class=\"button\">view source</a></span></label>
	      <input class=\"showmecheck\" checked=\"checked\" type=\"checkbox\" id=\"cmd-~a\">
	      <div class=\"hideme\">
		<pre class=\"code\">
~s</pre>
	      </div>
	    </div>
	    <div class=\"command-body\">
	      ~a
	    </div>
	  </div>" cmd-name pretty-args cmd-name cmd-name code docstring)))

(defun gen-doc ()
  (make-commands)
  (mapcar
   (lambda (x)
     (to-doc x))
   *command-info*))


;(to-doc (second *command-info*))
(gen-doc)
