(defmacro test (name answer &body body)
; ((comp (compress-code '(5 range permutations))))
;  (print (eval (do-compile comp))))
;  `(let ((it (car (eval (do-compile (compress-code ',body))))))
  `(let ((it (car (run ',body))))
     (if (equalp
	  ,answer
	  it)
	 (print ,(concatenate 'string "Test Passes: " name))
	 (progn
	   (print "-------")
	   (print ,(concatenate 'string "Test Fails: " name))
	   (print `(got ,it expected ,,answer))
	   (print "-------")
	   (assert nil)))))
	 
(test "Add" 5
  2 3 add)

(test "Subtract" 5
  7 2 subtract)

(test "Divide" 4
  8 2 divide)

(test "Stack" 1
  2 4 swap divide 3 4 unrot rot rot add swap subtract 2 drop)

(test "Call" 5
  2 (3) call ((add) call) call)

(test "Range" 10
  5 range sum)

(test "Map sum" 15
  5 range (1 add) map sum)

(test "Permutations length" 120
  5 range permutations length)

(test "Permutations values 1" 10
  5 range permutations (sum) map max)

(test "Permutations values 2" 10
  5 range permutations (sum) map min)

(test "Call-n-times" 98
  5 range (dup (dup add) call-n-times) map sum)

(test "Concatenate" (list #(0 1 2 0 1 2 3))
  3 range 4 range concatenate)

(test "Unknown types 1" 6
  5 range 3 get 3 add)

(test "Unknown types 2" 7
  3 (inc arg-a add) call)

(test "Unknown types 3" 14
  5 range (inc arg-a multiply) map 3 get 2 add)

(test "Reduce" 10
  5 range 0 (add) fold)

(test "Exploding" 3
  2 range (1 add) map (*exploding add) call)

(test "Exploding" '(#(0 2 4 6))
  4 range dup zip (*exploding add) map force)

(test "Zip" '(#(10 10 10 10 10 10))
  5 range prefixes force 5 range suffixes force zip (*exploding concatenate) map (sum) map force)

(test "Blocks can mess with stack" '(#(1 2 3 4 5))
  5 range ((1) call add) map force)

;(test "Prefixes + Suffixes" 10
;  5 range dup prefixes force swap suffixes)
