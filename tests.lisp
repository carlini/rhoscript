(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun test-with-stack-fn (name answer stack body)
    `(let ((it (car (run ',body ',stack))))
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
  (defmacro test (name answer &body body)
    (test-with-stack-fn name answer nil body))
  (defmacro test-with-stack (name answer stack &body body)
    (test-with-stack-fn name answer stack body)))


(test "Push a number" 3
  3)

(test "Equality" 'true
  3 3 eq)

(test "Inequality" 'true
  3 4 neq)

(test "Add" 5
  2 3 add)

(test "Subtract" 5
  7 2 subtract)

(test "Divide" 4
  8 2 divide)

(test "Mod" 7
  107 10 mod)

(test "Stack 2" 1
  1 2 3 rot rot)

(test "Drop" 3
  3 range 4 drop (0 add) map sum)

(test "Stack 3" 1
  2 4 swap divide 3 4 unrot rot rot add swap subtract 2 drop)

(test "Call" 5
  2 (3) call ((add) call) call)

(test "Select Args 1" 5
  1 2 3 4 (arg-a arg-d add) call)

(test "Select Args 2" 3
  1 2 3 4 (arg-a arg-d add arg-c subtract) call)

(test "Select Args 3" 8
  1 2 3 (4 5 range 6 unsure arg-b add) call)

(test "Range" 10
  5 range sum)

(test "Implode" 10
  1 2 3 4 4 implode sum)

(test "Map sum" 15
  5 range (1 add) map sum)

(test "Permutations length" 120
  5 range permutations length)

(test "Permutations values 1" '(10 10)
  5 range permutations (sum) map dup max swap min 2 implode)

(test "Nonrestoring is forced" '(2 3 5 8 12 17 23 30 38 47)
  1 10 range (add dup) map (*restoring inc) map)

(test "argmin" 3
  10 range (3 subtract dup multiply) map arg-min)

(test "Call-n-times" 98
  5 range (dup (dup add) call-n-times) map sum)

(test "Concatenate" '(0 1 2 0 1 2 3)
  3 range 4 range concatenate)

(test "Unknown types 1" 6
  5 range 3 get 3 add)

(test "Unknown types 2" 7
  3 (inc arg-a add) call)

(test "Unknown types 3" 14
  5 range (inc arg-a multiply) map 3 get 2 add)

(test "Basic nonrestoring" 5
  3 (5) call)

(test "Basic restoring" 3
  3 (*restoring 5) call)

(test "Restoring saves correctly" '(0 1 2 3 4)
  5 range dup (*restoring get) map 1 swap)

(test "Restoring saves correctly 2" '(12)
  12 1 range (*restoring add) map)

(test "Map from under" '(12 13 14 15 16)
  12 5 range (*restoring add) map)

(test "Arguments are half-lexically scoped" '(0 1 2 3 4)
  5 range dup (*restoring arg-b arg-a get) map 1 swap force)

(test "Reduce" 10
  5 range 0 (add) fold)

(test "Explode 1" 3
  2 range (1 add) map (explode add) call)

(test "Explode 2" '(0 2 4 6)
  4 range dup zip (explode add) map force)

(test "Exploding 1" 3
  2 range (*restoring 1 add) map (*exploding add) call)

(test "Exploding 2" '(0 2 4 6)
  4 range dup zip (*exploding add) map force)

(test "Transpose" '(6 3)
 3 range dup (1 add) map zip transpose (sum) map force)

(test "Zip" '(10 10 10 10 10 10)
  5 range prefixes force 5 range suffixes force zip (*exploding concatenate) map (sum) map force)

(test "Blocks can mess with stack" '(1 2 3 4 5)
  5 range ((1) call add) map force)

(test "Primes 1" 1161
  100 range (2 add) map (dup 2 subtract range (*restoring 2 add mod 0 neq) map all) filter sum)

(test "Primes 2" 1161
  102 range-from-1 butfirst (gcd 1 neq) uniq-by sum)

(test "Five queens 1" 10
  5 range permutations (with-index dup outer flatten (flatten) map (*restoring *exploding arg-c eq arg-c arg-a subtract abs arg-d arg-b subtract abs neq or) map all) filter length)

(test-with-stack "Five queens 2" 10 (5)
  dup range permutations (*restoring with-index dup (*restoring *exploding add) map uniq length arg-b eq swap (*restoring *exploding subtract) map uniq length arg-b eq and) filter length)

;; (test-with-stack "Partial sudoku" '(6 5 5 7 5 8 5 4 4 7 7 7 7 4 7 5 6 6 6 6 6 7 6 8 7 7 7 5 5 4 8 4 5 6 6 6 5 5 4 8 4 5 5 5 6 6 6 6 8 6 7 6 5 6 6 8 6 8 6 7 7 7 7 6 7 6 6 5 7 7 7 7 5 5 5 7 4 6 6 6 6)
;;     ((#((#(0 0 4 0 0 0 0 0 8)) (#(0 9 8 3 0 0 7 0 0)) (#(5 1 0 7 0 9 0 0 4)) (#(0 0 0 5 0 2 0 0 0)) (#(0 5 0 0 0 0 0 6 0)) (#(4 0 0 6 0 1 0 0 7)) (#(7 0 0 4 0 6 0 8 2)) (#(0 0 5 9 0 0 3 4 0)) (#(8 0 0 0 0 0 9 0 0)))))
;;     (dup 3 mod subtract dup 3 add swap) swap 9 range dup outer flatten (*restoring *exploding drop drop arg-a get arg-c transpose arg-b get concatenate arg-b arg-d call arg-c (*restoring rot substr) map force arg-a arg-d call substr flatten rot drop drop concatenate uniq (*restoring 0 neq) filter length) map force)

