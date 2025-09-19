#!/usr/bin/env sbcl --script
;; DILIGAF Test Suite
;; Test the core functionality of the DILIGAF language

(load "diligaf.lisp")

(defun run-tests ()
  "Run all DILIGAF tests"
  (format t "Running DILIGAF Test Suite...~%")
  (format t "============================~%~%")
  
  (let ((passed 0)
        (failed 0))
    
    ;; Test basic arithmetic
    (format t "Testing basic arithmetic...~%")
    (if (= (eval-expr '(+ 1 2 3) *diligaf*) 6)
        (progn (incf passed) (format t "✓ Addition test passed~%"))
        (progn (incf failed) (format t "✗ Addition test failed~%")))
    
    (if (= (eval-expr '(* 4 5) *diligaf*) 20)
        (progn (incf passed) (format t "✓ Multiplication test passed~%"))
        (progn (incf failed) (format t "✗ Multiplication test failed~%")))
    
    ;; Test variables
    (format t "~%Testing variables...~%")
    (eval-expr '(define x 42) *diligaf*)
    (if (= (eval-expr 'x *diligaf*) 42)
        (progn (incf passed) (format t "✓ Variable definition test passed~%"))
        (progn (incf failed) (format t "✗ Variable definition test failed~%")))
    
    ;; Test functions
    (format t "~%Testing functions...~%")
    (eval-expr '(define square (lambda (x) (* x x))) *diligaf*)
    (if (= (eval-expr '(square 5) *diligaf*) 25)
        (progn (incf passed) (format t "✓ Function definition test passed~%"))
        (progn (incf failed) (format t "✗ Function definition test failed~%")))
    
    ;; Test conditionals
    (format t "~%Testing conditionals...~%")
    (if (eval-expr '(if (> 5 3) t nil) *diligaf*)
        (progn (incf passed) (format t "✓ Conditional test passed~%"))
        (progn (incf failed) (format t "✗ Conditional test failed~%")))
    
    ;; Test lists
    (format t "~%Testing lists...~%")
    (let ((result (eval-expr '(list 1 2 3) *diligaf*)))
      (if (equal result '(1 2 3))
          (progn (incf passed) (format t "✓ List creation test passed~%"))
          (progn (incf failed) (format t "✗ List creation test failed~%"))))
    
    (if (= (eval-expr '(car (list 1 2 3)) *diligaf*) 1)
        (progn (incf passed) (format t "✓ Car test passed~%"))
        (progn (incf failed) (format t "✗ Car test failed~%")))
    
    (if (equal (eval-expr '(cdr (list 1 2 3)) *diligaf*) '(2 3))
        (progn (incf passed) (format t "✓ Cdr test passed~%"))
        (progn (incf failed) (format t "✗ Cdr test failed~%")))
    
    ;; Test XOR encryption
    (format t "~%Testing XOR encryption...~%")
    (let ((data "hello")
          (key "key")
          (encrypted (diligaf-xor data key))
          (decrypted (diligaf-xor encrypted key)))
      (if (string= data decrypted)
          (progn (incf passed) (format t "✓ XOR encryption test passed~%"))
          (progn (incf failed) (format t "✗ XOR encryption test failed~%"))))
    
    ;; Test obfuscation
    (format t "~%Testing obfuscation...~%")
    (let ((code "alert('hello')")
          (obfuscated (diligaf-obfuscate code :base64)))
      (if (not (string= code obfuscated))
          (progn (incf passed) (format t "✓ Obfuscation test passed~%"))
          (progn (incf failed) (format t "✗ Obfuscation test failed~%")))
    
    ;; Test polymorphic code
    (format t "~%Testing polymorphic code...~%")
    (let ((code "alert('hello')")
          (polymorph (diligaf-polymorph code)))
      (if (not (string= code polymorph))
          (progn (incf passed) (format t "✓ Polymorphic code test passed~%"))
          (progn (incf failed) (format t "✗ Polymorphic code test failed~%")))
    
    ;; Test payload generation
    (format t "~%Testing payload generation...~%")
    (let ((payload (diligaf-payload :reverse-shell :host "192.168.1.100" :port 4444)))
      (if (and (stringp payload) (search "192.168.1.100" payload))
          (progn (incf passed) (format t "✓ Payload generation test passed~%"))
          (progn (incf failed) (format t "✗ Payload generation test failed~%")))
    
    ;; Test fuzzing
    (format t "~%Testing fuzzing...~%")
    (let ((results (diligaf-fuzz "target.com" "GET /FUZZ HTTP/1.1" 5)))
      (if (and (listp results) (= (length results) 5))
          (progn (incf passed) (format t "✓ Fuzzing test passed~%"))
          (progn (incf failed) (format t "✗ Fuzzing test failed~%")))
    
    ;; Test self-modification
    (format t "~%Testing self-modification...~%")
    (let ((original "alert('hello')")
          (modified (diligaf-self-modify original)))
      (if (not (string= original modified))
          (progn (incf passed) (format t "✓ Self-modification test passed~%"))
          (progn (incf failed) (format t "✗ Self-modification test failed~%")))
    
    ;; Test macros
    (format t "~%Testing macros...~%")
    (eval-expr '(defmacro test-macro (x) `(list 'test ,x)) *diligaf*)
    (let ((result (eval-expr '(test-macro 42) *diligaf*)))
      (if (equal result '(test 42))
          (progn (incf passed) (format t "✓ Macro test passed~%"))
          (progn (incf failed) (format t "✗ Macro test failed~%")))
    
    ;; Test hot-swapping
    (format t "~%Testing hot-swapping...~%")
    (eval-expr '(define test-func (lambda () "original")) *diligaf*)
    (diligaf-hot-swap 'test-func '(lambda () "modified"))
    (let ((result (eval-expr '(test-func) *diligaf*)))
      (if (string= result "modified")
          (progn (incf passed) (format t "✓ Hot-swapping test passed~%"))
          (progn (incf failed) (format t "✗ Hot-swapping test failed~%")))
    
    ;; Print results
    (format t "~%============================~%")
    (format t "Test Results:~%")
    (format t "Passed: ~a~%" passed)
    (format t "Failed: ~a~%" failed)
    (format t "Total:  ~a~%" (+ passed failed))
    
    (if (= failed 0)
        (progn
          (format t "~%✅ All tests passed! DILIGAF is ready for hacking.~%")
          t)
        (progn
          (format t "~%❌ Some tests failed. Check the output above.~%")
          nil))))

(defun main ()
  "Main test function"
  (init-diligaf)
  (run-tests))

;; Run tests
(main)
