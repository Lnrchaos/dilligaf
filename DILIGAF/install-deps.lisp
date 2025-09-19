#!/usr/bin/env sbcl --script
;; DILIGAF Dependencies Installation Script
;; This script installs all required dependencies for DILIGAF

(load "~/.quicklisp/setup.lisp")

;; Required packages for DILIGAF
(defvar *required-packages*
  '(:uiop
    :split-sequence
    :base64
    :drakma
    :ironclad
    :cl-ppcre
    :babel
    :flexi-streams
    :trivial-features
    :cffi
    :osicat
    :cl-fad
    :cl-json
    :cl-yaml
    :cl-html5
    :cl-css
    :cl-who
    :hunchentoot
    :cl-http
    :drakma
    :cl-json
    :cl-base64
    :cl-ppcre
    :babel
    :flexi-streams
    :trivial-features
    :cffi
    :osicat
    :cl-fad
    :cl-json
    :cl-yaml
    :cl-html5
    :cl-css
    :cl-who
    :hunchentoot
    :cl-http))

(defun install-package (package)
  "Install a single package"
  (format t "Installing ~a...~%" package)
  (handler-case
      (progn
        (ql:quickload package)
        (format t "✓ ~a installed successfully~%" package))
    (error (e)
      (format t "✗ Failed to install ~a: ~a~%" package e))))

(defun install-all-packages ()
  "Install all required packages"
  (format t "Installing DILIGAF dependencies...~%")
  (format t "================================~%~%")
  
  (dolist (package *required-packages*)
    (install-package package))
  
  (format t "~%================================~%")
  (format t "Dependency installation complete!~%"))

(defun main ()
  "Main installation function"
  (format t "DILIGAF Dependency Installer~%")
  (format t "============================~%~%")
  
  (install-all-packages)
  
  (format t "~%DILIGAF is ready to use!~%")
  (format t "Run: sbcl --script diligaf.lisp~%"))

;; Run installation
(main)
