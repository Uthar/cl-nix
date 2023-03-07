(in-package nix)

#|

(init-nix)

(init-gc)

(defparameter *store* (open-store "auto"))
(defparameter *eval-state* (make-eval-state *store*))
(defparameter *system* "x86_64-linux")


(defun package (&key name version source build-stage)
  (make-derivation
   *store*
   (concatenate 'string name version)
   *system*
   "/bin/sh"
   (list "-c" build-stage)
   (make-hash-table)))

(defvar python
  (eval-expr "with import <nixpkgs> {}; python310" *eval-state* "/"))

(defvar hello
  (package
   :name "hello"
   :version "1.0.0"
   :source "test"
   :build-stage "echo hello > $out"))

(class-of hello)
(unparse-derivation hello *store* nil)

(defvar store-path (write-derivation *store* hello))

(defvar build-result (build-derivation *store* store-path hello))

(class-of build-result) ;; BUILD-RESULT
(success-p build-result) ;; T
(to-string build-result) ;; Built
   
|#
