nix
==========

Nix C++ bindings for Common Lisp

To build, do the following steps:

1. Clone this repository into the clasp/extensions/nix directory
2. Configure the build by running the following in the root of the Clasp repo
   ```sh
   ./koga --extensions=nix
   ```
3. Build Clasp by running
   ```sh
   ninja -C build
   ```

example
=========

- Get Python's `$out`

``` lisp
(defparameter store (nix:open-store "auto"))

(nix:init-gc)

(defparameter eval-state (nix:make-eval-state store))

(defparameter python (nix:eval-expr "with import <nixpkgs> {}; python" eval-state "/"))

(nix:coerce-to-path eval-state python)
```

- Build a derivation

```lisp
(defparameter store (nix:open-store "auto"))
(nix:init-gc)
(defparameter eval-state (nix:make-eval-state store))
(defparameter python (nix:eval-expr "with import <nixpkgs> {}; python310" eval-state "/"))
(defparameter python-store-path (nix:coerce-to-store-path eval-state python))
(defparameter python-path-info (nix:query-path-info store python-store-path))
(defparameter python-deriver (nix:path-info-deriver python-path-info))
(defparameter python-derivation (nix:derivation-from-path store python-deriver))
```

notes
======

```
(defparameter env (make-hash-table :test 'equal))

ENV
COMMON-LISP-USER> (setf (gethash "foo" env) "123")

"123"
COMMON-LISP-USER> (setf (gethash "bar" env) "456")

"456"
COMMON-LISP-USER> (nix:make-derivation store "foo" "x86_64-linux" "/bin/sh" env)

#<wrapped-pointer :ptr 0xbca7600 @0x7f2e884b3968>
COMMON-LISP-USER> (nix:unparse-derivation * store nil)

"Derive([(\"out\",\"/nix/store/qwmnpbqwh7dqjjzfqg0qrwbph4wg2193-foo\",\"\",\"\")],[],[],\"x86_64-linux\",\"/bin/sh\",[],[(\"bar\",\"456\"),(\"foo\",\"123\"),(\"out\",\"/nix/store/qwmnpbqwh7dqjjzfqg0qrwbph4wg2193-foo\")])"

```
