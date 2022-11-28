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

``` common-lisp
COMMON-LISP-USER> (load "test.lisp")

T
COMMON-LISP-USER> (nix:make-derivation store "foo" "x86_64-linux" "/bin/sh" (make-hash-table))

#<wrapped-pointer :ptr 0xafe0eb0 @0x7f89d46630f8>
COMMON-LISP-USER> (nix:unparse-derivation * store nil)

"Derive([(\"out\",\"/nix/store/kn6k98czgiw4fq0d0a5j9nixpxdlr5hj-foo\",\"\",\"\")],[],[],\"x86_64-linux\",\"/bin/sh\",[],[(\"out\",\"/nix/store/kn6k98czgiw4fq0d0a5j9nixpxdlr5hj-foo\")])"
COMMON-LISP-USER> (nix:write-derivation store **)

#<wrapped-pointer :ptr 0xb06db10 @0x7f89d4687d28>
COMMON-LISP-USER> 
(nix:build-derivation store * ***)
building '/nix/store/z8h0j4aylpi5x6r72phxis7l1nck13lw-foo.drv'...

#<wrapped-pointer :ptr 0xb024c30 @0x7f89d4687a58>
COMMON-LISP-USER> (nix:to-string *)

"OutputRejected : builder for '/nix/store/z8h0j4aylpi5x6r72phxis7l1nck13lw-foo.drv' failed to produce output path for output 'out' at '/nix/store/z8h0j4aylpi5x6r72phxis7l1nck13lw-foo.drv.chroot/nix/store/kn6k98czgiw4fq0d0a5j9nixpxdlr5hj-foo'"
COMMON-LISP-USER> 

```
