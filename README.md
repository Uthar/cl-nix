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
(nix:init-gc)

(defparameter eval-state (nix:make-eval-state store))

(defparameter foo (nix:make-derivation store "foo" "x86_64-linux" "/bin/sh"))

(defparameter foo-path (nix:write-derivation foo store))

(defparameter result (nix:build-derivation store foo-path foo))

(nix:to-string result)
```
