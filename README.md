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

``` lisp
(defparameter store (nix:open-store "auto"))

(nix:init-gc)

(defparameter eval-state (nix:make-eval-state store))

(defparameter python (nix:eval-expr "with import <nixpkgs> {}; python" eval-state "/"))

(nix:coerce-to-path eval-state python)
```

