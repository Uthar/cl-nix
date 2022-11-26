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

To run the demos run `./build/boehmprecise/clasp` then execute `(nix:hellp)` in
the Clasp REPL.
