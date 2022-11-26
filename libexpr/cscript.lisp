(k:sources :iclasp
           ;; #~"attr-path.cc"
           ;; #~"attr-set.cc"
           #~"eval.cc"
           ;; #~"eval-cache.cc"
           ;; #~"function-trace.cc"
           ;; #~"get-drvs.cc"
           ;; #~"json-to-value.cc"
           ;; #~"lexer-tab.cc"
           ;; #~"nixexpr.cc"
           ;; #~"parser-tab.cc"
           ;; #~"primops.cc"
           ;; #~"value-to-json.cc"
           ;; #~"value-to-xml.cc"
           )

(k:library "nix-expr" :required t)
(k:library "nix-store" :required t)

(k:systems :nix.expr)
