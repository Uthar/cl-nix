;; (k:recurse #P"libcmd/"
;;            #P"libexpr/"
;;            #P"libfetchers/"
;;            #P"libmain/"
;;            #P"libstore/"
;;            #P"libutil/")

(k:sources :iclasp #~"nix.cc")

(k:library "nix-expr" :required t)
(k:library "nix-store" :required t)

(k:systems :nix)
