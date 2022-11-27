#include <map>
#include <string>

#include <clasp/clasp.h>
#include <nix/config.h>
#include <nix/eval.hh>
#include <nix/value.hh>
#include <nix/store-api.hh>

PACKAGE_USE("COMMON-LISP");
PACKAGE_NICKNAME("NIX");
NAMESPACE_PACKAGE_ASSOCIATION(cl_nix_ns, cl_nix_pkg, "CL-NIX");

namespace cl_nix {
CL_EXPOSE
void cl_nix_startup() {
  using namespace clbind;
  package_ pkg(cl_nix_pkg);
  scope_ &s = pkg.scope();

  // Rough list of headers, going from the bottom up:

  //// libexpr
  // [x] ref.hh
  // [x] types.hh
  // [x] chunked-vector.hh
  // [ ] symbol-table.hh
  // [ ] value.hh
  // [ ] ansicolor.hh
  // [ ] fmt.hh
  // [ ] suggestions.hh
  // [ ] error.hh
  // [ ] nixexpr.hh
  // [ ] config.hh
  // [ ] comparator.hh
  // [ ] experimental-features.hh
  // [ ] eval.hh
  // [ ] attr-path.hh
  // [ ] attr-set.hh
  // [ ] sync.hh
  // [ ] util.hh
  // [ ] serialise.hh
  // [ ] hash.hh
  // [ ] eval-cache.hh
  // [ ] eval-inline.hh
  // [ ] function-trace.hh
  // [ ] content-address.hh
  // [ ] path.hh
  // [ ] get-drvs.hh
  // [ ] json-to-value.hh
  // [ ] primops.hh
  // [ ] value-to-json.hh
  // [ ] value-to-xml.hh

  //// libstore
  // [ ] crypto.hh
  // [ ] path-info.hh
  // [ ] nar-info.hh
  // [ ] realisation.hh
  // [ ] derived-path.hh
  // [ ] lru-cache.hh
  // [ ] config.hh
  // [ ] globals.hh
  // [ ] repair-flag.hh
  // [ ] store-api.hh
  // [ ] log-store.hh
  // [ ] pool.hh
  // [ ] binary-cache-store.hh
  // [ ] build-result.hh
  // [ ] derivations.hh
  // [ ] builtins.hh
  // [ ] daemon.hh
  // [ ] filetransfer.hh
  // [ ] fs-accessor.hh
  // [ ] gc-store.hh
  // [ ] local-fs-store.hh
  // [ ] pathlocks.hh
  // [ ] local-store.hh
  // [ ] lock.hh
  // [ ] machines.hh
  // [ ] make-content-addressed.hh
  // [ ] names.hh
  // [ ] nar-accessor.hh
  // [ ] nar-info-disk-cache.hh
  // [ ] parsed-derivations.hh
  // [ ] path-with-outputs.hh
  // [ ] profiles.hh
  // [ ] references.hh
  // [ ] remote-fs-accessor.hh
  // [ ] remote-store.hh
  // [ ] s3.hh
  // [ ] binary-cache-store.hh
  // [ ] s3-binary-cache-store.hh
  // [ ] serve-protocol.hh
  // [ ] sqlite.hh
  // [ ] ssh.hh
  // [ ] store-cast.hh
  // [ ] uds-remote-store.hh
  // [ ] worker-protocol.hh
  // [ ] goal.hh
  // [ ] derivation-goal.hh
  // [ ] drv-output-substitution-goal.hh
  // [ ] hook-instance.hh
  // [ ] local-derivation-goal.hh
  // [ ] substitution-goal.hh
  // [ ] worker.hh

  //// libmain
  // [ ] args.hh
  // [ ] common-args.hh
  // [ ] loggers.hh
  // [ ] progress-bar.hh
  // [ ] shared.hh

  //// libutil
  // [ ] archive.hh
  // [ ] compression.hh
  // [ ] compute-levels.hh
  // [ ] json.hh
  // [ ] json-utils.hh
  // [ ] logging.hh
  // [ ] monitor-fd.hh
  // [ ] suggestions.hh
  // [ ] tarfile.hh
  // [ ] thread-pool.hh
  // [ ] topo-sort.hh
  // [ ] url.hh
  // [ ] url-parts.hh
  // [ ] xml-writer.hh

  //// flakes
  // [ ] flakeref.hh
  // [ ] flake.hh
  // [ ] lockfile.hh

  //// libcmd
  // [ ] installables.hh
  // [ ] common-eval-args.hh
  // [ ] legacy.hh
  // [ ] markdown.hh

  //// libfetchers
  // [ ] attrs.hh
  // [ ] fetchers.hh
  // [ ] cache.hh
  // [ ] fetch-settings.hh
  // [ ] registry.hh

  //// Commands
  // [ ] add-to-store.cc
  // [ ] app.cc
  // [ ] build.cc
  // [ ] bundle.cc
  // [ ] cat.cc
  // [ ] copy.cc
  // [ ] daemon.cc
  // [ ] describe-stores.cc
  // [ ] develop.cc
  // [ ] diff-closures.cc
  // [ ] doctor.cc
  // [ ] dump-path.cc
  // [ ] edit.cc
  // [ ] eval.cc
  // [ ] flake.cc
  // [ ] fmt.cc
  // [ ] hash.cc
  // [ ] log.cc
  // [ ] ls.cc
  // [ ] main.cc
  // [ ] make-content-addressed.cc
  // [ ] nar.cc
  // [ ] optimise-store.cc
  // [ ] path-from-hash-part.cc
  // [ ] path-info.cc
  // [ ] ping-store.cc
  // [ ] prefetch.cc
  // [ ] profile.cc
  // [ ] realisation.cc
  // [ ] registry.cc
  // [ ] run.cc
  // [ ] search.cc
  // [ ] show-config.cc
  // [ ] show-derivation.cc
  // [ ] sigs.cc
  // [ ] store.cc
  // [ ] store-copy-log.cc
  // [ ] store-delete.cc
  // [ ] store-gc.cc
  // [ ] store-repair.cc
  // [ ] upgrade-nix.cc
  // [ ] verify.cc
  // [ ] why-depends.cc
  
  class_<nix::ref<nix::Store>>(s, "store-ref");

  pkg.def(
    "open-store",
    +[](std::string url) {
      nix::ref<nix::Store> res = nix::openStore(url);
      return res;
    });

  class_<nix::Value>(s, "value")
    .def("lambda-p",&nix::Value::isLambda)
    .def("primop-p",&nix::Value::isPrimOp)
    .def("app-p",&nix::Value::isApp)
    .def("trivial-p",&nix::Value::isTrivial);
  
  pkg.def("init-gc",&nix::initGC);

  pkg.def(
    "eval-expr",
    +[](std::string e, nix::ref<nix::Store> store) {
      auto searchPath = std::list<std::string>();
      auto state = nix::EvalState(searchPath, store);
      auto expr = state.parseExprFromString(e, "/home/kasper/");
      nix::Value value;
      state.eval(expr, value);
      return value;
    });

  // pkg.def("add-to-search-path",&nix::EvalState::addToSearchPath);
  // pkg.def("get-search-path",&nix::EvalState::getSearchPath);
  // pkg.def("allow-path", void(*)(const &nix::Path) &nix::EvalState::allowPath);
  // pkg.def("allow-path*", void(*)(const &nix::StorePath) &nix::EvalState::allowPath);
  // pkg.def("check-source-path",&nix::EvalState::checkSourcePath);  
  // pkg.def("check-uri",&nix::EvalState::checkURI);  
  // pkg.def("to-real-path",&nix::EvalState::toRealPath);
  // pkg.def("parse-expr-from-afile", nix::Expr(*)(const &nix::Path) &nix::EvalState:parseExprFromFile);
  // pkg.def("parse-expr-from-file*", nix::Expr(*)(const &nix::Path, &std::shared_ptr<nix::StaticEnv>)
          // &nix::EvalState:parseExprFromFile);
  // pkg.def("parse-expr-from-string", nix::Expr(*)(const &nix::Path) &nix::EvalState:parseExprFromString);
  // pkg.def("parse-expr-from-string*", nix::Expr(*)(const &nix::Path, &std::shared_ptr<nix::StaticEnv>)
          // &nix::EvalState:parseExprFromString);
  // pkg.def("parse-stdin",&nix::EvalState::parseStdin);
  // pkg.def("eval-file",&nix::EvalState::evalFile);
  // pkg.def("cache-file",&nix::EvalState::cacheFile);
  // pkg.def("reset-file-cache",&nix::EvalState::resetFileCache);
  // pkg.def("find-file", nix::Path(*)(const std::string_view) &nix::EvalState:findFile);
  // pkg.def("find-file*", nix::Path(*)(&nix::SearchPath, const std::string_view) &nix::EvalState:findFile);
  // pkg.def("resolve-search-path-elem",&nix::EvalState::resolveSearchPathElem);
  // pkg.def("eval",&nix::EvalState::eval);
  // pkg.def("force-value",&nix::EvalState::forceValue);
  // pkg.def("force-value-deep",&nix::EvalState::forceValueDeep);
  // pkg.def("derivation-p",&nix::EvalState::isDerivation);
  // pkg.def("try-attrs-to-string",&nix::EvalState::tryAttrsToString);
  // pkg.def("coerce-to-string",&nix::EvalState::coerceToString);
  // pkg.def("copy-path-to-store",&nix::EvalState::copyPathToStore);
  // pkg.def("coerce-to-path",&nix::EvalState::coerceToPath);
  // pkg.def("coerce-to-store-path",&nix::EvalState::coerceToStorePath);
  // pkg.def("get-builtin",&nix::EvalState::getBuiltin);
  // pkg.def("get-doc",&nix::EvalState::getDoc);
  // pkg.def("values=",&nix::EvalState::eqValues);
  // pkg.def("functor-p",&nix::EvalState::isFunctor);
  // pkg.def("call-function", void(*)(&nix::Value, &nix::Value, const nix::PosIdx)
          // &nix::EvalState:callFunction);
  // pkg.def("auto-call-function",&nix::EvalState::autoCallFunction);
  // pkg.def("print-stats",&nix::EvalState::printStats);

  // pkg.def("show-type-value-type", std::string_view(*)(nix::ValueType) nix::showType);
  // pkg.def("show-type-value", std::string(*)(&const nix::Value) nix::showType);
  // pkg.def("decode-context",&nix::decodeContext);
  // pkg.def("resolve-expr-path",&nix::resolveExprPath);

}
}; // namespace expr
