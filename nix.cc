#include <map>
#include <string>
#include <filesystem>

#include <clasp/clasp.h>
#include <nix/config.h>
#include <nix/eval.hh>
#include <nix/value.hh>
#include <nix/store-api.hh>
#include <nix/derivations.hh>

namespace translate {
  // string_view
  template <>
    struct to_object< std::string_view, translate::dont_adopt_pointer >
  {
    typedef std::string_view & DeclareType;
    static core::T_sp convert( DeclareType & v )
    {
      core::T_sp oi = core::str_create( std::string(v) );
      return ( oi );
    }
  };
  template <>
    struct from_object<std::string_view, std::true_type >
  {
    typedef std::string_view DeclareType;
    DeclareType _v;
    from_object( T_P o ) {
      _v = std::move(std::string_view(string_get_std_string(o)));
    };
  };
}

PACKAGE_USE("COMMON-LISP");
PACKAGE_NICKNAME("NIX");
NAMESPACE_PACKAGE_ASSOCIATION(cl_nix_ns, cl_nix_pkg, "CL-NIX");

SYMBOL_EXPORT_SC_(cl_nix_pkg,STARhashTypeTranslatorSTAR);                             
CLBIND_TRANSLATE_SYMBOL_TO_ENUM(nix::HashType, cl_nix_ns::_sym_STARhashTypeTranslatorSTAR);

SYMBOL_EXPORT_SC_(cl_nix_pkg,STARbaseTranslatorSTAR);                             
CLBIND_TRANSLATE_SYMBOL_TO_ENUM(nix::Base, cl_nix_ns::_sym_STARbaseTranslatorSTAR);

SYMBOL_EXPORT_SC_(cl_nix_pkg,STARvalueTypeTranslatorSTAR);                             
CLBIND_TRANSLATE_SYMBOL_TO_ENUM(nix::ValueType, cl_nix_ns::_sym_STARvalueTypeTranslatorSTAR);

namespace cl_nix {
CL_EXPOSE
void cl_nix_startup() {
  using namespace clbind;
  package_ pkg(cl_nix_pkg);
  scope_ &s = pkg.scope();

  // Rough list of headers, going from the bottom up:

  //// libexpr
  // [ ] ref.hh
  // [ ] types.hh
  // [ ] chunked-vector.hh
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

  //// Derivations


  enum_<nix::HashType>(s,cl_nix_ns::_sym_STARhashTypeTranslatorSTAR)
    .value("md5",nix::htMD5)
    .value("sha1",nix::htSHA1)
    .value("sha256",nix::htSHA256)
    .value("sha512",nix::htSHA512);

  enum_<nix::Base>(s,cl_nix_ns::_sym_STARbaseTranslatorSTAR)
    .value("base64",nix::Base64)
    .value("base32",nix::Base32)
    .value("base16",nix::Base16)
    .value("sri",nix::SRI);

  enum_<nix::ValueType>(s,cl_nix_ns::_sym_STARvalueTypeTranslatorSTAR)
    .value("thunk",nix::nThunk)
    .value("int",nix::nInt)
    .value("float",nix::nFloat)
    .value("bool",nix::nBool)
    .value("string",nix::nString)
    .value("path",nix::nPath)
    .value("nix-null",nix::nNull)
    .value("attrs",nix::nAttrs)
    .value("nix-list",nix::nList)
    .value("nix-function",nix::nFunction)
    .value("external",nix::nExternal)
    ;

  class_<nix::StorePath>(s, "store-path")
    .def("to-string",&nix::StorePath::to_string)
    .def("name",&nix::StorePath::name)
    .def("hash-part",&nix::StorePath::hashPart)
    .def("derivation-p",&nix::StorePath::isDerivation);

  class_<nix::Hash>(s, "hash")
    .def("git-rev",&nix::Hash::gitRev)
    .def("git-short-rev",&nix::Hash::gitShortRev)
    .def("to-string",&nix::Hash::to_string);

  pkg.def("hash-string",&nix::hashString);
  pkg.def("hash-file",&nix::hashFile);

  // pkg.def(
  //   "make-store-path",
  //   +[](const nix::Hash & hash, std::string_view name) {
  //     return nix::StorePath(hash, name);
  //   });

  pkg.def(
    "make-store-path",
    +[](std::string baseName) {
      return nix::StorePath(baseName);
    });

  // pkg.def(
  //   "make-input-addressed-derivation-output",
  //   +[](nix::StorePath path) {
  //     nix::DerivationOutputInputAddressed output;
  //     output.path = path;
  //     return output;
  //   });

  // pkg.def(
  //   "make-content-addressed-derivation-output",
  //   +[](nix::StorePath path) {
  //     nix::DerivationOutputInputAddressed output;
  //     output.storePath = path;
  //     return output;
  //   });

  // pkg.def(
  //   "make-derivation",
  //   +[](nix::DerivationOutputs outputs,
  //       nix::StorePathSet inputSrcs,
  //       std::string platform,
  //       nix::Path builder,
  //       nix::Strings args,
  //       nix::StringPairs env,
  //       std::string name) {
  //     nix::Derivation drv;
  //   });
  
  
  //// Store

  class_<nix::ref<nix::Store>>(s, "store-ref");
  class_<nix::Derivation>(s, "derivation");

  pkg.def(
    "open-store",
    +[](std::string url) {
      nix::ref<nix::Store> res = nix::openStore(url);
      return res;
    });

  pkg.def(
    "init",
    +[](nix::ref<nix::Store> store) {
      return store->init();
    });

  pkg.def(
    "get-uri",
    +[](nix::ref<nix::Store> store) {
      return store->getUri();
    });

  pkg.def(
    "add-text-to-store",
    +[](nix::ref<nix::Store> store,
        std::string name,
        std::string s
        // const nix::StorePathSet & references
        ) {
      return store->addTextToStore(name, s, {});
    });

  pkg.def(
    "nar-from-path",
    +[](nix::ref<nix::Store> store,
        const nix::StorePath & path,
        nix::Sink & sink) {
      return store->narFromPath(path, sink);
    });

  pkg.def(
    "optimise-store",
    +[](nix::ref<nix::Store> store) {
      return store->optimiseStore();
    });

  pkg.def(
    "connect",
    +[](nix::ref<nix::Store> store) {
      return store->connect();
    });

  pkg.def(
    "get-protocol",
    +[](nix::ref<nix::Store> store) {
      return store->getProtocol();
    });

  pkg.def(
    "parse-derivation",
    +[](nix::ref<nix::Store> store, std::string s, std::string name) {
      return nix::parseDerivation(*store, std::move(s), name);
    });  

  /// Eval
  
  pkg.def("init-gc",&nix::initGC);

  class_<nix::Value>(s, "value")
    .def("lambda-p",&nix::Value::isLambda)
    .def("primop-p",&nix::Value::isPrimOp)
    .def("app-p",&nix::Value::isApp)
    .def("trivial-p",&nix::Value::isTrivial)
    .def("value-type",&nix::Value::type);

  pkg.def(
    "eval-expr",
    +[](std::string e,
        nix::ref<nix::Store> store,
        std::string basePath = std::filesystem::current_path()) {
      auto searchPath = std::list<std::string>();
      auto state = nix::EvalState(searchPath, store);
      auto expr = state.parseExprFromString(e, basePath);
      nix::Value value;
      state.eval(expr, value);
      return value;
    });

}
};
