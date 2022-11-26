#include <map>
#include <string>

#include <clasp/clasp.h>
#include <nix/config.h>
#include <nix/eval.hh>
#include <nix/value.hh>
#include <nix/store-api.hh>

PACKAGE_USE("COMMON-LISP");
NAMESPACE_PACKAGE_ASSOCIATION(expr, exprPkg, "NIX.EXPR");

std::map<std::string, std::string> foo() {
  std::map<std::string, std::string> map;
  map["foo"] = "bar";
  map["baz"] = "quux";
  return map;
}

std::map<std::string, std::string> foo2(std::map<std::string, std::string> arg) {
  std::map<std::string, std::string> map;
  map["foo"] = arg["bar"];
  map["baz"] = arg["quux"];
  return map;
}

// std::list<std::string> bar() {
//   std::list<std::string> list;
//   list.push_back("bar");
//   return list;
// }

// std::list<std::string> bar2(std::list<std::string> arg) {
//   std::list<std::string> list;
//   list.insert(arg);
//   list.push_back("bar");
//   return list;
// }

std::string baz (std::string arg) {
  return arg + "baz";
};

class NixObject {
private:
  void * opaque;
};

namespace translate
{
  
  template <typename K, typename V>
  struct to_object<const std::map<K,V> &>
  {
    static core::T_sp convert(const std::map<K,V> &arg)
    {
      auto equal = core::lisp_intern("EQUAL","COMMON-LISP");
      auto table = core::HashTable_O::create(equal);
      for (const auto& [key, value] : arg)
        table->setf_gethash(to_object<K>::convert(key),
                            to_object<V>::convert(value));
      return table;
      
    }
  };

  // template <>
  // struct to_object<std::list<std::string>>
  // {
  //   static core::T_sp convert(std::list<std::string>> arg)
  //   {
  //     auto nil = core::lisp_intern("NIL","COMMON-LISP");
  //     auto list = core::Cons_O::createList("foo", nil);
  //     for (const auto& value : arg)
  //       list = core::Cons_O::create("foo",list);
  //     return list;
      
  //   }
  // };

  template <typename K, typename V>
  struct from_object<std::map<K,V>>
  {
    typedef std::map<K,V> DeclareType;
    DeclareType _v;                                    
    from_object(core::T_sp obj)
    {
      if (core::cl__hash_table_p(obj)) {
        this->_v = std::map<K,V>();
        gc::As<core::HashTable_sp>(obj)->maphash([this](core::T_sp k, core::T_sp v) {
          this->_v[from_object<K>(k)._v] = from_object<V>(v)._v;
        });
      } else {
        TYPE_ERROR(obj,cl::_sym_HashTable_O);
      }
    }
  };
  
};

namespace expr {
CL_EXPOSE
void libexpr_startup() {
  using namespace clbind;
  package_ pkg(exprPkg);
  scope_ &s = pkg.scope();

  // class_<nix::SearchPath>(s, "search-path");
  // class_<nix::StorePath>(s, "store-path");
  // class_<nix::Store>(sa, "store");
  // class_<nix::Env>(s, "env");
  // class_<nix::PrimOp>(s, "primop");
  // class_<nix::Expr>(s, "expr");
  // class_<nix::Value>(s, "value");
  // class_<nix::ValueType>(s, "value-type");
  // class_<nix::SearchPathElem>(s, "search-path-elem");
  // class_<nix::EvalSettings>(s, "eval-settings");
  // class_<nix::Strings>(s, "strings");
  // class_<nix::Setting>(s, "setting");

  // class_<nix::EvalState>(s, "eval-state")
    // .def_constructor("make-eval-state",
                     // constructor<const nix::Strings&, nix::ref<nix::Store>>())
    // .def("eval",&nix::EvalState::eval)
    // .def("print-stats",&nix::EvalState::printStats);

  // class_<nix::Store>(s, "store");
  
  

  
  // pkg.def("open-store",&nix::openStore);

  // pkg.def("foo",&foo);
  // pkg.def("foo2",&foo2);
  // pkg.def("bar",&bar);
  // pkg.def("baz",&baz);
  
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
  
  // class_<nix::Expr>(s, "expr");

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
