%module cudd

%feature("intern_function","1");
%feature("export");

%insert("lisphead")%{

(cl:in-package :scully.cudd)

(cffi:define-foreign-library scully.zdd::cudd
  (:darwin "./build/libcudd.dylib"))

(cffi:use-foreign-library scully.zdd::cudd)

%}

%inline %{

typedef unsigned int size_t;

%}

%include "build/cudd-3.0.0/cudd/cudd.h"
