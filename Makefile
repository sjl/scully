.PHONY: vendor cudd clean

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp

# CUDD ------------------------------------------------------------------------
build/cudd-3.0.0.tar.gz:
	mkdir -p build
	cd build && wget 'ftp://vlsi.colorado.edu/pub/cudd-3.0.0.tar.gz'

build/cudd-3.0.0: build/cudd-3.0.0.tar.gz
	cd build && tar -xzf cudd-3.0.0.tar.gz

build/libcudd.dylib: build/cudd-3.0.0
	cd build/cudd-3.0.0 && ./configure --enable-shared && make
	cp build/cudd-3.0.0/cudd/.libs/libcudd-*.dylib build/libcudd.dylib

src/cudd.lisp: build/cudd-3.0.0 src/cudd.i
	swig -cffi src/cudd.i

cudd: build/libcudd.dylib src/cudd.lisp

# Misc ------------------------------------------------------------------------
clean:
	rm -rf build
