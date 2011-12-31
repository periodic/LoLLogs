#!/bin/bash

rm -f *dll*
rm -f *.o 
rm -f Parser_stub* 
ghc -c StartEnd.c 
ghc -package ghc --make -shared -o Parser.dll Parser.hs StartEnd.o Parser.def -optl=--enable-stdcall-fixup
