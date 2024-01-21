#!/bin/sh

MAJORVER=`grep NASM_MAJOR_VER nasm.h | head -1 | cut -f3 -d' '`
MINORVER=`grep NASM_MINOR_VER nasm.h | head -1 | cut -f3 -d' '`
VERSION="${MAJORVER}.${MINORVER}"
DOSVERSION="${MAJORVER}${MINORVER}"
NASM_TAR_GZ=dist/nasm-${VERSION}.tar.gz
NASM_ZIP=dist/nasm${DOSVERSION}s.zip
NASM_DOS_ZIP=dist/nasm${DOSVERSION}.zip

if [ -d dist ]; then rm -rf dist; fi
if [ -d nasm-${VERSION} ]; then rm -rf nasm-${VERSION}; fi
if [ ! -d dist ]; then mkdir dist; fi
if [ -f dist/nasm.tar.gz ]; then rm dist/nasm.tar.gz; fi
mkdir nasm-${VERSION}
(cd nasm-${VERSION}; ln -s ../* .; rm nasm-${VERSION} dist Checklist)
tar chvf dist/nasm-${VERSION}.tar nasm-${VERSION}
gzip -9 dist/nasm-${VERSION}.tar
rm -rf nasm-${VERSION}
zip -l -k ${NASM_ZIP} Cha* L* R* M* insns.[dp]* *.c *.h *.doc \
                      lcc/* misc/* rdoff/* test/*
zip -k ${NASM_ZIP} *.exe
zip -l -k ${NASM_DOS_ZIP} Cha* L* R* *.doc lcc/* misc/* rdoff/* test/*
zip -k ${NASM_DOS_ZIP} *.exe
echo Distributions complete.
