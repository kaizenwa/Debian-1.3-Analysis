Root=BinPath
echo Setting up structure for variant translator...
BaseDir=`pwd`
CommonDir=$BaseDir/common
TranDir=$BaseDir/itran
HDir=$BaseDir/h
rm -rf $CommonDir $TranDir $HDir
mkdir $CommonDir $TranDir $HDir
echo Copying files ...
cp $Root../src/vtran/vtfiles/"*" $BaseDir
mv $BaseDir/vtmain.c $TranDir/vtmain.c
mv $BaseDir/vgrammar.c $TranDir/vgrammar.c
cp $Root../src/vtran/Vtmake1 $BaseDir/Makefile
cp $Root../src/vtran/Vtmake2 $TranDir/Makefile
for j in getopt.c alloc.c strtbl.c filepart.c ipp.c yylex.h error.h Makefile \
	tokens.txt op.txt mktoktab.icn fixgram.icn pscript.icn
do
   cp $Root../src/common/$j $CommonDir
done
for j in keyword.h lfile.h tglobals.h tproto.h tree.h tsym.h ttoken.h \
	tlex.c tlocal.c tmem.c trans.c tree.c tsym.c util.c trash.icn
do
   cp $Root../src/icont/$j $TranDir
done
rm -f $TranDir/tgram.c
for j in config.h cpuconf.h define.h esctab.h fdefs.h features.h kdefs.h \
	mproto.h proto.h version.h cstructs.h gsupport.h path.h\
	grammar.h lexdef.h typedefs.h dproto.h parserr.h
do
   cp $Root../src/h/$j $HDir
done
echo variant translator is complete.
