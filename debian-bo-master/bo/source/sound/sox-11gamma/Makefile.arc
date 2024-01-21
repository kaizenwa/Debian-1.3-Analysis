# Project:   sox10


# Toolflags:
CCflags = -c -pcc -depend !Depend -throwback -IUnixLib: -JUnixLib: -DARM
C++flags = -c -depend !Depend -throwback -IC:
Pascalflags = -c -depend !Depend -throwback -IP:
Linkflags = -c++ -o $@
ObjAsmflags = -depend !Depend -ThrowBack -Stamp -quit -CloseExec
CMHGflags = 
LibFileflags = -c -o $@
Squeezeflags = -o $@
AAsmflags = -depend !Depend -quit -CloseExec -To $@ -From
Drlinkflags = -aif -nounused -debimage -o $@ 


# Final targets:
@.sox:   @.o.8svx @.o.aiff @.o.au @.o.auto @.o.avg @.o.band @.o.copy @.o.cdr @.o.cut @.o.armceil \
        @.o.copy @.o.dat @.o.dyn @.o.echo @.o.armfloor @.o.g711 @.o.g721 @.o.g723_24 @.o.g723_40 \
        @.o.g72x @.o.getopt @.o.handlers @.o.hcom @.o.highp @.o.libst @.o.lowp @.o.misc \
        @.o.pred @.o.rate @.o.raw @.o.resample @.o.reverse @.o.sf @.o.smp @.o.sndrtool @.o.sox @.o.stat \
        @.o.vibro @.o.voc @.o.wav @.o.maud.o @.o.wve @.o.util
        drlink $(drlinkflags) @.o.8svx @.o.aiff @.o.au @.o.auto @.o.avg @.o.band \
        @.o.cdr @.o.armceil @.o.copy @.o.dat @.o.dyn @.o.echo @.o.armfloor @.o.g711 @.o.g721 \
        @.o.g723_24 @.o.g723_40 @.o.g72x @.o.getopt @.o.handlers @.o.hcom @.o.highp @.o.libst \
        @.o.lowp @.o.misc @.o.pick @.o.pred @.o.rate @.o.raw @.o.resample @.o.reverse @.o.sf @.o.smp @.o.sndrtool \
        @.o.sox @.o.split @.o.stat @.o.vibro @.o.voc @.o.wav @.o.maud.o \
	@.o.wve @.o.util UnixLib:o.UnixLib


# User-editable dependencies:

# Static dependencies:
@.o.8svx:   @.c.8svx
        cc $(ccflags) -o @.o.8svx @.c.8svx 
@.o.aiff:   @.c.aiff
        cc $(ccflags) -o @.o.aiff @.c.aiff 
@.o.au:   @.c.au
        cc $(ccflags) -o @.o.au @.c.au 
@.o.auto:   @.c.auto
        cc $(ccflags) -o @.o.auto @.c.auto 
@.o.avg:   @.c.avg
        cc $(ccflags) -o @.o.avg @.c.avg 
@.o.band:   @.c.band
        cc $(ccflags) -o @.o.band @.c.band 
@.o.copy:   @.c.copy
        cc $(ccflags) -o @.o.copy @.c.copy 
@.o.cut:   @.c.cut
        cc $(ccflags) -o @.o.cut @.c.cut 
@.o.armceil:   @.c.armceil
        cc $(ccflags) -o @.o.armceil @.c.armceil 
@.o.copy:   @.c.copy
        cc $(ccflags) -o @.o.copy @.c.copy 
@.o.dat:   @.c.dat
        cc $(ccflags) -o @.o.dat @.c.dat 
@.o.dyn:   @.c.dyn
        cc $(ccflags) -o @.o.dyn @.c.dyn 
@.o.echo:   @.c.echo
        cc $(ccflags) -o @.o.echo @.c.echo 
@.o.armfloor:   @.c.armfloor
        cc $(ccflags) -o @.o.armfloor @.c.armfloor 
@.o.g711:   @.c.g711
        cc $(ccflags) -o @.o.g711 @.c.g711 
@.o.g721:   @.c.g721
        cc $(ccflags) -o @.o.g721 @.c.g721 
@.o.g723_24:   @.c.g723_24
        cc $(ccflags) -o @.o.g723_24 @.c.g723_24 
@.o.g723_40:   @.c.g723_40
        cc $(ccflags) -o @.o.g723_40 @.c.g723_40 
@.o.g72x:   @.c.g72x
        cc $(ccflags) -o @.o.g72x @.c.g72x 
@.o.getopt:   @.c.getopt
        cc $(ccflags) -o @.o.getopt @.c.getopt 
@.o.handlers:   @.c.handlers
        cc $(ccflags) -o @.o.handlers @.c.handlers 
@.o.hcom:   @.c.hcom
        cc $(ccflags) -o @.o.hcom @.c.hcom 
@.o.highp:   @.c.highp
        cc $(ccflags) -o @.o.highp @.c.highp 
@.o.libst:   @.c.libst
        cc $(ccflags) -o @.o.libst @.c.libst 
@.o.lowp:   @.c.lowp
        cc $(ccflags) -o @.o.lowp @.c.lowp 
@.o.map:   @.c.map
        cc $(ccflags) -o @.o.map @.c.map 
@.o.misc:   @.c.misc
        cc $(ccflags) -o @.o.misc @.c.misc 
@.o.maud:   @.c.maud
        cc $(ccflags) -o @.o.maud @.c.maud 
@.o.pick:   @.c.pick
        cc $(ccflags) -o @.o.pick @.c.pick 
@.o.pred:   @.c.pred
        cc $(ccflags) -o @.o.pred @.c.pred 
@.o.rate:   @.c.rate
        cc $(ccflags) -o @.o.rate @.c.rate 
@.o.raw:   @.c.raw
        cc $(ccflags) -o @.o.raw @.c.raw 
@.o.resample:   @.c.resample
        cc $(ccflags) -o @.o.resample @.c.resample 
@.o.reverse:   @.c.reverse
        cc $(ccflags) -o @.o.reverse @.c.reverse 
@.o.sf:   @.c.sf
        cc $(ccflags) -o @.o.sf @.c.sf 
@.o.smp:   @.c.smp
        cc $(ccflags) -o @.o.smp @.c.smp 
@.o.sndrtool:   @.c.sndrtool
        cc $(ccflags) -o @.o.sndrtool @.c.sndrtool 
@.o.sox:   @.c.sox
        cc $(ccflags) -o @.o.sox @.c.sox 
@.o.split:   @.c.split
        cc $(ccflags) -o @.o.split @.c.split 
@.o.stat:   @.c.stat
        cc $(ccflags) -o @.o.stat @.c.stat 
@.o.util:   @.c.util
        cc $(ccflags) -o @.o.util @.c.util 
@.o.vibro:   @.c.vibro
        cc $(ccflags) -o @.o.vibro @.c.vibro 
@.o.voc:   @.c.voc
        cc $(ccflags) -o @.o.voc @.c.voc 
@.o.wav:   @.c.wav
        cc $(ccflags) -o @.o.wav @.c.wav 
@.o.wve:   @.c.wve
        cc $(ccflags) -o @.o.wve @.c.wve 


# Dynamic dependencies:

