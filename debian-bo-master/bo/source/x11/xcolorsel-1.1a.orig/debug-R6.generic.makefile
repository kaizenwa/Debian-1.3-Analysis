xcolorsel: xcolorsel.o RgbText.o RgbSink.o RgbSrc.o
	cc -g -o xcolorsel xcolorsel.o RgbText.o RgbSrc.o RgbSink.o -lXaw -lXmu -lXt -lSM -lICE -lXext -lX11
RgbText.o: RgbTextP.h RgbText.h RgbSink.h RgbSrc.h config.h RgbText.c
	cc -O2 -g -c RgbText.c
RgbSink.o: RgbSinkP.h RgbSink.h RgbTextP.h RgbText.h config.h RgbSink.c
	cc -O2 -g -c RgbSink.c
RgbSrc.o: RgbSrcP.h RgbSrc.h RgbTextP.h RgbText.h config.h RgbSrc.c config.h
	cc -O2 -g -c RgbSrc.c
xcolorsel.o: xcolorsel.c xcslicon RgbText.h lens lensMask biglens biglensMask defhelp.h config.h appdef-c.h appdef.h
	cc -O2 -g -c xcolorsel.c
appdef-c.h: Xcolorsel-color.ad
	app2head Xcolorsel-color.ad > appdef-c.h
appdef.h: Xcolorsel.ad
	app2head Xcolorsel.ad > appdef.h
clean:
	rm -f *.o *.tar.Z *.tar *.tgz
purge:
	rm -f xcolorsel *.o appdef.h appdef-c.h Xcolorsel.help *.tar.Z Makefile makefile *.tar *.tgz
Xcolorsel.help: xcolorsel.man
	nroff -Tascii -man xcolorsel.man | col -b > Xcolorsel.help
