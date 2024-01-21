WINLIBS = -lkernel32 -lgdi32 -luser32

CFLAGS  = -g
GUI     = -Wl,-subsystem,windows
CONSOLE =

all: winhello.exe winversion.exe compname.exe

winhello.exe : winhello.pas
	gpc $(CFLAGS) $(LDFLAGS) $(GUI) -o winhello.exe winhello.pas $(WINLIBS)

winversion.exe : winversion.pas
	gpc $(CFLAGS) $(LDFLAGS) $(CONSOLE) -o winversion.exe winversion.pas $(WINLIBS)

compname.exe : compname.pas
	gpc $(CFLAGS) $(LDFLAGS) $(CONSOLE) -o compname.exe compname.pas $(WINLIBS)

clean:
	-rm -f *.exe
	-rm -f *.o
	-rm -f *.gpi
	-rm -f *.gpc

