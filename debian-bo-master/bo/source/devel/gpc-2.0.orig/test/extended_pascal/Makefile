#
# Demos for GPC
#

PC = gpc
PFLAGS = -g
LIBS = # -lm

.SUFFIXES: .pas

.pas.o:
	$(PC) -c $(PFLAGS) -o $@ $<

# EXE = .exe

DEMOS = abs$(EXE)	\
	arctan$(EXE)	\
	conjugate$(EXE)	\
	cos$(EXE)	\
	exp$(EXE)	\
	ln$(EXE)	\
	polar$(EXE)	\
	pow$(EXE)	\
	sin$(EXE)	\
	sqr$(EXE)	\
	sqrt$(EXE)

# frac

all: $(DEMOS)

clean:
	rm -f $(DEMOS) *.o *~ core

abs$(EXE) : abs.pas  
	$(PC) $(PFLAGS) -o $@ $< $(LIBS)

arctan$(EXE): arctan.pas
	$(PC) $(PFLAGS) -o $@ $< $(LIBS)

cos$(EXE) : cos.pas
	$(PC) $(PFLAGS) -o $@ $< $(LIBS)

conjugate$(EXE) : conjugate.pas
	$(PC) $(PFLAGS) -o $@ $< $(LIBS)

exp$(EXE) : exp.pas
	$(PC) $(PFLAGS) -o $@ $< $(LIBS)

frac$(EXE) : frac.pas
	$(PC) $(PFLAGS) -o $@ $< $(LIBS)

ln$(EXE) : ln.pas
	$(PC) $(PFLAGS) -o $@ $< $(LIBS)

polar$(EXE) : polar.pas
	$(PC) $(PFLAGS) -o $@ $< $(LIBS)

pow$(EXE) : pow.pas
	$(PC) $(PFLAGS) -o $@ $< $(LIBS)

sin$(EXE) : sin.pas
	$(PC) $(PFLAGS) -o $@ $< $(LIBS)

sqr$(EXE) : sqr.pas
	$(PC) $(PFLAGS) -o $@ $< $(LIBS)

sqrt$(EXE) : sqrt.pas
	$(PC) $(PFLAGS) -o $@ $< $(LIBS)
