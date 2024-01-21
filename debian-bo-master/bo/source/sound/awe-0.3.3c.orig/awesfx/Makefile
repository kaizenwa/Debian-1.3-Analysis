#----------------------------------------------------------------
# Makefile for SFX utilities
#----------------------------------------------------------------

PROGRAMS = $(PRIMARY_PROGRAMS) $(CONVERT_PROGRAMS)
PRIMARY_PROGRAMS = sfxload
CONVERT_PROGRAMS = sf2text txt2sfx sfxtest sfxtext gus2sfx gusload

#
# default SoundFont path
#
DEFAULT_SF_PATH = /usr/local/lib/sfbank:/dos/sb16/sfbank:/dos/sb32/sfbank
DEF_PATH = -DDEFAULT_SF_PATH=\"$(DEFAULT_SF_PATH)\"

DEFINES = -DAWE_DEFAULT_VOLUME=70 $(DEF_PATH)

#----------------------------------------------------------------

INSTALL_DIR = /usr/local/bin

# Search path of awe_voice.h.  Comment out the lines if not necessary.
# FreeBSD with VoxWare 3.0
INCLUDES += -I/usr/src/sys/i386/isa/sound

CFLAGS = -O -Wall $(INCLUDES) $(DEFINES)

#----------------------------------------------------------------

primary: $(PRIMARY_PROGRAMS)

convert: $(CONVERT_PROGRAMS)

all: $(PROGRAMS)

sf2text: sbktext.o readsbk.o gentxt.o
	$(CC) $(CFLAGS) -o $@ sbktext.o readsbk.o gentxt.o

txt2sfx: txt2sfx.o laytxt.o gentxt.o sf2sfx.o sfxread.o awe_parm.o
	$(CC) $(CFLAGS) -o $@ txt2sfx.o laytxt.o gentxt.o sf2sfx.o sfxread.o awe_parm.o -lm

sfxload: sfxload.o sfxread.o seq.o gentxt.o readsbk.o sf2sfx.o awe_parm.o laysbk.o
	$(CC) $(CFLAGS) -o $@ sfxload.o sfxread.o seq.o gentxt.o readsbk.o sf2sfx.o awe_parm.o laysbk.o -lm

gus2sfx: gus2sfx.o sfxread.o awe_parm.o
	$(CC) $(CFLAGS) -o $@ gus2sfx.o sfxread.o awe_parm.o -lm

sfxtest: sfxtest.o seq.o
	$(CC) $(CFLAGS) -o $@ sfxtest.o seq.o

sfxtext: sfxtext.o sfxread.o
	$(CC) $(CFLAGS) -o $@ sfxtext.o sfxread.o

gusload: gusload.o seq.o
	$(CC) $(CFLAGS) -o $@ gusload.o seq.o

install: install-primary

install-all: $(PROGRAMS)
	mkdir -p $(INSTALL_DIR)
	install -c -s $(PROGRAMS) $(INSTALL_DIR)

install-primary: $(PRIMARY_PROGRAMS)
	mkdir -p $(INSTALL_DIR)
	install -c -s $(PRIMARY_PROGRAMS) $(INSTALL_DIR)

install-convert: $(CONVERT_PROGRAMS)
	mkdir -p $(INSTALL_DIR)
	install -c -s $(CONVERT_PROGRAMS) $(INSTALL_DIR)

clean:
	$(RM) *.o $(PROGRAMS)
