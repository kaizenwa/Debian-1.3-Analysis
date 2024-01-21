# The location of the verses
VERSE_LIB = /usr/lib/
VERSE_NAME = daily.verse
BIN_DIR = /usr/bin
#CCOPTS = -g
CCOPTS = -O2 -ansi

all : verse

clean :
	rm -f verse *~

distclean : clean

verse : verse.c
	gcc $(CCOPTS) -DVERSE_LIB=\"$(VERSE_LIB)$(VERSE_NAME)\" -o verse verse.c
	strip verse

install : verse $(VERSE_NAME)
	cp verse debian-tmp/$(BIN_DIR)
	cp $(VERSE_NAME) debian-tmp/$(VERSE_LIB)$(VERSE_NAME)

