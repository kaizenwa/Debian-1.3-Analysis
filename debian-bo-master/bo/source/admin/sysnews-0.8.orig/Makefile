# Makefile for news	int@link.xs4all.nl
#
# $Revision: 1.5 $
#

# system news location
NEWSDIR =/var/sysnews
# file in $HOME which indicates the last time news was read
NEWSTIME=.news_time

CC	= gcc
DEFS	= -DNEWSDIR=\"$(NEWSDIR)\" -DNEWSTIME=\"$(NEWSTIME)\"
CFLAGS	= -Wall -O2
LDFLAGS = -s

PREFIX	= /usr/local


OBJECTS = news.o getopt.o

%.o: %.c
	$(CC) $(CFLAGS) $(DEFS) -c -o $@ $<

all:	 news manpage

news:	 $(OBJECTS)
	 $(CC) $(LDFLAGS) -o $@ $(OBJECTS)

manpage: news.man
	 sed -e s,/var/sysnews,$(NEWSDIR),g\
	     -e s,.news_time,$(NEWSTIME),g < news.man > news.1

install: all
	 install -o bin -g bin -m 555 news $(PREFIX)/bin
	 install -m 444 news.1 $(PREFIX)/man/man1

clean:
	rm -f news $(OBJECTS) news.1
