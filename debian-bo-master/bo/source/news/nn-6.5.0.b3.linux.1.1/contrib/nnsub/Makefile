#
# create program nnsub
#
# define the smbol DEFS in accordance to your system
#
# define the symbol DEST as the directory in everybody's searchpath
# where the program should reside
#
# install the file nnsub.1 manually in the directory for local
# manpages (By us this is /usr/local/man/man1)

# this I use on convex
# DEFS= -pcc
# this should be used on SysV boxes, also HP/UX
DEFS= -DSYSV
# possible DEFS are:
#	ACTIVE		pathname of file containing active newsgroups
#	DEFRC		pathname of default .newsrc (see nn INSTALL)
#	ERR_EXIT	value for error exit
#	NCOLS		number of colums on a terminal screen (80)
#	NEWSGROUPS	pathname of file with short descriptions
#	NEWSRC		file .newsrc
#	NLINES		number of lines in a screen menu (22)
#	NNTP		hostname of nntp server
#	OLDRC		filename to save old .newsrc
#	PORT		port number for nntp, see file /etc/services
#	SYSV		compile and run on System V UNIX
#	TEMPRC		temporary template to write new .newsrc
#	TIMEOUT		timeout value while waiting for server reply
#	iShwfac		interval for writing a dot if debugging
#	iSlen		length of string buffer
#	iMeuGrpart	max space to be used to write groupname in menu

SUBS= avl.o Findsub.o strsave.o
DEST=/usr/local/bin
CC=   cc
CFLAGS= -O $(DEFS)
LDFLAGS=

.c.o:
	$(CC) $(CFLAGS) -c $*.c

nnsub:	nnsub.o $(SUBS)
	$(CC) -o nnsub nnsub.o $(SUBS)

install: nnsub
	mv nnsub $(DEST)
	strip $(DEST)/nnsub
	chmod 551 $(DEST)/nnsub

clean:
	rm -f nnsub core *.o a.out

nnsub.o:	patchlevel.h version.h avl.h nnsub.c
avl.o:		avl.h avl.c
