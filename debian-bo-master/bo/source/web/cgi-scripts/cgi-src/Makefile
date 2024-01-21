# For gcc
CC= gcc
# For ANSI compilers
#CC= cc

#For Optimization
CFLAGS= -O2 -s

RM= /bin/rm -f
#--- You shouldn't have to edit anything else. ---

.c.o: 
	$(CC) -c $(CFLAGS) $<

CGIDIR = ../cgi-bin
SECDIR = ../cgi-sec

CGIBIN = $(CGIDIR)/post-query $(CGIDIR)/query $(CGIDIR)/animate \
    $(CGIDIR)/count $(CGIDIR)/random $(CGIDIR)/imagemap

SECCGI = $(SECDIR)/change-passwd

all: $(CGIBIN)

$(CGIDIR)/post-query: post-query.o util.o
	$(CC) $(CFLAGS) post-query.o util.o -o $@

$(CGIDIR)/query: query.o util.o
	$(CC) $(CFLAGS) query.o util.o -o $@

$(CGIDIR)/animate: animate.o
	$(CC) $(CFLAGS) animate.o -o $@

$(CGIDIR)/count: count.o
	$(CC) $(CFLAGS) count.o -o $@ -ldbm

$(CGIDIR)/random: random.o
	$(CC) $(CFLAGS) random.o -o $@

$(CGIDIR)/imagemap:  imagemap.o
	$(CC) $(CFLAGS) imagemap.o -o $@
$(SECDIR)/change-passwd: change-passwd.o util.o
	$(CC) $(CFLAGS) change-passwd.o util.o -o $@

clean:
	rm -f *.o $(CGIBIN) $(SECDIR)

distclean: clean

