CFLAGS = -I.. -I../wn

OBJS	= wndex.o init.o content.o serveall.o

wndex: $(OBJS)
	$(CC) $(CFLAGS) -o wndex $(OBJS) $(LIBS)

install:	wndex
	@echo "Installing wndex in $(BINDIR)"
	@cp wndex $(BINDIR)
	@strip $(BINDIR)/wndex

clean:
	@rm -f *.o  *~ wndex

wndex.o: 	wndex.h err.h ../wn/common.h ../config.h
init.o: 	wndex.h err.h ../wn/common.h ../config.h
content.o: 	wndex.h err.h ../wn/common.h ../config.h ../wn/content.h
serveall.o: 	wndex.h err.h ../wn/common.h ../config.h
