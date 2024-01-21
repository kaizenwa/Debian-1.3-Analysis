#
#       @(#)Makefile.customer 1.6 90/06/19 
#
#	Makefile for the panels example programs for XView programmers
#

INCLUDE 	= -I${OPENWINHOME}/include

#
# If you want to compile for debugging, change "-O" to "-g"
#

#CFLAGS		= ${INCLUDE} -g -DDEBUG -DDONT_MOUNT
#CFLAGS		= ${INCLUDE} -g -DDEBUG 
CFLAGS		= ${INCLUDE} -O2
LDFLAGS         = #-static

XVIEW_LIBS 	= -L${OPENWINHOME}/lib -lxview -lolgx -lX11

OBJS	   	=  xvmount.o 

all:  xvmount strip
debug: xvmount

xvmount: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LDFLAGS) $(XVIEW_LIBS)

strip: xvmount
	strip xvmount

clean:
	rm -f core $(OBJS) xvmount
