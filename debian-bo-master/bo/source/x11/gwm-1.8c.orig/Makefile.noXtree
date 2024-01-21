# default rule
C_COMPILER=cc
# Copyright 1989 GROUPE BULL -- See license conditions in file COPYRIGHT
# Copyright 1989 Massachusetts Institute of Technology
#
#			      ###################
#                             ## GWM  makefile ##
#			      ###################

GWM_OBJS= bar.o gwm.o client.o error.o icccm.o gwm-spy.o \
	  menu.o plug.o  screen.o user.o wob.o wool-gwm.o wops.o revision.o \
	  wl_plug.o wl_fsm.o wl_bar.o wl_event.o wl_label.o wl_pixmap.o \
	  wl_client.o wl_cursor.o wl_menu.o 

GWM_SRCS= revision.c bar.c gwm.c client.c error.c icccm.c gwm-spy.c \
	  menu.c plug.c  screen.c user.c wob.c wool-gwm.c wops.c \
	  wl_plug.c wl_fsm.c wl_bar.c wl_event.c wl_label.c wl_pixmap.c \
	  wl_client.c wl_cursor.c wl_menu.c

WOOL_OBJS = y.tab.o wool.o wl_atom.o wl_coll.o wl_func.o wl_list.o \
	  wl_number.o wl_pointer.o wl_string.o malloc.o machine.o reference.o \
          wl_active.o wl_name.o

WOOL_SRCS    = wool.c wl_atom.c wl_coll.c wl_func.c wl_list.c \
	wl_number.c wl_pointer.c wl_string.c malloc.c machine.c reference.c \
	wl_active.c wl_name.c

OBJS = $(WOOL_OBJS) $(GWM_OBJS)
SRCS = $(WOOL_SRCS) $(GWM_SRCS)

INCLUDE	= -I$(XDIR) -I$(XDIR)/X11 -I$(XDIR)/X11/extensions -I. $(EXTRA_INCLUDES)
LIBDIR	= $(XLIBDIR)

GWMCFLAGS = -DGWM $(DEFINES) $(INCLUDE) -DINSTALL_PATH=\"$(INSTALL_PATH)\"\
 -DINSTALL_DIR=\"$(INSTALL_DIR)\" -DWOOL_APP_NAME=\"GWM\" -DWOOL_APP_name=\"gwm\" -DDEFAULT_DEFAULT_WLPATH=\"/usr/local/X11/gwm:/usr/local/X11/lib/gwm:/usr/lib/X11/gwm\"

CFLAGS=
REV=
CC=./gwm_cc

all	: makefile gwm

gwm 	: $(CC) $(OBJS) 
	rm -f gwm
	$(CC) $(LFLAGS) $(LIBDIR) $(OBJS) -o gwm -lXpm $(LIBS)
	@$(POSTPROCESSING) gwm

$(CC)	: makefile
	echo $(C_COMPILER) '$(FLAGS) $(GWMCFLAGS)' '$$*' > $(CC)
	- chmod a+x $(CC)
	- if test -z "$(C_COMPILER)";then rm -f $(CC);fi

$(CC)_pf	: makefile
	echo purify $(C_COMPILER) '$(FLAGS) $(GWMCFLAGS)' '$$*' > $(CC)_pf
	- chmod a+x $(CC)_pf

gwmpf 	: $(CC) $(OBJS) $(CC)_pf
	rm -f gwmpf
	$(CC)_pf $(LFLAGS) $(LIBDIR) $(OBJS) -o gwmpf -lXpm $(LIBS)
	@$(POSTPROCESSING) gwmpf

lex.yy.c : wool.lex 
	lex wool.lex
	-if grep FLEX lex.yy.c;then cp gwm-parsers/lex.yy.c .; fi

y.tab.c : wool.yac 
	yacc wool.yac
	-if grep YYBYACC y.tab.c;then cp gwm-parsers/y.tab.c .;fi

y.tab.o : y.tab.c lex.yy.c

depend: 
	@echo 'EXPECT SOME "includes EXTERN.h more than once!" WARNINGS'
	makedep -s "# DO NOT DELETE" $(GWMCFLAGS) $(SRCS)
	#cd xpm; make DEPEND=makedep depend

makefile: Makefile .dir $(LOCAL_MAKE)
	tail +3 Makefile > Makefile.2	  
	echo 'LOCAL_MAKE=Make.'`cat .dir`| \
	cat Make.`cat .dir` - Makefile.2 > makefile
	make

gwm.sh: $(SRCS)
	./gwm_util make_gwm.sh

doc	:
	cd doc; make

wool	: $(WOOL_OBJS) main.o
	rm -f wool
	mv wool.o wool.o.gwm
	mv wl_list.o wl_list.o.gwm
	$(C_COMPILER)  $(DEFINES) $(INCLUDE) $(FLAGS) -c wl_list.c
	$(C_COMPILER) $(FLAGS) $(DEFINES) $(INCLUDE)\
 -DINSTALL_PATH=\"$(INSTALL_PATH)\"\
 -DINSTALL_DIR=\"$(INSTALL_DIR)\" -DWOOL_APP_NAME=\"GWM\" -DWOOL_APP_name=\"gwm\" -DDEFAULT_DEFAULT_WLPATH=\"/usr/local/X11/gwm:/usr/local/X11/lib/gwm:/usr/lib/X11/gwm\" -c wool.c
	$(CC) $(LFLAGS)  $(WOOL_OBJS) main.o -o wool
	rm -f wl_list.o wool.o
	mv wool.o.gwm wool.o
	mv wl_list.o.gwm wl_list.o
	@$(POSTPROCESSING) wool

lint	: $(SRCS) 
	lint -axz -DLINT $(GWMCFLAGS) $(SRCS) xpm/*.c \
	xpm/llib-lXpm.ln /usr/lib/X11/llib-lXext.ln /usr/lib/X11/llib-lX11.ln \
	2>&1 >lint

ccheck  :
	ccheck $(GWMCFLAGS) $(SRCS)

clean:
	rm -f *.o lex.yy.c y.tab.c makefile gwm_cc
	-if test -d xpm; then cd xpm; make clean;fi

install: 
	./gwm_util install $(INSTALL_GWM) $(INSTALL_DIR)

dir:
	./gwm_util dir ${DIR}

sdir:
	./gwm_util sdir ${DIR}

# DO NOT DELETE

wool.o: INTERN.h wool.h machine.h EXTERN.h wl_atom.h wl_coll.h wl_func.h
wool.o: wl_list.h wl_number.h wl_string.h wl_pointer.h wl_active.h wl_name.h
wool.o: yacc.h
wl_atom.o: EXTERN.h wool.h machine.h wl_coll.h wl_number.h wl_list.h
wl_atom.o: wl_string.h wl_pointer.h wl_active.h wl_name.h INTERN.h wl_atom.h
wl_coll.o: EXTERN.h wool.h machine.h wl_number.h wl_atom.h wl_list.h INTERN.h
wl_coll.o: wl_coll.h
wl_func.o: EXTERN.h wool.h machine.h wl_atom.h wl_number.h wl_list.h INTERN.h
wl_func.o: wl_func.h
wl_list.o: EXTERN.h wool.h machine.h wl_number.h wl_atom.h wl_name.h
wl_list.o: wl_coll.h wl_func.h INTERN.h wl_list.h gwm.h wob.h wl_pixmap.h
wl_number.o: EXTERN.h wool.h machine.h wl_atom.h wl_list.h wl_pointer.h
wl_number.o: INTERN.h wl_number.h
wl_pointer.o: EXTERN.h wool.h machine.h wl_atom.h wl_list.h wl_number.h
wl_pointer.o: wl_string.h wl_active.h INTERN.h wl_pointer.h
wl_string.o: EXTERN.h wool.h machine.h wl_number.h wl_atom.h wl_active.h
wl_string.o: wl_pointer.h wl_name.h wl_list.h INTERN.h wl_string.h
malloc.o: EXTERN.h wool.h machine.h
reference.o: EXTERN.h wool.h machine.h
wl_active.o: EXTERN.h wool.h machine.h wl_atom.h wl_list.h wl_number.h
wl_active.o: wl_string.h INTERN.h wl_active.h
wl_name.o: EXTERN.h wool.h machine.h wl_atom.h wl_list.h wl_number.h
wl_name.o: wl_string.h INTERN.h wl_name.h
revision.o: wool.h machine.h
bar.o: EXTERN.h wool.h machine.h wl_atom.h gwm.h wob.h wl_number.h
bar.o: wl_pixmap.h wl_fsm.h wl_event.h wl_list.h wl_cursor.h wl_bar.h
bar.o: wl_plug.h
gwm.o: EXTERN.h wool.h machine.h wl_atom.h wl_string.h wl_list.h wl_client.h
gwm.o: wl_bar.h wl_plug.h wl_func.h yacc.h wl_number.h wl_pixmap.h INTERN.h
gwm.o: gwm.h wob.h
client.o: EXTERN.h wool.h machine.h wl_atom.h wl_string.h wl_list.h
client.o: wl_number.h wl_func.h gwm.h wob.h wl_pixmap.h wl_event.h wl_fsm.h
client.o: wl_cursor.h wl_client.h wl_bar.h wl_plug.h
error.o: EXTERN.h gwm.h wob.h wl_number.h wl_pixmap.h wl_atom.h yacc.h
icccm.o: EXTERN.h wool.h machine.h wl_atom.h wl_list.h wl_string.h gwm.h
icccm.o: wob.h wl_number.h wl_pixmap.h
gwm-spy.o: machine.h spy.c spy.h
menu.o: EXTERN.h wool.h machine.h wl_atom.h wl_number.h wl_string.h wl_list.h
menu.o: gwm.h wob.h wl_pixmap.h wl_fsm.h wl_event.h wl_cursor.h wl_bar.h
menu.o: wl_plug.h wl_menu.h
plug.o: EXTERN.h gwm.h wob.h wl_number.h wl_pixmap.h wl_atom.h wl_list.h
plug.o: wl_fsm.h wl_event.h wl_cursor.h wl_plug.h
screen.o: EXTERN.h wool.h machine.h wl_atom.h wl_list.h wl_number.h
screen.o: wl_string.h gwm.h wob.h wl_pixmap.h wl_fsm.h wl_event.h wl_cursor.h
screen.o: wl_client.h wl_bar.h wl_plug.h
user.o: EXTERN.h wool.h machine.h wl_atom.h gwm.h wob.h wl_number.h
user.o: wl_pixmap.h wl_string.h wl_cursor.h
wob.o: EXTERN.h gwm.h wob.h wl_number.h wl_pixmap.h wl_atom.h wl_list.h
wob.o: wl_cursor.h
wool-gwm.o: EXTERN.h wool.h machine.h wl_number.h wl_atom.h wl_string.h
wool-gwm.o: wl_list.h wl_func.h wl_active.h wl_pointer.h gwm.h wob.h
wool-gwm.o: wl_pixmap.h wl_event.h wl_fsm.h wl_label.h wl_cursor.h wl_plug.h
wool-gwm.o: wl_bar.h wl_menu.h wl_client.h wl_coll.h wl_name.h INTERN.h
wops.o: EXTERN.h wool.h machine.h wl_atom.h gwm.h wob.h wl_number.h
wops.o: wl_pixmap.h wl_string.h wl_client.h wl_bar.h wl_plug.h wl_menu.h
wops.o: wl_name.h wl_event.h
wl_plug.o: EXTERN.h wool.h machine.h wl_atom.h wl_number.h wl_string.h gwm.h
wl_plug.o: wob.h wl_pixmap.h wl_event.h wl_fsm.h wl_list.h wl_label.h
wl_plug.o: INTERN.h wl_plug.h
wl_fsm.o: EXTERN.h wool.h machine.h wl_number.h wl_atom.h wl_list.h wl_func.h
wl_fsm.o: gwm.h wob.h wl_pixmap.h wl_event.h INTERN.h wl_fsm.h
wl_bar.o: EXTERN.h wool.h machine.h wl_atom.h wl_number.h wl_string.h gwm.h
wl_bar.o: wob.h wl_pixmap.h wl_event.h wl_fsm.h wl_list.h wl_plug.h INTERN.h
wl_bar.o: wl_bar.h
wl_event.o: EXTERN.h wool.h machine.h wl_atom.h wl_pointer.h wl_string.h
wl_event.o: wl_number.h wl_list.h wl_func.h gwm.h wob.h wl_pixmap.h INTERN.h
wl_event.o: wl_event.h wl_cursor.h wl_fsm.h
wl_label.o: EXTERN.h wool.h machine.h wl_string.h wl_atom.h wl_number.h gwm.h
wl_label.o: wob.h wl_pixmap.h INTERN.h wl_label.h
wl_pixmap.o: EXTERN.h wool.h machine.h wl_string.h wl_atom.h wl_pointer.h
wl_pixmap.o: wl_active.h wl_number.h wl_label.h wl_list.h gwm.h wob.h
wl_pixmap.o: wl_pixmap.h INTERN.h def_bitmap.h
wl_client.o: EXTERN.h wool.h machine.h wl_atom.h wl_number.h wl_string.h
wl_client.o: wl_list.h gwm.h wob.h wl_pixmap.h wl_event.h wl_fsm.h wl_plug.h
wl_client.o: wl_bar.h wl_cursor.h wl_menu.h INTERN.h wl_client.h
wl_cursor.o: EXTERN.h wool.h machine.h wl_string.h wl_number.h gwm.h wob.h
wl_cursor.o: wl_pixmap.h INTERN.h wl_cursor.h
wl_menu.o: EXTERN.h wool.h machine.h wl_atom.h wl_number.h wl_string.h
wl_menu.o: wl_list.h gwm.h wob.h wl_pixmap.h wl_event.h wl_fsm.h wl_bar.h
wl_menu.o: wl_plug.h INTERN.h wl_menu.h
