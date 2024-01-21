# Generated automatically from Makefile.in by configure.
# soucre/Makefile.in
# ircii 2.3
# all rights reserved
# written by matthew green.
# copyright (c) 1993
# @(#)$Id: Makefile.in,v 1.20 1995/09/05 15:45:24 mrg Exp $

LIBS	= -ltermcap  -lresolv
CC	= gcc
CFLAGS	= -O2
LDFLAGS	= -rdynamic
DEFS	= -DHAVE_CONFIG_H
LN	= ln -s
RM	= rm -f

OBJECTS = alias.o array.o banlist.o bot.o cdcc.o chelp.o crypt.o ctcp.o\
	dcc.o commands.o commands2.o exec.o flood.o glob.o files.o funny.o help.o\
	history.o hold.o hook.o if.o ignore.o input.o irc.o ircaux.o\
	ircsig.o keys.o lastlog.o list.o log.o mail.o misc.o names.o\
	network.o newio.o notice.o notify.o numbers.o output.o parse.o\
	queue.o readlog.o reg.o scandir.o screen.o server.o functions.o\
	stack.o status.o term.o timer.o userlist.o vars.o\
	whois.o whowas.o window.o words.o bot_link.o struct.o dich_conf.o\
	$(PP_OBJS) $(TCL_OBJS) $(CD_OBJS)

SOURCES = alias.c array.c banlist.c bot.c cdcc.c chelp.c crypt.c ctcp.c\
	dcc.c commands.c commands2.c exec.c flood.c glob.c files.c funny.c help.c\
	history.c hold.c hook.c if.c ignore.c input.c irc.c ircaux.c\
	ircsig.c keys.c lastlog.c list.c log.c mail.c misc.c names.c\
	network.c newio.c notice.c notify.c numbers.c output.c parse.c\
	queue.c readlog.c reg.c scandir.c screen.c server.c functions.c\
	stack.c status.c term.c timer.c userlist.c vars.c\
	whois.c whowas.c window.c words.c bot_link.c struct.c dich_conf.c\
	$(PP_OBJS) $(TCL_SRCS) $(CD_SRCS)

INCLUDES = -I../include $(TCLINC)

default:
	@echo ........... Making BitchX-$(_VERSION_) by panasync
	(cd ..;make)
	@echo ........... Done making BitchX-$(_VERSION_)

all: BitchX

.c.o:
	$(CC) $(CFLAGS) $(ANSIFLAGS) $(CD_PLAY) $(TCL) $(COLOR) -D_VERSION_=\"$(_VERSION_)\" $(PP_DEFS) $(INCLUDES) -c $<

BitchX: $(OBJECTS)
	$(CC) $(CFLAGS)  $(LDFLAGS) $(DEFS) -o BitchX $(OBJECTS) $(LIBS) $(XLIBS)
clean::
	$(RM) BitchX $(OBJECTS)

# 'make install'
install: BitchX
	(cd ..; $(MAKE) install)

clean::
	$(RM) *~ ../*~ ../include/*~ core a.out debug.out gmon.out



# .o files needing special compilation flags

irc.o: Makefile ../Makefile
	$(CC) $(CFLAGS) $(TCL) $(CD_PLAY) $(COLOR) -D_VERSION_=\"$(_VERSION_)\" $(PP_DEFS) $(INCLUDES) \
	-DIRCLIB=\"$(IRCLIB)/\" -DIRCPATH=\"$(IRCPATH)\" -c irc.c

vars.o: Makefile ../Makefile
	$(CC) $(CFLAGS) $(TCL) $(CD_PLAY) $(COLOR) -D_VERSION_=\"$(_VERSION_)\" $(PP_DEFS) $(INCLUDES) \
	-DDEFAULT_HELP_PATH=\"$(HELP_DIR)\" -c vars.c

help.o: Makefile ../Makefile
	$(CC) $(CFLAGS) $(TCL) $(CD_PLAY) $(COLOR) $(PP_DEFS) $(INCLUDES) \
	-DDEFAULT_HELP_PATH=\"$(HELP_DIR)\" -c help.c

#server.o: Makefile ../Makefile
#	$(CC) $(CFLAGS) $(TCL) $(CD_PLAY) $(COLOR) $(PP_DEFS) $(INCLUDES) \
#	-c server.c

#screen.o: Makefile ../Makefile
#	$(CC) $(CFLAGS) $(TCL) $(CD_PLAY) $(COLOR) $(PP_DEFS) $(INCLUDES) \
#	-DWSERV_PATH=\"$(INSTALL_WSERV)\" -c screen.c

#
# dependencies
#

depend:
	@echo 'Making dependencies.  This takes a while'
	@sed -n '1,/^# Dont even THINK of deleting this line$$/p' < Makefile.in > tmp
	@mv -f tmp Makefile.in
	@gcc -E -MM -I ../include *.c >> Makefile.in
	@(cd ..;./config.status)

# Dont even THINK of deleting this line
alias.o: alias.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/alias.h ../include/array.h ../include/dcc.h \
 ../include/edit.h ../include/files.h ../include/history.h \
 ../include/hook.h ../include/input.h ../include/ircaux.h \
 ../include/names.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/notify.h ../include/numbers.h \
 ../include/output.h ../include/parse.h ../include/screen.h \
 ../include/server.h ../include/status.h ../include/vars.h \
 ../include/ircterm.h ../include/list.h ../include/keys.h \
 ../include/bot.h ../include/userlist.h ../include/misc.h expr.c
array.o: array.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/array.h ../include/ircaux.h ../include/output.h
banlist.o: banlist.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/edit.h ../include/list.h ../include/hook.h \
 ../include/ignore.h ../include/ircaux.h ../include/output.h \
 ../include/screen.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/server.h ../include/names.h \
 ../include/whois.h ../include/whowas.h ../include/vars.h \
 ../include/userlist.h ../include/misc.h
bot.o: bot.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/server.h \
 ../include/names.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/edit.h ../include/crypt.h \
 ../include/vars.h ../include/ircaux.h ../include/screen.h \
 ../include/whois.h ../include/hook.h ../include/input.h \
 ../include/ignore.h ../include/keys.h ../include/alias.h \
 ../include/history.h ../include/funny.h ../include/ctcp.h \
 ../include/dcc.h ../include/output.h ../include/exec.h \
 ../include/notify.h ../include/numbers.h ../include/status.h \
 ../include/list.h ../include/bot.h ../include/misc.h \
 ../include/userlist.h
bot_link.o: bot_link.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/ircaux.h ../include/bot.h ../include/dcc.h \
 ../include/server.h ../include/names.h ../include/window.h \
 ../include/hold.h ../include/lastlog.h ../include/edit.h \
 ../include/output.h ../include/list.h ../include/whois.h \
 ../include/ctcp.h ../include/tcl_bx.h ../include/status.h \
 ../include/vars.h ../include/misc.h ../include/userlist.h
cdcc.o: cdcc.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/ircaux.h \
 ../include/ignore.h ../include/ctcp.h ../include/hook.h \
 ../include/dcc.h ../include/flood.h ../include/screen.h \
 ../include/window.h ../include/hold.h ../include/lastlog.h \
 ../include/edit.h ../include/parse.h ../include/output.h \
 ../include/input.h ../include/server.h ../include/names.h \
 ../include/vars.h ../include/userlist.h ../include/misc.h \
 ../include/whois.h ../include/cdcc.h
cdrom.o: cdrom.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/ircaux.h ../include/cdrom.h ../include/output.h \
 ../include/misc.h ../include/vars.h
chelp.o: chelp.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/array.h ../include/ircaux.h ../include/chelp.h \
 ../include/output.h ../include/hook.h ../include/misc.h \
 ../include/vars.h
commands.o: commands.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/parse.h ../include/ircterm.h ../include/server.h \
 ../include/names.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/edit.h ../include/chelp.h \
 ../include/crypt.h ../include/vars.h ../include/ircaux.h \
 ../include/screen.h ../include/whois.h ../include/hook.h \
 ../include/input.h ../include/ignore.h ../include/keys.h \
 ../include/alias.h ../include/history.h ../include/funny.h \
 ../include/ctcp.h ../include/dcc.h ../include/output.h \
 ../include/exec.h ../include/notify.h ../include/numbers.h \
 ../include/status.h ../include/if.h ../include/help.h \
 ../include/stack.h ../include/queue.h ../include/timer.h \
 ../include/list.h ../include/userlist.h ../include/misc.h \
 ../include/tcl_bx.h
commands2.o: commands2.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/parse.h ../include/ircterm.h ../include/server.h \
 ../include/names.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/edit.h ../include/chelp.h \
 ../include/crypt.h ../include/vars.h ../include/ircaux.h \
 ../include/log.h ../include/screen.h ../include/whois.h \
 ../include/hook.h ../include/input.h ../include/ignore.h \
 ../include/keys.h ../include/alias.h ../include/history.h \
 ../include/funny.h ../include/ctcp.h ../include/dcc.h \
 ../include/output.h ../include/exec.h ../include/notify.h \
 ../include/numbers.h ../include/status.h ../include/if.h \
 ../include/help.h ../include/stack.h ../include/queue.h \
 ../include/timer.h ../include/list.h ../include/misc.h \
 ../include/userlist.h ../include/whowas.h
crypt.o: crypt.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/crypt.h ../include/vars.h ../include/ircaux.h \
 ../include/list.h ../include/ctcp.h ../include/output.h
ctcp.o: ctcp.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/crypt.h \
 ../include/ctcp.h ../include/dcc.h ../include/edit.h \
 ../include/flood.h ../include/hook.h ../include/if.h \
 ../include/ignore.h ../include/ircaux.h ../include/lastlog.h \
 ../include/list.h ../include/names.h ../include/window.h \
 ../include/hold.h ../include/output.h ../include/parse.h \
 ../include/server.h ../include/status.h ../include/vars.h \
 ../include/cdcc.h ../include/misc.h ../include/userlist.h \
 ../include/bot.h
dcc.o: dcc.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/ctcp.h \
 ../include/crypt.h ../include/cdcc.h ../include/dcc.h \
 ../include/hook.h ../include/ircaux.h ../include/lastlog.h \
 ../include/output.h ../include/parse.h ../include/server.h \
 ../include/names.h ../include/window.h ../include/hold.h \
 ../include/edit.h ../include/status.h ../include/vars.h \
 ../include/whois.h ../include/ircterm.h ../include/misc.h \
 ../include/tcl_bx.h ../include/screen.h
dich_conf.o: dich_conf.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/ircaux.h ../include/dich_conf.h
exec.o: exec.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/dcc.h \
 ../include/exec.h ../include/vars.h ../include/ircaux.h \
 ../include/edit.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/screen.h ../include/hook.h \
 ../include/input.h ../include/list.h ../include/server.h \
 ../include/names.h ../include/output.h ../include/parse.h \
 ../include/misc.h sig.inc
expr.o: expr.c
files.o: files.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/ircaux.h
flood.o: flood.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/alias.h ../include/hook.h ../include/ircaux.h \
 ../include/ignore.h ../include/flood.h ../include/vars.h \
 ../include/output.h ../include/list.h ../include/misc.h \
 ../include/server.h ../include/names.h ../include/window.h \
 ../include/hold.h ../include/lastlog.h ../include/edit.h \
 ../include/userlist.h ../include/timer.h ../include/status.h
functions.o: functions.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/alias.h ../include/array.h ../include/dcc.h \
 ../include/edit.h ../include/files.h ../include/history.h \
 ../include/hook.h ../include/input.h ../include/ircaux.h \
 ../include/names.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/output.h ../include/list.h \
 ../include/parse.h ../include/screen.h ../include/server.h \
 ../include/status.h ../include/vars.h ../include/ircterm.h \
 ../include/notify.h ../include/misc.h ../include/userlist.h \
 ../include/bot.h ../include/numbers.h
funny.o: funny.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/ircaux.h ../include/hook.h ../include/vars.h \
 ../include/funny.h ../include/names.h ../include/window.h \
 ../include/hold.h ../include/lastlog.h ../include/edit.h \
 ../include/server.h ../include/ircterm.h ../include/output.h \
 ../include/numbers.h ../include/parse.h ../include/status.h \
 ../include/misc.h ../include/screen.h
glob.o: glob.c ../include/config.h ../include/glob.h ../include/defs.h \
 ../include/ircaux.h ../include/irc.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h
help.o: help.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/help.h \
 ../include/input.h ../include/ircaux.h ../include/output.h \
 ../include/screen.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/edit.h ../include/server.h \
 ../include/names.h ../include/ircterm.h ../include/vars.h
history.o: history.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/ircaux.h ../include/vars.h ../include/history.h \
 ../include/output.h ../include/input.h
hold.o: hold.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/ircaux.h \
 ../include/window.h ../include/hold.h ../include/lastlog.h \
 ../include/edit.h ../include/screen.h ../include/vars.h \
 ../include/input.h
hook.o: hook.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/hook.h \
 ../include/vars.h ../include/ircaux.h ../include/if.h \
 ../include/alias.h ../include/list.h ../include/window.h \
 ../include/hold.h ../include/lastlog.h ../include/edit.h \
 ../include/server.h ../include/names.h ../include/output.h \
 ../include/parse.h ../include/misc.h
if.o: if.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/alias.h \
 ../include/ircaux.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/edit.h ../include/vars.h \
 ../include/output.h ../include/if.h
ignore.o: ignore.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/ignore.h ../include/ircaux.h ../include/list.h \
 ../include/input.h ../include/screen.h ../include/window.h \
 ../include/hold.h ../include/lastlog.h ../include/edit.h \
 ../include/misc.h ../include/vars.h ../include/output.h
input.o: input.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/alias.h ../include/edit.h ../include/exec.h \
 ../include/history.h ../include/hook.h ../include/input.h \
 ../include/ircaux.h ../include/keys.h ../include/screen.h \
 ../include/window.h ../include/hold.h ../include/lastlog.h \
 ../include/server.h ../include/names.h ../include/ircterm.h \
 ../include/vars.h ../include/misc.h ../include/output.h \
 ../include/chelp.h ../include/dcc.h ../include/cdcc.h \
 ../include/whowas.h ../include/tcl_bx.h
irc.o: irc.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/status.h \
 ../include/dcc.h ../include/names.h ../include/window.h \
 ../include/hold.h ../include/lastlog.h ../include/edit.h \
 ../include/vars.h ../include/input.h ../include/alias.h \
 ../include/output.h ../include/ircterm.h ../include/exec.h \
 ../include/screen.h ../include/log.h ../include/server.h \
 ../include/hook.h ../include/keys.h ../include/ircaux.h \
 ../include/history.h ../include/notify.h ../include/numbers.h \
 ../include/mail.h ../include/timer.h ../include/whowas.h \
 ../include/misc.h ../include/tcl_bx.h
ircaux.o: ircaux.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/alias.h ../include/log.h ../include/misc.h \
 ../include/vars.h ../include/screen.h ../include/window.h \
 ../include/hold.h ../include/lastlog.h ../include/edit.h \
 ../include/ircaux.h ../include/output.h ../include/ircterm.h
ircsig.o: ircsig.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h
keys.o: keys.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/edit.h \
 ../include/history.h ../include/hold.h ../include/ircaux.h \
 ../include/input.h ../include/keys.h ../include/names.h \
 ../include/window.h ../include/lastlog.h ../include/output.h \
 ../include/screen.h ../include/ircterm.h ../include/vars.h
lastlog.o: lastlog.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/lastlog.h ../include/window.h ../include/hold.h \
 ../include/edit.h ../include/screen.h ../include/vars.h \
 ../include/ircaux.h ../include/output.h ../include/misc.h \
 ../include/hook.h ../include/status.h
list.o: list.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/list.h \
 ../include/ircaux.h
log.o: log.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/log.h \
 ../include/vars.h ../include/screen.h ../include/window.h \
 ../include/hold.h ../include/lastlog.h ../include/edit.h \
 ../include/output.h ../include/ircaux.h
mail.o: mail.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/mail.h \
 ../include/lastlog.h ../include/hook.h ../include/vars.h \
 ../include/ircaux.h ../include/output.h ../include/window.h \
 ../include/hold.h ../include/edit.h ../include/status.h \
 ../include/misc.h
misc.o: misc.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/server.h \
 ../include/names.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/edit.h ../include/crypt.h \
 ../include/vars.h ../include/ircaux.h ../include/screen.h \
 ../include/whois.h ../include/hook.h ../include/input.h \
 ../include/ignore.h ../include/keys.h ../include/alias.h \
 ../include/history.h ../include/funny.h ../include/ctcp.h \
 ../include/dcc.h ../include/output.h ../include/exec.h \
 ../include/notify.h ../include/numbers.h ../include/status.h \
 ../include/list.h ../include/timer.h ../include/userlist.h \
 ../include/bot.h ../include/misc.h ../include/flood.h \
 ../include/parse.h ../include/whowas.h ../include/dich_conf.h
names.o: names.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/ircaux.h ../include/names.h ../include/window.h \
 ../include/hold.h ../include/lastlog.h ../include/edit.h \
 ../include/flood.h ../include/screen.h ../include/server.h \
 ../include/list.h ../include/output.h ../include/numbers.h \
 ../include/userlist.h ../include/timer.h ../include/input.h \
 ../include/hook.h ../include/parse.h ../include/whowas.h \
 ../include/misc.h ../include/bot.h ../include/vars.h \
 ../include/tcl_bx.h
network.o: network.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/ircaux.h
newio.o: newio.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/ircaux.h
notice.o: notice.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/whois.h ../include/ctcp.h ../include/window.h \
 ../include/hold.h ../include/lastlog.h ../include/edit.h \
 ../include/flood.h ../include/vars.h ../include/ircaux.h \
 ../include/hook.h ../include/ignore.h ../include/server.h \
 ../include/names.h ../include/funny.h ../include/output.h \
 ../include/parse.h ../include/notify.h ../include/misc.h \
 ../include/screen.h ../include/status.h
notify.o: notify.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/list.h ../include/notify.h ../include/ircaux.h \
 ../include/whois.h ../include/hook.h ../include/server.h \
 ../include/names.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/edit.h ../include/output.h \
 ../include/vars.h ../include/timer.h ../include/misc.h \
 ../include/status.h ../include/userlist.h
numbers.o: numbers.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/input.h ../include/edit.h ../include/ircaux.h \
 ../include/vars.h ../include/lastlog.h ../include/list.h \
 ../include/hook.h ../include/server.h ../include/names.h \
 ../include/window.h ../include/hold.h ../include/whois.h \
 ../include/numbers.h ../include/screen.h ../include/output.h \
 ../include/funny.h ../include/parse.h ../include/misc.h \
 ../include/status.h ../include/timer.h ../include/tcl_bx.h
output.o: output.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/output.h ../include/vars.h ../include/input.h \
 ../include/ircterm.h ../include/lastlog.h ../include/window.h \
 ../include/hold.h ../include/edit.h ../include/screen.h \
 ../include/hook.h ../include/ctcp.h ../include/log.h \
 ../include/misc.h
parse.o: parse.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/alias.h ../include/server.h ../include/names.h \
 ../include/window.h ../include/hold.h ../include/lastlog.h \
 ../include/edit.h ../include/vars.h ../include/ctcp.h \
 ../include/hook.h ../include/ignore.h ../include/whois.h \
 ../include/input.h ../include/ircaux.h ../include/funny.h \
 ../include/crypt.h ../include/ircterm.h ../include/flood.h \
 ../include/screen.h ../include/output.h ../include/numbers.h \
 ../include/parse.h ../include/notify.h ../include/status.h \
 ../include/list.h ../include/userlist.h ../include/bot.h \
 ../include/misc.h ../include/whowas.h ../include/tcl_bx.h
queue.o: queue.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/alias.h ../include/edit.h ../include/ircaux.h \
 ../include/queue.h ../include/output.h
readlog.o: readlog.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/ircterm.h ../include/server.h ../include/names.h \
 ../include/window.h ../include/hold.h ../include/lastlog.h \
 ../include/edit.h ../include/vars.h ../include/ircaux.h \
 ../include/input.h ../include/screen.h ../include/output.h \
 ../include/misc.h
reg.o: reg.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/ircaux.h \
 ../include/output.h
scandir.o: scandir.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h
screen.o: screen.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/screen.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/edit.h ../include/output.h \
 ../include/vars.h ../include/server.h ../include/names.h \
 ../include/list.h ../include/ircterm.h ../include/ircaux.h \
 ../include/input.h ../include/log.h ../include/hook.h \
 ../include/dcc.h ../include/exec.h ../include/misc.h
server.o: server.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/parse.h ../include/server.h ../include/names.h \
 ../include/window.h ../include/hold.h ../include/lastlog.h \
 ../include/edit.h ../include/ircaux.h ../include/whois.h \
 ../include/exec.h ../include/output.h ../include/hook.h \
 ../include/vars.h ../include/screen.h ../include/notify.h \
 ../include/misc.h ../include/status.h
stack.o: stack.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/vars.h ../include/stack.h ../include/hook.h \
 ../include/alias.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/edit.h ../include/ircaux.h \
 ../include/output.h ../include/list.h ../include/misc.h
status.o: status.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/ircterm.h ../include/status.h ../include/server.h \
 ../include/names.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/edit.h ../include/vars.h \
 ../include/hook.h ../include/input.h ../include/screen.h \
 ../include/mail.h ../include/output.h ../include/ircaux.h \
 ../include/misc.h
struct.o: struct.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/ircaux.h ../include/alias.h ../include/output.h
tcl.o: tcl.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/dcc.h \
 ../include/parse.h ../include/ircterm.h ../include/server.h \
 ../include/names.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/edit.h ../include/chelp.h \
 ../include/crypt.h ../include/vars.h ../include/ircaux.h \
 ../include/screen.h ../include/whois.h ../include/hook.h \
 ../include/input.h ../include/ignore.h ../include/keys.h \
 ../include/alias.h ../include/history.h ../include/funny.h \
 ../include/ctcp.h ../include/output.h ../include/exec.h \
 ../include/notify.h ../include/numbers.h ../include/status.h \
 ../include/if.h ../include/help.h ../include/stack.h \
 ../include/queue.h ../include/timer.h ../include/list.h \
 ../include/bot.h ../include/userlist.h ../include/misc.h \
 ../include/whowas.h
term-new.o: term-new.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/vars.h ../include/ircterm.h ../include/window.h \
 ../include/hold.h ../include/lastlog.h ../include/edit.h \
 ../include/screen.h ../include/output.h
term-old.o: term-old.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/vars.h ../include/ircterm.h ../include/window.h \
 ../include/hold.h ../include/lastlog.h ../include/edit.h \
 ../include/screen.h ../include/output.h
term.o: term.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/vars.h \
 ../include/ircterm.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/edit.h ../include/screen.h \
 ../include/output.h
timer.o: timer.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/ircaux.h ../include/lastlog.h ../include/timer.h \
 ../include/hook.h ../include/output.h ../include/edit.h \
 ../include/misc.h ../include/vars.h ../include/tcl_bx.h
userlist.o: userlist.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/server.h ../include/names.h ../include/window.h \
 ../include/hold.h ../include/lastlog.h ../include/edit.h \
 ../include/crypt.h ../include/vars.h ../include/ircaux.h \
 ../include/screen.h ../include/whois.h ../include/hook.h \
 ../include/input.h ../include/ignore.h ../include/keys.h \
 ../include/alias.h ../include/history.h ../include/funny.h \
 ../include/ctcp.h ../include/dcc.h ../include/output.h \
 ../include/exec.h ../include/notify.h ../include/numbers.h \
 ../include/status.h ../include/list.h ../include/timer.h \
 ../include/bot.h ../include/whowas.h ../include/misc.h \
 ../include/userlist.h ../include/parse.h
vars.o: vars.c ../include/irc.h ../include/defs.h ../include/config.h \
 ../include/glob.h ../include/irc_std.h ../include/debug.h \
 ../include/newio.h ../include/struct.h ../include/status.h \
 ../include/window.h ../include/hold.h ../include/lastlog.h \
 ../include/edit.h ../include/log.h ../include/crypt.h \
 ../include/history.h ../include/notify.h ../include/vars.h \
 ../include/input.h ../include/ircaux.h ../include/whois.h \
 ../include/ircterm.h ../include/output.h ../include/screen.h \
 ../include/list.h ../include/server.h ../include/names.h \
 ../include/misc.h ../include/stack.h ../include/hook.h \
 ../include/alias.h
whois.o: whois.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/whois.h ../include/hook.h ../include/lastlog.h \
 ../include/vars.h ../include/server.h ../include/names.h \
 ../include/window.h ../include/hold.h ../include/edit.h \
 ../include/ignore.h ../include/ircaux.h ../include/notify.h \
 ../include/numbers.h ../include/output.h ../include/parse.h \
 ../include/ctcp.h ../include/misc.h ../include/status.h \
 ../include/bot.h ../include/userlist.h
whowas.o: whowas.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/vars.h ../include/ircaux.h ../include/window.h \
 ../include/hold.h ../include/lastlog.h ../include/edit.h \
 ../include/whois.h ../include/hook.h ../include/input.h \
 ../include/names.h ../include/alias.h ../include/output.h \
 ../include/numbers.h ../include/status.h ../include/screen.h \
 ../include/list.h ../include/userlist.h ../include/misc.h \
 ../include/whowas.h
wildmat.o: wildmat.c
window.o: window.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/screen.h ../include/window.h ../include/hold.h \
 ../include/lastlog.h ../include/edit.h ../include/vars.h \
 ../include/server.h ../include/names.h ../include/list.h \
 ../include/ircterm.h ../include/ircaux.h ../include/input.h \
 ../include/status.h ../include/output.h ../include/log.h \
 ../include/hook.h ../include/dcc.h
words.o: words.c ../include/irc.h ../include/defs.h \
 ../include/config.h ../include/glob.h ../include/irc_std.h \
 ../include/debug.h ../include/newio.h ../include/struct.h \
 ../include/ircaux.h
