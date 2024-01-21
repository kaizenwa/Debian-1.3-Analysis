# =()<@<INCLUDE>@>()=
.include "../include/config.make"



A=$(NEWSARTS)
C=$(NEWSCTL)
NDIRS=$(A) $(A)/in.coming $(A)/in.coming/bad $(A)/out.master $(C) $(C)/bin

EGS=active.eg active.times mailname.eg mailpaths.eg organization\
	sys.eg whoami.eg
ALL=$(EGS) report.ctl crontab $(SERVER)

all:	$(ALL)
	$(MX) report.ctl config histsetup

install:	$(ALL)

LASTGROUP=`egrep -v '=|x$$' $(NEWSCTL)/active | tr . / | sed -n '$$s/ .*//p'`

setup:	$(ALL)
	$(MKDIR) $(NEWSCTL)/bin
	$(TO) -n $(NEWSCTL) report.ctl $(SERVER)
	$(TO) `echo $(NEWSCONFIG) | sed 's;/[^/]*$$;;'` config
	$(TO) -e -n $(NEWSCTL) $(EGS)
	./histsetup
	if test -d $(NEWSARTS)/$(LASTGROUP) -a -d $(NEWSOV)/$(LASTGROUP) ;\
	then \
		: no adddirs needed ;\
	else \
		$(NEWSBIN)/maint/adddirs ;\
	fi

versetup:
	$(TO) $(NEWSCTL) versionname

cmp:	$(ALL)
	: make sure major directories have right ownership
	@$(CF) $(UID) $(GID) '^drwx' $(NDIRS)
	: compare some files
	@$(IN) -i $(NEWSCTL) report.ctl
	@$(IN) `echo $(NEWSCONFIG) | sed 's;/[^/]*$$;;'` config
	@$(IN) $(NEWSCTL) versionname
	: make sure the whoami and mailname files have been configured
	test " `cat $(NEWSCTL)/whoami`" != " nosuchsite" ;
	test " `cat $(NEWSCTL)/mailname`" != " no.such.domain" ;
	: check that crucial files exist
	test -r $(NEWSCTL)/history ;
	test -s $(NEWSCTL)/history.dir ;
	test -r $(NEWSCTL)/history.pag ;
	: check that the last newsgroup in active has directories -- too
	: expensive to check all of them
	test -d $(NEWSARTS)/$(LASTGROUP) -a -d $(NEWSOV)/$(LASTGROUP) ;

mx:
	$(MX) checkfile cmpto cpto inall libcmp maker
	$(MX) mkdirs rconfig rsetup subst update.*

active.times:	active.eg
	sed 's/ .*/ 0 unknown/' active.eg >$@

crontab:	cron.proto
	egrep -v '^#' cron.proto >$@

server:	../include/config.make
	echo "$(SERVERNAME)" >$@

report.ctl:	../include/config.make
	echo "urgentto=$(URGENTTO)" >$@
	echo "nonurgentto=$(REPORTTO)" >>$@

liblist:
	cd .. ; $(MAKE) $(SIZE) DBZ=$(DBZ) $(STDIO)

r rclean:	$(ALL)

clean:
	rm -f active.times crontab server report.ctl
