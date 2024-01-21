SHELL=/bin/sh
MAKE=make

Ipl-icont:
		$(MAKE) Procs-icont
		$(MAKE) Progs-icont
		$(MAKE) Packs-icont

Ipl-iconc:
		@echo Icon program library not configured for iconc

Procs-icont:
		sh ./Translate-icont

Progs-icont:
		sh ./Compile-icont


Packs-icont:
		sh ./Pack-icont ucode packs/ftrace ftrace.u1 ftrace.u2
		sh ./Pack-icont icode packs/skeem  skeem
		sh ./Pack-icont icode gpacks/ged   ged
		sh ./Pack-icont icode gpacks/tiger tgrprep tgrlink tgrmap
		sh ./Pack-icont icode gpacks/vib   vib

Clean:
		cd cfuncs;        $(MAKE) Clean
		cd packs/ftrace;  $(MAKE) Clean
		cd packs/skeem;   $(MAKE) Clean
		cd gpacks/ged;    $(MAKE) Clean
		cd gpacks/tiger;  $(MAKE) Clean
		cd gpacks/vib;    $(MAKE) Clean
		rm -f *.bak

