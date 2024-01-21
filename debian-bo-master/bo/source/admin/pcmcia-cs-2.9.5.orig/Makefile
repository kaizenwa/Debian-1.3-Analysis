#
# Makefile 1.25 1995/12/13 18:56:15 (David Hinds)
#

DIRS = modules cardmgr flash debug-tools man etc

help:
	@echo "Pick one of the following targets:"
	@echo -e "\tmake config\t\t- configure and check system setup"
	@echo -e "\tmake dep\t\t- build dependencies"
	@echo -e "\tmake all\t\t- build modules and programs"
	@echo -e "\tmake install\t\t- install modules and programs"

config .prereq.ok:
	@./Configure

all:	.prereq.ok
	set -e ; for d in $(DIRS) ; do $(MAKE) -C $$d ; done

clean:
	set -e ; for d in $(DIRS) ; do $(MAKE) -C $$d clean ; done
	rm -f .prereq.ok config.out config.mk modules/config.h

dep:	.prereq.ok
	set -e ; for d in $(DIRS) ; do $(MAKE) -C $$d dep ; done

install: .prereq.ok
	set -e ; for d in $(DIRS) ; do $(MAKE) -C $$d install ; done
