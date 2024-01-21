#
# $Id: Makefile,v 1.5 1996/03/16 22:21:26 morgan Exp morgan $
#
# $Log: Makefile,v $
#

dummy:
	@echo "*** This is not a top level Makefile!"

##########################################################

all:
	@echo "*** Nothing to make in this directory"

install: /etc/pwdb.conf

/etc/pwdb.conf: ./pwdb.conf
	bash -f ./install_conf

remove:
	rm -f /etc/pwdb.conf

check:
	bash -f ./md5itall

RCScheck:
	if [ -x ./md5RCS ]; then bash -f ./md5RCS ; fi

clean:
	rm -f core *~ .ignore_age

