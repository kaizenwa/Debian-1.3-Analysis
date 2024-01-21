#################################################
#                                               #
# BWNFSD Makefile Version 2.3  27/11/91     gsw #
#                                               #
#################################################
#                                               #
# One can enable various debugging options here #
#                                               #
#################################################
DFLAGS	=
CC	= cc
CC2	= cc

##########################
#                        #
# Dependencies Section:  #
#                        #
##########################
make:
	@echo 'Make what?  You must tell which system to make BWNFSD for:'
	@echo 'make sun  - Sun or compatible running Sun-OS'
	@echo 'make bsd  - Generic Berkeley Unix'
	@echo 'make sys5 - Generic ATT System III or System V'
	@echo 'make sgi  - Silicon Graphics machine'
	@echo 'make sco  - Santa Cruz Organization (SCO)'
	@echo 'make aix  - IBM AIX for RS/6000'
	@echo 'make linux - LINUX 0.99.1 and up'


################################
#                              #
# Please add your system here  #
#                              #
################################
OBJS    = bwnfsd.o bwprint.o bwlock.o

all: bwnfsd bwtrm

bwnfsd: $(OBJS)
	$(CC) $(DFLAGS) -o bwnfsd $(OBJS) $(CFLAGS)

###################################################################
#                                                                 #
# Please note that BWTRM.C is only used on Sun systems and is NOT #
# required on non-Sun OS machines.                                #
#                                                                 #
###################################################################
bwtrm: bwtrm.o
	$(CC) $(DFLAGS) -o bwtrm bwtrm.o $(CFLAGS)


########################################
#                                      #
# Make commands for specific systems:  #
#                                      #
########################################
#Berkeley Unix  or Sun
sun:
	make all "CFLAGS= -O"

#Berkeley Unix
bsd:
	make bwnfsd "CFLAGS= -O"

#LINUX
linux:
	make bwnfsd "CFLAGS= -O6 -DLINUX"

#In case they type "make sys5"...
sys5:
	make bwnfsd "CFLAGS= -O -DSYSV32"

#Generic ATT System III or System V
sys3:
	make bwnfsd "CFLAGS= -O -DSYSV32"

#IBM RS/6000 or PS/2 running AIX
aix:
	make bwnfsd "CFLAGS= -O -DSYSV32 -DAIX"

#Silicon Graphics machine
sgi:
	make bwnfsd "CFLAGS= -O -I/usr/include/sun -I/usr/include/bsd -DSYSV32 -Dsgi -lrpcsvc -lsun -lbsd"

#Intel hardware running Santa Cruz Operation's SCO Unix
sco:
	make bwnfsd "CFLAGS= -O -DSYSV32 -DSCO -lprot -lrpcsvc -lrpc -lsocket -lc_s"

