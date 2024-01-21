#
# This software is provided as-is, with no warranties for suitability of
# use or support implied.  This package makes no guarantees that it will
# perform in any manner.  The authors and Texas A&M University accept no
# liability for any damages incurred while using this software.
#
# This software may be copied, redistributed, and updated in any
# fashion as long as this comment header is not removed or altered.
#
# Douglas Lee Schales
# Doug.Schales@sc.tamu.edu
# Texas A&M University
# Supercomputer Center
#
#01/26/1993
#------------------------------------------------------------------------
#
# Support NIS maps (since they usually are on disk somewhere, this is
# really just a convenience option and isn't required)
NIS=-DNISMAPS
NISSRC=addnismap.c
#NISSRC=
#NIS=
#
#
# What C compiler?
#
CC=gcc
#
# Trust your optimizer?
#CFLAGS=-g
CFLAGS=-O4 ${NIS}
#
# End of configuration
#------------------------------------------------------------------------
SRC=amdxref.c parser.c builder.c \
	scanner.c subpath.c fstree.c \
	getline.c findchar.c chario.c \
	${NISSRC}

OBJ=$(SRC:.c=.o)

amdxref: $(OBJ)
	$(CC) $(CFLAGS) $(OBJ) -o amdxref

clean:
	$(RM) -f $(OBJ) amdxref core
