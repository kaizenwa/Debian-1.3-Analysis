#
#  Copyright (c) 1994, Morten Hermanrud, Nordic S&TSS, IBM
#  All rights reserved.
# 
#  Copyright (c) 1993, Trusted Information Systems, Incorporated
#  All rights reserved.
# 
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#  1. Redistributions of source code must retain the above copyright
#     notice, this list of conditions, and the following disclaimer.
#  2. Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#  3. Consultants incorporating this software in deliverables must reproduce
#     this list of conditions and the following disclaimer within any
#     end-user documentation.
#  4. All advertising materials mentioning features or use of this software
#     must display the following acknowledgement:
# 	This product includes software developed by Trusted Information
# 		Systems, Incorporated
#  5. The name of Trusted Information Systems, Incorporated may not
#     be used to endorse or promote products derived from this software
#     without specific prior written permission.
# 
#  THIS SOFTWARE IS PROVIDED BY Trusted Information Systems ``AS IS'' AND
#  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#  ARE DISCLAIMED.  IN NO EVENT SHALL Trusted Information Systems BE LIABLE
#  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
#  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
#  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
#  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
#  SUCH DAMAGE.
#

#
#	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
#
# RcsId: "Header: Makefile,v 1.1 93/10/20 11:17:18 mjr rel "

include ../../../Makefile.config

CFLAGS= -I../../.. $(COPT)
FWLIB=		../../../libfwall.a
AUTHLIB=	../../../libauth.a

all:	authenticate

authenticate:	authenticate.o $(FWLIB)
	$(CC) $(LDFL) -o $@ authenticate.o $(AUTHLIB) $(FWLIB) $(AUXLIB)
	chmod 755 $@

clean:
	rm -f authenticate *.o

install:	all
	$(CP) authenticate $(DEST)
	chmod 500 $(DEST)/authenticate
