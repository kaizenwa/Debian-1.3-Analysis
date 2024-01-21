/*$Id: l_jmpbuf.h,v 11.22 96/02/18 11:46:28 al Exp $ -*- C++ -*-
 * temporary jmp_buf fudge
 * will go away when compiler exception handling is reliable
 */
#ifndef JMPBUF_H
#define JMPBUF_H

#include <setjmp.h>

struct JMP_BUF{
  jmp_buf p;
};

#endif
