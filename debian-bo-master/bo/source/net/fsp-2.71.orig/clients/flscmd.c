    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#include "tweak.h"
#include "client_def.h"
#include "c_extern.h"
#include "bsd_extern.h"

int ls_bad PROTO1(int, n)
{
  client_done();
  exit(n);
}

int main PROTO3(int, argc, char **, argv, char **, envp)
{
  unsigned long pos;
  RDIRENT **dp;
  
  env_client();
  
  fls_main(argc,argv,envp);
  
  client_done();
  
  exit(0);
}
