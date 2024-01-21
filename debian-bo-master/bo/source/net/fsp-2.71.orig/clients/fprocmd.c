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

#define Y_or_N(y) ((flags & (y)) ?  "Y" : "N")
#define Machine(y) ((flags & (y)) ? "you" : "other")

static int print_pro PROTO1(char *, p)
{
  char *op, flags;
  unsigned len, len1;
  UBUF *ub;
  char *pro1, *pro2;
  
  op = util_abs_path(p);
  
  ub = client_interact(CC_GET_PRO,0L, strlen(op), (unsigned char *)op+1, 0,
		       (unsigned char *)NULLP);
  len = BB_READ2(ub->bb_len); len1 = BB_READ4(ub->bb_pos);
  pro1 = ub->buf; pro2 = ub->buf+len;
  
  printf("%s\t: ",p);
  
  if(len1) {
    flags = *pro2;
    printf("(owner: %s)(del: %s)(create: %s)(mkdir: %s)(private: %s)\n",
	   Machine(DIR_OWNER), Y_or_N(DIR_DEL), Y_or_N(DIR_ADD),
	   Y_or_N(DIR_MKDIR), Y_or_N(DIR_PRIV));
  }
  printf("%s", pro1);
  printf("\n");
  
  return(0);
}

static int set_pro PROTO2(char *, p, char *, key)
{
  char *op, flags;
  unsigned len1, len;
  UBUF *ub;
  char *pro1, *pro2;
  
  op = util_abs_path(p);
  
  ub = client_interact(CC_SET_PRO,0L, strlen(op), (unsigned char *)op+1,
		       strlen(key)+1, (unsigned char *)key);
  len = BB_READ2(ub->bb_len); len1 = BB_READ4(ub->bb_pos);
  pro1 = ub->buf; pro2 = ub->buf+len;
  
  printf("%s\t: ",p);
  
  if(len1) {
    flags = *pro2;
    printf("(owner: %s)(del: %s)(create: %s)(mkdir: %s)(private: %s)\n",
	   Machine(DIR_OWNER), Y_or_N(DIR_DEL), Y_or_N(DIR_ADD),
	   Y_or_N(DIR_MKDIR), Y_or_N(DIR_PRIV));
  }
  printf("%s", pro1);
  printf("\n");
  
  return(0);
}

int main PROTO3(int, argc, char **, argv, char **, envp)
{
  char **av, *av2[2], *key;
  
  env_client();
  
  if(argv[1] && (argv[1][0] == '+' || argv[1][0] == '-') && !argv[1][2]) {
    key = *++argv;
    while(*++argv) {
      if(!(av = glob(*argv))) {
	av = av2;
	av2[0] = *argv;
	av2[1] = 0;
      }
      while(*av) set_pro(*av++,key);
    }
  } else {
    if(argv[1]) while(*++argv) {
      if(!(av = glob(*argv))) {
	av = av2;
	av2[0] = *argv;
	av2[1] = 0;
      }
      while(*av) print_pro(*av++);
    } else print_pro(".");
  }
  
  client_done();
  
  exit(0);
}
