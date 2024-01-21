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

extern char *env_dir;

#define Y_or_N(y) ((flags & (y)) ?  "Y" : "N")
#define Machine(y) ((flags & (y)) ? "you" : "other")

static int print_pro PROTO1(char *, p)
{
  UBUF *ub;
  char flags;
  unsigned len, len1;
  char *pro1, *pro2;
  
  ub = client_interact(CC_GET_PRO,0L, strlen(p), (unsigned char *)p+1, 0,
		       (unsigned char *)NULLP);
  len = BB_READ2(ub->bb_len); len1 = BB_READ4(ub->bb_pos);
  pro1 = ub->buf; pro2 = ub->buf+len;
  
  if(ub->cmd == CC_ERR) {
    fprintf(stderr, "ERR: %s\n",ub->buf);
    return(0);
  } else {
    fprintf(stderr, "directory mode: ");
    if(len1) {
      flags = *pro2;
      fprintf(stderr,
	      "(owner: %s)(del: %s)(create: %s)(mkdir: %s)(private: %s)\n",
	      Machine(DIR_OWNER), Y_or_N(DIR_DEL), Y_or_N(DIR_ADD),
	      Y_or_N(DIR_MKDIR), Y_or_N(DIR_PRIV));
    }
    fprintf(stderr, "%s", pro1);
    fprintf(stderr, "\n");
    return(1);
  }
}

int main PROTO3(int, argc, char **, argv, char **, envp)
{
  char *np;
  char **av, *av2[2];
  
  env_client();
  if(argc == 1) {
    print_pro("/");
    puts("/");
  } else {
    if(!(av = glob(argv[1]))) {
      av = av2;
      av2[0] = *argv;
      av2[1] = 0;
    }
    np = util_abs_path(*av);
    if(print_pro(np))puts(np);
    else {
#ifdef VMS
      puts(env_dir);
      client_done();
      exit(3);
#else
      puts(env_dir);
#endif /* VMS */
    }
  }
  client_done();
  exit(0);
}
