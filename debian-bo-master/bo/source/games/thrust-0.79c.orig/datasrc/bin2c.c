
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifdef HAVE_CONFIG_H
#include "../src/config.h"
#endif
#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>

#define LL (8)

void quit(char *str);
int writebuf(int count);

unsigned char buf[LL+1];

void
quit(char *str)
{
  perror(str);
  exit(1);
}

int
writebuf(int count)
{
  int i;
  
#if PRINTF_RETURN >= 0
  if(
#endif
     printf("\t")
#if PRINTF_RETURN == 1
                  != 1
#endif
#if PRINTF_RETURN >= 0
                      )
    return(1)
#endif
      ;

  for(i=0; i<count-1; i++)
#if PRINTF_RETURN >= 0
    if(
#endif
       printf("0x%02x, ", buf[i])
#if PRINTF_RETURN == 1
                                  != 6
#endif
#if PRINTF_RETURN >= 0
                                      )
      return(1)
#endif
      ;

  if(i<count)
#if PRINTF_RETURN >= 0
    if(
#endif
       printf("0x%02x", buf[i])
#if PRINTF_RETURN == 1
                                != 4
#endif
#if PRINTF_RETURN >= 0
                                    )
      return(1)
#endif
      ;

  return(0);
}

int
main(int argc, char *argv[])
{
  int stat;
  int end=0;

  if(argc!=2) {
    fprintf(stderr, "%s: Usage '%s variable_name'\n",
	    argv[0],
	    argv[0]);
    exit(1);
  }
#if PRINTF_RETURN >= 0
  if(
#endif
     printf("\nunsigned char %s[] = {\n", argv[1])
#if PRINTF_RETURN == 1
                                                   != (22 + strlen(argv[1]))
#endif
#if PRINTF_RETURN >= 0
                                                                            )
    quit(argv[0])
#endif
      ;
  stat=fread(buf+LL, 1, 1, stdin);
  if(stat!=1) {
    if(ferror(stdin))
      quit(argv[0]);
    end=1;
  }
  while(!end) {
    *buf=*(buf+LL);
    stat=fread(buf+1, 1, LL, stdin);
    if(stat!=LL) {
      if(ferror(stdin))
	quit(argv[0]);
      end=1;
      if(writebuf(stat+1))
	quit(argv[0]);
    }
    else {
      if(writebuf(LL))
	quit(argv[0]);
#if PRINTF_RETURN >= 0
      if(
#endif
         printf(",\n")
#if PRINTF_RETURN == 1
                       != 2
#endif
#if PRINTF_RETURN >= 0
                           )
	quit(argv[0])
#endif
	  ;
    }
  }  
#if PRINTF_RETURN >= 0
  if(
#endif
     printf(" };\n")
#if PRINTF_RETURN == 1
                     != 4
#endif
#if PRINTF_RETURN >= 0
                         )
    quit(argv[0])
#endif
      ;

  return(0);
}
