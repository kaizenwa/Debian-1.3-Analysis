/* $Id: dump.c,v 1.1 1996/09/09 13:26:57 claudio Exp $
 * This quick-and-dirty dump utility helped a lot when comparing the
 * description in * xm.txt with the real stuff. (It was hacked together
 * in less time that would be necessary to read the hexdump(1) manpage :))
 */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <strings.h>


int main(int argc,char **argv)
{
    FILE *pfile;
    int i,c;
    static char buffer[256],buf2[2];

    if(argc<2) {
        fprintf(stderr,"Usage: dump filename\n");
        exit(-1);
    }

    if((pfile=fopen(argv[1],"r"))==NULL) {
        perror(*argv);
        exit(-1);
    }

    for(i=0;(c=fgetc(pfile))!=-1;i++) {
        if (isprint(c)) {
             sprintf(buf2,"%c",c);
             strcat(buffer,buf2); 
        }else
             strcat(buffer,".");

        if (!(i%16)) printf("%5d [%04x] ",i,i);
        printf("%02x ",c);
        if ((i%16)==7) printf(" ");
        if ((i%16)==15) {
            printf(" %s\n",buffer);
            *buffer=0; 
        }
    }
    printf("\n");
    fclose(pfile);
    return 0;
}

