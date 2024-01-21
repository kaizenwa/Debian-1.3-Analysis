/*
 *   This program `ol2' is a converter
 * from a text for outline-mode of GNU Emacs to normal text.
 *   This is a filter. So, input is `stdin', output is `stdout'.
 *
 * Copyright 1992, Timtia Software.
 *
 * This program is Copyfree.
 */

#include <stdio.h>

main()
{
    unsigned char  depth=0, offset=1;
    
    for(;;){
	char str[256], *cp;
	unsigned short  section[255];
	unsigned char   d;
	unsigned short  i;
	unsigned short  code;

	if(gets(str)==NULL) break;
	
	for(d=0, cp=str; ;d++){
	    if(*cp++!='*') break;
	}
	if(*--cp==' ') cp++;
	if(d==0){
	    puts(str);
	}
	else if(d==1){
	    if(d>depth){
		depth=d;
		section[0]=0;
	    }
	    else if(d==depth){
		section[0]++;
	    }
	    else{
		depth=d;
		section[0]++;
	    }
	    code=0xa3b0+section[0]+offset;
	    printf("%c%c %s\n", code>>8,code&0xff, cp);
	}
	else if(d==depth){
	    section[depth-1]++;
	    for(i=0; i<depth-1; i++){
		printf("%d.", section[i]+offset);
	    }
	    printf("%d %s\n", section[i]+offset, cp);
	}
	else if(d>depth){
	    for(i=0; i<depth; i++){
		printf("%d.", section[i]+offset);
	    }
	    for(; i<d-1; i++){
		section[i]=0;
		printf("%d.", section[i]+offset);
	    }
	    section[i]=0;
	    printf("%d %s\n", section[i]+offset, cp);
	    depth=d;
	}
	else{
	    depth=d;
	    section[depth-1]++;
	    for(i=0; i<depth-1; i++){
		printf("%d.", section[i]+offset);
	    }
	    printf("%d %s\n", section[i]+offset, cp);
	}   
    }
}
