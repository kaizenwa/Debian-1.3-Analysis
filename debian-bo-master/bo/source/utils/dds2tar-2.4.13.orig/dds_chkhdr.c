
/*
 * This file is part of dds2tar.
 * Copyright by J"org Weule
 */

#include <stdlib.h>
#include <stdio.h>
#include <sys/mtio.h>
#include <unistd.h>
#include <string.h>
#include "dds2tar.h"
#include "dds_tape.h"

int dds_is_tar_header_record(tar_record*const ptr){
	int     i;
	unsigned int     n = 0;
	unsigned char	*p = (char*)ptr ;

	for (i = 0; i < 148; i++)
		n += p[i];
	for (i = 0; i < 8; i++)
		n += ' ';
	for (i = 156; i < 512; i++)
		n += p[i];
	sscanf(p + 148, "%8o", &i);
	if ( n != ((int)' ') * 8 ){
		if ( i == 0 ) {
			sprintf(p+148 , "%o", n );
		} else
		if ( i != n ) return 0;
		sscanf(ptr->hdr.size,"%o",&i);
		i += 512 + 511 ;
		i >>= 9 ;
	} else i=1 ;
	return i;
}


