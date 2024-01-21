/*
 * $Id: decode-b.c,v 3.0 1996/02/27 10:19:23 morioka Exp $
 *
 * modified by Kenji Rikitake <kenji@reseau.toyonaka.osaka.jp>
 *			(based on Henry Spencer's un64 shell script)
 *         and David Masterson <davidm@prism.kla.com>
 */

#include <stdio.h>
#include <stdlib.h>

int get_base64_char_value(int chr)
{
    if( ('A' <= chr) && (chr <= 'Z') ){
	return chr - 'A';
    }
    else if( ('a' <= chr) && (chr <= 'z') ){
	return chr - 'a' + 26;
    }
    else if( ('0' <= chr) && (chr <= '9') ){
	return chr - '0' + 52;
    }
    else if(chr == '+'){
	return 62;
    }
    else if(chr == '/'){
	return 63;
    }
    return -1;
}

main()
{
    FILE* rfp = stdin;
    FILE* wfp = stdout;
    char  str[128];
    char* sp;
    int   ret;
    unsigned int   v1, v2, v3, v4, o1, o2, o3;
    
    while (fgets(str, 128, rfp)) {
	for (sp = str; *sp; ) {
	    if ((*sp == '\r') || (*sp == '\n')) break;
	    v1 = get_base64_char_value(*sp++);
	    v2 = get_base64_char_value(*sp++);
	    if ((v3 = get_base64_char_value(*sp++)) >= 0) {
	        if ((v4 = get_base64_char_value(*sp++)) >= 0) {
	            o1 = (v1 << 2) + (v2 >> 4);
	            o2 = ((v2 & 0x0f) << 4) + (v3 >> 2);
	            o3 = ((v3 & 0x03) << 6) + v4;
	            putc(o1, wfp);
	            putc(o2, wfp);
	            putc(o3, wfp);
		    }
		else {
	            o1 = (v1 << 2) + (v2 >> 4);
	            o2 = ((v2 & 0x0f) << 4) + (v3 >> 2);
	            putc(o1, wfp);
	            putc(o2, wfp);
		    return 0;
		    }
		}
	    else {
	        o1 = (v1 << 2) + (v2 >> 4);
	        putc(o1, wfp);
		return 0;
		}
	    }
        }
    return 0;
}
