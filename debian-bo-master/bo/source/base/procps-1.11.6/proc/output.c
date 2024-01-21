/*
 * Some output conversion routines for libproc
 * Copyright (C) 1996, Charles Blake.  See COPYING for details.
 *
 * Helmut Geyer 18/7/1996:
 * converted all output functions to write to a string, so they can be used
 * for all programs. collected diverse output routines from w, top, ps
 * and showtask.c  and put them here. wrote some new output routines.
 *
 * General convention: 
 * sprint_routine (char *s, output_type output,unsigned max, special sp);
 * fprint_routine (FILE *fp, output_type output,unsigned max, special sp);
 * s must be at least of Length max+1 and has to be allocated/defined
 * by the calling function.
 *
 * output routines are provided for the following output types:
 *  type     routine       special optim  what
 *                                 length
 * char *s  ?print_str        -      ?    print a string
 * char **s ?print_strlist   sep     ?    print an array of strings
 * int hex  ?print_hex        -       8   print an integer as Hex
 * int dec  ?print_dec        -      10   print an integer as Dec
 * int oct  ?print_oct        -      11   print an integer as Oct
 * int bin  ?print_bin        -      32   print an integer as Bin
 * int perc ?print_perc       -       4   print an integer (0<=i<=999) as perc.
 * int size ?print_size      mode     5   print a size in M, k, or units
 * time_t t ?print_time_ival centis   6   print a time interval
 * time_t t ?print_utime                  print a universal time 
 * 
 * in this table optimal length is a worst case scenario, often much
 * shorter lengths suffice. all functions return the number of
 * characters printed.
 * 
 * There is no fprint_bin function.
 */

#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <string.h>
#include "proc/output.h"

static const unsigned long long lenarr[] = 
{ 0, 10, 100, 1000, 10000, 100000, 1000000LL, 10000000LL, 100000000LL,
  1000000000LL, 10000000000LL, 100000000000LL, 1000000000000LL,
  10000000000000LL, 100000000000000LL, 1000000000000000LL,
  10000000000000000LL, 100000000000000000LL, 1000000000000000000LL,
  10000000000000000000ULL};

int force_table_size = 0;

/*
 * output a string, converting unprintables to octal as we go, and
 * stopping after processing max chars of output (accounting for
 * expansion due to octal rep). 
 */
int sprint_str(char *s, char *str, int max) {
    int i,l;
    for (l=0,i=0; str[i] && l < max; i++)
	if (isprint(str[i]) || str[i] == ' ')
	    s[l++] = str[i];
	else {
	    if (max - l > 3) {
		sprintf(&(s[l]), "\\%03o", str[i]);
		l += 4; 
	    } else
		return l;
	}
    return l;
}

int fprint_str(FILE *fp, char *str, int max) {
    int i,l;
    for (l=0,i=0; str[i] && l < max; i++)
	if (isprint(str[i]) || str[i] == ' ') {
            l++;
	    fputc(str[i],fp);
        } else {
	    if (max - l > 3) {
		fprintf(fp, "\\%03o", str[i]);
		l += 4; 
	    } else
		return l;
	}
    return l;
}

/*
 * output an argv style NULL-terminated string list, converting unprintables
 * to octal as we go, separating items of the list by 'sep' and stopping after
 * processing max chars of output (accounting for expansion due to octal rep).
 */
int sprint_strlist(char *s, char **strs, int max, char* sep) {
    int n, seplen = strlen(sep);
    for (n=0; *strs && n < max; ) {
        n += sprint_str(&(s[n]), *strs, max - n);
        strs++;
	if (*strs && n + seplen < max) {
	    strcat(s,sep);
	    n += seplen;
	} else
	    return n;
    }
    return n;
}

int fprint_strlist(FILE *fp, char **strs, int max, char* sep) {
    int n, seplen = strlen(sep);
    for (n=0; *strs && n < max; ) {
        n += fprint_str(fp, *strs, max - n);
        strs++;
	if (*strs && n + seplen < max) {
	    fputs(sep, fp);
	    n += seplen;
	} else
	    return n;
    }
    return n;
}

int sprint_hex(char *s, unsigned long d, int max) {
 
    if (max >= 16) {
        sprintf(s, "%*lx", max, d);
    } else if ( (d >> (max*4 -1)) ) {
        if (force_table_size){
            sprintf(s, "%*.*s", max, max, ">>>>>>>>>>>>>>>>>>>>>>>>");
        } else {
            return ( - sprintf(s, "%lx", d));
        }
    } else {
        sprintf(s, "%*lx", max, d);
    }
    return max;
}

int fprint_hex(FILE *fp, unsigned long d, int max) {
 
    if (max >= 16) {
        fprintf(fp, "%*lx", max, d);
    } else if ( (d >> (max*4 -1)) ) {
        if (force_table_size){
            fprintf(fp, "%*.*s", max, max, ">>>>>>>>>>>>>>>>>>>>>>>>");
        } else {
            return ( - fprintf(fp, "%lx", d));
        }
    } else {
        fprintf(fp, "%*lx", max, d);
    }
    return max;
}

int sprint_oct(char *s, unsigned long d, int max) {
 
    if (max >=22) {
        sprintf(s, "%*lo", max, d);
    } else if ( (d >> (max*3 -1)) ) {
        if (force_table_size){
            sprintf(s, "%*.*s", max, max, ">>>>>>>>>>>>>>>>>>>>>>>>");
        } else {
            return ( - sprintf(s, "%lo", d));
        }
    } else {
        sprintf(s, "%*lo", max, d);
    }
    return max;
}

int fprint_oct(FILE *fp, unsigned long d, int max) {
 
    if (max >=22) {
        fprintf(fp, "%*lo", max, d);
    } else if ( (d >> (max*3 -1)) ) {
        if (force_table_size){
            fprintf(fp, "%*.*s", max, max, ">>>>>>>>>>>>>>>>>>>>>>>>");
        } else {
            return ( - fprintf(fp, "%lo", d));
        }
    } else {
        fprintf(fp, "%*lo", max, d);
    }
    return max;
}

int sprint_dec(char *s, long d, int max) {
    
    if (max >20) max=20;
    if (d < 0) {
        if ( -d > lenarr[max-1] ) {
            if (force_table_size) {
                sprintf(s, "%*.*s", max, max, "<<<<<<<<<<<<<<<<<<<<<<<<<");
                return max;
            } else {
                return (- sprintf(s, "%ld", d));
            }
        }
    } else {
        if ( d > lenarr[max] ) {
            if (force_table_size) {
                sprintf(s, "%*.*s", max, max, ">>>>>>>>>>>>>>>>>>>>>>>>");
                return max;
            } else {
                return (- sprintf(s, "%ld", d));
            }
        }
    }
    sprintf(s, "%*ld", max, d);
    return max;
}

int fprint_dec(FILE *fp, long d, int max) {
    
    if (max >19) max=19;
    if (d < 0) {
        if ( -d > lenarr[max-1] ) {
            if (force_table_size) {
                fprintf(fp, "%*.*s", max, max, "<<<<<<<<<<<<<<<<<<<<<<<<<");
                return max;
            } else {
                return (- fprintf(fp, "%ld", d));
            }
        }
    } else {
        if ( d > lenarr[max] ) {
            if (force_table_size) {
                fprintf(fp, "%*.*s", max, max, ">>>>>>>>>>>>>>>>>>>>>>>>");
                return max;
            } else {
                return (- fprintf(fp, "%ld", d));
            }
        }
    }
    fprintf(fp, "%*ld", max, d);
    return max;
}

int sprint_bin(char *s, unsigned long d, int max) {
    int i, j=d;

    if ( (d >> (max-1)) ) {
        sprintf(s, "%*.*s", max, max, ">>>>>>>>>>>>>>>>>>>>>>>>");
        return max;
    } else {
        while (d >> (max-1)) max++;
    }
    for (i=0; i< max; i++){
        if (j & 0x1)
            s[max-i-1]='1';
        else
            s[max-i-1]='0';
        j=j >> 1;
    }
    s[max]='\0';
    
    return max;
}

int sprint_perc (char *s, unsigned int d, int max) {
    
    if (max > 4) max =4;
    switch (max){
    case 4:
        sprintf(s, "%2d.%1d", d / 10, d % 10);
        return 4;
        break;
    case 3:
        if (d > 100) 
            sprintf(s, "%2d.", d / 10);
        else
            sprintf(s, "%1d.%1d ", d / 10, d % 10);
        return 3;
        break;
    case 2:
        sprintf(s, "%2d", d / 10);
        return 2;
        break;
    default:
        return 0;
    }
}

int fprint_perc (FILE *fp, unsigned int d, int max) {
    
    if (max > 4) max =4;
    switch (max){
    case 4:
        fprintf(fp, "%2d.%1d", d / 10, d % 10);
        return 4;
        break;
    case 3:
        if (d > 100) 
            fprintf(fp, "%2d.", d / 10);
        else
            fprintf(fp, "%1d.%1d ", d / 10, d % 10);
        return 3;
        break;
    case 2:
        fprintf(fp, "%2d", d / 10);
        return 2;
        break;
    default:
        return 0;
    }
}

int sprint_size(char *s, long d, int max, int mode) {

    switch (mode) {
    case OUT_SZ_PM:
        return (sprint_dec(s, d , max));
        break;
    case OUT_SZ_MK:
        if (d < lenarr[max]) {
            sprintf(s,"%*ld", max, d);
        } else if ( max > 2 && d/1024 < lenarr[max-2]) {
            sprintf(s,"%*ldM%1d", max-2, d/1024, ((int)(d/102.4))%10);
        } else if ( max > 1 && d/1024 < lenarr[max-1]) {
            return(sprintf(s, "%*ldM", max-1, d/1024));
        } else {
            if (force_table_size) {
                sprintf(s,"%*.*s", max, max, ">>>>>>>>>>>>>>>>>>>>>>>>");
            } else {
                return ( - sprint_size(s, d, max+1, mode));
            }   
        }
        return max;
        break;
    default:
        return 0;
        break;
    }
}

int fprint_size(FILE *fp, long d, int max, int mode) {

    switch (mode) {
    case OUT_SZ_PM:
        return (fprint_dec(fp, d , max));
        break;
    case OUT_SZ_MK:
        if (d < lenarr[max]) {
            fprintf(fp,"%*ld", max, d);
        } else if ( max > 2 && d/1024 < lenarr[max-2]) {
            fprintf(fp,"%*ldM%1d", max-2, d/1024, ((int)(d/102.4))%10);
        } else if ( max > 1 && d/1024 < lenarr[max-1]) {
            return(fprintf(fp, "%*ldM", max-1, d/1024));
        } else {
            if (force_table_size) {
                fprintf(fp,"%*.*s", max, max, ">>>>>>>>>>>>>>>>>>>>>>>>");
            } else {
                return ( - fprint_size(fp, d, max+1, mode));
            }   
        }
        return max;
        break;
    default:
        return 0;
        break;
    }
}

/*
 * print a time interval to 6 or 3 characters. in the shorter case, 
 * intervals larger than 99 days will be rendered as 99 days.
 */ 
int sprint_time_ival(char *s, time_t t, int max, int centi_sec) {

    if (max >6) max=6;
    
    switch(max) {
    case 6:
        if (t < 0 || t >= 24*60*60*99999LL)  {     /* time overflow */
            if (force_table_size) {
                sprintf(s, ">>>>>d");
            } else {
                return (- sprintf(s,"%lud",t/(24*60*60)));
            }   
        } else if (t >= 48*60*60)                          /* > 2 days */
            sprintf(s, "%5lud", t/(24*60*60));
        else if (t >= 60*60)                        /* > 1 hour */
            sprintf(s, "%2lu:%02uh", t/(60*60), (unsigned) ((t/60)%60));
        else if (t > 60)                            /* > 1 minute */
            sprintf(s, "%2lu:%02um", t/60, (unsigned) t%60);
        else
            sprintf(s, "%2lu.%02us", t, centi_sec);
        return 6;
        break;
    case 5:
    case 4:
    case 3:
        if (t >= 48*60*60*99) {
            if (force_table_size) {
                sprintf(s, ">>d");
            } else {
                return (- sprintf(s,"%lud",t/(24*60*60)));
            }   
        } else if (t >= 48*60*60)                          /* > 2 days */
            sprintf(s, "%2lud", t/(24*60*60));
        else if (t >= 60*60)                        /* > 1 hour */
            sprintf(s, "%2luh", t/(60*60));
        else if (t > 60)                            /* > 1 minute */
            sprintf(s, "%2lum", t/60);
        else
            sprintf(s, "%2lus", t);
        return 3;
        break;
    default:
        return 0;
        break;
    }
}

int fprint_time_ival(FILE *fp, time_t t, int max, int centi_sec) {

    if (max >6) max=6;
    
    switch(max) {
    case 6:
        if (t < 0 || t >= 24*60*60*99999LL) {      /* time overflow */
            if (force_table_size) {
                fprintf(fp, ">>>>>d");
            } else {
                return (- fprintf(fp,"%lud",t/(24*60*60)));
            }   
        } else if (t >= 48*60*60)                          /* > 2 days */
            fprintf(fp, "%5lud", t/(24*60*60));
        else if (t >= 60*60)                        /* > 1 hour */
            fprintf(fp, "%2lu:%02uh", t/(60*60), (unsigned) ((t/60)%60));
        else if (t > 60)                            /* > 1 minute */
            fprintf(fp, "%2lu:%02um", t/60, (unsigned) t%60);
        else
            fprintf(fp, "%2lu.%02us", t, centi_sec);
        return 6;
        break;
    case 5:
    case 4:
    case 3:
        if (t >= 24*60*60*99) {   /* > 99 days */
            if (force_table_size) {
                fprintf(fp, ">>d");
            } else {
                return (- fprintf(fp,"%lud",t/(24*60*60)));   
            }
        } else if (t >= 48*60*60)                          /* > 2 days */
            fprintf(fp, "%2lud", t/(24*60*60));
        else if (t >= 60*60)                        /* > 1 hour */
            fprintf(fp, "%2luh", t/(60*60));
        else if (t > 60)                            /* > 1 minute */
            fprintf(fp, "%2lum", t/60);
        else
            fprintf(fp, "%2lus", t);
        return 3;
        break;
    default:
        return 0;
        break;
    }
}

int sprint_time_old_ival(char *s, time_t t, int max ) {

    if (t/60 >= lenarr[max-3]){
        if (force_table_size) {
            sprintf(s, "%*.*s", max, max, ">>>>>>>>>>>>>>>>>>>>>>>>");
        } else {
            return( - sprintf(s, "%lu:%02u", t/60, (unsigned) t%60));
        }
    } else {
        sprintf(s, "%*lu:%02u", max-3, t/60, (unsigned) t%60);
    }
    return max;
}

int fprint_time_old_ival(FILE *fp, time_t t, int max ) {

    if (t/60 >= lenarr[max-3]){
        if (force_table_size) {
            fprintf(fp, "%*.*s", max, max, ">>>>>>>>>>>>>>>>>>>>>>>>");
        } else {
            return( - fprintf(fp, "%lu:%02u", t/60, (unsigned) t%60));
        }
    } else {
        fprintf(fp, "%*lu:%02u", max-3, t/60, (unsigned) t%60);
    }
    return max;
}

