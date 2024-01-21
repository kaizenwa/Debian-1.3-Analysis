/*
	System: Structured text retrieval tool sgrep.
	Module: output.c
	Author: Pekka Kilpeläinen & Jani Jaakkola
	Description: handles outputting of a gc list ( show_gclist() )
	Version history: Original version February 1995 by JJ & PK
	Copyright: University of Helsinki, Dept. of Computer Science
		   Distributed under GNU General Public Lisence
		   See file COPYING for details
*/

#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include "defines.h"

#define PRINT_BUF_SIZE 1024

/*
 * These are given with a call to show_gc_list. They are declared global
 * inside this module, so that they don't have to be given separately to
 * every function
 */
struct INPUT_FILE * output_files;
int last_output;

/*
 * Here we read the region to be printed from file 
 */
char print_buffer[PRINT_BUF_SIZE];

/* 
 * This is the number of region to be printed 
 */
int region_number=1;

/* 
 * When not in stream mode, the output won't start at position 0.
 * This points out the start of output
 */
int first_ind;

/*
 * The last character that was printed. Needed for appending last
 * newline 
 */
int last_char;

int last_ofile=-1; /* Index to last file, from which regions was printed */

int warned;	/* Has warnings about too long regions been given ? */

/* 
 * Prints a region from file. If necessary opens a new file. Only one file
 * is kept open at a time. s and e are offsets into one file, not into whole
 * input stream. 
 */
void print_file_region(int file,int s,int e)
{
	static int fd;
	int err;
	int r;
	
#ifdef DEBUG
	if (output_files[file].name==NULL)
	{
		fprintf(stderr,"file_region <stdin>");
	}
	else fprintf(stderr,"file_region %s",output_files[file].name);
	fprintf(stderr,":(%d,%d)\n",s,e);
#endif

	if (file!=last_ofile)
	{
		if (last_ofile!=-1 && fd!=stdin_fd) close(fd);
		if ( output_files[file].name==NULL )
		{
			fd=stdin_fd;
		} else
		{
			fd=open(output_files[file].name,O_RDONLY);
		}
		if (fd==-1)
		{
			fprintf(stderr,"open %s: %s\n",output_files[last_ofile].name,
				strerror(errno));
			last_ofile=-1;
			return;
		}
		last_ofile=file;
	}	
	
	err=lseek(fd,s,SEEK_SET);
	if (err==-1)
	{
		if (output_files[last_ofile].name==NULL)
		{
			perror("lseek <stdin>");
		}
		else fprintf(stderr,"lseek %s: %s\n",output_files[last_ofile].name,
			strerror(errno));
	}
	else
	{
		do
		{
			r=(PRINT_BUF_SIZE<e-s+1) ? PRINT_BUF_SIZE:e-s+1;
			err=read(fd,print_buffer,r);
			if (err==-1)
			{
				if (output_files[last_ofile].name==NULL)
				{
					perror("read <stdin>");
				}
				else fprintf(stderr,"read %s: %s\n",
					output_files[last_ofile].name,
					strerror(errno));
				return;
			}
			if (err!=r)
			{
				if (output_files[last_ofile].name==NULL)
				{
					fprintf(stderr,"short read <stdin>");
					return;
				}				
				fprintf(stderr,"%s: short read\n",
					output_files[last_ofile].name);
				return;
			}
			fwrite(print_buffer,1,r,stdout);
			last_char=print_buffer[r-1];
			s+=r;
		} while (s<=e);
	}
}

/*
 * Finds out a file num where a given region resides using binary search 
 */
int bin_file_search(int s)
{
	int bs,be,bm;

#ifdef ASSERT
	int rounds=0;
	assert(s<=LAST);
#endif
	if (last_output==1) return 0;
	
	bs=0;be=last_output;
	bm=(bs+be)/2;
	while ( output_files[bm].start>s || 
		output_files[bm].start+output_files[bm].length<=s )
	{
		if (output_files[bm].start>s) be=bm;
		else bs=bm+1;
		bm=(bs+be)/2;
#ifdef ASSERT
		assert(++rounds<1000);
#endif
	}
	return bm;
}

/*
 * By using constant gc lists it's possible to have regions, which
 * exceed input size. So we need to make a check
 */
int check_region(int s,int e)
{
	
	if ( s>LAST )
	{
		if (warned!=2)
	fprintf(stderr,"Warning: region start point greater than input size detected\n");
		warned=2;
		return warned;
	}
	if (e>LAST)
	{
		if (warned==0)
	fprintf(stderr,"Warning: region end point greater than input size detected\n");
		warned=1;
	}
	return warned;
}

	
/*
 * Shows a region which might reside in more than one file. This is done
 * by finding out the files, where region is and calling print_file_region
 */
void show_region(int s,int e)
{
	int l;
	int fn;
	/* Start point and length of current file*/
	int fs,fl;
	int w;
	
	w=check_region(s,e);
	if (w==2)
	{
		/* can't print anything s>last */
		return;
	}
	if (w==1)
	{
		/* e>last */
		e=LAST;
	}
	
	fn=bin_file_search(s);
#ifdef DEBUG
	fprintf(stderr,"Printing region from file %s\n",
		output_files[fn].name);
#endif
	do
	{
		fs=output_files[fn].start;
		fl=output_files[fn].length;
		/* If region is in one file, then we need to call print_file
		   region only once. Otherwise we must make one call for
		   each file */
		l=(fs+fl<=e) ? fl-1:e-fs;
		print_file_region(fn,s-fs,l);
		fn++;
		s+=l-(s-fs)+1;
	} while (s<=e);
}	

/* 
 * Handles % commands in output_style string 
 */
void expand(int ch, struct REGION r)
{
	int i;
	
	last_char=0;
	
	switch (ch) {
	case 'f':
		if (r.start>LAST)
		{
			fputs("<input exceeded>",stdout);
			break;
		}
		i=bin_file_search(r.start);
		if (output_files[i].name==NULL)
		{
			fputs("<stdin>",stdout);
			break;
		}
		fputs(output_files[i].name,stdout);
		break;
	case 's':
		printf("%d",r.start+first_ind);
		break;
	case 'e':
		printf("%d",r.end+first_ind);
		break;
	case 'l':
		printf("%d",r.end-r.start+1);
		break;
	case 'i':
		if (r.start>LAST)
			i=last_output-1;
		else i=bin_file_search(r.start);
		printf("%d",r.start-output_files[i].start);
		break;
	case 'j':
		if (r.end>LAST)
			i=last_output-1;
		else i=bin_file_search(r.end);
		printf("%d",r.end-output_files[i].start);
		break;
	case 'r':
		show_region(r.start,r.end);
		break;
	case 'n':
		printf("%d",region_number);
		break;
	default:
		putchar('%');
		putchar(ch);
		last_char=ch;
		break;
	}
}

/* 
 * Handles \ escapes in output_style string 
 * Note: missing \000 - \377 
 */
void escape(int ch)
{
	last_char=0;
	
	switch (ch) {
	case 'n':
		putchar('\n');
		last_char='\n';
		break;
	case 't':
		putchar('\t');
		break;
	case '\\':
		putchar('\\');
		break;
	case '\"':
		putchar('\"');
		break;
	case '\r':
		putchar('\r');
		break;
	case '\f':
		putchar('\f');
		break;
	case '\b':
		putchar('\b');
		break;
	case '%':
		putchar('%');
		break;
	}
}
	
/*
 * Prints a gc list using output_style 
 * Ifs points to array of file input file data, for finding out file sizes &
 * names. lf gives last file number in ifs.
 * output_style is a global variable.
 */
void show_gc_list(struct GC_LIST *l, struct INPUT_FILE *ifs,int lf)
{
	struct GC_POINTER lp;
	struct REGION r;
	struct REGION p;
	struct INPUT_FILE no_stream_file;
	int i;
	int ch;
	
	output_files=ifs;
	last_output=lf;
	
	warned=0;
		
	first_ind=ifs[0].start;
	if (first_ind>0)
	{
		/* We don't have streams switch */
		no_stream_file=*ifs;
		output_files=&no_stream_file;
		no_stream_file.start=0;
	}		
	
#ifdef DEBUG
	fprintf(stderr,"show_gc_list: length=%d end=%d\n",LIST_SIZE(l),LAST);
#endif
	start_region_search(l,&lp);
	get_region(&lp,&r);
	if (r.start>0 && print_all)
	{
		/* There is text before first region */
		show_region(0,r.start-1);
	}

	if (r.start==-1 && print_all) {
		/* There was no regions, but we are in filter mode */
		show_region(0,LAST);
	}
	
	while ( r.start!=-1 )
	{
		for(i=0;(ch=output_style[i]);i++)
		{
			if ( (ch=='%' || ch=='\\') && output_style[i+1] )
			{
				if (ch=='%') expand(output_style[++i],r);
				if (ch=='\\') escape(output_style[++i]);
			} else 
			{
				putchar(ch);
				last_char=ch;
			}
		}
		p=r;
		get_region(&lp,&r);
		if (r.start==-1 && print_all && p.end<LAST )
		{
			/* There is text after last region */
			show_region(p.end+1,LAST);
		}
		if (r.start>0 && p.end<r.start-1 && print_all)
		{
			/* There is text between two regions */
			show_region(p.end+1,r.start-1);
		}
		region_number++;
	}
	if (last_char!='\n' && print_newline ) putchar('\n');
	last_ofile=-1;
}
