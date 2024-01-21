/*
	System: Structured text retrieval tool sgrep.
	Module: pmatch.c
	Author: Pekka Kilpeläinen & Jani Jaakkola
	Description: Pattern matching using Aho-Corasick automate (ACsearch() )
	Version history: Original version February 1995 by JJ & PK
	Copyright: University of Helsinki, Dept. of Computer Science
		   Distributed under GNU General Public Lisence
		   See file COPYING for details
*/

/* NOTE: Aho-Corasick automate can only take constant patterns. There is
         no wild card expansions and it's always case sensitive. Maybe
         something should be done about this.
*/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <ctype.h>
#include "defines.h"

#define INPUT_BUF_SIZE 32768

struct INPUT_FILE *scan_files;
int last_scan;

unsigned char input_buf[INPUT_BUF_SIZE];
int buf_end;
int buf_pos;
int file_pos;
int file_num;
int fd;

#define NEXTCHAR ( (buf_pos)<(buf_end) ? input_buf[buf_pos++]:next_buf() )

struct OutPut {
	struct PHRASE_NODE *phrase;
	struct OutPut *next;
};

struct State_Data {
	struct State_Data *gotos[256];
	struct State_Data *fail;
	struct State_Data *next; /* queue needed when creating fail function */
	struct OutPut out_list;
#ifdef DEBUG
	int state_num;
#endif
};

struct State_Data *root_state=NULL;

struct State_Data *init_state();
void new_output( struct State_Data *s, struct PHRASE_NODE *pn);
void ACsearch();
void enter(struct PHRASE_NODE *pn);
void create_fail();
void create_goto();
int next_buf();

#ifdef DEBUG
void show_states(int depth,struct State_Data *s);
void show_ACtree();
void show_outputs(struct State_Data *s);
#endif

/*
 * Fills the input buffer and sets buf_pos, buf_end and file_num respectively
 * if all files have been processed returns -1 
 * otherwise returns next character 
 */
int next_buf()
{
	if ( file_num==-1 || scan_files[file_num].length==file_pos)
	{
		if (file_num!=-1 && scan_files[file_num].name!=NULL) 
			close(fd);
		file_num++;
		if (file_num>=last_scan)
		{
			return -1;
		}
		if (scan_files[file_num].name!=NULL)
		{
			/* Just a plain file */
			fd=open(scan_files[file_num].name,O_RDONLY);
		} else
		{
			/* We use tempfile, where stdin lies */
			fd=stdin_fd;
			if (lseek(fd,0,SEEK_SET)==-1)
			{
				perror("sgrep: lseek tmpfile");
				exit(2);
			}
		}
		if (fd==-1)
		{
			perror(scan_files[file_num].name);
			exit(2);
		}
		file_pos=0;
#ifdef DEBUG
		if (scan_files[file_num].name==NULL)
		{
			fprintf(stderr,"Processing temp file\n");
		}
		else fprintf(stderr,"Processing file %s\n",scan_files[file_num].name);
#endif
	}
	buf_end=read(fd,input_buf,
		(INPUT_BUF_SIZE < scan_files[file_num].length-file_pos)
		? INPUT_BUF_SIZE:scan_files[file_num].length-file_pos);
	if (buf_end==-1)
	{
		perror(scan_files[file_num].name);
		exit(2);
	}
	file_pos+=buf_end;
	if ( buf_end<INPUT_BUF_SIZE && file_pos!=scan_files[file_num].length )
	{
		fprintf(stderr,"File %s truncated before search\n",scan_files[file_num].name);
		exit(2);
	}
	buf_pos=1;
	return input_buf[0];
}
		
/*
 * Gives and inits a new state to the automate. If root_state is NULL
 * it inits the root_state 
 */
struct State_Data *init_state()
{
	int i;
	struct State_Data *s;
#ifdef DEBUG
	static int snum;
#endif
	s=(struct State_Data *)e_malloc( sizeof(struct State_Data));
	if ( root_state==NULL )
	{
#ifdef DEBUG
		printf("Init root state\n");
		snum=-1;
		s->fail=s;
#endif
		root_state=s;
	}
	for (i=0;i<256;i++) s->gotos[i]=NULL;
	s->out_list.phrase=NULL;
	s->out_list.next=NULL;
	s->next=NULL;
#ifdef DEBUG
	snum++;
	s->state_num=snum;
#endif
	return s;
}

/*
 * Enters a new output link to a state 
 */
void new_output( struct State_Data *s, struct PHRASE_NODE *pn)
{
	struct OutPut *op;
	
	if ( s->out_list.phrase==NULL )
	{
		s->out_list.phrase=pn;
		return;
	}
	op=&s->out_list;
	while (op->next!=NULL) op=op->next;
	op->next=(struct OutPut *) e_malloc(sizeof(struct OutPut));
	op=op->next;
	op->next=NULL;
	op->phrase=pn;
}

/*
 * Enters a new phrase to automate 
 */
void enter(struct PHRASE_NODE *pn)
{
	struct State_Data *state=root_state;
	int j;
	unsigned char pch;

#ifdef DEBUG
	printf("enter %s",pn->phrase->s);
#endif
	j=0;
	pch=pn->phrase->s[j];
	if (ignore_case) pch=toupper(pch);
	while ( state->gotos[pch]!=NULL && j<pn->phrase->length )
	{
		state=state->gotos[pch];
		j++;
		pch=pn->phrase->s[j];
		if (ignore_case) pch=toupper(pch);
	}
	
	while( j<pn->phrase->length )
	{
		state->gotos[pch]=init_state();
		state=state->gotos[pch];
		j++;
		pch=pn->phrase->s[j];
		if (ignore_case) pch=toupper(pch);
	}
	new_output(state,pn);
#ifdef DEBUG
	printf(" done\n");
#endif
}
	
/*
 * The creation of the AC goto function using the enter function 
 * and the phrase list. Also initializes a gc list for every
 * phrase 
 */
void create_goto(struct PHRASE_NODE *phrase_list)
{
	int i;
	struct PHRASE_NODE *pn=phrase_list;
#ifdef DEBUG
	struct PHRASE_NODE *lpn=NULL;
#endif

#ifdef DEBUG
	lpn=pn;
	while(lpn!=NULL)
	{
		fprintf(stderr,"Phrase:%s\n",lpn->phrase->s);
		lpn=lpn->next;
	}
	lpn=NULL;
#endif
	while (pn!=NULL)
	{
		/* Adding new phrase to automate */
		pn->GC_list=new_gclist();
		enter(pn);
		pn=pn->next;
	}
	
	/* there isn't any fail links from root state */
	for (i=0;i<256;i++)
	{
		if ( root_state->gotos[i]==NULL ) 
			root_state->gotos[i]=root_state;
	}
}

/*
 * The creation of the AC fail function and the final output function 
 */
void create_fail() 
{
	int i;
	struct State_Data *s,*r,*state;
	struct State_Data *first=NULL;
	struct State_Data *last=NULL;	
	struct OutPut *op;
	
#ifdef DEBUG
	printf("Create fail :");
#endif	
	for (i=0;i<256;i++)
	{
		if ( (s=root_state->gotos[i]) !=root_state )
		{		
			if (first==NULL) first=s;
			if (last==NULL) last=s;
			else
			{
				last->next=s;
				last=s;
			}
			last->next=NULL;
			s->fail=root_state;
		}
	}
#ifdef DEBUG
	printf(" root done");
#endif
	while (first!=NULL)
	{
		r=first;
		first=first->next;
		for (i=0;i<256;i++) if ( r->gotos[i]!=NULL )
		{
			s=r->gotos[i];
			last->next=s;
			last=s;
			last->next=NULL;
			if (first==NULL) first=last;
			state=r->fail;
			while (state->gotos[i]==NULL) state=state->fail;
			s->fail=state->gotos[i];
			for (	op=&(s->fail->out_list);
				op!=NULL && op->phrase!=NULL;
				op=op->next)
					new_output(s,op->phrase);
		}
	}
#ifdef DEBUG
	printf(", all done\n");
#endif
}

/* 
 * The AC automate search. Phrases list points to list of phrases to be
 * matched. ifs points to names of input files, and lf is the number of
 * input files.
 */
void ACsearch(struct PHRASE_NODE *phrase_list, struct INPUT_FILE *ifs,int lf)
{
	int ch;
	int i=0;
	struct State_Data *s;
	struct OutPut *op;

	scan_files=ifs;
	last_scan=lf;

	buf_end=-1;
	buf_pos=0;
	file_pos=0;
	file_num=-1;
	
	/* If root state==NULL the automate hasn't been created yet */	
	if (root_state==NULL)
	{
		init_state();
		create_goto(phrase_list);
		create_fail();
	}
	else
	{
		/* We have to create empty gc lists for phrases */
		struct PHRASE_NODE *j;
		for (j=phrase_list;j!=NULL;j=j->next)
		{
			j->GC_list=new_gclist();
		}
	}

#ifdef DEBUG
	show_ACtree();
#endif
	s=root_state;	
	ch=NEXTCHAR;
	if (ignore_case && ch!=-1) ch=toupper(ch);
	i++;
	while(ch!=-1)
	{
		while (s->gotos[ch]==NULL) s=s->fail;
		s=s->gotos[ch];
		op=&(s->out_list);
		if ( op->phrase!=NULL ) do
		{
			stats.phrases++;
#ifdef PROGRESS_REPORTS
			if (progress_output && !(i&4095))
			{
				fprintf(stderr,"searching phrases %d%% done\r",
					(i*100)/LAST);
				
			}
#endif
#ifdef assert
			assert(op->phrase->GC_list!=NULL);
#endif
			add_region( op->phrase->GC_list,
				i- ( op->phrase->phrase->length ) ,
				i-1);
#ifdef DEBUG
			printf("Found \"%s\" in file %s at %d  gc<-(%d,%d)\n",
				op->phrase->phrase->s,
				scan_files[file_num].name,
				file_pos-buf_end+buf_pos-(op->phrase->phrase->length),
				i- ( op->phrase->phrase->length ) , i-1);
#endif
			op=op->next;
		} while ( op!=NULL );
		ch=NEXTCHAR;
		if (ignore_case && ch!=-1) ch=toupper(ch);
		i++;
	}
#ifdef PROGRESS_REPORTS
	if (progress_output)
	{
		/* Cleaning the percentage line after pmatch */
		fprintf(stderr,"                              \r");
	}
#endif
}

#ifdef DEBUG
/*
 * These are (were) used for debugging the creation of ac-automate
 */
void show_ACtree()
{
	printf("-------------------\n AC-tree\n------------------\n");
	show_states(0,root_state);
	show_outputs(root_state); 
}

void show_states(int depth,struct State_Data *s)
{
	int end=0;
	int i,j;
	
	for(i=0;i<256;i++)
	{
		if (s->gotos[i]!=NULL && 
		   !( s==root_state && s->gotos[i]==root_state ) )
		{
			if (end>0)
			for (j=0;j<depth;j++) printf("                ");
			printf("%2d:f=%2d:",s->state_num,s->fail->state_num);
			end++;
			printf("%c->%2d , ",i,s->gotos[i]->state_num);
			show_states(depth+1,s->gotos[i]);
		}
	}
	if (!end)
	{
		printf("%2d:f=%2d <---\n",s->state_num,s->fail->state_num);
	}
}

void show_outputs(struct State_Data *s)
{
	int i;
	struct OutPut *op;
	
	op=&s->out_list;
	if (op->phrase!=NULL)
	{
		printf("state %d:",s->state_num);
		do {
			printf(" %s",op->phrase->phrase->s);
			op=op->next;
		} while ( op!=NULL );
		printf("\n");
	}
	
	for (i=0;i<256;i++)
	{
		if (s->gotos[i]!=NULL && s->gotos[i]!=root_state )
			show_outputs(s->gotos[i]);
	}
}		
#endif
