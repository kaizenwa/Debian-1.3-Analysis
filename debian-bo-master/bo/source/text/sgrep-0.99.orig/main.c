/*
	System: Structured text retrieval tool sgrep.
	Module: main.c
	Author: Pekka Kilpeläinen & Jani Jaakkola
	Description: Parsing of command line options,
		     Reading command files
		     Scanning for input files
		     Calling other modules for preprocessing, parsing,
		     	pattern matching, evaluation and output
	Version history: Original version February 1995 by JJ & PK
	Copyright: University of Helsinki, Dept. of Computer Science
		   Distributed under GNU General Public Lisence
		   See file COPYING for details
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/times.h>
#include "defines.h"

void check_files(int ,char *[],int);
void clear_stats();
void show_stats();
void show_times();
int get_options(char *[]);
void add_command(char *);
int read_stdin();
void read_expressions();
int environ_options();
void run_stream(struct TREE_NODE *, struct PHRASE_NODE *p_list);
void run_one_by_one(struct TREE_NODE *, struct PHRASE_NODE *p_list);
void create_constant_lists();
void copyright_notice();

/*
 * The global variables common to all modules. See declarations in defines.h 
 */
char *output_style=SHORT_OUTPUT; /* default is short */
int open_failure=OPEN_FAILURE;
struct STATS stats;
int print_newline=TRUE;
int stdin_fd=-1;	/* not opened yet */
int print_all=FALSE;
int gc_lists_now=0;
struct GC_LIST *end_list=NULL;       
struct GC_LIST *start_list=NULL;       
struct GC_LIST *chars_list=NULL;
int ignore_case=FALSE;
#ifdef STREAM_MODE
 int stream_mode=TRUE;
#else
 int stream_mode=FALSE;
#endif
#ifdef PROGRESS_REPORTS
int progress_output=FALSE;
#endif

/*
 * Global variables used inside main.c . These are mainly used for storing
 * information about given options
 */
int have_stats=FALSE;	/* Should we show statistics in the end (-T) */
int have_times=FALSE;   /* Should we show info about used time in the end (-t) */
int do_concat=TRUE;	/* Shall we do concat operation on result list (-d) */
int display_count=FALSE;/* Should we display only count of matching regions (-c) */
int no_output=FALSE;	/* Should we supress normal output (-q) */
int command_file_given=FALSE; /* If a command file name was given with -f 
				option this is set, and no commands are read
				from command line anymore */
int show_expr=FALSE;		/* only show expression, don't execute it (-P) */
char *preprocessor=PRE_PROCESSOR; /* Which preprocessor to use (-p) */
int read_sgreprc; 		/* are we going to read sgreprc (-n) */

char com_buf[COMBUF_SIZE]; 	/* preprosessed command buffer */
char com_file_buf[COMBUF_SIZE]; /* not preprocessed command file buffer */
int com_buf_size; 		/* How much it is actually used */
int com_file_buf_used;
char *home_file; 		/* pointer to whole path of $HOME/USER_SGREPRC */
struct INPUT_FILE *input_files=NULL; /* Table of input files */
int last_file=0;                 /* Index of last input file */
int stdin_read=FALSE;		/* Since expressions and files can both
				   be read from stdin, we got to make
				   sure that stdin is read only once */
/*
 * struct for list of expression strings ( or files ) to be executed 
 */
struct EXPR_TYPE {
	int type; 		/* If this is a file, or command line */
	char *expr; 		/* Pointer to either filename or expression */
} expr_table[MAX_EXPRESSIONS];
int exprs;			/* How many expressions there were */
enum EXPR_TYPES { E_FILE,E_TEXT };

/*
 * Struct for time information 
 */
struct time_points {
	struct tms start;
	struct tms parsing;
	struct tms acsearch;
	struct tms eval;
	struct tms output;
} tps;

/*
 * The copyright notice text. 
 */
char *copyright_text[]={
	"sgrep version "VERSION" - search a file for structured pattern",
	"Copyright (C) 1996  University of Helsinki",
	"",
	"This program is free software; you can redistribute it and/or modify",
	"it under the terms of the GNU General Public License as published by",
	"the Free Software Foundation; either version 2 of the License, or",
	"(at your option) any later version.",
	"",
	"This program is distributed in the hope that it will be useful,",
	"but WITHOUT ANY WARRANTY; without even the implied warranty of",
	"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the",
	"GNU General Public License for more details.",
	"",
	"You should have received a copy of the GNU General Public License",
	"along with this program; if not, write to the Free Software",
	"Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.",
	"",
	"Authors: Pekka Kilpeläinen       Pekka.Kilpelainen@cc.helsinki.fi",
	"         Jani Jaakkola           Jani.Jaakkola@cc.helsinki.fi",
	NULL
};

/* 
 * Struct for options 
 */
struct opt_data {
	char opt;
	char *have_param;
	char *what_does;
};

/*
 * List & description of options
 * If you add more options, add the descriptions here. Put the implementation
 * of option in get_options() 
 */
struct opt_data options[]= {
	{ 'a',NULL,"act as a filter" },
	{ 'C',NULL,"display copyright notice" },
	{ 'c',NULL,"display only count of matching regions" },
#ifdef PROGRESS_REPORTS
	{ 'D',NULL,"verbose, show progress" },
#endif
	{ 'd',NULL,"don't do concat on result list"},
	{ 'h',NULL,"help (means this text)" },
	{ 'i',NULL,"ignore case distinctions in phrases" },
	{ 'l',NULL,"long output format" },
	{ 'N',NULL,"don't add trailing newline" },
	{ 'n',NULL,"don't read $HOME/"USER_SGREPRC" or "SYSTEM_SGREPRC},
	{ 'P',NULL,"show preprocessed expression, don't execute it." },
	{ 'q',NULL,"supress normal output" },
	{ 'S',NULL,"stream mode (regions extend across files)"},
	{ 's',NULL,"short output format" },
	{ 'T',NULL,"show statistics about what was done" },
	{ 't',NULL,"show information about time spent"},
	{ 'V',NULL,"display version information" },
	{ 'e',"<expression>","execute expression (after preprocessing)" },
	{ 'f',"<file>","reads commands from file" },
	{ 'O',"<file>","reads output style from file"},
	{ 'o',"<style>","set output style. See man page for details"},
#ifdef USE_EXEC
	{ 'p',"<program>","preprocess expression using external preprocessor" },
#endif
	{ 0,NULL,NULL }
};

int main(int argc, char *argv[])
{
	struct TREE_NODE concat;
	struct TREE_NODE *root;
	struct PHRASE_NODE *p_list;
	int end_options;
	char *hp;
	
	/* Initialize the statistics gathering struct */
	clear_stats();
	times(&tps.start);
	
	com_buf_size=0;
	com_file_buf_used=0;
	
	/*
	 * If we are going to use either $HOME/sgreprc or system/sgreprc,
	 *  we must check if we can read those files 
	 */ 
	read_sgreprc=FALSE;
	hp=getenv("HOME");
	if (hp!=NULL)
	{
		home_file=(char *)e_malloc(strlen(hp)+strlen(USER_SGREPRC)+1);
		strcpy(home_file,hp);
		strcat(home_file,"/");
		strcat(home_file,USER_SGREPRC);
		if (access(home_file,R_OK)==0)
		{
			expr_table[0].type=E_FILE;
			expr_table[0].expr=home_file;
			read_sgreprc=TRUE;
		}
	}
	if (!read_sgreprc && access(SYSTEM_SGREPRC,R_OK)==0)
	{
		expr_table[0].type=E_FILE;
		expr_table[0].expr=SYSTEM_SGREPRC;
		read_sgreprc=TRUE;
	}
	exprs=1;
	
	/* 
	 * Process environment options 
	 */
	end_options=environ_options();
		
	/* 
	 * Get the command line options 
	 */
	if (end_options!=-1 )
		end_options=get_options(argv+1);
	if ( end_options!=-1 && !command_file_given && argv[end_options]==NULL )
	{
		/* we need some expression to process */
		fprintf(stderr,"You have to give an expression line if you don't use -f or -e switch.\n");
		end_options=-1;
	}
	if (end_options==-1)
	{
		/* There was error. Let's print usage information */
		struct opt_data *o=options;
		fprintf(stderr,"Usage: sgrep [ -");
		while (o->opt!=0)
		{
			if (o->have_param!=NULL)
			{
				fprintf(stderr," -%c %s",
					o->opt,o->have_param);
	         		} else fprintf(stderr,"%c",o->opt);
			o++;
		}
		fprintf(stderr," ] \'expr\' [<files...>]\n");
		fprintf(stderr,"sgrep -h for help\n");
		exit(2);
	}
	
	/* 
	 * Shall we get expression from command line 
	 */
	if (!command_file_given)
	{
		expr_table[exprs].type=E_TEXT;
		expr_table[exprs++].expr=(argv[end_options]);
		end_options++;
	}
	
	/* 
	 * Reading all expressions to buffer 
	 */
	read_expressions();
		
	/* 
	 * Invoking preprocessor (external because there is no internal yet) 
	 */
#ifdef DEBUG
	fprintf(stderr,"Preprocessing expression.\n");
#endif
	preprocess(com_file_buf,com_buf,preprocessor,COMBUF_SIZE);
	
	/*
	 * If we have show_expr then we show preprocessed expression, and
	 * stop here 
	 */
	if (show_expr)
	{
		fprintf(stderr,"%s\n",com_buf);
		exit(0);
	}
	
	/* 
	 * Should we read stdin to temp file 
	 */
	if (argv[end_options]==NULL)
	{
#ifdef DEBUG
		fprintf(stderr,"Reading stdin.\n");
#endif
		if (read_stdin()==0)
		{
			fprintf(stderr,"Empty stdin\n");
			exit(2);
		}
	} else
	/*
	 * If stdin is not used, we check every input file 
	 */
	{
#ifdef DEBUG
			fprintf(stderr,"Scanning through files.");
#endif
		check_files(argc,argv,end_options);
	}

	/*  
	 * Counting the input size 
	 */
	stats.input_size=input_files[last_file-1].start+
		input_files[last_file-1].length-1;

	/* 
	 * Creating constant lists 
	 */
	create_constant_lists();
	
	/* 
	 * Invoking parser 
	 */
#ifdef DEBUG
	fprintf(stderr,"Starting parser.\n");
#endif
	root=parse_string(com_buf,&p_list);

	/*
	 * Optimize the operator tree 
	 */
#ifdef DEBUG
	fprintf(stderr,"Optimizing operator tree\n");
#endif
	optimize_tree(&root,&p_list);

	if (do_concat)
	{
	/* If we do concat on result list, we have to add it to parse tree */
		concat.oper=CONCAT;
		concat.left=root;
		concat.right=NULL;
		concat.leaf=NULL;
		concat.parent=NULL;
		concat.refcount=1;
		concat.GC_list=NULL;
		root=&concat;
	};

	times(&tps.parsing);
	
	/*
	 * Evaluation style depends on stream_mode 
	 */
	if (stream_mode)
		run_stream(root,p_list);
	else
		run_one_by_one(root,p_list);
	
	/* 
	 * Should we show statistics 
	 */
	if (have_stats) show_stats();
	
	/* 
	 * Should we show information about time spend 
	 */
	if (have_times) show_times();
	
	if (stats.output==0)
		return 1; /* Empty result list */
	/* non empty result list */
	return 0;
}

/* 
 * Runs sgrep file by file
 */
void run_one_by_one(struct TREE_NODE *root, struct PHRASE_NODE *p_list)
{
	struct GC_LIST *result;
	int i;
	int save_print_newline;
	struct tms t_pmatch,t_eval,t_output,t_last,t_now;

#define CALC_TIME(TIME)	do { \
	times(&t_now);  \
	(TIME).tms_utime+=t_now.tms_utime-t_last.tms_utime; \
	(TIME).tms_stime+=t_now.tms_stime-t_last.tms_stime; \
	t_last=t_now; } while (0)
	
	t_last=tps.parsing;
	t_pmatch.tms_utime=0;
	t_pmatch.tms_stime=0;
	t_eval=t_pmatch;
	t_output=t_pmatch;
	
#ifdef DEBUG
	fprintf(stderr,"one by one: input_files=%d\n",last_file);
#endif
	save_print_newline=print_newline;
	print_newline=FALSE;
	
	for (i=0;i<last_file;i++)
	{
#ifdef DEBUG
		fprintf(stderr,"file #%d:%s\n",i,input_files[i].name);
#endif
		/* We got to clear root nodes gc list so that eval won't think
		   that it's already evaluated */
		root->GC_list=NULL;

		/* end is now the size of file now being evaluated */
		end_list->first->list[0].start=input_files[i].length-1;
		end_list->first->list[0].end=input_files[i].length-1;
		/* chars list size is the size of file being evaluates */
		chars_list->length=input_files[i].length;
		
		ACsearch(p_list,&input_files[i],1);
		CALC_TIME(t_pmatch);
		
		result=eval(root);
		stats.output+=LIST_SIZE(result);
		CALC_TIME(t_eval);
		
		if (i==last_file-1) print_newline=save_print_newline;
		if ( !display_count && !no_output && (
			LIST_SIZE(result)>0 || print_all ))
		{
			show_gc_list(result,&input_files[i],1);
		}

		/* We free result list,except when we got constant list
		   as result list */
		if (gc_lists_now==stats.constant_lists+1)
		{
			free_gclist(result);
		}
		CALC_TIME(t_output);
		
#ifdef ASSERT
		/*
		 * Now should only constant lists be left
		 */
		assert(gc_lists_now==stats.constant_lists);
#endif
	}
	if ( display_count && !no_output )
	{
		printf("%d\n",stats.output);
	}
	fflush(stdout);

	tps.acsearch=tps.parsing;
	tps.acsearch.tms_utime+=t_pmatch.tms_utime;
	tps.acsearch.tms_stime+=t_pmatch.tms_stime;
	tps.eval=tps.acsearch;
	tps.eval.tms_utime+=t_eval.tms_utime;
	tps.eval.tms_stime+=t_eval.tms_stime;
	tps.output=tps.eval;
	tps.output.tms_utime+=t_output.tms_utime;
	tps.output.tms_stime+=t_output.tms_stime;
	
	
}
		
#undef DEBUG
/*
 * Runs sgrep in stream mode
 */
void run_stream(struct TREE_NODE *root, struct PHRASE_NODE *p_list)
{
	struct GC_LIST *result;
			
	/* Pattern matching on input files */	
#ifdef DEBUG
	fprintf(stderr,"Starting ACsearch\n");
#endif
	ACsearch(p_list,input_files,last_file);
	times(&tps.acsearch);
	
	/* Evaluate the expression */
#ifdef DEBUG
	fprintf(stderr,"Evaluating.\n");
#endif
	result=eval(root);
#ifdef ASSERT
	assert(gc_lists_now<=4);
#endif
	times(&tps.eval);
	
	/* Outputting result */
#ifdef DEBUG
	fprintf(stderr,"Output result.\n");
#endif
	fflush(stderr);
	
	stats.output=LIST_SIZE(result);
	/* Should we show the count of matching regions */
	if ( display_count )
	{
		printf("%d\n",LIST_SIZE(result));
	}
	/* We show result list only if there wasn't -c option, and there was
	   something to output */
	if ( !display_count && !no_output && (
			stats.output>0 || print_all ))
		show_gc_list(result,input_files,last_file);
	fflush(stdout);
	times(&tps.output);
}

/*
 * Prints help 
 */
void print_help()
{
	int i;
	
	printf("Usage: sgrep <options> 'region expression' [<files...>]\n");
	printf("If no files are given stdin is used instead.\n");
	printf("\noptions are:\n");
	for (i=0;options[i].opt!=0;i++)
	{
		printf("\t-%c %s\t%s\n",
			options[i].opt,
			(options[i].have_param==NULL) ?
				(char *)"\t":
				options[i].have_param,
			options[i].what_does);
	}
	printf("\t--\t\tno more options\n");
	printf("Options can also be specified with "ENV_OPTIONS" environment variable\n");
	printf("\nCopyright (C) 1996 University of Helsinki. Use sgrep -C for details,\n\n");	
	exit(0);
}

/*
 * Creates and initializes the constant lists, start end and chars.
 * They may need to be modified later, because when scanning each
 * file separately end point keeps changing
 */
void create_constant_lists()
{
	/* start list always is just (0,0) */
	start_list=new_gclist();
	add_region(start_list,0,0);
	
	/* if in one by one mode, end lists region will be changed
	   to the file size being evaluated */
	end_list=new_gclist();
	add_region(end_list,stats.input_size,stats.input_size);
	
	/* Chars list is optimized and created in a special way */
	chars_list=new_gclist();
	to_chars(chars_list,1);
	
	stats.constant_lists+=3;
}

/*
 * Returns argument given to option like -o <arg> or -o<arg> 
 */
char *get_arg(char *(*argv[]),int *i,int *j)
{
	char *r;
	
	if ((*(*argv))[*j+1]==0)
	{
		if ( ((*argv)[1])==NULL )
		{
			fprintf(stderr,"-%c requires an argument\n",
					(**argv)[*j]);
			exit (2);
		}
		r=*(++(*argv));
		(*i)++;
		*j=strlen(r)-1;
	}
	else {
		r=&(*(*argv))[(*j)+1];
		*j=strlen(*(*argv))-1;
	}
#ifdef DEBUG
	fprintf(stderr,"Got argument %s\n",r);
#endif
	return r;
}

/*
 * Adds a command to com_file_buf 
 */
void add_command(char *com)
{
	if (COMBUF_SIZE-com_file_buf_used < (int)strlen(com)+2)
	{
		fprintf(stderr,"Expression too long (>%d)\n",COMBUF_SIZE);
		exit(2);
	}
	strcpy(&com_file_buf[com_file_buf_used],com);
	com_file_buf_used+=strlen(com);
}
	
/*
 * Reads command file to command buffer 
 */
void read_com_file(char *fname)
{
	int i;
	int size;
	int r;
	
	if (fname[0]=='-' && fname[1]==0)
	{
		/* Commands are coming from stdin */
		if (stdin_read) {
			fprintf(stderr,
		"Stdin already read, Can't read expressions from stdin\n");
			exit(2);
		}
		stdin_read=TRUE;
		i=0;
	} 
	else {
	 	i=open(fname,O_RDONLY);
		if (i==-1)
		{
			fprintf(stderr,"Command file %s : %s\n",
				fname,strerror(errno));
			exit(2);
		}
	}
	size=0;
	
	/*
	 * When reading from file this loop is done only once
	 * When reading from pipe (file descriptor==i==0 )
	 * this loop is done as long as there is input coming
	 */
	do {
		r=read(i,&com_file_buf[com_file_buf_used+size],
			COMBUF_SIZE-com_file_buf_used-size);
		if ( r==-1 )
		{
			perror("Read command file");
			exit(2);
		}
		if ( r==0 && ( i!=0 ||  (i==0 && size==0) ) )
		{
			fprintf(stderr,"Empty command file %s\n",fname);
			exit(2);
		}
		size+=r;
	} while ( i==0 && r!=0 );
	
	if ( size-2>COMBUF_SIZE-com_file_buf_used)
	{
		fprintf(stderr,"Expression too long (>%d)\n",COMBUF_SIZE);
		exit(2);
	}
	com_file_buf_used+=size;
	command_file_given=TRUE;
	if (i!=0) close(i);
}

/*
 * Reads the expression commands to com_file_buf 
 */
void read_expressions()
{
	int i;
	
	i= (read_sgreprc) ? 0:1;
#ifdef ASSERT
	assert(exprs>0);
#endif
	while (i<exprs)
	{
		switch(expr_table[i].type){
		case E_FILE:
			read_com_file(expr_table[i].expr);
			break;
		case E_TEXT:
			add_command(expr_table[i].expr);
			break;
		default:
			fprintf(stderr,"Strange expression type\n");
			exit(3);
			break;
		}
		/* If there wasn't nl between command expressions we add one */
		if (com_file_buf_used>0)
			if (com_file_buf[com_file_buf_used-1]!='\n')
			 com_file_buf[com_file_buf_used++]='\n';
		i++;
	}
	com_file_buf[com_file_buf_used]=0;
}

/*
 * Reads output style from file
 */
void read_style_file(char *fname)
{
	int fd;
	int l,r;
	
	fd=open(fname,O_RDONLY);
	if (fd==-1)
	{
		fprintf(stderr,"open style file %s : %s\n",fname,strerror(errno));
		exit(2);
	}
	l=lseek(fd,0,SEEK_END);
	if (l==-1)
	{
		fprintf(stderr,"lseek style file %s : %s\n",fname,strerror(errno));
		exit(2);
	}
	lseek(fd,0,SEEK_SET);
	output_style=(char *)e_malloc(l+1);
	r=read(fd,output_style,l);
	if (r==-1)
	{
		fprintf(stderr,"read style file %s : %s\n",fname,strerror(errno));
		exit(2);
	}
	if (r==0)
	{
		fprintf(stderr,"Empty style file %s\n",fname);
		exit(2);
	}
	output_style[r]=0;
	close(fd);
}

/*
 * Checks the command line options 
 */
int get_options(char *argv[])
{
	int o,i,j;
	
	i=1;
	j=1;
	
	while ( *argv!=NULL && *argv[0]=='-' )
	{
		/* option -- means no more options */
		if (strcmp(*argv,"--")==0) return i+1;
		o=0;
		while (options[o].opt!=0)
		{
			if (options[o].opt==(*argv)[j]) break;
			o++;
		}
		switch((*argv)[j])
		{
		case 'h':
			print_help();
			break;
		case 'V':
			printf("sgrep version %s compiled at %s\n",
				VERSION,__DATE__);
			exit(0);
			break;
		case 'T':
			have_stats=TRUE;
			break;
		case 't':
			have_times=TRUE;
			break;
		case 'a':
			print_all=TRUE;
			break;
		case 'i':
			ignore_case=TRUE;
			break;
		case 'l':
			output_style=LONG_OUTPUT;
			do_concat=FALSE;
			break;
		case 's':
			output_style=SHORT_OUTPUT;
			do_concat=TRUE;
			break;
		case 'o':
			output_style=get_arg(&argv,&i,&j);
			do_concat=FALSE;
			break;
		case 'c':
			display_count=TRUE;
			do_concat=FALSE;
			no_output=FALSE;
			break;
		case 'd':
			do_concat=FALSE;
			break;
		case 'N':
			print_newline=FALSE;
			break;
		case 'C':
			copyright_notice();
			exit(0);
			break;
		case 'f':
			if (exprs==MAX_EXPRESSIONS)
			{
				fprintf(stderr,
	"too many expressions. (-e and -f options more than %d)\n",
				MAX_EXPRESSIONS);
				exit(2);
			}
			expr_table[exprs].expr=get_arg(&argv,&i,&j);
			expr_table[exprs++].type=E_FILE;
			command_file_given=TRUE;
			break;
		case 'e':
			if (exprs==MAX_EXPRESSIONS)
			{
				fprintf(stderr,
	"too many expressions. (-e and -f options more than %d)\n",
				MAX_EXPRESSIONS);
				exit(2);
			}
			expr_table[exprs].expr=get_arg(&argv,&i,&j);
			expr_table[exprs++].type=E_TEXT;
			command_file_given=TRUE;
			break;
		case 'p':
			preprocessor=get_arg(&argv,&i,&j);
			break;
		case 'n':
			read_sgreprc=FALSE;
			break;
		case 'O':
			read_style_file(get_arg(&argv,&i,&j));
			break;
		case 'P':
			show_expr=TRUE;
			break;
#ifdef PROGRESS_REPORTS
		case 'D':
			progress_output=TRUE;
			break;
#endif
		case 'S':
			stream_mode=TRUE;
			break;
			
		case 'q':
			no_output=TRUE;
			break;
			
/*		case 'ö':
			fprintf(stderr,"Option not implemented yet.\n");
			exit(2);
			break; */
		default:
			fprintf(stderr,"Illegal option -%c\n",(*argv)[j]);
			return -1;
			break;
		}
		if ((*argv)[++j]==0)
		{
			argv++;
			i++;
			j=1;
		}
	}
	return i;
}

/*
 * Clears the stats struct which we use for gathering statistical information
 */
void clear_stats()
{
	/* Everything is zero. At least so far */
	memset(&stats,0,sizeof(stats));	
}

/*
 * Shows the statistics ( from stats struct ) 
 */
void show_stats()
{
	fprintf(stderr,
	"Scanned %d files, having total of %dK size finding %d phrases.\n",
		last_file,
		(input_files[last_file-1].start+
			input_files[last_file-1].length)/1024,
		stats.phrases);
	fprintf(stderr,"Operator tree size was %d, optimized %d\n",
		stats.tree_size,stats.tree_size-stats.opt_nodes);
	fprintf(stderr,"Output list size was %d regions.\n",stats.output);		 
	fprintf(stderr,
		"Operations:\n%15s:%-4d%6s:%-4d%5s:%-4d%5s:%-4d%11s:%-4d%3s:%-4d\n",
		"containing",stats.containing,
		"in",stats.in,
		"order",stats.order,
		"or",stats.or,
		"extracting",stats.extracting,
		"quote",stats.quote);
	fprintf(stderr,"%15s:%-4d%6s:%-4d%5s:%-4d%5s:%-4d%11s:%-4d%4s:%-4d\n",
		"not containing",stats.not_containing,
		"not in",stats.not_in,
		"inner",stats.inner,
		"outer",stats.outer,
		"concat",stats.concat,
		"join",stats.join);
	fprintf(stderr,"%15s:%-4d%6s:%-4d\n",
		"equal",stats.equal,
		"not equal",stats.not_equal);
	fprintf(stderr,"Memory:\n %dK memory allocated, %d realloc operations\n",
		stats.e_mallocs/1024,stats.reallocs);
	fprintf(stderr," %d gc lists, %d gc lists allocated\n",
		stats.gc_lists,stats.gc_lists_allocated);
  	fprintf(stderr," %d gc blocks used, %d gc blocks allocated.\n",
		stats.gc_nodes,stats.gc_nodes_allocated);
	fprintf(stderr," Longest list size was %d regions.\n",
		stats.longest_list);
	fprintf(stderr,
			" %dK nest stack size, %dK inner tablesize\n",
			stats.nest_stacksize/1024,
			stats.inner_tablesize/1024);
	fprintf(stderr,
#ifdef REMOVE_DUPLICATES
		"Things done:\n %d %s, %d %s, %d %s\n %d %s, %d %s, %d %s\n",
#else
		"Things done:\n %d %s, %d %s, %d %s\n %d %s, %d %s\n",
#endif
		stats.regions,"regions created",
		stats.scans,"gc lists scanned",
		stats.scanned_regions,"regions scanned",
		stats.sorts_by_start,"sorts by start point",
		stats.sorts_by_end,"sorts by end point"
#ifdef REMOVE_DUPLICATES
		,stats.remove_duplicates,"remove duplicates"
#endif
		);
#ifdef OPTIMIZE_SORTS
	fprintf(stderr," %d sorts optimized\n",stats.sorts_optimized);
#endif
	if (stats.skipped_phrases)
	{
		fprintf(stderr," %d same phrases\n",stats.skipped_phrases);
	}
}		

/*
 * Checks that files which are given in the command line really exist.
 * If open_failure==true nonexistent files are considered fatal.
 * Creates input_file list, skipping zero length files 
 */
void check_files(int argc, char *argv[], int optind)
{
	int fd,ls,r=0;
	int pos;
	char buf[1];
	
	input_files=(struct INPUT_FILE *) 
		e_malloc( sizeof(struct INPUT_FILE) * (argc-optind) );
	last_file=0;
	pos=0;
	ls=0;
	while (optind<argc)
	{
		if (strcmp(argv[optind],"-")==0)
		{
			optind++;
			/* We try to read stdin */
			pos+=read_stdin();
			continue;
		}
#ifdef DEBUG
		fprintf(stderr,"checking file %s\n",argv[optind]);
#endif
		/* We do sgrep only on files which we can open,read, lseek
		   and which are not empty */
		fd=open(argv[optind],O_RDONLY);
		if (fd!=-1) r=read(fd,buf,1);
		if (fd!=-1 && r!=-1 ) ls=lseek(fd,0,SEEK_END);
		if (fd==-1 || ls==-1 || r==-1 )
		{
			fprintf(stderr,"sgrep: %s: %s\n",argv[optind],strerror(errno));
			if (open_failure) exit(2);
		} else if (ls>0)
		{
			input_files[last_file].start=pos;
			input_files[last_file].length=ls;
			input_files[last_file].name=argv[optind];
			pos+=ls;
			last_file++;
		}
		close(fd);
		optind++;
	}
	if (last_file==0)
	{
		fprintf(stderr,"No valid files\n");
		exit(2);
	}
}

/*
 * Calculates the difference between two times in seconds
 * and returns it 
 */
float calc_time(clock_t b,clock_t e)
{
	static long clktck=0;
	
	if (clktck==0) clktck=sysconf(_SC_CLK_TCK);
	if (clktck<0) return 0;
	
	return ((float)(e-b)/(float)clktck);
}

/* 
 * Prints a nice looking line of time information with label 
 */
void print_time(char *label,struct tms *b,struct tms *e)
{
	float sys,usr;
	
	usr=calc_time(b->tms_utime,e->tms_utime);
	sys=calc_time(b->tms_stime,e->tms_stime);
	fprintf(stderr,"  %-18s%6.2fs %6.2fs %6.2fs\n",label,usr,sys,usr+sys);;
}
	
/* 
 * Prints information about time used to stderr
 */
void show_times()
{
	fprintf(stderr,"%-18s%8s%8s%8s\n",
		"sgrep time usage","usr","sys","total");
	print_time("parsing",&tps.start,&tps.parsing);
	print_time("acsearch",&tps.parsing,&tps.acsearch);
	print_time("evaluating",&tps.acsearch,&tps.eval);
	print_time("output",&tps.eval,&tps.output);
	fprintf(stderr,"  -----------------------------------------\n");
	print_time("total",&tps.start,&tps.output);
	if (tps.output.tms_cutime>0)
	{
		fprintf(stderr,"\n");
		print_time("preprocessor",
		 (struct tms *)&tps.start.tms_cutime,
		 (struct tms *)&tps.output.tms_cutime);
	}
		
}

/* 
 * Reads stdin to a temp file. Leaves temp file open and stdin_fd pointing
 * to it. File name will be NULL. Unlinks temp file, so that it will be 
 * removed when program exits.
 * returns size of input file read
 */
int read_stdin()
{
	char buf[4096];
	char *temp_file;
	int r,w;
	static int length=-1;
	
	if (length==0) 
		return 0; /* If stdin was already read, and was empty */
	
	if ( input_files==NULL )
	{
		/*
		 * If input_files hasn't been malloced, there is none,
		 * and we read only from stdin 
		 */
		input_files= (struct INPUT_FILE *) 
			e_malloc(sizeof(*input_files));
		last_file=0;
	}
	if (last_file==0)
	{
		input_files[0].start=0;
	} else
	{
		input_files[last_file].start=
			input_files[last_file-1].start+
			input_files[last_file-1].length;
	}
	input_files[last_file].name=NULL;
	if (length>0)
	{
		/* We have already read stdin, so we just return what
		   we already know */
		input_files[last_file++].length=length;
		return length;
	}
	
	if (stdin_read) {
		/*
		 * Somebody had already used stdin for something
		 */
		 fprintf(stderr,
	"Can't read input from stdin, it's already used\n");
		exit(2);
	}
	/* We read stdin to temporary file */
	temp_file=tmpnam(NULL);
	stdin_fd=open(temp_file,O_RDWR | O_CREAT);
	if (unlink(temp_file)==-1)
	{
		perror("sgrep warning: unlinking temp file failed");
	}
	if (stdin_fd==-1)
	{
		 perror("creating tempfile: open");
		 exit(2);
	}
	length=0;
	while ( (r=read(0,buf,4096))!=0 )
	{
		if (r==-1)
		{
			perror("read stdin");
			exit(2);
		}
		length+=r;
		w=write(stdin_fd,buf,r);
		if (w==-1)
		{
			perror("write tempfile");
			exit(2);
		}
		if (w!=r)
		{
			fprintf(stderr,"Short write to tempfile\n");
			exit(2);
		}
	}
	if (length>0) 
	{
		return (input_files[last_file++].length=length);
	}
	return 0;
}

/*
 * Reads the options from environ variable ENV_OPTIONS
 */
int environ_options()
{
	char *av[100];
	int i=0;
	int j=0;
	char *o;
	
	if (getenv(ENV_OPTIONS)==NULL) return 0;

	o=(char *)e_malloc(strlen(getenv(ENV_OPTIONS)+1));
	strcpy(o,getenv(ENV_OPTIONS));
	
	do {
		while( o[i]==' ' ) 
		{
			o[i++]=0;
		}
		if (!o[i]) break;
		av[j++]=&o[i];
		if (j==100)
		{
			fprintf(stderr,"Too complex "ENV_OPTIONS"\n");
			exit(2);
		}
		while( o[i]!=' ' && o[i]!=0 ) i++;
	} while (o[i]);
	av[j]=NULL;

#ifdef DEBUG
	fprintf(stderr,"Environment options: ");
	for (i=0;av[i]!=NULL;i++)
	{
		fprintf(stderr,"'%s' ",av[i]);
	}
	fprintf(stderr,"\n");
#endif
	i=get_options(av);
	if (i==-1)
	{
		fprintf(stderr,"Invalid "ENV_OPTIONS" ("ENV_OPTIONS"=%s)\n",getenv(ENV_OPTIONS));
		return -1;
	}
	if (i<=j)
	{
		fprintf(stderr,"No files or expressions allowed in "ENV_OPTIONS"\n");
		return -1;
	}
	return 0;
}

/*
 * Displays the copyright notice.
 */
void copyright_notice()
{
	int i;
	
	for (i=0;copyright_text[i]!=NULL;i++)
	{
		printf("\t%s\n",copyright_text[i]);
	}
}
