/*
	System: Structured text retrieval tool sgrep.
	Module: parser.c
	Author: Pekka Kilpeläinen & Jani Jaakkola
	Description: Parses expression string ( which has been gathered in
	             main.c and preprocessed in preprocess.c )
	             Returns operator tree.
	             Used through fuction parse_tree()
	Version history: Original version February 1995 by JJ & PK
	Copyright: University of Helsinki, Dept. of Computer Science
		   Distributed under GNU General Public Lisence
		   See file COPYING for details
*/
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "defines.h"

/* Maximum length of one phrase. */
#define MAX_PHRASE_LENGTH 512
/* Maximum length of a reserved word (operation). Could also be smaller */
#define MAX_R_WORD 20

/* 
 * This points to first phrase in the phrase list 
 */
struct PHRASE_NODE *first_phrase=NULL;

void parse_error(char *);
char *give_str();
int get_next_char();
int get_next(string **);
struct TREE_NODE *reg_expr(int *, string **);
struct TREE_NODE *basic_expr(int *, string **);
struct TREE_NODE *oper_expr(int *, string **,struct TREE_NODE *);

#ifdef DEBUG
void print_tree(struct TREE_NODE *);
#endif

/*
 * When looking for reserved words they are matched to these strings 
 */
char *r_word_str[]= 
	{ "in","not","containing","or","..","_.","._","__","extracting", 
	"quote","_quote","quote_","_quote_",
	"equal",		/* PK Febr 12 '96 */
	"outer","inner","concat","start","end","chars","join" };

/*
 * The command string and a index to it 
 */
char *expr_str=NULL;
int expr_ind=0;

/* 
 * for telling where errors occurred 
 */
#define ELLENGTH 79
#define SHOWR 5
int line,column;
int errcol,errind;

/*
 * Gets next character from input expression. Counts also lines and columns
 * return character must be unsigned to keep sgrep is 8-bit clean
 */
int get_next_char()
{
	switch (expr_str[expr_ind]) {
		case '\n':
			column=-1;
			line++;
			break;
		case '\t':
			expr_str[expr_ind]=' ';
			break;
	}
	column++;
	return ( (unsigned char *)expr_str)[expr_ind++];	
}
	
/*
 * Scans next 'word' from input string. If word was a phrase sets **phrase 
 */
int get_next(string **phrase)
{
	static int ch=-1;
	char str[MAX_PHRASE_LENGTH];
	int i=0,j=0;
	int start;
	static int state=0;
	int onum=0;

/* if we already have next character in ch then ch!=-1. Otherwise we have to
   get one */
	if (ch==-1) ch=get_next_char();
 
/* These show the place where possible error started */
	errcol=column;
	errind=expr_ind;

/* Finite automaton for recognizing next word */	
	do {
	switch (state) {
	
	/* This is the start state */
	case 0: 
		/* Lets skip the spaces */
		while (isspace(ch)) ch=get_next_char();

		/* These show the place where possible error started */
		errcol=column;
		errind=expr_ind;
		
		switch(ch) {
		case 0:
			return W_END;
		case '(':
			ch=-1;
			return W_LPAREN;
		case ')': 
			ch=-1;
			return W_RPAREN;
		case '[':
			ch=-1;
			return W_LBRACK;
		case ']':
			ch=-1;
			return W_RBRACK;
		case ',':
			ch=-1;
			return W_COMMA;
		case '\"': 
			i=0;
			state=1;
			break;
		case '#':
			state=4;
			break;
		}
		if ( isalpha(ch) || ch=='.' || ch=='_' )
		{
			i=1;
			str[0]=tolower(ch);
			state=2;
			start=column;
		}
		if (ch>='0' && ch<='9')
		{
			i=1;
			str[0]=ch;
			start=column;
			state=5;
		}
		if ( state==0 )
		{
#ifdef DEBUG
			fprintf(stderr,"Character %c ascii(%d)\n",ch,ch);
#endif
			parse_error("Invalid character");
		}
		break;
		
	/* This state reads a phrase string */
	case 1:
		if (ch==0 || ch=='\n') 
			parse_error("Unterminated phrase string");
		if ( ch<' ' ) 
			parse_error("Unprintable character in phrase");
		if (ch=='\"') 
		{
			if ( i==0 )
				parse_error("Empty phrase");
			*phrase=init_string(i,str);
			state=0;
			ch=-1;
			return W_PHRASE;
		} 	
		if ( i==MAX_PHRASE_LENGTH )
		{
			fprintf(stderr,"%s ( > %d ) %s %d\n%s\n%s\n",
	"Phrase length exceeds",MAX_PHRASE_LENGTH,"on line",line,
	"Either you have forgotten the quote  at the end of phrase or",
	"you have to recompile with greater MAX_PHRASE_LENGTH.");
			exit(2);
		}
		if (ch=='\\' ) state=3;
		if ( state==1 ) str[i++]=ch;
		break;
		
	/* This state reads a reserved word (operator) */
	case 2:
		if ( ch==0 || ( !isalpha(ch) && ch!='.' && ch!='_' ) )
		{
			state=0;
			str[i]=0;
			for(j=0;j<R_WORDS;j++)
				if (strcmp(r_word_str[j],str)==0) return j;
			parse_error("Unknown word");
		}
		if (i==MAX_R_WORD)
			parse_error("Unknown word");
		str[i++]=tolower(ch);
		break;
	
	/* This state handles escape sequences */
	case 3:
		if ( ch==0 ) parse_error("Unterminated phrase");
		switch (ch) {
		case 't': ch='\t';break;
		case 'n': ch='\n';break;
		case 'r': ch='\r';break;
		case 'f': ch='\f';break;
		case 'b': ch='\b';break;
		case '\\': ch='\\';break;
		case '\"': ch='\"';break;
		case '0':
		case '1':
		case '2':
		case '3':
			/* Octal number requires new state */
			state=6;
			onum=64*(ch-'0');
			break;
		default:
			errcol=column;
			errind=expr_ind;
			parse_error("Unknown escape sequence");
			break;
		}
		if (state==3) /* always true except for octal number */
		{
			str[i++]=ch;
			state=1;
		}
		break;
	/* This state skips comments */
	case 4:
		if (ch==0)
		{
			state=0;
			return W_END;
		}
		if (ch=='\n') state=0;
		break;
	/* We read a number */
	case 5:
		if ( ch<'0'|| ch>'9' )
		{
			state=0;
			str[i]=0;
			*phrase=init_string(i,str);
			return W_NUMBER;
		}
		if (i==9)
			parse_error("Too big number");
		str[i++]=ch;
		break;
	/* Read octal number #2 */
	case 6:
		if ( ch<'0' || ch>'7')
		{
			errcol=column;
			errind=expr_ind;
			parse_error("Octal number expected");
		}
		onum+=8*(ch-'0');
		state=7;
		break;
	/* Read octal number #3 */
	case 7:
		if ( ch<'0' || ch>'7')
		{
			errcol=column;
			errind=expr_ind;
			parse_error("Octal number expected");
		}
		onum+=ch-'0';
		if (onum==0)
		{
                        errcol=column-3;
                        errind=expr_ind-3;                        
			parse_error("\\000 in phrase does'nt work (yet)\n");
		}
		str[i++]=onum;
		state=1;
		break;
	}
	
	ch=get_next_char();	
	} while (TRUE);

}

/* 
 * Shows a given error message & where it occurred 
 */
void parse_error(char *error)
{
	char erlin[ELLENGTH+1];
	int i;
	
	if (errcol-ELLENGTH+SHOWR>0)
		errind-=ELLENGTH-SHOWR;
	else errind-=errcol;
	for(i=0;i<ELLENGTH && expr_str[i+errind] && expr_str[i+errind]!='\n';i++) 
		erlin[i]=expr_str[i+errind];
	erlin[i]=0;
	fprintf(stderr,"Parse error on line %d column %d :\n\t%s\n%s\n",
		line,errcol,error,erlin); 
	if ( errcol>ELLENGTH-SHOWR ) errcol=ELLENGTH-SHOWR; 
        for (i=0;i<errcol-1;i++) fprintf(stderr," "); 
        fprintf(stderr,"^\n");
        exit(2);
}

/*
 * Creates and initializez an operator node to parse tree
 */
struct TREE_NODE *create_tree_node(int oper)
{
	struct TREE_NODE *n;
	
	n=(struct TREE_NODE *)e_malloc(sizeof(struct TREE_NODE));
	n->left=NULL;
	n->right=NULL;
	n->parent=NULL;
	n->oper=oper;
	n->number=-1;
	n->leaf=NULL;
	n->label_left=LABEL_NOTKNOWN;
	n->label_right=LABEL_NOTKNOWN;
	n->refcount=0;
	n->GC_list=NULL;
	return n;
}
	
/* 
 * Creates and initializes a leaf node to parse tree 
 */
struct TREE_NODE * create_leaf_node(int oper, string *phrase, int phrase_label)
{
	struct TREE_NODE *n;
	
	/* Let's create a leaf node */
	n=create_tree_node(oper);
	n->label_left=phrase_label;
	/* Let's create a phrase node for the phrase */
	n->leaf=(struct PHRASE_NODE *) 
		e_malloc(sizeof (struct PHRASE_NODE));
	n->leaf->phrase=phrase;
	return n;
}

/* 
 * Here starts recursive parser 
 */

/* production basic_expr->constant_list */
void cons_list(int *next, string **phrase, struct GC_LIST *c_list)
{
	int s,e,ps,pe;
	char *cons_err="invalid constant region list";

	stats.constant_lists++;
		
	ps=-1;pe=-1;
	while (*next!=W_RBRACK) /* We can fall out here immediately,
				   which means we have empty list */
	{
		if (*next!=W_LPAREN)
			parse_error(cons_err);
		*next=get_next(phrase);
		if (*next!=W_NUMBER)
			parse_error(cons_err);
		s=atoi((char *)(**phrase).s);
		*next=get_next(phrase);
		if (*next!=W_COMMA)
			parse_error(cons_err);
		*next=get_next(phrase);
		if (*next!=W_NUMBER)
			parse_error(cons_err);
		e=atoi((char *)(**phrase).s);
		*next=get_next(phrase);
		if (*next!=W_RPAREN)
			parse_error(cons_err);
		if (e<s)
			parse_error("region end point must be greater than start point");
		*next=get_next(phrase);
		if (s<ps || (s==ps && e<=pe) )
			parse_error("constant gc list must be sorted");
		if (e<=pe || s==ps)
		{
			/* nesting detected */
			c_list->nested=TRUE;
		}
		add_region(c_list,s,e);
		ps=s;pe=e;
	}
}

/* Which kind of basic expressions */			
struct TREE_NODE *basic_expr(int *next, string **phrase)
{
	struct TREE_NODE *n;
	
	switch (*next) {
	case W_PHRASE:
		/* A phrase was found */
		n=create_leaf_node(PHRASE,*phrase,LABEL_PHRASE);
		/* Let's put the new phrase node to phrase list */
		n->leaf->next=first_phrase;
		first_phrase=n->leaf;
		*next=get_next(phrase);
		return n;
	case W_START:
		/* reserved word 'start' was found */
		n=create_leaf_node(PHRASE,NULL,LABEL_START);
		/* we use already created list (see create_constant_lists() )*/
		n->leaf->GC_list=start_list;
		*next=get_next(phrase);
		return n;
	case W_LAST:
		/* reserved word 'end' was found */
		n=create_leaf_node(PHRASE,NULL,LABEL_END);
		/* we use already created list (see create_constant_lists() )*/
		n->leaf->GC_list=end_list;
		*next=get_next(phrase);
		return n;
	case W_LBRACK:
		/* We got a constant region list */
		n=create_leaf_node(PHRASE,NULL,LABEL_CONS);
		n->leaf->GC_list=new_gclist();
		*next=get_next(phrase);
		cons_list(next,phrase,n->leaf->GC_list);
		*next=get_next(phrase);
		return n;
	case W_CHARS:
		/* reserved wors 'chars' was found */
		n=create_leaf_node(PHRASE,NULL,LABEL_CHARS);
		/* we use already created list (see create_constant_lists() )*/
		n->leaf->GC_list=chars_list;
		*next=get_next(phrase);
		return n;
	case W_LPAREN:
		*next=get_next(phrase);
		n=reg_expr(next,phrase);
		if (*next!=W_RPAREN)
			parse_error(") expected");
		*next=get_next(phrase);
		return n;
	case W_OUTER:
	case W_INNER:
	case W_CONCAT:
		/* So we have function. Let's create a tree node for it */
		n=create_tree_node( (*next==W_OUTER) ? OUTER:
			((*next==W_INNER) ? INNER:CONCAT) ); 
		/* Let's parse the parameter */
		*next=get_next(phrase);
		if (*next!=W_LPAREN)
			parse_error("( expected");
		*next=get_next(phrase);
		n->left=reg_expr(next,phrase);
		n->right=NULL; /* Function has only one parameter */
		if (*next!=W_RPAREN)
			parse_error(") expected");
		*next=get_next(phrase);
		return n;
	case W_JOIN:
		/* So we have join function. Let's create a tree node for it */
		n=create_tree_node(JOIN);
		/* Let's parse the parameters */
		*next=get_next(phrase);
		if (*next!=W_LPAREN)
			parse_error("( expected");
		*next=get_next(phrase);
		if (*next!=W_NUMBER)
			parse_error("join needs a number: join(NUMBER,expression)");
		n->number=atoi((char *)(**phrase).s);
		if (n->number<1)
			parse_error("join number must be >= 1");
		*next=get_next(phrase);
		if (*next!=W_COMMA)
			parse_error("join expected comma: join(NUMBER,expression)");
		*next=get_next(phrase);
		n->left=reg_expr(next,phrase);
		n->right=NULL;
		if (*next!=W_RPAREN)
			parse_error(") expected");
		*next=get_next(phrase);
		return n;
	default:
		parse_error("Basic expression expected\n");
	}
	/* Dummy return for keeping c++ compilers quiet */
	return NULL;
}		

/* Which kind region_expression */
struct TREE_NODE *reg_expr(int *next, string **phrase)
{
	struct TREE_NODE *left;
	if (*next==W_END)
		parse_error("Unexpected end of expression");

	left=basic_expr(next,phrase);
	if ( *next==W_END || *next==W_RPAREN ) return left;
	return oper_expr(next,phrase,left);
}	

/* Which kind of operator expression */
struct TREE_NODE *oper_expr(int *next, string **phrase,struct TREE_NODE *left)
{
	struct TREE_NODE *o=NULL;

	switch (*next)
	{
	case W_IN:		
		o=create_tree_node(IN);
		break;
	case W_CONTAINING:	
		o=create_tree_node(CONTAINING);
		break;
/* PK Febr 95 */
	case W_EQUAL:	
		o=create_tree_node(EQUAL);
		break;
/* PK Febr 95 */
	case W_OR: 		
		o=create_tree_node(OR);
		break;
	case W_ORDERED: 
		o=create_tree_node(ORDERED);
		break;
	case W_L_ORDERED:
		o=create_tree_node(L_ORDERED);
		break;
	case W_R_ORDERED:
		o=create_tree_node(R_ORDERED);
		break;
	case W_LR_ORDERED:
		o=create_tree_node(LR_ORDERED);
		break;
	case W_EXTRACTING:
		o=create_tree_node(EXTRACTING);
		break;
	case W_QUOTE:
		o=create_tree_node(QUOTE);
		break;
	case W_R_QUOTE:
		o=create_tree_node(R_QUOTE);
		break;
	case W_L_QUOTE:
		o=create_tree_node(L_QUOTE);
		break;
	case W_LR_QUOTE:
		o=create_tree_node(LR_QUOTE);
		break;
	case W_NOT:
		*next=get_next(phrase);
		if (*next==W_CONTAINING) o=create_tree_node(NOT_CONTAINING);
		else if (*next==W_IN) o=create_tree_node(NOT_IN);
		else if (*next==W_EQUAL) o=create_tree_node(NOT_EQUAL);
		else parse_error("'not' must be followed by 'in', 'containing' or 'equal'");
		break;
	default:
		parse_error("Operator expected");
	}
	*next=get_next(phrase);
	o->right=basic_expr(next,phrase);
	o->left=left;
	if (*next==W_END || *next==W_RPAREN) return o;
	return oper_expr(next,phrase,o);
}		

/* End of recursive parser */

/*
 * Parses the given command string. Returns root node of the parse tree.
 * phrase_list returns pointer to phrase list 
 */
struct TREE_NODE *parse_string(const char *str, 
	struct PHRASE_NODE **phrase_list)
{
	string *phrase;
	int next;
	struct TREE_NODE *root;
	
	line=1;
	column=0;
	
	expr_str=(char *) str;
	
	next=get_next(&phrase);
	root=reg_expr(&next,&phrase);

	if (next==W_RPAREN)
	{
		parse_error("Too many )'s");
	}
		
	if (next!=W_END)
	{
		fprintf(stderr,"Premature end of parsing. ( This shouldn't happen!! )\n");
		exit(3);
	}

#ifdef DEBUG
	print_tree(root);
	fprintf(stderr,"\n");
#endif	
	*phrase_list=first_phrase;
	return root;	
}

#ifdef DEBUG
void print_tree(struct TREE_NODE *root)
{
	if ( root==NULL )
	{
		fprintf(stderr,"\nprint_tree: got NULL node\n");
		exit(3);
	}
	if ( root->oper==PHRASE )
	{
		fprintf(stderr," \"%s\"",root->leaf->phrase->s);
		return;
	}
	if (root->oper<0 || root->oper>R_WORDS)
	{
		printf("\nprint tree: got invalid oper (%d)\n",root->oper);
		exit(3);
	}
	
	if (root->right!=NULL)
	{
		fprintf(stderr," (");
		print_tree(root->left);
	
		fprintf(stderr," %s",r_word_str[root->oper]);
	
		print_tree(root->right);
		fprintf(stderr," )");
	} else
	{
		fprintf(stderr," %s(",r_word_str[root->oper]);
		print_tree(root->left);
		fprintf(stderr," )");
	}
}
#endif
