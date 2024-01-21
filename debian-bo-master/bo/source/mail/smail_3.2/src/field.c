/*
#ident	"@(#)smail/src:RELEASE-3_2:field.c,v 1.10 1996/02/28 06:47:00 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * field.c:
 *	routines to process header fields and alias/.forward files
 *
 *	The routines defined in this file are not complicated
 *	conceptually.  The basic algorithm is tokenize a string,
 *	match patterns of tokens to specific addressing forms,
 *	and insert a comma separator between addresses, if
 *	necessary.
 *
 *	The pattern matching is made somewhat more complicated
 *	in that when an address is found, it may be modified
 *	to make it somewhat more conforming to standards than
 *	it may have been to begin with.  Also, an address may
 *	be extracted and added to a list.
 *
 *	external functions:  process_field, tokenize, detokenize, dump_tokens
 */
#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include "defs.h"
#include "smail.h"
#include "field.h"
#include "addr.h"
#include "log.h"
#include "dys.h"
#include "exitcodes.h"
#ifndef DEPEND
# include "debug.h"
# include "extern.h"
#endif

/* functions local to this file */
static char *finish_mod_clean();
static void insert_comma();
static int match_route_or_group();
static int match_group_term();
static int match_general();
static char *queue_qualify_domain();
static int enqueue_address();

/* macros local to this file */
#define DUMP_TOKENS(d,t)  {if (d <= debug) dump_tokens(t);}


/*
 * tokenize - turn a string into a queue of tokens.
 *
 * Given a string, parse the string into a list of tokens.  The list
 * is to be terminated by either an ERRORTOK or an ENDTOK token which
 * do not have a successor.
 *
 * This routine is somewhat long, however, it is basically a state
 * machine with some initialization at the beginning and cleanup at
 * the end.
 *
 * inputs:
 *	field	- string to tokenize.
 *	ret_q	- address of a struct token variable in which to
 *		  return the head of the queue of tokens.
 *	alias	- TRUE if # is a comment character and ':include:'
 *		  is allowed at the beginning of text tokens.  For
 *		  use in parsing alias, forward and mailing list
 *		  files.
 *	space	- TRUE if white space is to be put in space.
 * output:
 *	an error message is returned on error, or NULL if no error.
 *	Also, the value pointed to by ret_q is filled with the header
 *	of the queue of tokens.
 *
 * called by: process_field, external functions
 */
char *
tokenize(field, ret_q, alias, space)
    char *field;			/* string to be tokenized */
    struct token **ret_q;		/* return start of token queue here */
    int alias;				/* TRUE if scanning alias file */
    int space;				/* if TRUE put space in space */
{
    struct token *tq;			/* member pointer for building queue */
    register char *fp;			/* pointer to chars in field */
    char *p;
    struct str str;
    register struct str *sp = &str;	/* pointer to string building region */
    enum e_state {			/* state machine definitions */
	s_domlit,			/* inside a domain literal */
	s_text,				/* inside a text literal token */
	s_quote,			/* inside a quoted literal */
	s_comment,			/* inside a comment */
	s_cquote,			/* previous character was a \ */
	s_space,			/* skipping through white space */
	s_newtok,			/* finished a token, start a new one */
	s_hash_comment			/* comment from '#' to a newline */
    } state;
    enum e_state save_state;		/* save state from before a \ */
    int comment_level;			/* embeddedness level in comments */
    char *non_text_tokens;		/* chars not in text literal tokens */
    int text_offs;			/* offset to text area in p */

    /*
     * initialize state
     */
    if (alias) {
	/* if parsing alias file, # is white space */
	non_text_tokens = ":;<>][\",.!%@ \t\n#";
    } else {
	/* otherwise, # is a token char */
	non_text_tokens = ":;<>][\",.!%@ \t\n";
    }

    /* initialize the dynamic string variables */
    STR_INIT(sp);

    /* allocate space for initial token */
    *ret_q = tq = (struct token *)xmalloc(sizeof(*tq));

    /* begin by reading through white space */
    state = s_space;

    /*
     * loop until we have reached the end of the string,
     * going through the state machine to build up tokens.
     */
    for (fp = field; *fp != '\0'; fp++) {
	switch(state) {

	/*
	 * initial state
	 *
	 * scan for the end of white space and when found set
	 * state as appropriate to the next character.  If the
	 * next state is to be anything other than s_comment
	 * or s_hash_comment, finish off the white-space associated
	 * with the current token and begin the text associated
	 * with the current token.
	 *
	 * entry state:  s_newtok, s_comment
	 * exit state:  s_comment, s_quote, s_text, s_comment, s_domain,
	 *		s_hash_comment, s_newtok
	 */
	case s_space:
	    if (alias && *fp == '#') {
		/* found a '#' comment, skip through to the end of the line */
		state = s_hash_comment;
	    } else {
		if (*fp == '(') {
		    /* found a comment, scan through it next */
		    state = s_comment;
		    comment_level = 1;	/* comment finished when this is 0 */
		    if (space) {
			STR_NEXT(sp, *fp);
		    }
		    break;
		} else if (isspace(*fp)) {
		    if (space) {
			STR_NEXT(sp, *fp);
		    }
		    break;		/* continue with space token */
		} else {
		    if (space) {
			/* end of white space and comments preceding token */
			STR_NEXT(sp, '\0'); /* end white space */
		    }

		    /*
		     * leave room for possible comma in the white space
		     * also, if we are not putting white space in space,
		     * this will at least make space valid
		     */
		    STR_NEXT(sp, '\0');
	        }
		text_offs = sp->i;	/* mark offset for token text in p */

		/* determine what form this token will be */
		switch (*fp) {
		case '[':		/* a domain literal comes next */
		    state = s_domlit;
		    tq->form = T_DOMLIT;
		    break;
		case '"':		/* a quoted literal comes next */
		    state = s_quote;
		    tq->form = T_QUOTE;
		    break;
		case '\\':		/* text token with first char quoted */
		    state = s_cquote;
		    save_state = s_text; /* state after \ */
		    tq->form = T_TEXT;
		    break;
		default:
		    if (alias && *fp == ':' &&
			strncmpic(fp, ":include:", sizeof(":include:")-1) == 0)
		    {
			p = fp + sizeof(":include:") - 1;
			while (isspace(*p) && *p != '\n')
			    p++;
			if (*p && index(non_text_tokens, *p) == NULL) {
			    str_ncat(sp, fp, (unsigned) (p - fp));
			    fp = p;
			    state = s_text;
			    tq->form = T_TEXT;
			    break;
			}
		    }
		    if (index(non_text_tokens, *fp)) {
			state = s_newtok;
			tq->form = T_OPER;
		    } else {
			state = s_text;
			tq->form = T_TEXT;
		    }
		    break;
		}
		STR_NEXT(sp, *fp);	/* copy character into token */
	    }
	    break;

	/*
	 * a comment was begun with a '#' character and a newline
	 * terminates it.  This state is entered only when parsing
	 * an alias file.
	 *
	 * entry state:  s_space
	 * exit state:  s_space
	 */
	case s_hash_comment:
	    if (*fp == '\n') {
		state = s_space;
	    }
	    break;

	/*
	 * a domain literal was begun with a '[' character
	 * and a ']' terminates it, however, a "\]" sequence
	 * does not terminate a domain literal.
	 *
	 * entry state:  s_space
	 * exit state:  s_newtok
	 */
	case s_domlit:
	    STR_NEXT(sp, *fp);
	    if (*fp == '\\') {
		/* \ quotes next character, save s_domlit state */
		save_state = s_domlit;
		state = s_cquote;
	    } else if (*fp == ']') {
		/* ] terminates a domain literal */
		state = s_newtok;
	    }
	    break;

	/*
	 * a text token was begun by a non white space character
	 * which is not in the set "[!@%.\"" and ends with a white
	 * space character or a character that is in that set.
	 * a special character can be prefixed with \ to be included
	 * in the text literal.
	 *
	 * entry state:  s_space
	 * exit state:  s_newtok
	 */
	case s_text:
	    if (*fp == '\\') {
	    	/* \ quotes next character, save s_text state */
	    	STR_NEXT(sp, *fp);	/* copy char into token */
		save_state = s_text;
		state = s_cquote;
	    } else if (index(non_text_tokens, *fp)) {
		/* space or an operator follows a text literal */
		fp--;		/* re-scan character */
		state = s_newtok;
	    } else {
	    	STR_NEXT(sp, *fp);	/* copy char into token */
	    }
	    break;

	/*
	 * a quoted literal was begun by a " character and ends with
	 * a " character.  A \" sequence does not end a quoted literal.
	 *
	 * entry state:  s_space
	 * exit state:  s_newtok
	 */
	case s_quote:
	    STR_NEXT(sp, *fp);		/* copy char into token */
	    if (*fp == '\\') {
		/* \ quotes next character, save s_quote state */
		save_state = s_quote;
		state = s_cquote;
	    } else if (*fp == '"') {
		/* " terminates a quoted literal */
		state = s_newtok;
	    }
	    break;

	/*
	 * a comment begins with a ( and ends when a balancing ) is
	 * found.  A \( or \) sequence does not count in determining
	 * balancing of parentheses.
	 *
	 * entry state:  s_space
	 * exit state:  s_space
	 */
	case s_comment:
	    if (space) {
		STR_NEXT(sp, *fp);	/* copy char into token */
	    }
	    if (*fp == '\\') {
		/* \ quotes next character, save s_comment state */
		save_state = s_comment;
		state = s_cquote;
	    } else if (*fp == ')') {
		comment_level--;
		if (comment_level == 0) {
		    /* balanced parentheses--done with comment */
		    state = s_space;
		}
	    } else if (*fp == '(') {
		comment_level++;
	    }
	    break;

	/*
	 * \ escape in quote, text literal, comment or domain
	 * include the character following a \ in the token and
	 * retain the previous state.
	 *
	 * entry state:  s_quote, s_text, s_comment or s_domain
	 * exit state:  the entry state
	 */
	case s_cquote:
	    STR_NEXT(sp, *fp);		/* copy character into token */
	    /* restore previous state */
	    state = save_state;
	    break;

	/*
	 * finished up a complete token--set up for the next one.
	 * this involes ending the dynamic string region
	 * creating a new token and linking the previous token
	 * before the new one.
	 *
	 * entry state:  s_quote, s_text, s_comment, s_domain
	 * exit state:  s_space
	 */
	case s_newtok:			/* finished a token, setup for next */
	    /* finish up dynamic string region */
	    STR_NEXT(sp, '\0');
	    STR_DONE(sp);
	    /* create new token which is the current token's successor */
	    tq->succ = (struct token *)xmalloc(sizeof(*tq));
	    tq->space = sp->p;		/* mark pointer to white space */
	    tq->text = sp->p + text_offs; /* mark pointer to token text */
	    tq = tq->succ;
	    /* scan through white space next */
	    state = s_space;
	    /* create a new dynamic string region */
	    STR_INIT(sp);
	    fp--;			/* re-read current character */
	    break;
	}
    }

    /*
     * we reached the end of the string.  This is either okay, if we
     * are scanning white space or a text literal, or it is not okay.
     * if we are scanning white space or a comment, we need to close
     * off the white-space for the token, properly, otherwise we need
     * to close off the text associated with the token.
     */
    if (state == s_hash_comment) {
	state = s_space;
    }
    if (state == s_space || state == s_comment) {
	/* no token text exists for last token, fill in with empty text */
	if (space) {
	    STR_NEXT(sp, '\0');		/* terminate space */
	}
	STR_NEXT(sp, '\0');		/* leave room for a possible comma */
	tq->text = "";			/* empty text */
	STR_DONE(sp);
    } else {
	/* last token does contain some text */
	STR_NEXT(sp, '\0');
	STR_DONE(sp);
	tq->text = sp->p + text_offs;
    }

    tq->space = sp->p;	/* first part of p is the white space and comments */

    /*
     * if the current token is white space, then make it the ending
     * token in the generated list.  Otherwise, allocate a new token
     * with no text or white-space and make that the ending token.
     * In the second case, check for errors as well.
     */
    if (state == s_space) {
	tq->form = T_END;
	tq->succ = NULL;
    } else {
	struct token *end_q;
	end_q = tq->succ = (struct token *)xmalloc(sizeof(*end_q));
	end_q->text = end_q->space = "";
	end_q->form = T_END;
	end_q->succ = NULL;

	/* is it an error? */
	if (state == s_cquote) {
	    return end_q->text = "no character after \\";
	}

	/* what was the specific state for the error */
	switch (state) {

	case s_domlit:		/* unterminated domain literal */
	    tq->form = T_ERROR;
	    return end_q->text = "unterminated domain literal";

	case s_comment:
	    tq->form = T_ERROR;
	    return end_q->text = "unterminated comment";

	case s_quote:
	    tq->form = T_ERROR;
	    return end_q->text = "unterminated quoted literal";
	}
    }

    /*
     * everything went fine.  ret_q is computed queue of tokens.
     * don't return an error message.
     */
    return NULL;
}


/*
 * detokenize - convert a list of tokens into its string representation
 *
 * given a queue of tokens, such as produced by tokenize, return
 * a string corresponding to the space and text of the tokens.
 *
 * inputs:
 *	buf	- buffer in which to store result.  NULL if we should
 *		  use the dynamic string region facility.  If buf is
 *		  non-NULL it is assumed to be large enough to store
 *		  the result
 *	tq_head	- head of queue of tokens
 *	tq_end	- end of tokens to tokenize, or NULL to tokenize up
 *		  to an ENDTOK token
 *
 * output:
 *	string representing list of tokens
 *
 * called by:  finish_modified_clean, enqueue_address, external functions
 */
char *
detokenize(space, buf, tq_head, tq_end)
    int space;				/* TRUE if space should be copied */
    char *buf;				/* store result here, if non-NULL */
    struct token *tq_head;		/* list of tokens to detokenize */
    struct token *tq_end;		/* end of tokens to, or NULL */
{
    register struct token *tq;		/* temp for scanning through tokens */

    if (buf) {
	register char *bp= buf;		/* point to buf */

	bp[0] = '\0';
	/* loop through contatenating space and text from tokens */
	for (tq = tq_head; !ENDTOK(tq->form); tq = tq->succ) {
	    if (space) {
		(void)strcat(bp, tq->space);
	    }
	    (void)strcat(bp, tq->text);
	    if (tq == tq_end) {
		return bp;
	    }
	}
	/* get the white space from the ending token */
	(void)strcat(bp, tq->space);
	return bp;			/* return the buffer */
    } else {
	struct str str;
	register struct str *sp = &str;	/* dynamic string region */

	STR_INIT(sp);			/* initialize dynamic string region */

	for (tq = tq_head; !ENDTOK(tq->form); tq = tq->succ) {
	    if (space) {
		STR_CAT(sp, tq->space);
	    }
	    STR_CAT(sp, tq->text);
	    if (tq == tq_end) {
		STR_NEXT(sp, '\0');	/* null terminate */
		STR_DONE(sp);		/* finish dynamic string */

		return sp->p;		/* return string */
	    }
	}

	STR_CAT(sp, tq->space);		/* add space from last token */
	STR_NEXT(sp, '\0');		/* null terminate */
	STR_DONE(sp);			/* finish dynamic string */

	return sp->p;			/* return it */
    }
}


/*
 * process_field - cleanly separate addresses in a header field, and extract
 *		   and cleanup those addresses
 *
 * given a header field which contains addresses, cleanly separate
 * each address with a comma, if it is not separated already.
 * Optionally clean local addresses by appending an RFC822 '@domain' form.
 *
 * Recognized addressing forms are:
 *
 *	ANY*<ANY*>		- route, can be recursive.
 *	ANY*:			- beginning of a group.
 *	;[@WORD]		- end of a group.
 *	WORD [op WORD [op ... WORD]]
 *				- op is from the list ".!%@"
 *				  the simple form "WORD" is a local address.
 *
 * inputs:
 *	field	- a header field which contains addresses.  If NULL
 *		  no header is returned.
 *	fp	- start of region to tokenize and clean.
 *	domain	- if non-NULL, a domain which is to be appended in
 *		  RFC822 '@domain' form to local addresses.
 *	uucp_host - if non-NULL, a string to prepend to ! routes.
 *		  The purpose of this field is to keep ! routes in
 *		  From: or Sender: fields in ! route notation and to
 *		  ensure that the ! route will correctly return to
 *		  the sender, assuming software on other machines
 *		  doing something else.
 *	extract_q - Address queue in which to insert extracted addresses.
 *		  NULL if we are not extracting addresses.
 *	flags	- A bitwise or of the following flags from field.h:
 *		  F_LOCAL  - set if message originated on the local host.
 *			     This causes domains to be fully qualified.
 *		  F_STRICT - set to adhere more closely to RFC822.  When
 *			     this is set, then all local addressing forms,
 *			     bang routes and tokens%domain forms are appended
 *			     with @domain, if domain is given, and prepended
 *			     with uucp_host, if uucp_host is given.  This is
 *			     for use in gatewaying to stricter networks.
 *		  F_ALIAS  - set to parse an aliases-style file.  In these
 *			     cases, '#' introduces a comment and a the
 *			     string ":include:" is allowed at the start of
 *			     a text token, and does not introduce a group.
 *	error	- if an error occurs, an error message is stored here,
 *		  otherwise error is left alone.
 *
 * output:
 *	a header cleaned according to the rules stated above, or NULL
 *	if `field' was NULL.
 *
 * called by: external functions
 * calls: tokenize, match_route_or_group, match_group_term, match_general
 */
char *
process_field(field, fp, domain, uucp_host, extract_q, flags, error)
    char *field;			/* header field to be cleaned */
    char *fp;				/* pointer to field contents */
    char *domain;			/* domain to add to local addresses */
    char *uucp_host;			/* uucp host to prepend to ! routes */
    struct addr **extract_q;		/* queue in which to put addresses */
    int flags;				/* miscellaneous flags */
    char **error;			/* store error message here */
{
    int modified = FALSE;		/* set to TRUE if field is modified */
    char *error_message;		/* error returned by tokenize */
    struct token *tq_head;		/* list of tokens to return */
    struct token *tq_anchor;		/* anchor point for pattern scan */
    struct token *tq_new;		/* new anchor found by pattern scan */
    int new_group = FALSE;		/* set if group: pattern newly found */
    int group = FALSE;			/* set when inside of a group */
    unsigned len = 0;			/* length of cleaned header */
    int need_comma = FALSE;		/* TRUE if we may need a , at anchor */
    int check_route = TRUE;		/* TRUE if we must scan for routes */
    int i;				/* temp */

    if (field) {
	len = strlen(field) + 1;
    }
    DEBUG(DBG_FIELD_LO, "process_field: entry\n");
    /* tokenize the contents to make parsing easy */
    error_message = tokenize(fp, &tq_head, flags&F_ALIAS, field != NULL);

    DUMP_TOKENS(DBG_FIELD_HI, tq_head);

    /*
     * If tokenize found an error, then there is a syntax error
     * which would make processing this header of dubious value.
     * If we are not depending on the correctness of the header
     * for extracting addresses, this is not enough to warrant return
     * of mail.
     */
    if (error_message) {
	*error = error_message;
	DEBUG1(DBG_FIELD_LO, "process_field: error: %s\n", error_message);
    	return field;			/* return field unmodified */
    }

    /*
     * scan through until no more tokens are left
     *
     * starting at anchor points, find an addressing form that
     * matches a set of tokens starting at that anchor point.
     * If the addressing form needs to be separated from the previous
     * by a comma, and it is not currently so separated then
     * insert a comma in the white space before the anchor point token.
     */
    tq_anchor = tq_head;
    while (!ENDTOK(tq_anchor->form)) {
	tq_new = NULL;			/* set when address pattern found */
	if (check_route) {
	    /* scan for:  phrase <route-addr> or phrase : */
	    i = match_route_or_group(tq_anchor, &tq_new, extract_q, group,
				     &len, domain, uucp_host, flags, error);
	    switch (i) {

	    case T_NOMATCH:		/* didn't match route or group */
		tq_new = NULL;
		break;
	    case T_ROUTE:		/* matched a route */
		/* tq_new points to end of complete route form */
		break;
	    case T_GROUP:		/* matched a group */
		/* tq_new points to : at end of group */
		group = TRUE;
		/* NOTE:  next address does not need comma separator */
		new_group = TRUE;
		break;
	    case T_MODIFIED:		/* matched and something modified */
		modified = TRUE;
		break;
	    default:			/* error occured */
		DEBUG1(DBG_FIELD_LO, "process_field: error: %s\n", *error);
		return field;		/* return the field unchanged */
	    }
	}

	if (!tq_new) {
	    /* scan for group terminator: ;[@WORD] */
	    i = match_group_term(tq_anchor, &tq_new, extract_q, group, error);
	    switch (i) {

	    case T_NOMATCH:		/* didn't match group terminator */
		tq_new = NULL;
		break;
	    case T_GROUPTERM:		/* matched a group terminator */
		/* tq_new points to end of complete group terminator */
		need_comma = FALSE;	/* never need a comma before this */
		group = FALSE;		/* not in a group anymore */
		break;
	    default:			/* error occured */
		DEBUG1(DBG_FIELD_LO, "process_field: error: %s\n", *error);
		return field;		/* return the field unchanged */
	    }
	}

	if (!tq_new) {
	    /*
	     * scan for:  WORD [op WORD [op ... WORD]]
	     *	where op is from the set [.!%@] and the sequence
	     *	ends in a WORD.
	     */
	    i = match_general(tq_anchor, &tq_new, &len, extract_q,
			      domain, uucp_host, flags, error);
	    DEBUG1(DBG_FIELD_MID, "match_general returned %d\n", i);
	    switch (i) {

	    case T_NOMATCH:		/* didn't match general address form */
		tq_new = NULL;
		break;
	    case T_GENERAL:		/* matched a general address */
		/* tq_new points to end of address */
		DEBUG(DBG_FIELD_MID, "just match, no mods\n");
		break;
	    case T_MODIFIED:		/* matched and changed in some way */
		/* tq_new points to end of address */
		modified = TRUE;	/* modified in match_general */
		break;
	    case T_MUTANT_FORM:		/* not allowed outside of a route */
		*error = "mutant addressing form outside of route";
		DEBUG1(DBG_FIELD_LO, "process_field: error: %s\n", *error);
		return field;		/* return the field unchanged */

	    default:			/* error occured */
		DEBUG1(DBG_FIELD_LO, "process_field: error: %s\n", *error);
		return field;		/* return the field unchanged */
	    }
	}

	if (!tq_new) {
	    /* we didn't find an addressing form that matched */
	    *error = "unknown addressing form";
	    DEBUG1(DBG_FIELD_LO, "process_field: error: %s\n", *error);
	    return field;
	}

	if (need_comma && field != NULL) {
	    /* there is an address and previous address needs a comma */
	    insert_comma(tq_anchor->space);
	    modified = TRUE;		/* field has been modified */
	    len++;			/* 1 character inserted */
	}

	/*
	 * set state for next pass through the loop
	 */
	if (new_group) {
	    /*
	     * if a group was found, the next address should not be
	     * preceded by a comma and the next token is the token
	     * immediately following the :
	     */
	    need_comma = FALSE;
	    new_group = FALSE;
	    tq_anchor = tq_new->succ;
	} else {
	    if (tq_new->succ->text[0] == ',') {
		/*
		 * if the next token is a comma, then we will not need to
		 * insert one before the next address.
		 * The next token is the one after the ','
		 */
		need_comma = FALSE;
		/* skip the comma */
		tq_anchor = tq_new->succ->succ;
	    } else {
		/*
		 * not a new group, and next token not a comma, we
		 * may need to insert a comma before the next address
		 * The next token is the one after the end of the previous
		 * match.
		 */
		need_comma = TRUE;
		tq_anchor = tq_new->succ;
	    }
	}
    }

    if (modified && field != NULL) {
	/* copy finished results into buffer for returning to caller */
	field = finish_mod_clean(field, (unsigned)(fp-field), tq_head, len);
    }

    DEBUG1(DBG_FIELD_LO, "process_field: return %s\n", field);
    return field;			/* all done, return the header */
}


/*
 * finish_mod_clean - return string for field name token list
 *
 * Finish process_field for the case that the header field was modified
 * by copying the field name and the tokens into a string area and
 * returning a pointer to the string.
 *
 * inputs:
 *	field_name - string to copy to beginning of buffer
 *	name_len - number of chars to copy from field_name
 *	tq_head	- token queue to copy into buffer
 *	len	- computed total length of result
 *
 * output:
 *	pointer to string representing completed header field
 *
 * called by: process_field
 * calls: detokenize
 */
static char *
finish_mod_clean(field_name, name_len, tq_head, len)
    char *field_name;			/* field name string */
    unsigned name_len;			/* length of field name */
    struct token *tq_head;		/* head of list to convert to string */
    unsigned len;			/* computed length of result */
{
    register char *p = xmalloc(len);	/* where to store result */

    DEBUG(DBG_FIELD_HI, "field was modified--build string for return\n");
    DEBUG1(DBG_FIELD_HI, "field = %s\n", field_name);
    /* copy field name up to colon */
    (void)memcpy(p, field_name, (size_t) name_len);

    /*
     * copy space and text from queued tokens
     */
    (void)detokenize(TRUE, p+name_len, tq_head, (struct token *)NULL);

    DEBUG1(DBG_FIELD_HI, "completed string: %s\n", p);

    return p;
}


/*
 * insert_comma - insert a comma after a comment or at beginning of string
 *
 * Given the space field from a token, insert a ',' character either
 * after the last comment (if one exists) or at the beginning of the
 * string, if no comment exists in the string.
 *
 * input:
 *	s	- string in which to insert a comma
 *
 * outputs:
 *	none
 *
 * called by: process_field
 */
static void
insert_comma(s)
    char *s;
{
    register char *p;			/* end point of copy */
    register char *q;			/* temp pointer */

    /* put comma at beginning of white space or after last comment */
    p = rindex(s, ')');
    if (!p) {
	p = s;
    } else {
	p++;				/* advance beyond ) */
    }

    /* copy text up one byte to allow space for comma */
    for (q = p+strlen(p)+1; q != p; --q) {
	q[0] = q[-1];
    }

    *q = ',';				/* insert the comma */
}


/*
 * match_route_or_group - reduce on a route or group form if possible
 *
 * This function is called by process_field to determine if the current
 * anchor point is the beginning of a route or a group.  If so the
 * route or group is processed and the end of the route or group is
 * returned.
 *
 * inputs:
 *	tq_anchor - the anchor point from process_field.
 *	tq_new	- pointer to variable in which to return the end
 *		  of the matched form.
 *	extract_q - Address queue in which to insert extracted addresses.
 *		  NULL if we are not extracting addresses.
 *	group	- TRUE if a matched group would be recursive,
 *		  this is specifically an RFC822 no-no and is likely
 *		  to mean that an unsupported addressing form has
 *		  been used.
 *	domain	- if non-NULL, a domain which is to be appended in
 *		  RFC822 '@domain' form to local addresses.
 *	uucp_host - if non-NULL, a string to prepend to ! routes.
 *		  The purpose of this field is to keep ! routes in
 *		  From: or Sender: fields in ! route notation and to
 *		  ensure that the ! route will correctly return to
 *		  the sender, assuming software on other machines
 *		  doing something else.
 *	flags	- A bitwise or of the following flags from field.h:
 *		  F_LOCAL  - set if message originated on the local host.
 *			     This causes domains to be fully qualified.
 *		  F_STRICT - set to adhere more closely to RFC822.  When
 *			     this is set, then all local addressing forms,
 *			     bang routes and tokens%domain forms are appended
 *			     with @domain, if domain is given, and prepended
 *			     with uucp_host, if uucp_host is given.  This is
 *			     for use in gatewaying to stricter networks.
 *		  F_ALIAS  - set to parse an aliases-style file.  In these
 *			     cases, '#' introduces a comment and a the
 *			     string ":include:" is allowed at the start of
 *			     a text token, and does not introduce a group.
 *	error	- store an error message here if an error occurs.
 *
 * output:
 *	T_NOMATCH if not matched, T_ROUTE if matched a route,
 *	T_GROUP if matched a group,
 *	T_MODIFIED if general addressing form which modified field,
 *	FAIL if error.
 *
 * called by: process_field
 * calls: match_general, enqueue_address
 */
static int
match_route_or_group(tq_anchor, tq_new, extract_q, group,
		     len, domain, uucp_host, flags, error)
    struct token *tq_anchor;		/* anchor point from process_field */
    struct token **tq_new;		/* return last matched token here */
    struct addr **extract_q;		/* queue in which to put addresses */
    unsigned *len;			/* len variable from process_field */
    int group;				/* TRUE if group would be recursive */
    char *domain;			/* domain to add to local addresses */
    char *uucp_host;			/* uucp host to prepend to ! routes */
    int flags;				/* miscellaneous flags */
    char **error;			/* store error message here */
{
    register struct token *tq;		/* temp for scanning tokens */
    int recursion_level = 1;		/* embeddedness of route */
    struct token *tq_start;		/* start of innermost route */
    struct token *tq_end;		/* end of innermost route */
    struct token *tq_temp;		/* temp */
    int seek_end;			/* we are scanning for end of route */
    int i;				/* temp */

    *tq_new = NULL;			/* nothing yet */

    /*
     * scan through tokens until we know what we have:
     * a route, a group or something else.
     */
    tq = tq_anchor;
    for (;;) {
    	if (tq->text[0] == ',' || ENDTOK(tq->form)) {
	    /* we have something else nothing left to do here */
    	    return T_NOMATCH;		/* we didn't match anything */
    	}
    	if (tq->text[0] == '<') {
    	    /* we have a route */
    	    DEBUG(DBG_FIELD_MID, "We have a route\n");
    	    *tq_new = tq;
	    break;
    	}
    	if (tq->text[0] == ':' && tq->form == T_OPER) {
    	    /* we have a group */
    	    DEBUG(DBG_FIELD_MID, "We have a group\n");
    	    *tq_new = tq;
    	    if (group) {
    	    	/* catch recursive groups */
		*error = "recursive address group";
		return FAIL;
    	    }
    	    return T_GROUP;		/* signal that we have a group */
    	}
	tq = tq->succ;		/* get next token */
    }

    /*
     * we have a route, search for end point of route
     * and note the tokens in the innermost recursion
     * level so that we can extract them as an address.
     */
    tq_start = (*tq_new)->succ;
    seek_end = 1;			/* assume we are innermost for now */

    /* allow recursion because it happens sometimes */
    for (tq = (*tq_new)->succ;
	 !ENDTOK(tq->form);
	 tq = tq->succ)
    {
	if (tq->text[0] == '<') {
	    recursion_level++;
	    DEBUG(DBG_FIELD_HI, "bump up recursion level on route\n");

	    /* at more deeply nested address, forget what we had before */
	    tq_start = tq->succ;
	    tq_end = NULL;
	    seek_end = 1;
	} else if (tq->text[0] == '>') {
	    recursion_level--;
	    DEBUG(DBG_FIELD_HI, "bump down recursion level on route\n");
	    seek_end = 0;		/* the end, if no more < tokens */

	    if (recursion_level == 0) {
		break;
	    }
	} else if (seek_end) {
	    tq_end = tq;		/* could be the end of the address */
	}
    }

    *tq_new = tq;		/* end of matched route */
    if (recursion_level) {
	*error = "unterminated route";
	return FAIL;			/* signal an error */
    }
    if (tq_end == NULL) {
	*error = "null route";
	return FAIL;			/* signal an error */
    }

    /*
     * route may match a general WORD op WORD op ... WORD form
     */
    i = match_general(tq_start, &tq_temp, len, extract_q,
		      domain, uucp_host, flags, error);
    switch (i) {

    case T_NOMATCH:
	break;				/* didn't match */

    case T_MUTANT_FORM:
	break;				/* mutant form allowed in route */

    case T_GENERAL:
	/* match, didn't modify anything */
	if (tq_temp != tq_end) {
	    /* not a complete match--this is a problem */
	    *error = "syntax error in address";
	    return FAIL;
	}
	return T_ROUTE;			/* matched route, nothing modified */

    case T_MODIFIED:			/* match and something was modified */
	return T_MODIFIED;

    default:				/* an error occured */
	return FAIL;			/* propogate the error */
    }

    if (extract_q) {
	if (enqueue_address(extract_q, tq_start, tq_end, error) == FAIL) {
	    /* enqueue_address returned error, specific error already logged */
	    return FAIL;		/* signal an error */
	}
    }
    return T_ROUTE;			/* signal a route */
}


/*
 * match_group_term - match a group terminator pattern (;[@TOKEN]).
 *
 * Called from check_field to determine if the tokens after the
 * anchor point match a group terminator pattern (a semicolon optionally
 * followed by the pattern @WORD.
 *
 * inputs:
 *	tq_anchor - the anchor point from process_field.
 *	tq_new	- pointer to variable in which to return the end
 *		  of the matched form.
 *	extract_q - Address queue in which to insert extracted addresses.
 *		  NULL if we are not extracting addresses.
 *	group	- TRUE if we are now in a group.  If this is not
 *		  the case then a match on a group terminator would
 *		  be an error.
 *	error	- store an error message here, on errors.
 *
 * output:
 *	T_NOMATCH if not matched, T_GROUPTERM if match, FAIL on error.
 */
/*ARGSUSED*/
static int
match_group_term(tq_anchor, tq_new, extract_q, group, error)
    struct token *tq_anchor;		/* anchor point from process_field */
    struct token **tq_new;		/* return last matched token here */
    struct addr **extract_q;		/* queue in which to add addresses */
    int group;				/* TRUE if we are processing a group */
    char **error;			/* store error message here */
{
    register struct token *tq;		/* temp for scanning list of tokens */

    tq = tq_anchor;			/* copy this into a register */

    /*
     * if first token is a ; then we have a terminator and it
     * just remains to see if an optional, correct @WORD pattern
     * follows it, or if matching a group terminator is an error.
     */
    if (tq->text[0] == ';') {
	if (!group) {
	    /* no matching group : form exists, this is not correct */
	    *error = "\";\" does not terminate a group";
	    return FAIL;		/* signal an error */
	}
	if (tq->succ->text[0] == '@') {
	    /* optional @WORD given, make sure the WORD exists */
	    *tq_new = tq = tq->succ->succ;
	    if (!WORDTOK(tq->form)) {
		*error = "syntax error in address";
		return FAIL;		/* signal an error */
	    }
	    DEBUG(DBG_FIELD_MID, "group terminator of form ;@WORD\n");
	    return T_GROUPTERM;		/* match */
	} else {
	    /* no optional @WORD */
	    DEBUG(DBG_FIELD_MID, "simple group terminator\n");
	    *tq_new = tq;
	    return T_GROUPTERM;		/* match */
	}
    }
    return T_NOMATCH;			/* no match */
}


/*
 * match_general - match a general address form WORD [op WORD [op ... WORD]]
 *
 * Called from check_field to determine if the tokens after the
 * anchor point match a general address form, which is a sequence
 * of WORD tokens separated by operators from the set ".!%@".
 *
 * If domain is given and we have an address which is just WORD, then
 * append @domain to the address.
 *
 * If uucp_host is given and we have a bang route, then prepend
 * uucp_host! to the address.
 *
 * If local is TRUE and address is WORD*@WORD1 or WORD*%WORD1 then
 * have the domain WORD1 fully qualfied if possible.
 *
 * inputs:
 *	tq_anchor - the anchor point from process_field.
 *	tq_new	- pointer to variable in which to return the end
 *		  of the matched form.
 *	len	- len variable from check_form.  This routine may modify
 *		  an address.  If so, the len variable is modified to
 *		  taken into account the change in length of the
 *		  header field.
 *	extract_q - Address queue in which to insert extracted addresses.
 *		  NULL if we are not extracting addresses.
 *	domain	- if non-NULL, a domain which is to be appended in
 *		  RFC822 '@domain' form to local addresses.
 *	uucp_host - if non-NULL, a string to prepend to ! routes.
 *		  The purpose of this field is to keep ! routes in
 *		  From: or Sender: fields in ! route notation and to
 *		  ensure that the ! route will correctly return to
 *		  the sender, assuming software on other machines
 *		  doing something else.
 *	flags	- A bitwise or of the following flags from field.h:
 *		  F_LOCAL  - set if message originated on the local host.
 *			     This causes domains to be fully qualified.
 *		  F_STRICT - set to adhere more closely to RFC822.  When
 *			     this is set, then all local addressing forms,
 *			     bang routes and tokens%domain forms are appended
 *			     with @domain, if domain is given, and prepended
 *			     with uucp_host, if uucp_host is given.  This is
 *			     for use in gatewaying to stricter networks.
 *		  F_ALIAS  - set to parse an aliases-style file.  In these
 *			     cases, '#' introduces a comment and a the
 *			     string ":include:" is allowed at the start of
 *			     a text token, and does not introduce a group.
 *	error	- store any error message here.
 *
 * output:
 *	T_NOMATCH if no match found, T_GENERAL if match and unmodified,
 *	T_MODIFIED if @domain appended, uucp_host! prepended, or domain
 *	qualified, FAIL on error.
 *
 * called by: process_field
 * calls: queue_qualify_domain, enqueue_address
 */
static int
match_general(tq_anchor, tq_new, len, extract_q, domain,
	      uucp_host, flags, error)
    struct token *tq_anchor;		/* anchor point from process_field */
    struct token **tq_new;		/* return last matched token here */
    unsigned *len;			/* len variable from process_field */
    struct addr **extract_q;		/* queue in which to add addresses */
    char *domain;			/* domain to add to local addresses */
    char *uucp_host;			/* uucp host to prepend to ! routes */
    int flags;				/* miscellaneous flags */
    char **error;			/* store error message here */
{
    register struct token *tq;		/* temp for scanning token list */
    register struct token *tq_temp;	/* temp */
    int bang_route = FALSE;		/* TRUE if bang route */
    int pure_bang_route = TRUE;		/* TRUE if pure bang route */
    int domain_address = FALSE;		/* TRUE if domain address */
    int at_found = FALSE;		/* TRUE if @ token found */
    int ret_val = T_GENERAL;		/* value to be returned */
    struct token *tq_mark = NULL;	/* mark primary domain */

    tq = tq_anchor;			/* load anchor into a register */
    if (!WORDTOK(tq->form) && tq->text[0] != '.') {
	/* it doesn't begin with WORD token */
	return T_NOMATCH;		/* signal no match */
    }

    /* some part of the remaining tokens matches the form */

    /* skip initial collection of zero or more WORD tokens delimited
     * by one or more "." tokens */
    for (;;) {
	tq_temp = tq->succ;
	if (tq->text[0] == '.' && tq_temp->text[0] == '.') {
	    tq = tq_temp;
	    continue;
	}
	if ((WORDTOK(tq->form) || WORDTOK(tq_temp->form)) &&
	    (tq->text[0] == '.' || tq_temp->text[0] == '.')) {
	    tq = tq_temp;
	    continue;
	}
	break;
    }

    while (!ENDTOK(tq->succ->form) && index("!%@", tq->succ->text[0]) &&
	   (WORDTOK(tq->succ->succ->form) ||
	    tq->succ->succ->text[0] == '.'))
    {
	switch(tq->succ->text[0]) {

	case '!':			/* take first host in ! route */
	    bang_route = TRUE;
	    break;
	case '%':			/* alternately, last % host */
	    if (!bang_route) {
		tq_mark = tq->succ->succ;
		domain_address = TRUE;
	    }
	    pure_bang_route = FALSE;
	    break;
	case '@':			/* always take last @ host */
	    tq_mark = tq->succ->succ;
	    domain_address = TRUE;
	    at_found = TRUE;
	    pure_bang_route = FALSE;
	    break;
	}
	tq = tq->succ->succ;

	/*
	 * skip initial collection of zero or more WORD tokens delimited
	 * by one or more "." tokens
	 */
	for (;;) {
	    tq_temp = tq->succ;
	    if (tq->text[0] == '.' && tq_temp->text[0] == '.') {
		tq = tq_temp;
		continue;
	    }
	    if ((WORDTOK(tq->form) || WORDTOK(tq_temp->form)) &&
		(tq->text[0] == '.' || tq_temp->text[0] == '.')) {
		tq = tq_temp;
		continue;
	    }
	    break;
	}
    }
    /* do we match host!(host!)*@route ? */
    if (pure_bang_route && tq->succ->text[0] == '!' &&
	tq->succ->succ->text[0] == '@')
    {
	return T_MUTANT_FORM;		/* mutant form allowed for route */
    }

    DEBUG(DBG_FIELD_HI, "found a WORD op WORD op ... WORD sequence\n");
    *tq_new = tq;		/* at end of sequence */

    /*
     * qualify domain by appending qualifier to it, if needed
     *
     * If we have a WORD*@WORD1 or a WORD*%WORD1 form, qualify
     * the domain WORD1, if necessary by appending a qualifier
     * to the domain.  len is updated to reflect length change.
     */
    if (domain_address && (flags&F_LOCAL)) {
	char *s = queue_qualify_domain(tq_mark, tq);

	if (s) {
	    /* append .s */
	    (*tq_new)->text = xprintf("%s.%s", (*tq_new)->text, s);

	    /* field length increased */
	    *len += 1 + strlen(s);
	    ret_val = T_MODIFIED;
	}
    }

    if (!tq_mark && uucp_host && (bang_route || (flags&F_STRICT))) {
	/*
	 * we have a bang route, prepend uucp_host! to the route.
	 * Also prepend uucp_host! to the route if we are doing
	 * strict RFC822.  In this case an address will be
	 * prepended with the route back to the sender and
	 * appended with the current domain.
	 */
	tq_anchor->text = xprintf("%s!%s", uucp_host, tq_anchor->text);
	/* field length increased */
	*len += 1 + strlen(uucp_host);
	ret_val = T_MODIFIED;
    }

    /*
     * if we want strict addresses and we don't have one, or if
     * we have a local address but we don't want one (for a local
     * message only), then append @domain
     */

    if (domain) {
	if ((flags & F_STRICT && ! at_found) ||
	    ((flags & F_LOCAL) &&
	     ! (domain_address || bang_route || at_found)))
	    {
		(*tq_new)->text = xprintf("%s@%s", (*tq_new)->text, domain);

		/* field length increased */
		*len += 1 + strlen(domain);
		ret_val = T_MODIFIED;
	    }
    }

    if (extract_q) {
    	/*
    	 * have the address added to the extraction queue
    	 */
	if (enqueue_address(extract_q, tq_anchor, *tq_new, error) < FAIL) {
	    /* enqueue_address returned error, specific error already logged */
	    return FAIL;		/* signal an error */
	}
	DEBUG(DBG_FIELD_MID, "address enqueued\n");
    }

    return ret_val;
}


/*
 * queue_qualify_domain - untokenize a domain and call qualify_domain
 *
 * Called from match_general, this routine takes a token list
 * representing a domain, converts it back to a string and calls
 * qualify_domain() to determine if any text needs to be appended
 * in order to make the domain fully qualified.
 *
 * inputs:
 *	tq_start - first token in domain
 *	tq_end	- last token in domain
 *
 * output:
 *	NULL if nothing should be appended to the domain,
 *	otherwise a string which represents the complete super
 *	domain that the given domain should be qualified in.
 *
 * called by:  match_general
 * calls:  qualify_domain(external), detokenize
 */
static char *
queue_qualify_domain(tq_start, tq_end)
    struct token *tq_start;		/* beginning of domain reference */
    struct token *tq_end;		/* end of domain reference */
{
    struct str str;
    register struct str *sp = &str;	/* dynamic string region */
    register struct token *tq;		/* temp for scanning through tokens */
    char *ret;				/* return value from qualify_domain */

    STR_INIT(sp);			/* initialize dynamic string region */

    /* get string represented by domain tokens */
    tq = tq_start;
    do {
	STR_CAT(sp, tq->text);
    } while (tq != tq_end && (tq = tq->succ));
    STR_NEXT(sp, '\0');

    /* send out for the actual qualification */
    ret = qualify_domain(sp->p);
    DEBUG2(200, "qualify_domain(%s) returns %s\n", sp->p, ret? ret: "(null)");

    STR_FREE(sp);		/* free region */

    return ret;			/* return the value from qualify_domain() */
}


/*
 * enqueue_address - insert a new address into a queue
 *
 * Given a token list representing an address, detokenize the list
 * and add it to the given address queue.
 *
 * inputs:
 *	q	- pointer to queue of addresses
 *	tq_start - first token in the address
 *	tq_end	- ending token of the address
 *	errro	- store any error message here
 *
 * outputs:
 *	SUCCEED if everything went okay, FAIL on error
 *
 * called by: match_or_route_group, match_general
 * calls: detokenize
 */
static int
enqueue_address(q, tq_start, tq_end, error)
    struct addr **q;			/* queue in which to insert */
    struct token *tq_start;		/* first token in the address */
    struct token *tq_end;		/* ending token in the address */
    char **error;			/* store error message here */
{
    register char *s;			/* string representing the address */
    register struct addr *temp_q;	/* temp */
    char *parse_error;			/* error from parse_address() */

    /* grab the string corresponding to the tokens */
    s = detokenize(FALSE, (char *)NULL, tq_start, tq_end);

    DEBUG1(DBG_FIELD_LO, "enqueue_address(%s)\n", s);
    /* insert it into the queue */
    temp_q = alloc_addr();		/* get an address queue entry */
    temp_q->succ = *q;
    temp_q->in_addr = s;
    /* work_addr gets a mungeable copy */
    if ((temp_q->work_addr = preparse_address(s, &parse_error)) == NULL) {
	*error = xprintf("%s: %s", s, error);
	return FAIL;
    }
    *q = temp_q;			/* insert at beginning of list */

    return SUCCEED;			/* added to the list */
}


/*
 * dump_tokens - list tokens to standard error for debugging purposes
 *
 * called from the DUMP_TOKENS macro, this function generates
 * a verbose description of what is going on with a list of tokens.
 *
 * input:
 *	tq	- head of a queue of tokens
 *
 * outputs:
 *	none
 *
 * called by:  DUMP_TOKENS(local macro)
 */
void
dump_tokens(tq)
    register struct token *tq;		/* dump these tokens on errfile */
{
    (void)fprintf(errfile, "token list:\n");
    while (tq) {
	register char *s;
	char buf[100+1];

	switch(tq->form) {

	case T_QUOTE:
	    s = "T_QUOTE";
	    break;
	case T_DOMLIT:
	    s = "T_DOMLIT";
	    break;
	case T_OPER:
	    s = "T_OPER";
	    break;
	case T_TEXT:
	    s = "T_TEXT";
	    break;
	case T_END:
	    s = "T_END";
	    break;
	case T_ERROR:
	    s = "T_ERROR";
	    break;
	default:
	    (void)sprintf(s = buf, "form=%d", tq->form);
	    break;
	}
	(void)fprintf(errfile, "\t|%s|%s|%s|\n",
		      tq->space, tq->text, s);
	tq = tq->succ;
    }
    (void)fprintf(errfile, "end of list\n");
}


#ifdef STANDALONE

int send_to_postmaster = FALSE;		/* see if this gets set */
int return_to_sender = FALSE;		/* see if this gets set */
struct addr *recipients = NULL;		/* initial list here is zero */
char **args_recipients = {0};		/* nothing in this list */
int exitvalue = 0;
FILE *errfile = stderr;
char *primary_name = NULL;
char *program = "field";
int compile_num = 999;

extern int getopt();
extern char *optarg;
extern int optind;

#ifdef DEBUG_LEVEL
 int debug = DEBUG_LEVEL;
#else	/* DEBUG_LEVEL */
 int debug = 0;
#endif	/* DEBUG_LEVEL */

/*
 * test the above functions by calling process_field for each
 * argument given to the program.
 */
void
main(argc, argv)
    int argc;				/* count of arguments */
    char **argv;			/* vector of arguments */
{
    char *s;				/* return value from process_field */
    struct addr *q;			/* temp for scanning hdr_recipients */
    char *error;
    char *domain = NULL;
    char *uucp_host = NULL;
    int flags = 0;
    int c;

    while ((c = getopt(argc, argv, "v:p:d:u:lsaD:")) != EOF) {
	    switch (c) {
	    case 'v':
		    visible_name = optarg;
		    break;

	    case 'p':
		    primary_name = optarg;
		    break;

	    case 'd':
		    domain = optarg;
		    break;

	    case 'u':
		    uucp_host = optarg;
		    break;

	    case 'l':
		    flags |= F_LOCAL;
		    break;

	    case 's':
		    flags |= F_STRICT;
		    break;

	    case 'a':
		    flags |= F_ALIAS;
		    break;

	    case 'D':
		    debug = atoi(optarg);
		    break;
	    }
    }

    argc -= optind;
    argv += optind;

    /*
     * loop over all arguments
     */
    if (argc > 0) {
	while (*argv) {
	    (void)fprintf(stderr, "input:  %s\n", *argv);
	    s = index(*argv, ':');
	    if (s) {
		s++;
	    } else {
		s = *argv;
	    }

	    /*
	     * non-strict RFC822, from local machine
	     */
	    error = NULL;
	    s = process_field(*argv, s, domain, uucp_host,
			      &recipients, flags, &error);
	    if (error) {
		(void) fprintf(stderr, "error: %s\n", error);
	    } else {
		(void)fprintf(stderr, "output: %s\n", s? s: "(null)");
	    }
	    argv++;
	}
    } else {
	char line[4096];

	while (fgets(line, sizeof(line), stdin) != NULL) {
	    (void)fprintf(stderr, "input:  %s\n", line);
	    s = index(line, ':');
	    if (s) {
		s++;
	    } else {
		s = line;
	    }

	    /*
	     * non-strict RFC822, from local machine
	     */
	    error = NULL;
	    s = process_field(line, s, domain, uucp_host,
			      &recipients, flags, &error);
	    if (error) {
		(void) fprintf(stderr, "error: %s\n", error);
	    } else {
		(void)fprintf(stderr, "output: %s\n", s? s: "(null)");
	    }
	}
    }

    for (q = recipients; q; q = q->succ) {
	(void)printf("%s\n", q->in_addr);
    }
    exit(exitvalue);
}

#endif	/* STANDALONE */
