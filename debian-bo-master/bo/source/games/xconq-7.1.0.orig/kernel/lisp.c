/* Support for Lisp-style data.
   Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* (should have some deallocation support, since some game init data
   can be discarded) */

#include "config.h"
#include "misc.h"
#include "lisp.h"

static Symentry *lookup_string PARAMS ((char *str));
static int hash_name PARAMS ((char *str));

/* Pointer to "nil", the empty list. */

Obj *lispnil;

/* Pointer to "eof", which is returned if no more forms in a file. */

Obj *lispeof;

/* Pointer to a "closing paren" object used only during list reading. */

Obj *lispclosingparen;

/* Pointer to an "unbound" object that indicates unbound variables. */

Obj *lispunbound;

/* Current number of symbols in the symbol table. */

int numsymbols = 0;

/* Pointer to the base of the symbol table itself. */

static Symentry **symboltablebase = NULL;

/* The number of Lisp objects allocated so far. */

int lispmalloc = 0;

/* This variable is used to track the depth of nested #| |# comments. */

int commentdepth = 0;

int actually_read_lisp = TRUE;

#define BIGBUF 1000

static char *lispstrbuf = NULL;

static int *startlineno;
static int *endlineno;
static char linenobuf[50];

/* Allocate a new Lisp object, count it as such. */

static Obj *
newobj()
{
    lispmalloc += sizeof(Obj);
    return ((Obj *) xmalloc(sizeof(Obj)));
}

/* Pre-create some objects that should always exist. */

void
init_lisp()
{
    /* Allocate Lisp's NIL. */
    lispnil = newobj();
    lispnil->type = NIL;
    /* Do this so car/cdr of nil is nil, might cause infinite loops though. */
    lispnil->v.cons.car = lispnil;
    lispnil->v.cons.cdr = lispnil;
    /* We use the eof object to recognize eof when reading a file. */
    lispeof = newobj();
    lispeof->type = EOFOBJ;
    /* The "closing paren" object just flags closing parens while reading. */
    lispclosingparen = newobj();
    /* The "unbound" object is for unbound variables. */
    lispunbound = newobj();
    /* Set up the symbol table. */
    symboltablebase = (Symentry **) xmalloc(256 * sizeof(Symentry *));
    numsymbols = 0;
    init_predefined_symbols();
}

/* Ultra-simple "streams" that can be stdio FILEs or strings. */

int
strmgetc(strm)
Strm *strm;
{
    if (strm->type == stringstrm) {
	if (*(strm->ptr.sp) == '\0')
	  return EOF;
	return *((strm->ptr.sp)++);
    } else {
	return getc(strm->ptr.fp);
    }
}

void
strmungetc(ch, strm)
int ch;
Strm *strm;
{
    if (strm->type == stringstrm) {
	--strm->ptr.sp;
    } else {
	ungetc(ch, strm->ptr.fp);
    }
}

/* El cheapo Lisp reader.  Lisp objects are generally advertised by their
   first characters, but lots of semantics actions happen while reading, so
   this isn't really a regular expression reader. */

Obj *
read_form(fp, p1, p2)
FILE *fp;
int *p1, *p2;
{
    Obj *rslt;
    Strm tmpstrm;

    commentdepth = 0;
    startlineno = p1;
    endlineno = p2;
    tmpstrm.type = filestrm;
    tmpstrm.ptr.fp = fp;
    rslt = read_form_aux(&tmpstrm);
    if (rslt == lispclosingparen) {
	sprintf_context(linenobuf, 50, startlineno, endlineno, NULL);
	init_warning("extra close paren, substituting nil%s", linenobuf);
	rslt = lispnil;
    }
    return rslt;
}

Obj *
read_form_from_string(str, p1, p2)
char *str;
int *p1, *p2;
{
    Obj *rslt;
    Strm tmpstrm;

    commentdepth = 0;
    startlineno = p1;
    endlineno = p2;
    tmpstrm.type = stringstrm;
    tmpstrm.ptr.sp = str;
    rslt = read_form_aux(&tmpstrm);
    if (rslt == lispclosingparen) {
	sprintf_context(linenobuf, 50, startlineno, endlineno, NULL);
	init_warning("extra close paren, substituting nil%s", linenobuf);
	rslt = lispnil;
    }
    return rslt;
}

void
sprintf_context(buf, n, start, end, context)
char *buf, *context;
int n, *start, *end;
{
    buf[0] = '\0';
    if (start != NULL && end != NULL) {
	if (*start == *end) {
	    sprintf(buf, " (at line %d)", *start);
	} else {
	    sprintf(buf, " (lines %d to %d)", *start, *end);
	}
    }
    if (!empty_string(context)) {
	strncpy(buf+strlen(buf), context, n - 1);
	buf[n - 1] = '\0';
    }
}

Obj *
read_form_aux(strm)
Strm *strm;
{
    int minus, factor, commentclosed, ch, ch2, ch3, ch4, num;

    while ((ch = strmgetc(strm)) != EOF) {
	/* Recognize nested comments specially. */
	if (ch == '#') {
	    if ((ch2 = strmgetc(strm)) == '|') {
		commentclosed = FALSE;
		++commentdepth;
		while ((ch3 = strmgetc(strm)) != EOF) {
		    if (ch3 == '|') {
			/* try to recognize # */
			if ((ch4 = strmgetc(strm)) == '#') {
			    --commentdepth;
			    if (commentdepth == 0) {
				commentclosed = TRUE;
				break;
			    }
			} else {
			    strmungetc(ch4, strm);
			}
		    } else if (ch3 == '#') {
			if ((ch4 = strmgetc(strm)) == '|') {
			    ++commentdepth;
			} else {
			    strmungetc(ch4, strm);
			}
		    } else if (ch3 == '\n') {
			if (endlineno != NULL)
			  ++(*endlineno);
			announce_read_progress();
		    }
		}
		if (!commentclosed) {
		    init_warning("comment not closed at eof");
		}
		/* Always pick up the next char. */
		ch = strmgetc(strm);
	    } else {
		strmungetc(ch2, strm);
	    	return intern_symbol("#");
	    }
	}
	/* Regular lexical recognition. */
	if (isspace(ch)) {
	    /* Nothing to do here except count lines. */
	    if (ch == '\n') {
		if (endlineno != NULL)
		  ++(*endlineno);
		if (startlineno != NULL)
		  ++(*startlineno);
		announce_read_progress();
	    }
	} else if (ch == ';') {
	    /* Discard all from here to the end of this line. */
	    while ((ch = strmgetc(strm)) != EOF && ch != '\n');
	    if (endlineno != NULL)
	      ++(*endlineno);
	    announce_read_progress();
	} else if (ch == '(') {
	    /* Jump into a list-reading mode. */
	    return read_list(strm);
	} else if (ch == ')') {
	    /* This is just to flag the end of the list for read_list. */
	    return lispclosingparen;
	} else if (ch == '"') {
	    read_delimited_text(strm, "\"", FALSE, FALSE);
	    if (!actually_read_lisp)
	      return lispnil;
	    return new_string(copy_string(lispstrbuf));
	} else if (ch == '|') {
	    read_delimited_text(strm, "|", FALSE, FALSE);
	    if (!actually_read_lisp)
	      return lispnil;
	    return intern_symbol(lispstrbuf);
	} else if (strchr("`'", ch)) {
	    if (!actually_read_lisp)
	      return lispnil;
	    return cons(intern_symbol("quote"),
			cons(read_form_aux(strm), lispnil));
	} else if (isdigit(ch) || ch == '-' || ch == '+' || ch == '.') {
	    int numdice = 0, dice = 0, indice = FALSE;

	    minus = (ch == '-');
	    factor = (ch == '.' ? 100 : 1);
	    num = (minus ? 0 : ch - '0');
	    while ((ch = strmgetc(strm)) != EOF) {
	    	if (isdigit(ch)) {
	    	    /* should ignore decimal digits past second one */
		    num = num * 10 + ch - '0';
		    if (factor > 1)
		      factor /= 10;
		} else if (ch == 'd') {
		    numdice = num;
		    num = 0;
		    indice = TRUE;
		} else if (ch == '+' || ch == '-') {
		    dice = num;
		    num = 0;
		    indice = FALSE;
		} else if (ch == '.') {
		    factor = 100;
		} else {
		    break;
		}
	    }
	    /* If number was followed by a % char, discard the char, otherwise
	       put it back on the stream. */
	    if (ch != '%')
	      strmungetc(ch, strm);
	    if (indice) {
		dice = num;
		num = 0;
	    }
	    if (minus)
	      num = 0 - num;
	    if (numdice > 0) {
	    	num = (1 << 14) | (numdice << 11) | (dice << 7) | (num & 0x7f);
	    } else {
	    	num = factor * num;
	    }
	    if (!actually_read_lisp)
	      return lispnil;
	    return new_number(num);
	} else {
	    /* Read a regular symbol. */
	    /* The char we just looked will be the first char. */
	    strmungetc(ch, strm);
	    /* Now read until any special char seen. */
	    ch = read_delimited_text(strm, "();\"'`#", TRUE, TRUE);
	    /* Undo the extra char we read in order to detect the end
	       of the symbol. */
	    strmungetc(ch, strm);
	    /* Need to recognize nil specially here. */
	    if (strcmp("nil", lispstrbuf) == 0) {
		return lispnil;
	    } else if (!actually_read_lisp) {
	    	if (strcmp("else", lispstrbuf) == 0)
		  return intern_symbol(lispstrbuf);
	        if (strcmp("end-if", lispstrbuf) == 0)
		  return intern_symbol(lispstrbuf);
		return lispnil;    
	    } else {
		return intern_symbol(lispstrbuf);
	    }
	}
    }
    return lispeof;
}

/* Read a sequence of expressions terminated by a closing paren.  This works
   by looping; although recursion is more elegant, if the compiler does not
   turn tail-recursion into loops, long lists can blow the stack.  (This has
   happened with real saved games.) */

Obj *
read_list(strm)
Strm *strm;
{
    Obj *thecar, *thenext, *lis, *endlis;

    thecar = read_form_aux(strm);
    if (thecar == lispclosingparen) {
	return lispnil;
    } else if (thecar == lispeof) {
	goto at_eof;
    } else {
	lis = cons(thecar, lispnil);
	endlis = lis;
	while (TRUE) {
	    thenext = read_form_aux(strm);
	    if (thenext == lispclosingparen) {
		break;
	    } else if (thenext == lispeof) {
		goto at_eof;
	    } else {
		set_cdr(endlis, cons(thenext, lispnil));
		endlis = cdr(endlis);
	    }
	}
	return lis;
    }
  at_eof:
    sprintf_context(linenobuf, 50, startlineno, endlineno, NULL);
    init_warning("missing a close paren, returning EOF%s", linenobuf);
    return lispeof;
}

/* Read a quantity of text delimited by a char from the given string,
   possibly also by whitespace or EOF. */

int
read_delimited_text(strm, delim, spacedelimits, eofdelimits)
Strm *strm;
char *delim;
int spacedelimits, eofdelimits;
{
    int ch, octch, j = 0, warned = FALSE;

    if (lispstrbuf == NULL)
      lispstrbuf = (char *) xmalloc(BIGBUF);
    while ((ch = strmgetc(strm)) != EOF
	   && (!spacedelimits || !isspace(ch))
	   && !strchr(delim, ch)) {
	/* Handle escape char by replacing with next char,
	   or maybe interpret an octal sequence. */
	if (ch == '\\') {
	    ch = strmgetc(strm);
	    /* Octal chars introduced by a leading zero. */
	    if (ch == '0') {
		octch = 0;
		/* Soak up numeric digits (don't complain about 8 or 9,
		   sloppy but traditional). */
		while ((ch = strmgetc(strm)) != EOF && isdigit(ch)) {
		    octch = 8 * octch + ch - '0';
		}
		/* The non-digit char is actually next one in the string. */
		strmungetc(ch, strm);
		ch = octch;
	    }
	}
	if (j >= BIGBUF) {
	    /* Warn about buffer overflow, but only once per string,
	       then still read chars but discard them. */
	    if (!warned) {
		init_warning(
		 "exceeded max sym/str length (%d chars), ignoring rest",
			     BIGBUF);
		warned = TRUE;
	    }
	} else {
	    lispstrbuf[j++] = ch;
	}
    }
    lispstrbuf[j] = '\0';
    return ch;
}

/* The usual list length function. */

int
length(list)
Obj *list;
{
    int rslt = 0;

    while (list != lispnil) {
	list = cdr(list);
	++rslt;
    }
    return rslt;
}


/* Basic allocation routines. */

Obj *
new_string(str)
char *str;
{
    Obj *new = newobj();

    new->type = STRING;
    new->v.str = str;
    return new;
}

Obj *
new_number(num)
int num;
{
    Obj *new = newobj();

    new->type = NUMBER;
    new->v.num = num;
    return new;
}

Obj *
new_utype(u)
int u;
{
    Obj *new = newobj();

    new->type = UTYPE;
    new->v.num = u;
    return new;
}

Obj *
new_mtype(r)
int r;
{
    Obj *new = newobj();

    new->type = MTYPE;
    new->v.num = r;
    return new;
}

Obj *
new_ttype(t)
int t;
{
    Obj *new = newobj();

    new->type = TTYPE;
    new->v.num = t;
    return new;
}

Obj *
new_pointer(sym, ptr)
Obj *sym;
char *ptr;
{
    Obj *new = newobj();

    new->type = POINTER;
    new->v.ptr.sym = sym;
    new->v.ptr.data = ptr;
    return new;
}

Obj *
cons(x, y)
Obj *x, *y;
{
    Obj *new = newobj();

    new->type = CONS;  
    new->v.cons.car = x;  new->v.cons.cdr = y;
    if (!listp(y))
      run_warning("cdr of cons is not a list");
    return new;
}

void
type_warning(funname, x, typename, subst)
char *funname, *typename;
Obj *x, *subst;
{
    char buf1[BUFSIZE], buf2[BUFSIZE];

    sprintlisp(buf1, x);
    sprintlisp(buf2, subst);
    run_warning("%s of non-%s `%s' being taken, returning `%s' instead",
                funname, typename, buf1, buf2);
}

/* The usual suspects. */

Obj *
car(x)
Obj *x;
{
    if (x->type == CONS || x->type == NIL) {
	return x->v.cons.car;
    } else {
    	type_warning("Car", x, "list", lispnil);
	return lispnil;
    }
}

Obj *
cdr(x)
Obj *x;
{
    if (x->type == CONS || x->type == NIL) {
	return x->v.cons.cdr;
    } else {
    	type_warning("Cdr", x, "list", lispnil);
	return lispnil;
    }
}

Obj *
cadr(x)
Obj *x;
{
    return car(cdr(x));
}

Obj *
cddr(x)
Obj *x;
{
    return cdr(cdr(x));
}

Obj *
caddr(x)
Obj *x;
{
    return car(cdr(cdr(x)));
}

void
set_cdr(x, v)
Obj *x, *v;
{
    if (x->type == CONS) {
	x->v.cons.cdr = v;
    } else {
    	type_warning("set_cdr", x, "cons", lispnil);
    }
}

/* Return the string out of both strings and symbols. */

char *
c_string(x)
Obj *x;
{
    if (x->type == STRING) {
	return x->v.str;
    } else if (x->type == SYMBOL) {
	return x->v.sym.symentry->name;
    } else {
     	type_warning("c_string", x, "string/symbol", lispnil);
	return "";
   }
}

/* Return the actual number in a number object. */

int
c_number(x)
Obj *x;
{
    if (x->type == NUMBER
	|| x->type == UTYPE
	|| x->type == MTYPE
	|| x->type == TTYPE) {
	return x->v.num;
    } else {
     	type_warning("c_number", x, "number", lispnil);
	return 0;
    }
}

Obj *
intern_symbol(str)
char *str;
{
    int n;
    Symentry *se;
    Obj *new1;

    se = lookup_string(str);
    if (se) {
	return se->symbol;
    } else {
	new1 = newobj();
	new1->type = SYMBOL;
	se = (Symentry *) xmalloc(sizeof(Symentry));
	new1->v.sym.symentry = se;
	/* Declare a newly created symbol to be unbound. */
	new1->v.sym.value = lispunbound;
	se->name = copy_string(str); 
	se->symbol = new1;
	se->constantp = FALSE;
	n = hash_name(str);
	/* Push the symbol entry onto the front of its hash bucket. */
	se->next = symboltablebase[n];
	symboltablebase[n] = se;
	++numsymbols;
	return new1;
    }
}

/* Given a string, try to find a symbol entry with that as its name. */

static Symentry *
lookup_string(str)
char *str;
{
    Symentry *se;

    for (se = symboltablebase[hash_name(str)]; se != NULL; se = se->next) {
	if (strcmp(se->name, str) == 0)
	  return se;
    }
    return NULL;
}

static int
hash_name(str)
char *str;
{
    return str[0];
}

Obj *
symbol_value(sym)
Obj *sym;
{
    Obj *val = sym->v.sym.value;

    if (val == lispunbound) {
	run_warning("unbound symbol `%s', substituting nil", c_string(sym));
	val = lispnil;
    }
    return val;
}

Obj *
setq(sym, x)
Obj *sym, *x;
{
    if (!symbolp(sym)) {
	run_warning("Can't set a non-symbol, ignoring attempt");
	return x;
    }
    if (constantp(sym)) {
    	run_warning("Can't alter the constant `%s', ignoring attempt",
		    c_string(sym));
    	return x;
    }
    sym->v.sym.value = x;
    return x;
}

void
makunbound(sym)
Obj *sym;
{
    sym->v.sym.value = lispunbound;
}

void
flag_as_constant(sym)
Obj *sym;
{
    sym->v.sym.symentry->constantp = TRUE;
}

int
constantp(sym)
Obj *sym;
{	
    return (sym->v.sym.symentry->constantp);
}

int
numberp(x)
Obj *x;
{
    return (x->type == NUMBER);
}

int
stringp(x)
Obj *x;
{
    return (x->type == STRING);
}

int
symbolp(x)
Obj *x;
{
    return (x->type == SYMBOL);
}

int
consp(x)
Obj *x;
{
    return (x->type == CONS);
}

int
utypep(x)
Obj *x;
{
    return (x->type == UTYPE);
}

int
mtypep(x)
Obj *x;
{
    return (x->type == MTYPE);
}

int
ttypep(x)
Obj *x;
{
    return (x->type == TTYPE);
}

int
pointerp(x)
Obj *x;
{
    return (x->type == POINTER);
}

int
boundp(sym)
Obj *sym;
{
    return (sym->v.sym.value != lispunbound);
}

int
numberishp(x)
Obj *x;
{
    return (x->type == NUMBER
	    || x->type == UTYPE
	    || x->type == MTYPE
	    || x->type == TTYPE);
}

int
listp(x)
Obj *x;
{
    return (x->type == NIL || x->type == CONS);
}

/* General structural equality test.  Assumes that it is not getting
   passed any circular structures. */

int
equal(x, y)
Obj *x, *y;
{
    /* Objects of different types can never be equal. */
    if (x->type != y->type)
      return FALSE;
	/* Identical objects are always equal. */
    if (x == y)
      return TRUE;
    switch (x->type) {
      case NUMBER:
      case UTYPE:
      case MTYPE:
      case TTYPE:
	return (c_number(x) == c_number(y));
      case STRING:
	return (strcmp(c_string(x), c_string(y)) == 0);
      case SYMBOL:
	return (strcmp(c_string(x), c_string(y)) == 0);
      case CONS:
	return (equal(car(x), car(y)) && equal(cdr(x), cdr(y)));
      case POINTER:
	return FALSE;
      default:
	case_panic("lisp type", x->type);
	return FALSE;
    }
}

int
member(x, lis)
Obj *x, *lis;
{
    if (lis == lispnil) {
	return FALSE;
    } else if (!consp(lis)) {
	/* should probably be an error of some sort */
	return FALSE;
    } else if (equal(x, car(lis))) {
	return TRUE;
    } else {
	return member(x, cdr(lis));
    }
}

/* Return the nth element of a list. */

Obj *
elt(lis, n)
Obj *lis;
int n;
{
    while (n-- > 0) {
	lis = cdr(lis);
    }
    return car(lis);
}

Obj *
reverse(lis)
Obj *lis;
{
    Obj *rslt = lispnil;

    for (; lis != lispnil; lis = cdr(lis)) {
	rslt = cons(car(lis), rslt);
    }
    return rslt;
}

Obj *
find_at_key(lis, key)
Obj *lis;
char *key;
{
    Obj *rest, *bdgs, *bdg;

    for_all_list(lis, rest) {
	bdgs = car(rest);
	bdg = car(bdgs);
	if (stringp(bdg) && strcmp(key, c_string(bdg)) == 0) {
	    return cdr(bdgs);
	}
    }
    return lispnil;
}

Obj *
replace_at_key(lis, key, newval)
Obj *lis, *newval;
char *key;
{
    Obj *rest, *bdgs, *bdg;

    for_all_list(lis, rest) {
	bdgs = car(rest);
	bdg = car(bdgs);
	if (stringp(bdg) && strcmp(key, c_string(bdg)) == 0) {
	    set_cdr(bdgs, newval);
	    return lis;
	}
    }
    return cons(cons(new_string(key), newval), lis);
}

void
fprintlisp(fp, obj)
FILE *fp;
Obj *obj;
{
    int needescape;
    char *str, *tmp;

    /* Doublecheck, just in case caller is not so careful. */
    if (obj == NULL) {
	run_warning("Trying to print NULL as object, skipping");
	return;
    }
    switch (obj->type) {
      case NIL:
	fprintf(fp, "nil");
	break;
      case NUMBER:
	fprintf(fp, "%d", obj->v.num);
	break;
      case STRING:
	if (strchr(obj->v.str, '"')) {
	    fprintf(fp, "\"");
	    for (tmp = obj->v.str; *tmp != '\0'; ++tmp) {
		if (*tmp == '"')
		  fprintf(fp, "\\");
		fprintf(fp, "%c", *tmp);
	    }
	    fprintf(fp, "\"");
	} else {
	    /* Just printf the whole string. */
	    fprintf(fp, "\"%s\"", obj->v.str);
	}
	break;
      case SYMBOL:
	needescape = FALSE;
	str = c_string(obj);
	if (isdigit(str[0])) {
	    needescape = TRUE;
	} else {
	    /* Scan the symbol's name looking for special chars. */
	    for (tmp = str; *tmp != '\0'; ++tmp) {
		if (strchr(" ()#\";|", *tmp)) {
		    needescape = TRUE;
		    break;
		}
	    }
	}
	if (needescape) {
	    fprintf(fp, "|%s|", str);
	} else {
	    fprintf(fp, "%s", str);
	}
	break;
      case CONS:
	fprintf(fp, "(");
	fprintlisp(fp, car(obj));
	/* Note that there are no dotted pairs in our version of Lisp. */
	fprint_list(fp, cdr(obj));
	break;
      case UTYPE:
	fprintf(fp, "u#%d", obj->v.num);
	break;
      case MTYPE:
	fprintf(fp, "m#%d", obj->v.num);
	break;
      case TTYPE:
	fprintf(fp, "t#%d", obj->v.num);
	break;
      case POINTER:
	fprintlisp(fp, obj->v.ptr.sym);
	fprintf(fp, " #|0x%lx|#", (long) obj->v.ptr.data);
	break;
      default:
	case_panic("lisp type", obj->type);
	break;
    }
}

void
fprint_list(fp, obj)
FILE *fp;
Obj *obj;
{
    Obj *tmp;

    for_all_list(obj, tmp) {
	fprintf(fp, " ");
	fprintlisp(fp, car(tmp));
    }	
    fprintf(fp, ")");
}

void
sprintlisp(buf, obj)
char *buf;
Obj *obj;
{
    if (strlen(buf) + 20 > BUFSIZE)
      return;
    switch (obj->type) {
      case NIL:
	sprintf(buf, "nil");
	break;
      case NUMBER:
	sprintf(buf, "%d", obj->v.num);
	break;
      case STRING:
	/* (should print escape chars if needed) */
	sprintf(buf, "\"%s\"", obj->v.str);
	break;
      case SYMBOL:
	/* (should print escape chars if needed) */
	sprintf(buf, "%s", c_string(obj));
	break;
      case CONS:
	strcpy(buf, "(");
	sprintlisp(buf+strlen(buf), car(obj));
	/* No dotted pairs allowed in our version of Lisp. */
	sprint_list(buf+strlen(buf), cdr(obj));
	break;
      case UTYPE:
	sprintf(buf, "u#%d", obj->v.num);
	break;
      case MTYPE:
	sprintf(buf, "m#%d", obj->v.num);
	break;
      case TTYPE:
	sprintf(buf, "t#%d", obj->v.num);
	break;
      case POINTER:
	sprintlisp(buf, obj->v.ptr.sym);
	sprintf(buf+strlen(buf), " #|0x%lx|#", (long) obj->v.ptr.data);
	break;
      default:
	case_panic("lisp type", obj->type);
	break;
    }
}

void
sprint_list(buf, obj)
char *buf;
Obj *obj;
{
    Obj *tmp;

    buf[0] = '\0';
    for (tmp = obj; tmp != lispnil; tmp = cdr(tmp)) {
	if (strlen(buf) + 10 > BUFSIZE)
	  return;
	strcat(buf, " ");
	sprintlisp(buf+strlen(buf), car(tmp));
    }	
    strcat(buf, ")");
}

#ifdef DEBUGGING
/* For calling from debuggers, at least that those that support output to stderr. */

void
dlisp(x)
Obj *x;
{
    fprintlisp(stderr, x);
    fprintf(stderr, "\n");
}
#endif /* DEBUGGING */

void
print_form_and_value(fp, form)
FILE *fp;
Obj *form;
{
    fprintlisp(fp, form);
    if (symbolp(form)) {
	if (boundp(form)) {
	    fprintf(fp, " -> ");
	    fprintlisp(fp, symbol_value(form));
	} else {
	    fprintf(fp, " <unbound>");
	}
    }
    fprintf(fp, "\n");
}

Obj *
append_two_lists(x1, x2)
Obj *x1, *x2;
{
    if (!listp(x1))
      x1 = cons(x1, lispnil);
    if (!listp(x2))
      x2 = cons(x2, lispnil);
    if (x2 == lispnil) {
	return x1;
    } else if (x1 == lispnil) {
	return x2;
    } else {
    	return cons(car(x1), append_two_lists(cdr(x1), x2));
    }
}

Obj *
append_lists(lis)
Obj *lis;
{
    if (lis == lispnil) {
	return lispnil;
    } else if (!consp(lis)) {
    	return cons(lis, lispnil);
    } else {
    	return append_two_lists(car(lis), append_lists(cdr(lis)));
    }
}

/* Remove all occurrences of a single object from a given list. */

Obj *
remove_from_list(elt, lis)
Obj *elt, *lis;
{
    Obj *tmp;

    if (lis == lispnil) {
	return lispnil;
    } else {
	tmp = remove_from_list(elt, cdr(lis));
	if (equal(elt, car(lis))) {
	    return tmp;
	} else {
	    return cons(car(lis), tmp);
	}
    }
}

void
push_binding(lis, key, val)
Obj **lis, *key, *val;
{
    *lis = cons(cons(key, cons(val, lispnil)), *lis);
}

void
push_cdr_binding(lis, key, val)
Obj **lis, *key, *val;
{
    *lis = cons(cons(key, val), *lis);
}

void
push_int_binding(lis, key, val)
Obj **lis, *key;
int val;
{
    *lis = cons(cons(key, cons(new_number(val), lispnil)), *lis);
}

void
push_key_binding(lis, key, val)
Obj **lis, *val;
int key;
{
    *lis = cons(cons(intern_symbol(keyword_name(key)), cons(val, lispnil)), *lis);
}

void
push_key_cdr_binding(lis, key, val)
Obj **lis, *val;
int key;
{
    *lis = cons(cons(intern_symbol(keyword_name(key)), val), *lis);
}

void
push_key_int_binding(lis, key, val)
Obj **lis;
int key, val;
{
    *lis = cons(cons(intern_symbol(keyword_name(key)), cons(new_number(val), lispnil)),
			    *lis);
}

/* Our version of evaluation derefs symbols and evals through lists,
   unless the list car is a "special form". */

Obj *
eval(x)
Obj *x;
{
    int code;
    Obj *specialform;

    switch (x->type) {
      case SYMBOL:
	return eval_symbol(x);
      case CONS:
	specialform = car(x);
	if (symbolp(specialform)
	    && !boundp(specialform)
	    && (code = keyword_code(c_string(specialform))) >= 0) {
	    switch (code) {
	      case K_QUOTE:
		return cadr(x);
	      case K_LIST:
		return eval_list(cdr(x));
	      case K_APPEND:
		return append_lists(eval_list(cdr(x)));
	      case K_REMOVE:
	      	return remove_from_list(eval(cadr(x)), eval(caddr(x)));
	      default:
		break;
	    }
	}
	/* A dubious default, but convenient. */
	return eval_list(x);
      default:
        /* Everything else evaluates to itself. */
	return x;
    }
}

/* Some symbols are lazily bound, meaning that they don't get a value
   until it is first asked for. */
	    
Obj *
eval_symbol(sym)
Obj *sym;
{
    if (boundp(sym)) {
	return symbol_value(sym);
    } else if (lazy_bind(sym)) {
    	return symbol_value(sym);
    } else {
	run_warning("`%s' is unbound, returning self", c_string(sym));
	/* kind of a hack */
	return sym;
    }
}

/* List evaluation just blasts straight through the list. */

Obj *
eval_list(lis)
Obj *lis;
{
    if (lis == lispnil) {
	return lispnil;
    } else {
	return cons(eval(car(lis)), eval_list(cdr(lis)));
    }
}

int
eval_boolean_expression(expr, fn, dflt)
Obj *expr;
int (*fn) PARAMS ((Obj *)), dflt;
{
    char *opname;

    if (expr == lispnil) {
	return dflt;
    } else if (consp(expr) && symbolp(car(expr))) {
	opname = c_string(car(expr));
	switch (keyword_code(opname)) {
	  case K_AND:
	    return (eval_boolean_expression(cadr(expr), fn, dflt)
		    && eval_boolean_expression(car(cddr(expr)), fn, dflt));
	  case K_OR:
	    return (eval_boolean_expression(cadr(expr), fn, dflt)
		    || eval_boolean_expression(car(cddr(expr)), fn, dflt));
	  case K_NOT:
	    return !eval_boolean_expression(cadr(expr), fn, dflt);
	  default:
	    return (*fn)(expr);
	}
    } else {
	return (*fn)(expr);
    }
}

int
interpolate_in_list(val, lis, rslt)
int val, *rslt;
Obj *lis;
{
    int first, thisin, thisval, nextin, nextval;
    Obj *rest, *head, *next;

    first = TRUE;
    for_all_list(lis, rest) {
	head = car(rest);
	thisin = c_number(car(head));
	thisval = c_number(cadr(head));
	if (cdr(rest) != lispnil) {
	    next = cadr(rest);
	    nextin = c_number(car(next));
	    nextval = c_number(cadr(next));
	    first = FALSE;
	} else if (first) {
	    if (val == thisin) {
		*rslt = thisval;
		return 0;
	    } else if (val < thisin) {
		return (-1);
	    } else {
		return 1;
	    }
	} else {
	    /* We're at the end of a several-item list; the value
	       must be too high. */
	    return 1;
	}
	if (val < thisin) {
	    return (-1);
	} else if (between(thisin, val, nextin)) {
	    if (val == thisin) {
		*rslt = thisval;
	    } else if (val == nextin) {
		*rslt = nextval;
	    } else {
		*rslt = thisval;
		if (val != thisin && nextin != thisin) {
		    /* Add the linear interpolation. */
		    *rslt += ((nextval - thisval) * (val - thisin)) / (nextin - thisin);
		}
	    }
	    return 0;
	}
    }
    return (-1);
}

int
interpolate_in_list_ext(val, lis, mindo, minval, minrslt, maxdo, maxval, maxrslt, rslt)
int val, mindo, minval, minrslt, maxdo, maxval, maxrslt, *rslt;
Obj *lis;
{
    int first, thisin, thisval, nextin, nextval;
    Obj *rest, *head, *next;

    /* (should use the additional parameters) */
    first = TRUE;
    for_all_list(lis, rest) {
	head = car(rest);
	thisin = c_number(car(head));
	thisval = c_number(cadr(head));
	if (cdr(rest) != lispnil) {
	    next = cadr(rest);
	    nextin = c_number(car(next));
	    nextval = c_number(cadr(next));
	    first = FALSE;
	} else if (first) {
	    if (val == thisin) {
		*rslt = thisval;
		return 0;
	    } else if (val < thisin) {
		return (-1);
	    } else {
		return 1;
	    }
	} else {
	    /* We're at the end of a several-item list; the value
	       must be too high. */
	    return 1;
	}
	if (val < thisin) {
	    return (-1);
	} else if (between(thisin, val, nextin)) {
	    if (val == thisin) {
		*rslt = thisval;
	    } else if (val == nextin) {
		*rslt = nextval;
	    } else {
		*rslt = thisval;
		if (val != thisin && nextin != thisin) {
		    /* Add the linear interpolation. */
		    *rslt += ((nextval - thisval) * (val - thisin)) / (nextin - thisin);
		}
	    }
	    return 0;
	}
    }
    return (-1);
}

