I_noop:	/* noop ( -- ) */
/*  */
NAME("noop")
{
DEF_CA
NEXT_P0;
{
#line 107 "./primitives"
;
}
NEXT_P1;
NEXT_P2;
}

I_lit:	/* lit ( -- w ) */
/*  */
NAME("lit")
{
DEF_CA
Cell w;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 112 "./primitives"
w = (Cell)NEXT_INST;
INC_IP(1);
}
NEXT_P1;
TOS = (Cell)w;
NEXT_P2;
}

I_execute:	/* execute ( xt -- ) */
/*  */
NAME("execute")
{
DEF_CA
Xt xt;
NEXT_P0;
xt = (Xt) TOS;
sp += 1;
{
#line 116 "./primitives"
ip=IP;
IF_TOS(TOS = sp[0]);
EXEC(xt);
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_perform:	/* perform ( a_addr -- ) */
/* equivalent to @code{@ execute} */
NAME("perform")
{
DEF_CA
Cell * a_addr;
NEXT_P0;
a_addr = (Cell *) TOS;
sp += 1;
{
#line 122 "./primitives"
/* and pfe */
ip=IP;
IF_TOS(TOS = sp[0]);
EXEC(*(Xt *)a_addr);
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_branch_lp_plus_store_number:	/* branch-lp+!# ( -- ) */
/*  */
NAME("branch-lp+!#")
{
DEF_CA
NEXT_P0;
{
#line 130 "./primitives"
/* this will probably not be used */
branch_adjust_lp:
lp += (Cell)(IP[1]);
goto branch;
}
NEXT_P1;
NEXT_P2;
}

I_branch:	/* branch ( -- ) */
/*  */
NAME("branch")
{
DEF_CA
NEXT_P0;
{
#line 136 "./primitives"
branch:
ip = (Xt *)(((Cell)IP)+(Cell)NEXT_INST);
NEXT_P0;
}
NEXT_P1;
NEXT_P2;
}

I_question_branch:	/* ?branch ( f -- ) */
/*  */
NAME("?branch")
{
DEF_CA
Bool f;
NEXT_P0;
f = (Bool) TOS;
sp += 1;
{
#line 164 "./primitives"
if (f==0) {
#line 164
    IF_TOS(TOS = sp[0]);
#line 164
	ip = (Xt *)(((Cell)IP)+(Cell)NEXT_INST);
#line 164
        NEXT_P0;
#line 164
	NEXT;
#line 164
}
#line 164
else
#line 164
    INC_IP(1);
#line 164
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_question_branch_lp_plus_store_number:	/* ?branch-lp+!# ( f -- ) */
/*  */
NAME("?branch-lp+!#")
{
DEF_CA
Bool f;
NEXT_P0;
f = (Bool) TOS;
sp += 1;
{
#line 164 "./primitives"
if (f==0) {
#line 164
    IF_TOS(TOS = sp[0]);
#line 164
    goto branch_adjust_lp;
#line 164
}
#line 164
else
#line 164
    INC_IP(2);
#line 164
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_question_dupe_question_branch:	/* ?dup-?branch ( f -- f ) */
/* The run-time procedure compiled by @code{?DUP-IF}. */
NAME("?dup-?branch")
{
DEF_CA
Bool f;
NEXT_P0;
f = (Bool) TOS;
{
#line 171 "./primitives"
if (f==0) {
  sp++;
  IF_TOS(TOS = sp[0]);
  ip = (Xt *)(((Cell)IP)+(Cell)NEXT_INST);
  NEXT_P0;
  NEXT;
}
else
  INC_IP(1);
}
NEXT_P1;
IF_TOS(TOS = (Cell)f;);
NEXT_P2;
}

I_question_dupe_zero_equals_question_branch:	/* ?dup-0=-?branch ( f -- ) */
/* The run-time procedure compiled by @code{?DUP-0=-IF}. */
NAME("?dup-0=-?branch")
{
DEF_CA
Bool f;
NEXT_P0;
f = (Bool) TOS;
sp += 1;
{
#line 183 "./primitives"
/* the approach taken here of declaring the word as having the stack
effect ( f -- ) and correcting for it in the branch-taken case costs a
few cycles in that case, but is easy to convert to a CONDBRANCH
invocation */
if (f!=0) {
  sp--;
  ip = (Xt *)(((Cell)IP)+(Cell)NEXT_INST);
  NEXT_P0;
  NEXT;
}
else
  INC_IP(1);
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_paren_next:	/* (next) ( -- ) */
/*  */
NAME("(next)")
{
DEF_CA
NEXT_P0;
{
#line 198 "./primitives"
if ((*rp)--) {
#line 198
	ip = (Xt *)(((Cell)IP)+(Cell)NEXT_INST);
#line 198
        NEXT_P0;
#line 198
	NEXT;
#line 198
}
#line 198
else
#line 198
    INC_IP(1);
#line 198
}
NEXT_P1;
NEXT_P2;
}

I_paren_next_lp_plus_store_number:	/* (next)-lp+!# ( -- ) */
/*  */
NAME("(next)-lp+!#")
{
DEF_CA
NEXT_P0;
{
#line 198 "./primitives"
if ((*rp)--) {
#line 198
    goto branch_adjust_lp;
#line 198
}
#line 198
else
#line 198
    INC_IP(2);
#line 198
}
NEXT_P1;
NEXT_P2;
}

I_paren_loop:	/* (loop) ( -- ) */
/*  */
NAME("(loop)")
{
DEF_CA
NEXT_P0;
{
#line 205 "./primitives"
Cell index = *rp+1;
#line 205
Cell limit = rp[1];
#line 205
if (index != limit) {
#line 205
    *rp = index;
#line 205
	ip = (Xt *)(((Cell)IP)+(Cell)NEXT_INST);
#line 205
        NEXT_P0;
#line 205
	NEXT;
#line 205
}
#line 205
else
#line 205
    INC_IP(1);
#line 205
}
NEXT_P1;
NEXT_P2;
}

I_paren_loop_lp_plus_store_number:	/* (loop)-lp+!# ( -- ) */
/*  */
NAME("(loop)-lp+!#")
{
DEF_CA
NEXT_P0;
{
#line 205 "./primitives"
Cell index = *rp+1;
#line 205
Cell limit = rp[1];
#line 205
if (index != limit) {
#line 205
    *rp = index;
#line 205
    goto branch_adjust_lp;
#line 205
}
#line 205
else
#line 205
    INC_IP(2);
#line 205
}
NEXT_P1;
NEXT_P2;
}

I_paren_plus_loop:	/* (+loop) ( n -- ) */
/*  */
NAME("(+loop)")
{
DEF_CA
Cell n;
NEXT_P0;
n = (Cell) TOS;
sp += 1;
{
#line 228 "./primitives"
/* !! check this thoroughly */
#line 228
Cell index = *rp;
#line 228
/* sign bit manipulation and test: (x^y)<0 is equivalent to (x<0) != (y<0) */
#line 228
/* dependent upon two's complement arithmetic */
#line 228
Cell olddiff = index-rp[1];
#line 228
#ifndef undefined
#line 228
if ((olddiff^(olddiff+n))>=0   /* the limit is not crossed */
#line 228
    || (olddiff^n)>=0          /* it is a wrap-around effect */) {
#line 228
#else
#line 228
#ifndef MAXINT
#line 228
#define MAXINT ((((Cell)1)<<(8*sizeof(Cell)-1))-1)
#line 228
#endif
#line 228
if(((olddiff^MAXINT) >= n) ^ ((olddiff+n) < 0)) {
#line 228
#endif
#line 228
#ifdef i386
#line 228
    *rp += n;
#line 228
#else
#line 228
    *rp = index + n;
#line 228
#endif
#line 228
    IF_TOS(TOS = sp[0]);
#line 228
	ip = (Xt *)(((Cell)IP)+(Cell)NEXT_INST);
#line 228
        NEXT_P0;
#line 228
	NEXT;
#line 228
}
#line 228
else
#line 228
    INC_IP(1);
#line 228
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_paren_plus_loop_lp_plus_store_number:	/* (+loop)-lp+!# ( n -- ) */
/*  */
NAME("(+loop)-lp+!#")
{
DEF_CA
Cell n;
NEXT_P0;
n = (Cell) TOS;
sp += 1;
{
#line 228 "./primitives"
/* !! check this thoroughly */
#line 228
Cell index = *rp;
#line 228
/* sign bit manipulation and test: (x^y)<0 is equivalent to (x<0) != (y<0) */
#line 228
/* dependent upon two's complement arithmetic */
#line 228
Cell olddiff = index-rp[1];
#line 228
#ifndef undefined
#line 228
if ((olddiff^(olddiff+n))>=0   /* the limit is not crossed */
#line 228
    || (olddiff^n)>=0          /* it is a wrap-around effect */) {
#line 228
#else
#line 228
#ifndef MAXINT
#line 228
#define MAXINT ((((Cell)1)<<(8*sizeof(Cell)-1))-1)
#line 228
#endif
#line 228
if(((olddiff^MAXINT) >= n) ^ ((olddiff+n) < 0)) {
#line 228
#endif
#line 228
#ifdef i386
#line 228
    *rp += n;
#line 228
#else
#line 228
    *rp = index + n;
#line 228
#endif
#line 228
    IF_TOS(TOS = sp[0]);
#line 228
    goto branch_adjust_lp;
#line 228
}
#line 228
else
#line 228
    INC_IP(2);
#line 228
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_paren_minus_loop:	/* (-loop) ( u -- ) */
/*  */
NAME("(-loop)")
{
DEF_CA
UCell u;
NEXT_P0;
u = (UCell) TOS;
sp += 1;
{
#line 241 "./primitives"
/* !! check this thoroughly */
#line 241
Cell index = *rp;
#line 241
UCell olddiff = index-rp[1];
#line 241
if (olddiff>u) {
#line 241
#ifdef i386
#line 241
    *rp -= u;
#line 241
#else
#line 241
    *rp = index - u;
#line 241
#endif
#line 241
    IF_TOS(TOS = sp[0]);
#line 241
	ip = (Xt *)(((Cell)IP)+(Cell)NEXT_INST);
#line 241
        NEXT_P0;
#line 241
	NEXT;
#line 241
}
#line 241
else
#line 241
    INC_IP(1);
#line 241
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_paren_minus_loop_lp_plus_store_number:	/* (-loop)-lp+!# ( u -- ) */
/*  */
NAME("(-loop)-lp+!#")
{
DEF_CA
UCell u;
NEXT_P0;
u = (UCell) TOS;
sp += 1;
{
#line 241 "./primitives"
/* !! check this thoroughly */
#line 241
Cell index = *rp;
#line 241
UCell olddiff = index-rp[1];
#line 241
if (olddiff>u) {
#line 241
#ifdef i386
#line 241
    *rp -= u;
#line 241
#else
#line 241
    *rp = index - u;
#line 241
#endif
#line 241
    IF_TOS(TOS = sp[0]);
#line 241
    goto branch_adjust_lp;
#line 241
}
#line 241
else
#line 241
    INC_IP(2);
#line 241
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_paren_symmetric_plus_loop:	/* (s+loop) ( n -- ) */
/* The run-time procedure compiled by S+LOOP. It loops until the index
crosses the boundary between limit and limit-sign(n). I.e. a symmetric
version of (+LOOP). */
NAME("(s+loop)")
{
DEF_CA
Cell n;
NEXT_P0;
n = (Cell) TOS;
sp += 1;
{
#line 262 "./primitives"
/* !! check this thoroughly */
#line 262
Cell index = *rp;
#line 262
Cell diff = index-rp[1];
#line 262
Cell newdiff = diff+n;
#line 262
if (n<0) {
#line 262
    diff = -diff;
#line 262
    newdiff = -newdiff;
#line 262
}
#line 262
if (diff>=0 || newdiff<0) {
#line 262
#ifdef i386
#line 262
    *rp += n;
#line 262
#else
#line 262
    *rp = index + n;
#line 262
#endif
#line 262
    IF_TOS(TOS = sp[0]);
#line 262
	ip = (Xt *)(((Cell)IP)+(Cell)NEXT_INST);
#line 262
        NEXT_P0;
#line 262
	NEXT;
#line 262
}
#line 262
else
#line 262
    INC_IP(1);
#line 262
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_paren_symmetric_plus_loop_lp_plus_store_number:	/* (s+loop)-lp+!# ( n -- ) */
/* The run-time procedure compiled by S+LOOP. It loops until the index
crosses the boundary between limit and limit-sign(n). I.e. a symmetric
version of (+LOOP). */
NAME("(s+loop)-lp+!#")
{
DEF_CA
Cell n;
NEXT_P0;
n = (Cell) TOS;
sp += 1;
{
#line 262 "./primitives"
/* !! check this thoroughly */
#line 262
Cell index = *rp;
#line 262
Cell diff = index-rp[1];
#line 262
Cell newdiff = diff+n;
#line 262
if (n<0) {
#line 262
    diff = -diff;
#line 262
    newdiff = -newdiff;
#line 262
}
#line 262
if (diff>=0 || newdiff<0) {
#line 262
#ifdef i386
#line 262
    *rp += n;
#line 262
#else
#line 262
    *rp = index + n;
#line 262
#endif
#line 262
    IF_TOS(TOS = sp[0]);
#line 262
    goto branch_adjust_lp;
#line 262
}
#line 262
else
#line 262
    INC_IP(2);
#line 262
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_unloop:	/* unloop ( -- ) */
/*  */
NAME("unloop")
{
DEF_CA
NEXT_P0;
{
#line 265 "./primitives"
rp += 2;
}
NEXT_P1;
NEXT_P2;
}

I_paren_for:	/* (for) ( ncount -- ) */
/*  */
NAME("(for)")
{
DEF_CA
Cell ncount;
NEXT_P0;
ncount = (Cell) TOS;
sp += 1;
{
#line 270 "./primitives"
/* or (for) = >r -- collides with unloop! */
*--rp = 0;
*--rp = ncount;
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_paren_do:	/* (do) ( nlimit nstart -- ) */
/*  */
NAME("(do)")
{
DEF_CA
Cell nlimit;
Cell nstart;
NEXT_P0;
nlimit = (Cell) sp[1];
nstart = (Cell) TOS;
sp += 2;
{
#line 277 "./primitives"
/* or do it in high-level? 0.09/0.23% */
*--rp = nlimit;
*--rp = nstart;
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_paren_question_do:	/* (?do) ( nlimit nstart -- ) */
/*  */
NAME("(?do)")
{
DEF_CA
Cell nlimit;
Cell nstart;
NEXT_P0;
nlimit = (Cell) sp[1];
nstart = (Cell) TOS;
sp += 2;
{
#line 284 "./primitives"
*--rp = nlimit;
*--rp = nstart;
if (nstart == nlimit) {
    IF_TOS(TOS = sp[0]);
    goto branch;
    }
else {
    INC_IP(1);
}
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_paren_plus_do:	/* (+do) ( nlimit nstart -- ) */
/*  */
NAME("(+do)")
{
DEF_CA
Cell nlimit;
Cell nstart;
NEXT_P0;
nlimit = (Cell) sp[1];
nstart = (Cell) TOS;
sp += 2;
{
#line 295 "./primitives"
*--rp = nlimit;
*--rp = nstart;
if (nstart >= nlimit) {
    IF_TOS(TOS = sp[0]);
    goto branch;
    }
else {
    INC_IP(1);
}
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_paren_u_plus_do:	/* (u+do) ( ulimit ustart -- ) */
/*  */
NAME("(u+do)")
{
DEF_CA
UCell ulimit;
UCell ustart;
NEXT_P0;
ulimit = (UCell) sp[1];
ustart = (UCell) TOS;
sp += 2;
{
#line 306 "./primitives"
*--rp = ulimit;
*--rp = ustart;
if (ustart >= ulimit) {
    IF_TOS(TOS = sp[0]);
    goto branch;
    }
else {
    INC_IP(1);
}
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_paren_minus_do:	/* (-do) ( nlimit nstart -- ) */
/*  */
NAME("(-do)")
{
DEF_CA
Cell nlimit;
Cell nstart;
NEXT_P0;
nlimit = (Cell) sp[1];
nstart = (Cell) TOS;
sp += 2;
{
#line 317 "./primitives"
*--rp = nlimit;
*--rp = nstart;
if (nstart <= nlimit) {
    IF_TOS(TOS = sp[0]);
    goto branch;
    }
else {
    INC_IP(1);
}
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_paren_u_minus_do:	/* (u-do) ( ulimit ustart -- ) */
/*  */
NAME("(u-do)")
{
DEF_CA
UCell ulimit;
UCell ustart;
NEXT_P0;
ulimit = (UCell) sp[1];
ustart = (UCell) TOS;
sp += 2;
{
#line 328 "./primitives"
*--rp = ulimit;
*--rp = ustart;
if (ustart <= ulimit) {
    IF_TOS(TOS = sp[0]);
    goto branch;
    }
else {
    INC_IP(1);
}
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_i:	/* i ( -- n ) */
/*  */
NAME("i")
{
DEF_CA
Cell n;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 339 "./primitives"
n = *rp;
}
NEXT_P1;
TOS = (Cell)n;
NEXT_P2;
}

I_j:	/* j ( -- n ) */
/*  */
NAME("j")
{
DEF_CA
Cell n;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 342 "./primitives"
n = rp[2];
}
NEXT_P1;
TOS = (Cell)n;
NEXT_P2;
}

I_paren_key:	/* (key) ( -- n ) */
/*  */
NAME("(key)")
{
DEF_CA
Cell n;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 347 "./primitives"
fflush(stdout);
/* !! noecho */
n = key();
}
NEXT_P1;
TOS = (Cell)n;
NEXT_P2;
}

I_key_q:	/* key? ( -- n ) */
/*  */
NAME("key?")
{
DEF_CA
Cell n;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 352 "./primitives"
fflush(stdout);
n = key_query;
}
NEXT_P1;
TOS = (Cell)n;
NEXT_P2;
}

I_form:	/* form ( -- urows ucols ) */
/* The number of lines and columns in the terminal. These numbers may change
with the window size. */
NAME("form")
{
DEF_CA
UCell urows;
UCell ucols;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -2;
{
#line 358 "./primitives"
/* we could block SIGWINCH here to get a consistent size, but I don't
 think this is necessary or always beneficial */
urows=rows;
ucols=cols;
}
NEXT_P1;
sp[1] = (Cell)urows;
TOS = (Cell)ucols;
NEXT_P2;
}

I_move:	/* move ( c_from c_to ucount -- ) */
/*  */
NAME("move")
{
DEF_CA
Char * c_from;
Char * c_to;
UCell ucount;
NEXT_P0;
c_from = (Char *) sp[2];
c_to = (Char *) sp[1];
ucount = (UCell) TOS;
sp += 3;
{
#line 364 "./primitives"
memmove(c_to,c_from,ucount);
/* make an Ifdef for bsd and others? */
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_cmove:	/* cmove ( c_from c_to u -- ) */
/*  */
NAME("cmove")
{
DEF_CA
Char * c_from;
Char * c_to;
UCell u;
NEXT_P0;
c_from = (Char *) sp[2];
c_to = (Char *) sp[1];
u = (UCell) TOS;
sp += 3;
{
#line 370 "./primitives"
while (u-- > 0)
  *c_to++ = *c_from++;
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_c_move_up:	/* cmove> ( c_from c_to u -- ) */
/*  */
NAME("cmove>")
{
DEF_CA
Char * c_from;
Char * c_to;
UCell u;
NEXT_P0;
c_from = (Char *) sp[2];
c_to = (Char *) sp[1];
u = (UCell) TOS;
sp += 3;
{
#line 376 "./primitives"
while (u-- > 0)
  c_to[u] = c_from[u];
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_fill:	/* fill ( c_addr u c -- ) */
/*  */
NAME("fill")
{
DEF_CA
Char * c_addr;
UCell u;
Char c;
NEXT_P0;
c_addr = (Char *) sp[2];
u = (UCell) sp[1];
c = (Char) TOS;
sp += 3;
{
#line 384 "./primitives"
memset(c_addr,c,u);
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_compare:	/* compare ( c_addr1 u1 c_addr2 u2 -- n ) */
/* Compare the strings lexicographically. If they are equal, n is 0; if
the first string is smaller, n is -1; if the first string is larger, n
is 1. Currently this is based on the machine's character
comparison. In the future, this may change to considering the current
locale and its collation order. */
NAME("compare")
{
DEF_CA
Char * c_addr1;
UCell u1;
Char * c_addr2;
UCell u2;
Cell n;
NEXT_P0;
c_addr1 = (Char *) sp[3];
u1 = (UCell) sp[2];
c_addr2 = (Char *) sp[1];
u2 = (UCell) TOS;
sp += 3;
{
#line 395 "./primitives"
n = memcmp(c_addr1, c_addr2, u1<u2 ? u1 : u2);
if (n==0)
  n = u1-u2;
if (n<0)
  n = -1;
else if (n>0)
  n = 1;
}
NEXT_P1;
TOS = (Cell)n;
NEXT_P2;
}

I_dash_text:	/* -text ( c_addr1 u c_addr2 -- n ) */
/*  */
NAME("-text")
{
DEF_CA
Char * c_addr1;
UCell u;
Char * c_addr2;
Cell n;
NEXT_P0;
c_addr1 = (Char *) sp[2];
u = (UCell) sp[1];
c_addr2 = (Char *) TOS;
sp += 2;
{
#line 412 "./primitives"
n = memcmp(c_addr1, c_addr2, u);
if (n<0)
  n = -1;
else if (n>0)
  n = 1;
}
NEXT_P1;
TOS = (Cell)n;
NEXT_P2;
}

I_capscomp:	/* capscomp ( c_addr1 u c_addr2 -- n ) */
/*  */
NAME("capscomp")
{
DEF_CA
Char * c_addr1;
UCell u;
Char * c_addr2;
Cell n;
NEXT_P0;
c_addr1 = (Char *) sp[2];
u = (UCell) sp[1];
c_addr2 = (Char *) TOS;
sp += 2;
{
#line 425 "./primitives"
n = memcasecmp(c_addr1, c_addr2, u); /* !! use something that works in all locales */
if (n<0)
  n = -1;
else if (n>0)
  n = 1;
}
NEXT_P1;
TOS = (Cell)n;
NEXT_P2;
}

I_dash_trailing:	/* -trailing ( c_addr u1 -- c_addr u2 ) */
/*  */
NAME("-trailing")
{
DEF_CA
Char * c_addr;
UCell u1;
UCell u2;
NEXT_P0;
c_addr = (Char *) sp[1];
u1 = (UCell) TOS;
{
#line 436 "./primitives"
u2 = u1;
while (c_addr[u2-1] == ' ')
  u2--;
}
NEXT_P1;
TOS = (Cell)u2;
NEXT_P2;
}

I_slash_string:	/* /string ( c_addr1 u1 n -- c_addr2 u2 ) */
/*  */
NAME("/string")
{
DEF_CA
Char * c_addr1;
UCell u1;
Cell n;
Char * c_addr2;
UCell u2;
NEXT_P0;
c_addr1 = (Char *) sp[2];
u1 = (UCell) sp[1];
n = (Cell) TOS;
sp += 1;
{
#line 444 "./primitives"
c_addr2 = c_addr1+n;
u2 = u1-n;
}
NEXT_P1;
sp[1] = (Cell)c_addr2;
TOS = (Cell)u2;
NEXT_P2;
}

I_plus:	/* + ( n1 n2 -- n ) */
/*  */
NAME("+")
{
DEF_CA
Cell n1;
Cell n2;
Cell n;
NEXT_P0;
n1 = (Cell) sp[1];
n2 = (Cell) TOS;
sp += 1;
{
#line 450 "./primitives"
n = n1+n2;
}
NEXT_P1;
TOS = (Cell)n;
NEXT_P2;
}

I_minus:	/* - ( n1 n2 -- n ) */
/*  */
NAME("-")
{
DEF_CA
Cell n1;
Cell n2;
Cell n;
NEXT_P0;
n1 = (Cell) sp[1];
n2 = (Cell) TOS;
sp += 1;
{
#line 461 "./primitives"
n = n1-n2;
}
NEXT_P1;
TOS = (Cell)n;
NEXT_P2;
}

I_negate:	/* negate ( n1 -- n2 ) */
/*  */
NAME("negate")
{
DEF_CA
Cell n1;
Cell n2;
NEXT_P0;
n1 = (Cell) TOS;
{
#line 466 "./primitives"
/* use minus as alias */
n2 = -n1;
}
NEXT_P1;
TOS = (Cell)n2;
NEXT_P2;
}

I_one_plus:	/* 1+ ( n1 -- n2 ) */
/*  */
NAME("1+")
{
DEF_CA
Cell n1;
Cell n2;
NEXT_P0;
n1 = (Cell) TOS;
{
#line 472 "./primitives"
n2 = n1+1;
}
NEXT_P1;
TOS = (Cell)n2;
NEXT_P2;
}

I_one_minus:	/* 1- ( n1 -- n2 ) */
/*  */
NAME("1-")
{
DEF_CA
Cell n1;
Cell n2;
NEXT_P0;
n1 = (Cell) TOS;
{
#line 477 "./primitives"
n2 = n1-1;
}
NEXT_P1;
TOS = (Cell)n2;
NEXT_P2;
}

I_max:	/* max ( n1 n2 -- n ) */
/*  */
NAME("max")
{
DEF_CA
Cell n1;
Cell n2;
Cell n;
NEXT_P0;
n1 = (Cell) sp[1];
n2 = (Cell) TOS;
sp += 1;
{
#line 482 "./primitives"
if (n1<n2)
  n = n2;
else
  n = n1;
}
NEXT_P1;
TOS = (Cell)n;
NEXT_P2;
}

I_min:	/* min ( n1 n2 -- n ) */
/*  */
NAME("min")
{
DEF_CA
Cell n1;
Cell n2;
Cell n;
NEXT_P0;
n1 = (Cell) sp[1];
n2 = (Cell) TOS;
sp += 1;
{
#line 490 "./primitives"
if (n1<n2)
  n = n1;
else
  n = n2;
}
NEXT_P1;
TOS = (Cell)n;
NEXT_P2;
}

I_abs:	/* abs ( n1 -- n2 ) */
/*  */
NAME("abs")
{
DEF_CA
Cell n1;
Cell n2;
NEXT_P0;
n1 = (Cell) TOS;
{
#line 498 "./primitives"
if (n1<0)
  n2 = -n1;
else
  n2 = n1;
}
NEXT_P1;
TOS = (Cell)n2;
NEXT_P2;
}

I_star:	/* * ( n1 n2 -- n ) */
/*  */
NAME("*")
{
DEF_CA
Cell n1;
Cell n2;
Cell n;
NEXT_P0;
n1 = (Cell) sp[1];
n2 = (Cell) TOS;
sp += 1;
{
#line 506 "./primitives"
n = n1*n2;
}
NEXT_P1;
TOS = (Cell)n;
NEXT_P2;
}

I_slash:	/* / ( n1 n2 -- n ) */
/*  */
NAME("/")
{
DEF_CA
Cell n1;
Cell n2;
Cell n;
NEXT_P0;
n1 = (Cell) sp[1];
n2 = (Cell) TOS;
sp += 1;
{
#line 511 "./primitives"
n = n1/n2;
}
NEXT_P1;
TOS = (Cell)n;
NEXT_P2;
}

I_mod:	/* mod ( n1 n2 -- n ) */
/*  */
NAME("mod")
{
DEF_CA
Cell n1;
Cell n2;
Cell n;
NEXT_P0;
n1 = (Cell) sp[1];
n2 = (Cell) TOS;
sp += 1;
{
#line 516 "./primitives"
n = n1%n2;
}
NEXT_P1;
TOS = (Cell)n;
NEXT_P2;
}

I_slash_mod:	/* /mod ( n1 n2 -- n3 n4 ) */
/*  */
NAME("/mod")
{
DEF_CA
Cell n1;
Cell n2;
Cell n3;
Cell n4;
NEXT_P0;
n1 = (Cell) sp[1];
n2 = (Cell) TOS;
{
#line 521 "./primitives"
n4 = n1/n2;
n3 = n1%n2; /* !! is this correct? look into C standard! */
}
NEXT_P1;
sp[1] = (Cell)n3;
TOS = (Cell)n4;
NEXT_P2;
}

I_two_star:	/* 2* ( n1 -- n2 ) */
/*  */
NAME("2*")
{
DEF_CA
Cell n1;
Cell n2;
NEXT_P0;
n1 = (Cell) TOS;
{
#line 527 "./primitives"
n2 = 2*n1;
}
NEXT_P1;
TOS = (Cell)n2;
NEXT_P2;
}

I_two_slash:	/* 2/ ( n1 -- n2 ) */
/*  */
NAME("2/")
{
DEF_CA
Cell n1;
Cell n2;
NEXT_P0;
n1 = (Cell) TOS;
{
#line 532 "./primitives"
/* !! is this still correct? */
n2 = n1>>1;
}
NEXT_P1;
TOS = (Cell)n2;
NEXT_P2;
}

I_f_m_slash_mod:	/* fm/mod ( d1 n1 -- n2 n3 ) */
/* floored division: d1 = n3*n1+n2, n1>n2>=0 or 0>=n2>n1 */
NAME("fm/mod")
{
DEF_CA
DCell d1;
Cell n1;
Cell n2;
Cell n3;
NEXT_P0;
FETCH_DCELL(d1, sp[2], sp[1]);
n1 = (Cell) TOS;
sp += 1;
{
#line 537 "./primitives"
#ifdef BUGGY_LONG_LONG
DCell r = fmdiv(d1,n1);
n2=r.hi;
n3=r.lo;
#else
/* assumes that the processor uses either floored or symmetric division */
n3 = d1/n1;
n2 = d1%n1;
/* note that this 1%-3>0 is optimized by the compiler */
if (1%-3>0 && (d1<0) != (n1<0) && n2!=0) {
  n3--;
  n2+=n1;
}
#endif
}
NEXT_P1;
sp[1] = (Cell)n2;
TOS = (Cell)n3;
NEXT_P2;
}

I_s_m_slash_rem:	/* sm/rem ( d1 n1 -- n2 n3 ) */
/* symmetric division: d1 = n3*n1+n2, sign(n2)=sign(d1) or 0 */
NAME("sm/rem")
{
DEF_CA
DCell d1;
Cell n1;
Cell n2;
Cell n3;
NEXT_P0;
FETCH_DCELL(d1, sp[2], sp[1]);
n1 = (Cell) TOS;
sp += 1;
{
#line 559 "./primitives"
#ifdef BUGGY_LONG_LONG
DCell r = smdiv(d1,n1);
n2=r.hi;
n3=r.lo;
#else
/* assumes that the processor uses either floored or symmetric division */
n3 = d1/n1;
n2 = d1%n1;
/* note that this 1%-3<0 is optimized by the compiler */
if (1%-3<0 && (d1<0) != (n1<0) && n2!=0) {
  n3++;
  n2-=n1;
}
#endif
}
NEXT_P1;
sp[1] = (Cell)n2;
TOS = (Cell)n3;
NEXT_P2;
}

I_m_star:	/* m* ( n1 n2 -- d ) */
/*  */
NAME("m*")
{
DEF_CA
Cell n1;
Cell n2;
DCell d;
NEXT_P0;
n1 = (Cell) sp[1];
n2 = (Cell) TOS;
{
#line 580 "./primitives"
#ifdef BUGGY_LONG_LONG
d = mmul(n1,n2);
#else
d = (DCell)n1 * (DCell)n2;
#endif
}
NEXT_P1;
STORE_DCELL(d, sp[1], TOS);
NEXT_P2;
}

I_u_m_star:	/* um* ( u1 u2 -- ud ) */
/*  */
NAME("um*")
{
DEF_CA
UCell u1;
UCell u2;
UDCell ud;
NEXT_P0;
u1 = (UCell) sp[1];
u2 = (UCell) TOS;
{
#line 591 "./primitives"
/* use u* as alias */
#ifdef BUGGY_LONG_LONG
ud = ummul(u1,u2);
#else
ud = (UDCell)u1 * (UDCell)u2;
#endif
}
NEXT_P1;
STORE_DCELL(ud, sp[1], TOS);
NEXT_P2;
}

I_u_m_slash_mod:	/* um/mod ( ud u1 -- u2 u3 ) */
/*  */
NAME("um/mod")
{
DEF_CA
UDCell ud;
UCell u1;
UCell u2;
UCell u3;
NEXT_P0;
FETCH_DCELL(ud, sp[2], sp[1]);
u1 = (UCell) TOS;
sp += 1;
{
#line 599 "./primitives"
#ifdef BUGGY_LONG_LONG
UDCell r = umdiv(ud,u1);
u2=r.hi;
u3=r.lo;
#else
u3 = ud/u1;
u2 = ud%u1;
#endif
}
NEXT_P1;
sp[1] = (Cell)u2;
TOS = (Cell)u3;
NEXT_P2;
}

I_m_plus:	/* m+ ( d1 n -- d2 ) */
/*  */
NAME("m+")
{
DEF_CA
DCell d1;
Cell n;
DCell d2;
NEXT_P0;
FETCH_DCELL(d1, sp[2], sp[1]);
n = (Cell) TOS;
sp += 1;
{
#line 618 "./primitives"
#ifdef BUGGY_LONG_LONG
d2.lo = d1.lo+n;
d2.hi = d1.hi - (n<0) + (d2.lo<d1.lo);
#else
d2 = d1+n;
#endif
}
NEXT_P1;
STORE_DCELL(d2, sp[1], TOS);
NEXT_P2;
}

I_d_plus:	/* d+ ( d1 d2 -- d ) */
/*  */
NAME("d+")
{
DEF_CA
DCell d1;
DCell d2;
DCell d;
NEXT_P0;
FETCH_DCELL(d1, sp[3], sp[2]);
FETCH_DCELL(d2, sp[1], TOS);
sp += 2;
{
#line 628 "./primitives"
#ifdef BUGGY_LONG_LONG
d.lo = d1.lo+d2.lo;
d.hi = d1.hi + d2.hi + (d.lo<d1.lo);
#else
d = d1+d2;
#endif
}
NEXT_P1;
STORE_DCELL(d, sp[1], TOS);
NEXT_P2;
}

I_d_minus:	/* d- ( d1 d2 -- d ) */
/*  */
NAME("d-")
{
DEF_CA
DCell d1;
DCell d2;
DCell d;
NEXT_P0;
FETCH_DCELL(d1, sp[3], sp[2]);
FETCH_DCELL(d2, sp[1], TOS);
sp += 2;
{
#line 638 "./primitives"
#ifdef BUGGY_LONG_LONG
d.lo = d1.lo - d2.lo;
d.hi = d1.hi-d2.hi-(d1.lo<d2.lo);
#else
d = d1-d2;
#endif
}
NEXT_P1;
STORE_DCELL(d, sp[1], TOS);
NEXT_P2;
}

I_dnegate:	/* dnegate ( d1 -- d2 ) */
/*  */
NAME("dnegate")
{
DEF_CA
DCell d1;
DCell d2;
NEXT_P0;
FETCH_DCELL(d1, sp[1], TOS);
{
#line 648 "./primitives"
/* use dminus as alias */
#ifdef BUGGY_LONG_LONG
d2 = dnegate(d1);
#else
d2 = -d1;
#endif
}
NEXT_P1;
STORE_DCELL(d2, sp[1], TOS);
NEXT_P2;
}

I_d_two_star:	/* d2* ( d1 -- d2 ) */
/*  */
NAME("d2*")
{
DEF_CA
DCell d1;
DCell d2;
NEXT_P0;
FETCH_DCELL(d1, sp[1], TOS);
{
#line 658 "./primitives"
#ifdef BUGGY_LONG_LONG
d2.lo = d1.lo<<1;
d2.hi = (d1.hi<<1) | (d1.lo>>(CELL_BITS-1));
#else
d2 = 2*d1;
#endif
}
NEXT_P1;
STORE_DCELL(d2, sp[1], TOS);
NEXT_P2;
}

I_d_two_slash:	/* d2/ ( d1 -- d2 ) */
/*  */
NAME("d2/")
{
DEF_CA
DCell d1;
DCell d2;
NEXT_P0;
FETCH_DCELL(d1, sp[1], TOS);
{
#line 668 "./primitives"
#ifdef BUGGY_LONG_LONG
d2.hi = d1.hi>>1;
d2.lo= (d1.lo>>1) | (d1.hi<<(CELL_BITS-1));
#else
d2 = d1>>1;
#endif
}
NEXT_P1;
STORE_DCELL(d2, sp[1], TOS);
NEXT_P2;
}

I_and:	/* and ( w1 w2 -- w ) */
/*  */
NAME("and")
{
DEF_CA
Cell w1;
Cell w2;
Cell w;
NEXT_P0;
w1 = (Cell) sp[1];
w2 = (Cell) TOS;
sp += 1;
{
#line 679 "./primitives"
w = w1&w2;
}
NEXT_P1;
TOS = (Cell)w;
NEXT_P2;
}

I_or:	/* or ( w1 w2 -- w ) */
/*  */
NAME("or")
{
DEF_CA
Cell w1;
Cell w2;
Cell w;
NEXT_P0;
w1 = (Cell) sp[1];
w2 = (Cell) TOS;
sp += 1;
{
#line 682 "./primitives"
w = w1|w2;
}
NEXT_P1;
TOS = (Cell)w;
NEXT_P2;
}

I_xor:	/* xor ( w1 w2 -- w ) */
/*  */
NAME("xor")
{
DEF_CA
Cell w1;
Cell w2;
Cell w;
NEXT_P0;
w1 = (Cell) sp[1];
w2 = (Cell) TOS;
sp += 1;
{
#line 685 "./primitives"
w = w1^w2;
}
NEXT_P1;
TOS = (Cell)w;
NEXT_P2;
}

I_invert:	/* invert ( w1 -- w2 ) */
/*  */
NAME("invert")
{
DEF_CA
Cell w1;
Cell w2;
NEXT_P0;
w1 = (Cell) TOS;
{
#line 688 "./primitives"
w2 = ~w1;
}
NEXT_P1;
TOS = (Cell)w2;
NEXT_P2;
}

I_rshift:	/* rshift ( u1 n -- u2 ) */
/*  */
NAME("rshift")
{
DEF_CA
UCell u1;
Cell n;
UCell u2;
NEXT_P0;
u1 = (UCell) sp[1];
n = (Cell) TOS;
sp += 1;
{
#line 693 "./primitives"
  u2 = u1>>n;
}
NEXT_P1;
TOS = (Cell)u2;
NEXT_P2;
}

I_lshift:	/* lshift ( u1 n -- u2 ) */
/*  */
NAME("lshift")
{
DEF_CA
UCell u1;
Cell n;
UCell u2;
NEXT_P0;
u1 = (UCell) sp[1];
n = (Cell) TOS;
sp += 1;
{
#line 696 "./primitives"
  u2 = u1<<n;
}
NEXT_P1;
TOS = (Cell)u2;
NEXT_P2;
}

I_zero_equals:	/* 0= ( n -- f ) */
/*  */
NAME("0=")
{
DEF_CA
Cell n;
Bool f;
NEXT_P0;
n = (Cell) TOS;
{
#line 720 "./primitives"
f = FLAG(n==0);
#line 720
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_zero_different:	/* 0<> ( n -- f ) */
/*  */
NAME("0<>")
{
DEF_CA
Cell n;
Bool f;
NEXT_P0;
n = (Cell) TOS;
{
#line 720 "./primitives"
f = FLAG(n!=0);
#line 720
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_zero_less:	/* 0< ( n -- f ) */
/*  */
NAME("0<")
{
DEF_CA
Cell n;
Bool f;
NEXT_P0;
n = (Cell) TOS;
{
#line 720 "./primitives"
f = FLAG(n<0);
#line 720
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_zero_greater:	/* 0> ( n -- f ) */
/*  */
NAME("0>")
{
DEF_CA
Cell n;
Bool f;
NEXT_P0;
n = (Cell) TOS;
{
#line 720 "./primitives"
f = FLAG(n>0);
#line 720
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_zero_less_or_equal:	/* 0<= ( n -- f ) */
/*  */
NAME("0<=")
{
DEF_CA
Cell n;
Bool f;
NEXT_P0;
n = (Cell) TOS;
{
#line 720 "./primitives"
f = FLAG(n<=0);
#line 720
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_zero_greater_or_equal:	/* 0>= ( n -- f ) */
/*  */
NAME("0>=")
{
DEF_CA
Cell n;
Bool f;
NEXT_P0;
n = (Cell) TOS;
{
#line 720 "./primitives"
f = FLAG(n>=0);
#line 720
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_equals:	/* = ( n1 n2 -- f ) */
/*  */
NAME("=")
{
DEF_CA
Cell n1;
Cell n2;
Bool f;
NEXT_P0;
n1 = (Cell) sp[1];
n2 = (Cell) TOS;
sp += 1;
{
#line 721 "./primitives"
f = FLAG(n1==n2);
#line 721
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_different:	/* <> ( n1 n2 -- f ) */
/*  */
NAME("<>")
{
DEF_CA
Cell n1;
Cell n2;
Bool f;
NEXT_P0;
n1 = (Cell) sp[1];
n2 = (Cell) TOS;
sp += 1;
{
#line 721 "./primitives"
f = FLAG(n1!=n2);
#line 721
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_less:	/* < ( n1 n2 -- f ) */
/*  */
NAME("<")
{
DEF_CA
Cell n1;
Cell n2;
Bool f;
NEXT_P0;
n1 = (Cell) sp[1];
n2 = (Cell) TOS;
sp += 1;
{
#line 721 "./primitives"
f = FLAG(n1<n2);
#line 721
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_greater:	/* > ( n1 n2 -- f ) */
/*  */
NAME(">")
{
DEF_CA
Cell n1;
Cell n2;
Bool f;
NEXT_P0;
n1 = (Cell) sp[1];
n2 = (Cell) TOS;
sp += 1;
{
#line 721 "./primitives"
f = FLAG(n1>n2);
#line 721
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_less_or_equal:	/* <= ( n1 n2 -- f ) */
/*  */
NAME("<=")
{
DEF_CA
Cell n1;
Cell n2;
Bool f;
NEXT_P0;
n1 = (Cell) sp[1];
n2 = (Cell) TOS;
sp += 1;
{
#line 721 "./primitives"
f = FLAG(n1<=n2);
#line 721
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_greater_or_equal:	/* >= ( n1 n2 -- f ) */
/*  */
NAME(">=")
{
DEF_CA
Cell n1;
Cell n2;
Bool f;
NEXT_P0;
n1 = (Cell) sp[1];
n2 = (Cell) TOS;
sp += 1;
{
#line 721 "./primitives"
f = FLAG(n1>=n2);
#line 721
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_u_equals:	/* u= ( u1 u2 -- f ) */
/*  */
NAME("u=")
{
DEF_CA
UCell u1;
UCell u2;
Bool f;
NEXT_P0;
u1 = (UCell) sp[1];
u2 = (UCell) TOS;
sp += 1;
{
#line 722 "./primitives"
f = FLAG(u1==u2);
#line 722
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_u_different:	/* u<> ( u1 u2 -- f ) */
/*  */
NAME("u<>")
{
DEF_CA
UCell u1;
UCell u2;
Bool f;
NEXT_P0;
u1 = (UCell) sp[1];
u2 = (UCell) TOS;
sp += 1;
{
#line 722 "./primitives"
f = FLAG(u1!=u2);
#line 722
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_u_less:	/* u< ( u1 u2 -- f ) */
/*  */
NAME("u<")
{
DEF_CA
UCell u1;
UCell u2;
Bool f;
NEXT_P0;
u1 = (UCell) sp[1];
u2 = (UCell) TOS;
sp += 1;
{
#line 722 "./primitives"
f = FLAG(u1<u2);
#line 722
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_u_greater:	/* u> ( u1 u2 -- f ) */
/*  */
NAME("u>")
{
DEF_CA
UCell u1;
UCell u2;
Bool f;
NEXT_P0;
u1 = (UCell) sp[1];
u2 = (UCell) TOS;
sp += 1;
{
#line 722 "./primitives"
f = FLAG(u1>u2);
#line 722
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_u_less_or_equal:	/* u<= ( u1 u2 -- f ) */
/*  */
NAME("u<=")
{
DEF_CA
UCell u1;
UCell u2;
Bool f;
NEXT_P0;
u1 = (UCell) sp[1];
u2 = (UCell) TOS;
sp += 1;
{
#line 722 "./primitives"
f = FLAG(u1<=u2);
#line 722
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_u_greater_or_equal:	/* u>= ( u1 u2 -- f ) */
/*  */
NAME("u>=")
{
DEF_CA
UCell u1;
UCell u2;
Bool f;
NEXT_P0;
u1 = (UCell) sp[1];
u2 = (UCell) TOS;
sp += 1;
{
#line 722 "./primitives"
f = FLAG(u1>=u2);
#line 722
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_equals:	/* d= ( d1 d2 -- f ) */
/*  */
NAME("d=")
{
DEF_CA
DCell d1;
DCell d2;
Bool f;
NEXT_P0;
FETCH_DCELL(d1, sp[3], sp[2]);
FETCH_DCELL(d2, sp[1], TOS);
sp += 3;
{
#line 770 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 770
f = FLAG(d1.lo==d2.lo && d1.hi==d2.hi);
#line 770
#else
#line 770
f = FLAG(d1==d2);
#line 770
#endif
#line 770
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_different:	/* d<> ( d1 d2 -- f ) */
/*  */
NAME("d<>")
{
DEF_CA
DCell d1;
DCell d2;
Bool f;
NEXT_P0;
FETCH_DCELL(d1, sp[3], sp[2]);
FETCH_DCELL(d2, sp[1], TOS);
sp += 3;
{
#line 770 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 770
f = FLAG(d1.lo!=d2.lo || d1.hi!=d2.hi);
#line 770
#else
#line 770
f = FLAG(d1!=d2);
#line 770
#endif
#line 770
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_less:	/* d< ( d1 d2 -- f ) */
/*  */
NAME("d<")
{
DEF_CA
DCell d1;
DCell d2;
Bool f;
NEXT_P0;
FETCH_DCELL(d1, sp[3], sp[2]);
FETCH_DCELL(d2, sp[1], TOS);
sp += 3;
{
#line 770 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 770
f = FLAG(d1.hi==d2.hi ? d1.lo<d2.lo : d1.hi<d2.hi);
#line 770
#else
#line 770
f = FLAG(d1<d2);
#line 770
#endif
#line 770
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_greater:	/* d> ( d1 d2 -- f ) */
/*  */
NAME("d>")
{
DEF_CA
DCell d1;
DCell d2;
Bool f;
NEXT_P0;
FETCH_DCELL(d1, sp[3], sp[2]);
FETCH_DCELL(d2, sp[1], TOS);
sp += 3;
{
#line 770 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 770
f = FLAG(d1.hi==d2.hi ? d1.lo>d2.lo : d1.hi>d2.hi);
#line 770
#else
#line 770
f = FLAG(d1>d2);
#line 770
#endif
#line 770
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_less_or_equal:	/* d<= ( d1 d2 -- f ) */
/*  */
NAME("d<=")
{
DEF_CA
DCell d1;
DCell d2;
Bool f;
NEXT_P0;
FETCH_DCELL(d1, sp[3], sp[2]);
FETCH_DCELL(d2, sp[1], TOS);
sp += 3;
{
#line 770 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 770
f = FLAG(d1.hi==d2.hi ? d1.lo<=d2.lo : d1.hi<=d2.hi);
#line 770
#else
#line 770
f = FLAG(d1<=d2);
#line 770
#endif
#line 770
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_greater_or_equal:	/* d>= ( d1 d2 -- f ) */
/*  */
NAME("d>=")
{
DEF_CA
DCell d1;
DCell d2;
Bool f;
NEXT_P0;
FETCH_DCELL(d1, sp[3], sp[2]);
FETCH_DCELL(d2, sp[1], TOS);
sp += 3;
{
#line 770 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 770
f = FLAG(d1.hi==d2.hi ? d1.lo>=d2.lo : d1.hi>=d2.hi);
#line 770
#else
#line 770
f = FLAG(d1>=d2);
#line 770
#endif
#line 770
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_zero_equals:	/* d0= ( d -- f ) */
/*  */
NAME("d0=")
{
DEF_CA
DCell d;
Bool f;
NEXT_P0;
FETCH_DCELL(d, sp[1], TOS);
sp += 1;
{
#line 771 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 771
f = FLAG(d.lo==DZERO.lo && d.hi==DZERO.hi);
#line 771
#else
#line 771
f = FLAG(d==DZERO);
#line 771
#endif
#line 771
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_zero_different:	/* d0<> ( d -- f ) */
/*  */
NAME("d0<>")
{
DEF_CA
DCell d;
Bool f;
NEXT_P0;
FETCH_DCELL(d, sp[1], TOS);
sp += 1;
{
#line 771 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 771
f = FLAG(d.lo!=DZERO.lo || d.hi!=DZERO.hi);
#line 771
#else
#line 771
f = FLAG(d!=DZERO);
#line 771
#endif
#line 771
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_zero_less:	/* d0< ( d -- f ) */
/*  */
NAME("d0<")
{
DEF_CA
DCell d;
Bool f;
NEXT_P0;
FETCH_DCELL(d, sp[1], TOS);
sp += 1;
{
#line 771 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 771
f = FLAG(d.hi==DZERO.hi ? d.lo<DZERO.lo : d.hi<DZERO.hi);
#line 771
#else
#line 771
f = FLAG(d<DZERO);
#line 771
#endif
#line 771
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_zero_greater:	/* d0> ( d -- f ) */
/*  */
NAME("d0>")
{
DEF_CA
DCell d;
Bool f;
NEXT_P0;
FETCH_DCELL(d, sp[1], TOS);
sp += 1;
{
#line 771 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 771
f = FLAG(d.hi==DZERO.hi ? d.lo>DZERO.lo : d.hi>DZERO.hi);
#line 771
#else
#line 771
f = FLAG(d>DZERO);
#line 771
#endif
#line 771
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_zero_less_or_equal:	/* d0<= ( d -- f ) */
/*  */
NAME("d0<=")
{
DEF_CA
DCell d;
Bool f;
NEXT_P0;
FETCH_DCELL(d, sp[1], TOS);
sp += 1;
{
#line 771 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 771
f = FLAG(d.hi==DZERO.hi ? d.lo<=DZERO.lo : d.hi<=DZERO.hi);
#line 771
#else
#line 771
f = FLAG(d<=DZERO);
#line 771
#endif
#line 771
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_zero_greater_or_equal:	/* d0>= ( d -- f ) */
/*  */
NAME("d0>=")
{
DEF_CA
DCell d;
Bool f;
NEXT_P0;
FETCH_DCELL(d, sp[1], TOS);
sp += 1;
{
#line 771 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 771
f = FLAG(d.hi==DZERO.hi ? d.lo>=DZERO.lo : d.hi>=DZERO.hi);
#line 771
#else
#line 771
f = FLAG(d>=DZERO);
#line 771
#endif
#line 771
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_u_equals:	/* du= ( ud1 ud2 -- f ) */
/*  */
NAME("du=")
{
DEF_CA
UDCell ud1;
UDCell ud2;
Bool f;
NEXT_P0;
FETCH_DCELL(ud1, sp[3], sp[2]);
FETCH_DCELL(ud2, sp[1], TOS);
sp += 3;
{
#line 772 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 772
f = FLAG(ud1.lo==ud2.lo && ud1.hi==ud2.hi);
#line 772
#else
#line 772
f = FLAG(ud1==ud2);
#line 772
#endif
#line 772
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_u_different:	/* du<> ( ud1 ud2 -- f ) */
/*  */
NAME("du<>")
{
DEF_CA
UDCell ud1;
UDCell ud2;
Bool f;
NEXT_P0;
FETCH_DCELL(ud1, sp[3], sp[2]);
FETCH_DCELL(ud2, sp[1], TOS);
sp += 3;
{
#line 772 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 772
f = FLAG(ud1.lo!=ud2.lo || ud1.hi!=ud2.hi);
#line 772
#else
#line 772
f = FLAG(ud1!=ud2);
#line 772
#endif
#line 772
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_u_less:	/* du< ( ud1 ud2 -- f ) */
/*  */
NAME("du<")
{
DEF_CA
UDCell ud1;
UDCell ud2;
Bool f;
NEXT_P0;
FETCH_DCELL(ud1, sp[3], sp[2]);
FETCH_DCELL(ud2, sp[1], TOS);
sp += 3;
{
#line 772 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 772
f = FLAG(ud1.hi==ud2.hi ? ud1.lo<ud2.lo : ud1.hi<ud2.hi);
#line 772
#else
#line 772
f = FLAG(ud1<ud2);
#line 772
#endif
#line 772
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_u_greater:	/* du> ( ud1 ud2 -- f ) */
/*  */
NAME("du>")
{
DEF_CA
UDCell ud1;
UDCell ud2;
Bool f;
NEXT_P0;
FETCH_DCELL(ud1, sp[3], sp[2]);
FETCH_DCELL(ud2, sp[1], TOS);
sp += 3;
{
#line 772 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 772
f = FLAG(ud1.hi==ud2.hi ? ud1.lo>ud2.lo : ud1.hi>ud2.hi);
#line 772
#else
#line 772
f = FLAG(ud1>ud2);
#line 772
#endif
#line 772
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_u_less_or_equal:	/* du<= ( ud1 ud2 -- f ) */
/*  */
NAME("du<=")
{
DEF_CA
UDCell ud1;
UDCell ud2;
Bool f;
NEXT_P0;
FETCH_DCELL(ud1, sp[3], sp[2]);
FETCH_DCELL(ud2, sp[1], TOS);
sp += 3;
{
#line 772 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 772
f = FLAG(ud1.hi==ud2.hi ? ud1.lo<=ud2.lo : ud1.hi<=ud2.hi);
#line 772
#else
#line 772
f = FLAG(ud1<=ud2);
#line 772
#endif
#line 772
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_d_u_greater_or_equal:	/* du>= ( ud1 ud2 -- f ) */
/*  */
NAME("du>=")
{
DEF_CA
UDCell ud1;
UDCell ud2;
Bool f;
NEXT_P0;
FETCH_DCELL(ud1, sp[3], sp[2]);
FETCH_DCELL(ud2, sp[1], TOS);
sp += 3;
{
#line 772 "./primitives"
#ifdef BUGGY_LONG_LONG
#line 772
f = FLAG(ud1.hi==ud2.hi ? ud1.lo>=ud2.lo : ud1.hi>=ud2.hi);
#line 772
#else
#line 772
f = FLAG(ud1>=ud2);
#line 772
#endif
#line 772
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_within:	/* within ( u1 u2 u3 -- f ) */
/*  */
NAME("within")
{
DEF_CA
UCell u1;
UCell u2;
UCell u3;
Bool f;
NEXT_P0;
u1 = (UCell) sp[2];
u2 = (UCell) sp[1];
u3 = (UCell) TOS;
sp += 2;
{
#line 775 "./primitives"
f = FLAG(u1-u2 < u3-u2);
}
NEXT_P1;
TOS = (Cell)f;
NEXT_P2;
}

I_spat:	/* sp@ ( -- a_addr ) */
/*  */
NAME("sp@")
{
DEF_CA
Cell * a_addr;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 780 "./primitives"
a_addr = sp+1;
}
NEXT_P1;
TOS = (Cell)a_addr;
NEXT_P2;
}

I_spstore:	/* sp! ( a_addr -- ) */
/*  */
NAME("sp!")
{
DEF_CA
Cell * a_addr;
NEXT_P0;
a_addr = (Cell *) TOS;
sp += 1;
{
#line 783 "./primitives"
sp = a_addr;
/* works with and without TOS caching */
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_rpat:	/* rp@ ( -- a_addr ) */
/*  */
NAME("rp@")
{
DEF_CA
Cell * a_addr;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 787 "./primitives"
a_addr = rp;
}
NEXT_P1;
TOS = (Cell)a_addr;
NEXT_P2;
}

I_rpstore:	/* rp! ( a_addr -- ) */
/*  */
NAME("rp!")
{
DEF_CA
Cell * a_addr;
NEXT_P0;
a_addr = (Cell *) TOS;
sp += 1;
{
#line 790 "./primitives"
rp = a_addr;
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_fp_fetch:	/* fp@ ( -- f_addr ) */
/*  */
NAME("fp@")
{
DEF_CA
Float * f_addr;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 793 "./primitives"
f_addr = fp;
}
NEXT_P1;
TOS = (Cell)f_addr;
NEXT_P2;
}

I_fp_store:	/* fp! ( f_addr -- ) */
/*  */
NAME("fp!")
{
DEF_CA
Float * f_addr;
NEXT_P0;
f_addr = (Float *) TOS;
sp += 1;
{
#line 796 "./primitives"
fp = f_addr;
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_semis:	/* ;s ( -- ) */
/*  */
NAME(";s")
{
DEF_CA
NEXT_P0;
{
#line 799 "./primitives"
ip = (Xt *)(*rp++);
NEXT_P0;
}
NEXT_P1;
NEXT_P2;
}

I_to_r:	/* >r ( w -- ) */
/*  */
NAME(">r")
{
DEF_CA
Cell w;
NEXT_P0;
w = (Cell) TOS;
sp += 1;
{
#line 803 "./primitives"
*--rp = w;
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_r_from:	/* r> ( -- w ) */
/*  */
NAME("r>")
{
DEF_CA
Cell w;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 806 "./primitives"
w = *rp++;
}
NEXT_P1;
TOS = (Cell)w;
NEXT_P2;
}

I_r_fetch:	/* r@ ( -- w ) */
/*  */
NAME("r@")
{
DEF_CA
Cell w;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 809 "./primitives"
/* use r as alias */
/* make r@ an alias for i */
w = *rp;
}
NEXT_P1;
TOS = (Cell)w;
NEXT_P2;
}

I_rdrop:	/* rdrop ( -- ) */
/*  */
NAME("rdrop")
{
DEF_CA
NEXT_P0;
{
#line 814 "./primitives"
rp++;
}
NEXT_P1;
NEXT_P2;
}

I_i_tick:	/* i' ( -- w ) */
/*  */
NAME("i'")
{
DEF_CA
Cell w;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 817 "./primitives"
w=rp[1];
}
NEXT_P1;
TOS = (Cell)w;
NEXT_P2;
}

I_two_to_r:	/* 2>r ( w1 w2 -- ) */
/*  */
NAME("2>r")
{
DEF_CA
Cell w1;
Cell w2;
NEXT_P0;
w1 = (Cell) sp[1];
w2 = (Cell) TOS;
sp += 2;
{
#line 820 "./primitives"
*--rp = w1;
*--rp = w2;
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_two_r_from:	/* 2r> ( -- w1 w2 ) */
/*  */
NAME("2r>")
{
DEF_CA
Cell w1;
Cell w2;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -2;
{
#line 824 "./primitives"
w2 = *rp++;
w1 = *rp++;
}
NEXT_P1;
sp[1] = (Cell)w1;
TOS = (Cell)w2;
NEXT_P2;
}

I_two_r_fetch:	/* 2r@ ( -- w1 w2 ) */
/*  */
NAME("2r@")
{
DEF_CA
Cell w1;
Cell w2;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -2;
{
#line 828 "./primitives"
w2 = rp[0];
w1 = rp[1];
}
NEXT_P1;
sp[1] = (Cell)w1;
TOS = (Cell)w2;
NEXT_P2;
}

I_two_r_drop:	/* 2rdrop ( -- ) */
/*  */
NAME("2rdrop")
{
DEF_CA
NEXT_P0;
{
#line 832 "./primitives"
rp+=2;
}
NEXT_P1;
NEXT_P2;
}

I_over:	/* over ( w1 w2 -- w1 w2 w1 ) */
/*  */
NAME("over")
{
DEF_CA
Cell w1;
Cell w2;
NEXT_P0;
w1 = (Cell) sp[1];
w2 = (Cell) TOS;
sp += -1;
{
#line 835 "./primitives"
}
NEXT_P1;
IF_TOS(sp[1] = (Cell)w2;);
TOS = (Cell)w1;
NEXT_P2;
}

I_drop:	/* drop ( w -- ) */
/*  */
NAME("drop")
{
DEF_CA
Cell w;
NEXT_P0;
w = (Cell) TOS;
sp += 1;
{
#line 837 "./primitives"
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_swap:	/* swap ( w1 w2 -- w2 w1 ) */
/*  */
NAME("swap")
{
DEF_CA
Cell w1;
Cell w2;
NEXT_P0;
w1 = (Cell) sp[1];
w2 = (Cell) TOS;
{
#line 839 "./primitives"
}
NEXT_P1;
sp[1] = (Cell)w2;
TOS = (Cell)w1;
NEXT_P2;
}

I_dup:	/* dup ( w -- w w ) */
/*  */
NAME("dup")
{
DEF_CA
Cell w;
NEXT_P0;
w = (Cell) TOS;
sp += -1;
{
#line 841 "./primitives"
}
NEXT_P1;
IF_TOS(sp[1] = (Cell)w;);
TOS = (Cell)w;
NEXT_P2;
}

I_rote:	/* rot ( w1 w2 w3 -- w2 w3 w1 ) */
/*  */
NAME("rot")
{
DEF_CA
Cell w1;
Cell w2;
Cell w3;
NEXT_P0;
w1 = (Cell) sp[2];
w2 = (Cell) sp[1];
w3 = (Cell) TOS;
{
#line 843 "./primitives"
}
NEXT_P1;
sp[2] = (Cell)w2;
sp[1] = (Cell)w3;
TOS = (Cell)w1;
NEXT_P2;
}

I_not_rote:	/* -rot ( w1 w2 w3 -- w3 w1 w2 ) */
/*  */
NAME("-rot")
{
DEF_CA
Cell w1;
Cell w2;
Cell w3;
NEXT_P0;
w1 = (Cell) sp[2];
w2 = (Cell) sp[1];
w3 = (Cell) TOS;
{
#line 845 "./primitives"
}
NEXT_P1;
sp[2] = (Cell)w3;
sp[1] = (Cell)w1;
TOS = (Cell)w2;
NEXT_P2;
}

I_nip:	/* nip ( w1 w2 -- w2 ) */
/*  */
NAME("nip")
{
DEF_CA
Cell w1;
Cell w2;
NEXT_P0;
w1 = (Cell) sp[1];
w2 = (Cell) TOS;
sp += 1;
{
#line 849 "./primitives"
}
NEXT_P1;
TOS = (Cell)w2;
NEXT_P2;
}

I_tuck:	/* tuck ( w1 w2 -- w2 w1 w2 ) */
/*  */
NAME("tuck")
{
DEF_CA
Cell w1;
Cell w2;
NEXT_P0;
w1 = (Cell) sp[1];
w2 = (Cell) TOS;
sp += -1;
{
#line 853 "./primitives"
}
NEXT_P1;
sp[2] = (Cell)w2;
sp[1] = (Cell)w1;
TOS = (Cell)w2;
NEXT_P2;
}

I_question_dupe:	/* ?dup ( w -- w ) */
/*  */
NAME("?dup")
{
DEF_CA
Cell w;
NEXT_P0;
w = (Cell) TOS;
{
#line 857 "./primitives"
if (w!=0) {
  IF_TOS(*sp-- = w;)
#ifndef USE_TOS
  *--sp = w;
#endif
}
}
NEXT_P1;
IF_TOS(TOS = (Cell)w;);
NEXT_P2;
}

I_pick:	/* pick ( u -- w ) */
/*  */
NAME("pick")
{
DEF_CA
UCell u;
Cell w;
NEXT_P0;
u = (UCell) TOS;
{
#line 867 "./primitives"
w = sp[u+1];
}
NEXT_P1;
TOS = (Cell)w;
NEXT_P2;
}

I_two_drop:	/* 2drop ( w1 w2 -- ) */
/*  */
NAME("2drop")
{
DEF_CA
Cell w1;
Cell w2;
NEXT_P0;
w1 = (Cell) sp[1];
w2 = (Cell) TOS;
sp += 2;
{
#line 872 "./primitives"
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_two_dupe:	/* 2dup ( w1 w2 -- w1 w2 w1 w2 ) */
/*  */
NAME("2dup")
{
DEF_CA
Cell w1;
Cell w2;
NEXT_P0;
w1 = (Cell) sp[1];
w2 = (Cell) TOS;
sp += -2;
{
#line 876 "./primitives"
}
NEXT_P1;
IF_TOS(sp[2] = (Cell)w2;);
sp[1] = (Cell)w1;
TOS = (Cell)w2;
NEXT_P2;
}

I_two_over:	/* 2over ( w1 w2 w3 w4 -- w1 w2 w3 w4 w1 w2 ) */
/*  */
NAME("2over")
{
DEF_CA
Cell w1;
Cell w2;
Cell w3;
Cell w4;
NEXT_P0;
w1 = (Cell) sp[3];
w2 = (Cell) sp[2];
w3 = (Cell) sp[1];
w4 = (Cell) TOS;
sp += -2;
{
#line 880 "./primitives"
}
NEXT_P1;
IF_TOS(sp[2] = (Cell)w4;);
sp[1] = (Cell)w1;
TOS = (Cell)w2;
NEXT_P2;
}

I_two_swap:	/* 2swap ( w1 w2 w3 w4 -- w3 w4 w1 w2 ) */
/*  */
NAME("2swap")
{
DEF_CA
Cell w1;
Cell w2;
Cell w3;
Cell w4;
NEXT_P0;
w1 = (Cell) sp[3];
w2 = (Cell) sp[2];
w3 = (Cell) sp[1];
w4 = (Cell) TOS;
{
#line 884 "./primitives"
}
NEXT_P1;
sp[3] = (Cell)w3;
sp[2] = (Cell)w4;
sp[1] = (Cell)w1;
TOS = (Cell)w2;
NEXT_P2;
}

I_two_rote:	/* 2rot ( w1 w2 w3 w4 w5 w6 -- w3 w4 w5 w6 w1 w2 ) */
/*  */
NAME("2rot")
{
DEF_CA
Cell w1;
Cell w2;
Cell w3;
Cell w4;
Cell w5;
Cell w6;
NEXT_P0;
w1 = (Cell) sp[5];
w2 = (Cell) sp[4];
w3 = (Cell) sp[3];
w4 = (Cell) sp[2];
w5 = (Cell) sp[1];
w6 = (Cell) TOS;
{
#line 888 "./primitives"
}
NEXT_P1;
sp[5] = (Cell)w3;
sp[4] = (Cell)w4;
sp[3] = (Cell)w5;
sp[2] = (Cell)w6;
sp[1] = (Cell)w1;
TOS = (Cell)w2;
NEXT_P2;
}

I_two_nip:	/* 2nip ( w1 w2 w3 w4 -- w3 w4 ) */
/*  */
NAME("2nip")
{
DEF_CA
Cell w1;
Cell w2;
Cell w3;
Cell w4;
NEXT_P0;
w1 = (Cell) sp[3];
w2 = (Cell) sp[2];
w3 = (Cell) sp[1];
w4 = (Cell) TOS;
sp += 2;
{
#line 892 "./primitives"
}
NEXT_P1;
sp[1] = (Cell)w3;
TOS = (Cell)w4;
NEXT_P2;
}

I_two_tuck:	/* 2tuck ( w1 w2 w3 w4 -- w3 w4 w1 w2 w3 w4 ) */
/*  */
NAME("2tuck")
{
DEF_CA
Cell w1;
Cell w2;
Cell w3;
Cell w4;
NEXT_P0;
w1 = (Cell) sp[3];
w2 = (Cell) sp[2];
w3 = (Cell) sp[1];
w4 = (Cell) TOS;
sp += -2;
{
#line 896 "./primitives"
}
NEXT_P1;
sp[5] = (Cell)w3;
sp[4] = (Cell)w4;
sp[3] = (Cell)w1;
sp[2] = (Cell)w2;
sp[1] = (Cell)w3;
TOS = (Cell)w4;
NEXT_P2;
}

I_fetch:	/* @ ( a_addr -- w ) */
/*  */
NAME("@")
{
DEF_CA
Cell * a_addr;
Cell w;
NEXT_P0;
a_addr = (Cell *) TOS;
{
#line 902 "./primitives"
w = *a_addr;
}
NEXT_P1;
TOS = (Cell)w;
NEXT_P2;
}

I_store:	/* ! ( w a_addr -- ) */
/*  */
NAME("!")
{
DEF_CA
Cell w;
Cell * a_addr;
NEXT_P0;
w = (Cell) sp[1];
a_addr = (Cell *) TOS;
sp += 2;
{
#line 905 "./primitives"
*a_addr = w;
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_plus_store:	/* +! ( n a_addr -- ) */
/*  */
NAME("+!")
{
DEF_CA
Cell n;
Cell * a_addr;
NEXT_P0;
n = (Cell) sp[1];
a_addr = (Cell *) TOS;
sp += 2;
{
#line 908 "./primitives"
*a_addr += n;
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_cfetch:	/* c@ ( c_addr -- c ) */
/*  */
NAME("c@")
{
DEF_CA
Char * c_addr;
Char c;
NEXT_P0;
c_addr = (Char *) TOS;
{
#line 911 "./primitives"
c = *c_addr;
}
NEXT_P1;
TOS = (Cell)c;
NEXT_P2;
}

I_cstore:	/* c! ( c c_addr -- ) */
/*  */
NAME("c!")
{
DEF_CA
Char c;
Char * c_addr;
NEXT_P0;
c = (Char) sp[1];
c_addr = (Char *) TOS;
sp += 2;
{
#line 914 "./primitives"
*c_addr = c;
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_two_store:	/* 2! ( w1 w2 a_addr -- ) */
/*  */
NAME("2!")
{
DEF_CA
Cell w1;
Cell w2;
Cell * a_addr;
NEXT_P0;
w1 = (Cell) sp[2];
w2 = (Cell) sp[1];
a_addr = (Cell *) TOS;
sp += 3;
{
#line 917 "./primitives"
a_addr[0] = w2;
a_addr[1] = w1;
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_two_fetch:	/* 2@ ( a_addr -- w1 w2 ) */
/*  */
NAME("2@")
{
DEF_CA
Cell * a_addr;
Cell w1;
Cell w2;
NEXT_P0;
a_addr = (Cell *) TOS;
sp += -1;
{
#line 923 "./primitives"
w2 = a_addr[0];
w1 = a_addr[1];
}
NEXT_P1;
sp[1] = (Cell)w1;
TOS = (Cell)w2;
NEXT_P2;
}

I_cell_plus:	/* cell+ ( a_addr1 -- a_addr2 ) */
/*  */
NAME("cell+")
{
DEF_CA
Cell * a_addr1;
Cell * a_addr2;
NEXT_P0;
a_addr1 = (Cell *) TOS;
{
#line 929 "./primitives"
a_addr2 = a_addr1+1;
}
NEXT_P1;
TOS = (Cell)a_addr2;
NEXT_P2;
}

I_cells:	/* cells ( n1 -- n2 ) */
/*  */
NAME("cells")
{
DEF_CA
Cell n1;
Cell n2;
NEXT_P0;
n1 = (Cell) TOS;
{
#line 934 "./primitives"
n2 = n1 * sizeof(Cell);
}
NEXT_P1;
TOS = (Cell)n2;
NEXT_P2;
}

I_care_plus:	/* char+ ( c_addr1 -- c_addr2 ) */
/*  */
NAME("char+")
{
DEF_CA
Char * c_addr1;
Char * c_addr2;
NEXT_P0;
c_addr1 = (Char *) TOS;
{
#line 944 "./primitives"
c_addr2 = c_addr1 + 1;
}
NEXT_P1;
TOS = (Cell)c_addr2;
NEXT_P2;
}

I_paren_cares:	/* (chars) ( n1 -- n2 ) */
/*  */
NAME("(chars)")
{
DEF_CA
Cell n1;
Cell n2;
NEXT_P0;
n1 = (Cell) TOS;
{
#line 949 "./primitives"
n2 = n1 * sizeof(Char);
}
NEXT_P1;
TOS = (Cell)n2;
NEXT_P2;
}

I_count:	/* count ( c_addr1 -- c_addr2 u ) */
/*  */
NAME("count")
{
DEF_CA
Char * c_addr1;
Char * c_addr2;
UCell u;
NEXT_P0;
c_addr1 = (Char *) TOS;
sp += -1;
{
#line 954 "./primitives"
u = *c_addr1;
c_addr2 = c_addr1+1;
}
NEXT_P1;
sp[1] = (Cell)c_addr2;
TOS = (Cell)u;
NEXT_P2;
}

I_paren_bye:	/* (bye) ( n -- ) */
/*  */
NAME("(bye)")
{
DEF_CA
Cell n;
NEXT_P0;
n = (Cell) TOS;
sp += 1;
{
#line 960 "./primitives"
return (Label *)n;
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_peren_system:	/* (system) ( c_addr u -- wretval wior ) */
/*  */
NAME("(system)")
{
DEF_CA
Char * c_addr;
UCell u;
Cell wretval;
Cell wior;
NEXT_P0;
c_addr = (Char *) sp[1];
u = (UCell) TOS;
{
#line 963 "./primitives"
int old_tp=terminal_prepped;
deprep_terminal();
wretval=system(cstr(c_addr,u,1)); /* ~ expansion on first part of string? */
wior = IOR(wretval==-1 || (wretval==127 && errno != 0));
if (old_tp)
  prep_terminal();
}
NEXT_P1;
sp[1] = (Cell)wretval;
TOS = (Cell)wior;
NEXT_P2;
}

I_getenv:	/* getenv ( c_addr1 u1 -- c_addr2 u2 ) */
/*  */
NAME("getenv")
{
DEF_CA
Char * c_addr1;
UCell u1;
Char * c_addr2;
UCell u2;
NEXT_P0;
c_addr1 = (Char *) sp[1];
u1 = (UCell) TOS;
{
#line 971 "./primitives"
c_addr2 = getenv(cstr(c_addr1,u1,1));
u2 = (c_addr2 == NULL ? 0 : strlen(c_addr2));
}
NEXT_P1;
sp[1] = (Cell)c_addr2;
TOS = (Cell)u2;
NEXT_P2;
}

I_open_pipe:	/* open-pipe ( c_addr u ntype -- wfileid wior ) */
/*  */
NAME("open-pipe")
{
DEF_CA
Char * c_addr;
UCell u;
Cell ntype;
Cell wfileid;
Cell wior;
NEXT_P0;
c_addr = (Char *) sp[2];
u = (UCell) sp[1];
ntype = (Cell) TOS;
sp += 1;
{
#line 975 "./primitives"
wfileid=(Cell)popen(cstr(c_addr,u,1),fileattr[ntype]); /* ~ expansion of 1st arg? */
wior = IOR(wfileid==0); /* !! the man page says that errno is not set reliably */
}
NEXT_P1;
sp[1] = (Cell)wfileid;
TOS = (Cell)wior;
NEXT_P2;
}

I_close_pipe:	/* close-pipe ( wfileid -- wretval wior ) */
/*  */
NAME("close-pipe")
{
DEF_CA
Cell wfileid;
Cell wretval;
Cell wior;
NEXT_P0;
wfileid = (Cell) TOS;
sp += -1;
{
#line 979 "./primitives"
wretval = pclose((FILE *)wfileid);
wior = IOR(wretval==-1);
}
NEXT_P1;
sp[1] = (Cell)wretval;
TOS = (Cell)wior;
NEXT_P2;
}

I_time_and_date:	/* time&date ( -- nsec nmin nhour nday nmonth nyear ) */
/*  */
NAME("time&date")
{
DEF_CA
Cell nsec;
Cell nmin;
Cell nhour;
Cell nday;
Cell nmonth;
Cell nyear;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -6;
{
#line 983 "./primitives"
struct timeval time1;
struct timezone zone1;
struct tm *ltime;
gettimeofday(&time1,&zone1);
ltime=localtime((time_t *)&time1.tv_sec);
nyear =ltime->tm_year+1900;
nmonth=ltime->tm_mon+1;
nday  =ltime->tm_mday;
nhour =ltime->tm_hour;
nmin  =ltime->tm_min;
nsec  =ltime->tm_sec;
}
NEXT_P1;
sp[5] = (Cell)nsec;
sp[4] = (Cell)nmin;
sp[3] = (Cell)nhour;
sp[2] = (Cell)nday;
sp[1] = (Cell)nmonth;
TOS = (Cell)nyear;
NEXT_P2;
}

I_ms:	/* ms ( n -- ) */
/*  */
NAME("ms")
{
DEF_CA
Cell n;
NEXT_P0;
n = (Cell) TOS;
sp += 1;
{
#line 996 "./primitives"
struct timeval timeout;
timeout.tv_sec=n/1000;
timeout.tv_usec=1000*(n%1000);
(void)select(0,0,0,0,&timeout);
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_allocate:	/* allocate ( u -- a_addr wior ) */
/*  */
NAME("allocate")
{
DEF_CA
UCell u;
Cell * a_addr;
Cell wior;
NEXT_P0;
u = (UCell) TOS;
sp += -1;
{
#line 1002 "./primitives"
a_addr = (Cell *)malloc(u?u:1);
wior = IOR(a_addr==NULL);
}
NEXT_P1;
sp[1] = (Cell)a_addr;
TOS = (Cell)wior;
NEXT_P2;
}

I_free:	/* free ( a_addr -- wior ) */
/*  */
NAME("free")
{
DEF_CA
Cell * a_addr;
Cell wior;
NEXT_P0;
a_addr = (Cell *) TOS;
{
#line 1006 "./primitives"
free(a_addr);
wior = 0;
}
NEXT_P1;
TOS = (Cell)wior;
NEXT_P2;
}

I_resize:	/* resize ( a_addr1 u -- a_addr2 wior ) */
/* Change the size of the allocated area at @i{a_addr1} to @i{u}
address units, possibly moving the contents to a different
area. @i{a_addr2} is the address of the resulting area. If
@code{a_addr1} is 0, Gforth's (but not the standard) @code{resize}
@code{allocate}s @i{u} address units. */
NAME("resize")
{
DEF_CA
Cell * a_addr1;
UCell u;
Cell * a_addr2;
Cell wior;
NEXT_P0;
a_addr1 = (Cell *) sp[1];
u = (UCell) TOS;
{
#line 1015 "./primitives"
/* the following check is not necessary on most OSs, but it is needed
   on SunOS 4.1.2. */
if (a_addr1==NULL)
  a_addr2 = (Cell *)malloc(u);
else
  a_addr2 = (Cell *)realloc(a_addr1, u);
wior = IOR(a_addr2==NULL);	/* !! Define a return code */
}
NEXT_P1;
sp[1] = (Cell)a_addr2;
TOS = (Cell)wior;
NEXT_P2;
}

I_paren_f83find:	/* (f83find) ( c_addr u f83name1 -- f83name2 ) */
/*  */
NAME("(f83find)")
{
DEF_CA
Char * c_addr;
UCell u;
F83Name * f83name1;
F83Name * f83name2;
NEXT_P0;
c_addr = (Char *) sp[2];
u = (UCell) sp[1];
f83name1 = (F83Name *) TOS;
sp += 2;
{
#line 1024 "./primitives"
for (; f83name1 != NULL; f83name1 = f83name1->next)
  if (F83NAME_COUNT(f83name1)==u &&
      memcasecmp(c_addr, f83name1->name, u)== 0 /* or inline? */)
    break;
f83name2=f83name1;
}
NEXT_P1;
TOS = (Cell)f83name2;
NEXT_P2;
}

I_paren_hashfind:	/* (hashfind) ( c_addr u a_addr -- f83name2 ) */
/*  */
NAME("(hashfind)")
{
DEF_CA
Char * c_addr;
UCell u;
Cell * a_addr;
F83Name * f83name2;
NEXT_P0;
c_addr = (Char *) sp[2];
u = (UCell) sp[1];
a_addr = (Cell *) TOS;
sp += 2;
{
#line 1038 "./primitives"
F83Name *f83name1;
f83name2=NULL;
while(a_addr != NULL)
{
   f83name1=(F83Name *)(a_addr[1]);
   a_addr=(Cell *)(a_addr[0]);
   if (F83NAME_COUNT(f83name1)==u &&
       memcasecmp(c_addr, f83name1->name, u)== 0 /* or inline? */)
     {
	f83name2=f83name1;
	break;
     }
}
}
NEXT_P1;
TOS = (Cell)f83name2;
NEXT_P2;
}

I_paren_tablefind:	/* (tablefind) ( c_addr u a_addr -- f83name2 ) */
/* A case-sensitive variant of @code{(hashfind)} */
NAME("(tablefind)")
{
DEF_CA
Char * c_addr;
UCell u;
Cell * a_addr;
F83Name * f83name2;
NEXT_P0;
c_addr = (Char *) sp[2];
u = (UCell) sp[1];
a_addr = (Cell *) TOS;
sp += 2;
{
#line 1061 "./primitives"
F83Name *f83name1;
f83name2=NULL;
while(a_addr != NULL)
{
   f83name1=(F83Name *)(a_addr[1]);
   a_addr=(Cell *)(a_addr[0]);
   if (F83NAME_COUNT(f83name1)==u &&
       memcmp(c_addr, f83name1->name, u)== 0 /* or inline? */)
     {
	f83name2=f83name1;
	break;
     }
}
}
NEXT_P1;
TOS = (Cell)f83name2;
NEXT_P2;
}

I_paren_hashkey:	/* (hashkey) ( c_addr u1 -- u2 ) */
/*  */
NAME("(hashkey)")
{
DEF_CA
Char * c_addr;
UCell u1;
UCell u2;
NEXT_P0;
c_addr = (Char *) sp[1];
u1 = (UCell) TOS;
sp += 1;
{
#line 1083 "./primitives"
u2=0;
while(u1--)
   u2+=(Cell)toupper(*c_addr++);
}
NEXT_P1;
TOS = (Cell)u2;
NEXT_P2;
}

I_paren_hashkey1:	/* (hashkey1) ( c_addr u ubits -- ukey ) */
/* ukey is the hash key for the string c_addr u fitting in ubits bits */
NAME("(hashkey1)")
{
DEF_CA
Char * c_addr;
UCell u;
UCell ubits;
UCell ukey;
NEXT_P0;
c_addr = (Char *) sp[2];
u = (UCell) sp[1];
ubits = (UCell) TOS;
sp += 2;
{
#line 1091 "./primitives"
/* this hash function rotates the key at every step by rot bits within
   ubits bits and xors it with the character. This function does ok in
   the chi-sqare-test.  Rot should be <=7 (preferably <=5) for
   ASCII strings (larger if ubits is large), and should share no
   divisors with ubits.
*/
unsigned rot = ((char []){5,0,1,2,3,4,5,5,5,5,3,5,5,5,5,7,5,5,5,5,7,5,5,5,5,6,5,5,5,5,7,5,5})[ubits];
Char *cp = c_addr;
for (ukey=0; cp<c_addr+u; cp++)
    ukey = ((((ukey<<rot) | (ukey>>(ubits-rot))) 
	     ^ toupper(*cp))
	    & ((1<<ubits)-1));
}
NEXT_P1;
TOS = (Cell)ukey;
NEXT_P2;
}

I_paren_parse_white:	/* (parse-white) ( c_addr1 u1 -- c_addr2 u2 ) */
/*  */
NAME("(parse-white)")
{
DEF_CA
Char * c_addr1;
UCell u1;
Char * c_addr2;
UCell u2;
NEXT_P0;
c_addr1 = (Char *) sp[1];
u1 = (UCell) TOS;
{
#line 1117 "./primitives"
/* use !isgraph instead of isspace? */
Char *endp = c_addr1+u1;
while (c_addr1<endp && isspace(*c_addr1))
  c_addr1++;
if (c_addr1<endp) {
  for (c_addr2 = c_addr1; c_addr1<endp && !isspace(*c_addr1); c_addr1++)
    ;
  u2 = c_addr1-c_addr2;
}
else {
  c_addr2 = c_addr1;
  u2 = 0;
}
}
NEXT_P1;
sp[1] = (Cell)c_addr2;
TOS = (Cell)u2;
NEXT_P2;
}

I_close_file:	/* close-file ( wfileid -- wior ) */
/*  */
NAME("close-file")
{
DEF_CA
Cell wfileid;
Cell wior;
NEXT_P0;
wfileid = (Cell) TOS;
{
#line 1137 "./primitives"
wior = IOR(fclose((FILE *)wfileid)==EOF);
}
NEXT_P1;
TOS = (Cell)wior;
NEXT_P2;
}

I_open_file:	/* open-file ( c_addr u ntype -- w2 wior ) */
/*  */
NAME("open-file")
{
DEF_CA
Char * c_addr;
UCell u;
Cell ntype;
Cell w2;
Cell wior;
NEXT_P0;
c_addr = (Char *) sp[2];
u = (UCell) sp[1];
ntype = (Cell) TOS;
sp += 1;
{
#line 1140 "./primitives"
w2 = (Cell)fopen(tilde_cstr(c_addr, u, 1), fileattr[ntype]);
wior =  IOR(w2 == 0);
}
NEXT_P1;
sp[1] = (Cell)w2;
TOS = (Cell)wior;
NEXT_P2;
}

I_create_file:	/* create-file ( c_addr u ntype -- w2 wior ) */
/*  */
NAME("create-file")
{
DEF_CA
Char * c_addr;
UCell u;
Cell ntype;
Cell w2;
Cell wior;
NEXT_P0;
c_addr = (Char *) sp[2];
u = (UCell) sp[1];
ntype = (Cell) TOS;
sp += 1;
{
#line 1144 "./primitives"
Cell	fd;
fd = open(tilde_cstr(c_addr, u, 1), O_CREAT|O_RDWR|O_TRUNC, 0666);
if (fd != -1) {
  w2 = (Cell)fdopen(fd, fileattr[ntype]);
  wior = IOR(w2 == 0);
} else {
  w2 = 0;
  wior = IOR(1);
}
}
NEXT_P1;
sp[1] = (Cell)w2;
TOS = (Cell)wior;
NEXT_P2;
}

I_delete_file:	/* delete-file ( c_addr u -- wior ) */
/*  */
NAME("delete-file")
{
DEF_CA
Char * c_addr;
UCell u;
Cell wior;
NEXT_P0;
c_addr = (Char *) sp[1];
u = (UCell) TOS;
sp += 1;
{
#line 1155 "./primitives"
wior = IOR(unlink(tilde_cstr(c_addr, u, 1))==-1);
}
NEXT_P1;
TOS = (Cell)wior;
NEXT_P2;
}

I_rename_file:	/* rename-file ( c_addr1 u1 c_addr2 u2 -- wior ) */
/*  */
NAME("rename-file")
{
DEF_CA
Char * c_addr1;
UCell u1;
Char * c_addr2;
UCell u2;
Cell wior;
NEXT_P0;
c_addr1 = (Char *) sp[3];
u1 = (UCell) sp[2];
c_addr2 = (Char *) sp[1];
u2 = (UCell) TOS;
sp += 3;
{
#line 1158 "./primitives"
char *s1=tilde_cstr(c_addr2, u2, 1);
wior = IOR(rename(tilde_cstr(c_addr1, u1, 0), s1)==-1);
}
NEXT_P1;
TOS = (Cell)wior;
NEXT_P2;
}

I_file_position:	/* file-position ( wfileid -- ud wior ) */
/*  */
NAME("file-position")
{
DEF_CA
Cell wfileid;
UDCell ud;
Cell wior;
NEXT_P0;
wfileid = (Cell) TOS;
sp += -2;
{
#line 1162 "./primitives"
/* !! use tell and lseek? */
ud = LONG2UD(ftell((FILE *)wfileid));
wior = IOR(UD2LONG(ud)==-1);
}
NEXT_P1;
STORE_DCELL(ud, sp[2], sp[1]);
TOS = (Cell)wior;
NEXT_P2;
}

I_reposition_file:	/* reposition-file ( ud wfileid -- wior ) */
/*  */
NAME("reposition-file")
{
DEF_CA
UDCell ud;
Cell wfileid;
Cell wior;
NEXT_P0;
FETCH_DCELL(ud, sp[2], sp[1]);
wfileid = (Cell) TOS;
sp += 2;
{
#line 1167 "./primitives"
wior = IOR(fseek((FILE *)wfileid, UD2LONG(ud), SEEK_SET)==-1);
}
NEXT_P1;
TOS = (Cell)wior;
NEXT_P2;
}

I_file_size:	/* file-size ( wfileid -- ud wior ) */
/*  */
NAME("file-size")
{
DEF_CA
Cell wfileid;
UDCell ud;
Cell wior;
NEXT_P0;
wfileid = (Cell) TOS;
sp += -2;
{
#line 1170 "./primitives"
struct stat buf;
wior = IOR(fstat(fileno((FILE *)wfileid), &buf)==-1);
ud = LONG2UD(buf.st_size);
}
NEXT_P1;
STORE_DCELL(ud, sp[2], sp[1]);
TOS = (Cell)wior;
NEXT_P2;
}

I_resize_file:	/* resize-file ( ud wfileid -- wior ) */
/*  */
NAME("resize-file")
{
DEF_CA
UDCell ud;
Cell wfileid;
Cell wior;
NEXT_P0;
FETCH_DCELL(ud, sp[2], sp[1]);
wfileid = (Cell) TOS;
sp += 2;
{
#line 1175 "./primitives"
wior = IOR(ftruncate(fileno((FILE *)wfileid), UD2LONG(ud))==-1);
}
NEXT_P1;
TOS = (Cell)wior;
NEXT_P2;
}

I_read_file:	/* read-file ( c_addr u1 wfileid -- u2 wior ) */
/*  */
NAME("read-file")
{
DEF_CA
Char * c_addr;
UCell u1;
Cell wfileid;
UCell u2;
Cell wior;
NEXT_P0;
c_addr = (Char *) sp[2];
u1 = (UCell) sp[1];
wfileid = (Cell) TOS;
sp += 1;
{
#line 1178 "./primitives"
/* !! fread does not guarantee enough */
u2 = fread(c_addr, sizeof(Char), u1, (FILE *)wfileid);
wior = FILEIO(u2<u1 && ferror((FILE *)wfileid));
/* !! is the value of ferror errno-compatible? */
if (wior)
  clearerr((FILE *)wfileid);
}
NEXT_P1;
sp[1] = (Cell)u2;
TOS = (Cell)wior;
NEXT_P2;
}

I_read_line:	/* read-line ( c_addr u1 wfileid -- u2 flag wior ) */
/*  */
NAME("read-line")
{
DEF_CA
Char * c_addr;
UCell u1;
Cell wfileid;
UCell u2;
Bool flag;
Cell wior;
NEXT_P0;
c_addr = (Char *) sp[2];
u1 = (UCell) sp[1];
wfileid = (Cell) TOS;
{
#line 1186 "./primitives"
/*
Cell c;
flag=-1;
for(u2=0; u2<u1; u2++)
{
   *c_addr++ = (Char)(c = getc((FILE *)wfileid));
   if(c=='\n') break;
   if(c==EOF)
     {
	flag=FLAG(u2!=0);
	break;
     }
}
wior=FILEIO(ferror((FILE *)wfileid));
*/
if ((flag=FLAG(!feof((FILE *)wfileid) &&
	       fgets(c_addr,u1+1,(FILE *)wfileid) != NULL))) {
  wior=FILEIO(ferror((FILE *)wfileid)); /* !! ior? */
  if (wior)
    clearerr((FILE *)wfileid);
  u2 = strlen(c_addr);
  u2-=((u2>0) && (c_addr[u2-1]==NEWLINE));
}
else {
  wior=0;
  u2=0;
}
}
NEXT_P1;
sp[2] = (Cell)u2;
sp[1] = (Cell)flag;
TOS = (Cell)wior;
NEXT_P2;
}

I_write_file:	/* write-file ( c_addr u1 wfileid -- wior ) */
/*  */
NAME("write-file")
{
DEF_CA
Char * c_addr;
UCell u1;
Cell wfileid;
Cell wior;
NEXT_P0;
c_addr = (Char *) sp[2];
u1 = (UCell) sp[1];
wfileid = (Cell) TOS;
sp += 2;
{
#line 1215 "./primitives"
/* !! fwrite does not guarantee enough */
{
  Cell u2 = fwrite(c_addr, sizeof(Char), u1, (FILE *)wfileid);
  wior = FILEIO(u2<u1 && ferror((FILE *)wfileid));
  if (wior)
    clearerr((FILE *)wfileid);
}
}
NEXT_P1;
TOS = (Cell)wior;
NEXT_P2;
}

I_emit_file:	/* emit-file ( c wfileid -- wior ) */
/*  */
NAME("emit-file")
{
DEF_CA
Char c;
Cell wfileid;
Cell wior;
NEXT_P0;
c = (Char) sp[1];
wfileid = (Cell) TOS;
sp += 1;
{
#line 1224 "./primitives"
wior = FILEIO(putc(c, (FILE *)wfileid)==EOF);
if (wior)
  clearerr((FILE *)wfileid);
}
NEXT_P1;
TOS = (Cell)wior;
NEXT_P2;
}

I_flush_file:	/* flush-file ( wfileid -- wior ) */
/*  */
NAME("flush-file")
{
DEF_CA
Cell wfileid;
Cell wior;
NEXT_P0;
wfileid = (Cell) TOS;
{
#line 1229 "./primitives"
wior = IOR(fflush((FILE *) wfileid)==EOF);
}
NEXT_P1;
TOS = (Cell)wior;
NEXT_P2;
}

I_file_status:	/* file-status ( c_addr u -- ntype wior ) */
/*  */
NAME("file-status")
{
DEF_CA
Char * c_addr;
UCell u;
Cell ntype;
Cell wior;
NEXT_P0;
c_addr = (Char *) sp[1];
u = (UCell) TOS;
{
#line 1232 "./primitives"
char *filename=tilde_cstr(c_addr, u, 1);
if (access (filename, F_OK) != 0) {
  ntype=0;
  wior=IOR(1);
}
else if (access (filename, R_OK | W_OK) == 0) {
  ntype=2; /* r/w */
  wior=0;
}
else if (access (filename, R_OK) == 0) {
  ntype=0; /* r/o */
  wior=0;
}
else if (access (filename, W_OK) == 0) {
  ntype=4; /* w/o */
  wior=0;
}
else {
  ntype=1; /* well, we cannot access the file, but better deliver a legal
	    access mode (r/o bin), so we get a decent error later upon open. */
  wior=0;
}
}
NEXT_P1;
sp[1] = (Cell)ntype;
TOS = (Cell)wior;
NEXT_P2;
}

I_stdout:	/* stdout ( -- wfileid ) */
/*  */
NAME("stdout")
{
DEF_CA
Cell wfileid;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 1256 "./primitives"
wfileid = (Cell)stdout;
}
NEXT_P1;
TOS = (Cell)wfileid;
NEXT_P2;
}

I_stderr:	/* stderr ( -- wfileid ) */
/*  */
NAME("stderr")
{
DEF_CA
Cell wfileid;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 1259 "./primitives"
wfileid = (Cell)stderr;
}
NEXT_P1;
TOS = (Cell)wfileid;
NEXT_P2;
}

I_f_equals:	/* f= ( r1 r2 -- f ) */
/*  */
NAME("f=")
{
DEF_CA
Float r1;
Float r2;
Bool f;
NEXT_P0;
IF_TOS(sp[0] = TOS);
r1 = fp[1];
r2 = FTOS;
sp += -1;
fp += 2;
{
#line 1261 "./primitives"
f = FLAG(r1==r2);
#line 1261
}
NEXT_P1;
TOS = (Cell)f;
IF_FTOS(FTOS = fp[0]);
NEXT_P2;
}

I_f_different:	/* f<> ( r1 r2 -- f ) */
/*  */
NAME("f<>")
{
DEF_CA
Float r1;
Float r2;
Bool f;
NEXT_P0;
IF_TOS(sp[0] = TOS);
r1 = fp[1];
r2 = FTOS;
sp += -1;
fp += 2;
{
#line 1261 "./primitives"
f = FLAG(r1!=r2);
#line 1261
}
NEXT_P1;
TOS = (Cell)f;
IF_FTOS(FTOS = fp[0]);
NEXT_P2;
}

I_f_less:	/* f< ( r1 r2 -- f ) */
/*  */
NAME("f<")
{
DEF_CA
Float r1;
Float r2;
Bool f;
NEXT_P0;
IF_TOS(sp[0] = TOS);
r1 = fp[1];
r2 = FTOS;
sp += -1;
fp += 2;
{
#line 1261 "./primitives"
f = FLAG(r1<r2);
#line 1261
}
NEXT_P1;
TOS = (Cell)f;
IF_FTOS(FTOS = fp[0]);
NEXT_P2;
}

I_f_greater:	/* f> ( r1 r2 -- f ) */
/*  */
NAME("f>")
{
DEF_CA
Float r1;
Float r2;
Bool f;
NEXT_P0;
IF_TOS(sp[0] = TOS);
r1 = fp[1];
r2 = FTOS;
sp += -1;
fp += 2;
{
#line 1261 "./primitives"
f = FLAG(r1>r2);
#line 1261
}
NEXT_P1;
TOS = (Cell)f;
IF_FTOS(FTOS = fp[0]);
NEXT_P2;
}

I_f_less_or_equal:	/* f<= ( r1 r2 -- f ) */
/*  */
NAME("f<=")
{
DEF_CA
Float r1;
Float r2;
Bool f;
NEXT_P0;
IF_TOS(sp[0] = TOS);
r1 = fp[1];
r2 = FTOS;
sp += -1;
fp += 2;
{
#line 1261 "./primitives"
f = FLAG(r1<=r2);
#line 1261
}
NEXT_P1;
TOS = (Cell)f;
IF_FTOS(FTOS = fp[0]);
NEXT_P2;
}

I_f_greater_or_equal:	/* f>= ( r1 r2 -- f ) */
/*  */
NAME("f>=")
{
DEF_CA
Float r1;
Float r2;
Bool f;
NEXT_P0;
IF_TOS(sp[0] = TOS);
r1 = fp[1];
r2 = FTOS;
sp += -1;
fp += 2;
{
#line 1261 "./primitives"
f = FLAG(r1>=r2);
#line 1261
}
NEXT_P1;
TOS = (Cell)f;
IF_FTOS(FTOS = fp[0]);
NEXT_P2;
}

I_f_zero_equals:	/* f0= ( r -- f ) */
/*  */
NAME("f0=")
{
DEF_CA
Float r;
Bool f;
NEXT_P0;
IF_TOS(sp[0] = TOS);
r = FTOS;
sp += -1;
fp += 1;
{
#line 1262 "./primitives"
f = FLAG(r==0.);
#line 1262
}
NEXT_P1;
TOS = (Cell)f;
IF_FTOS(FTOS = fp[0]);
NEXT_P2;
}

I_f_zero_different:	/* f0<> ( r -- f ) */
/*  */
NAME("f0<>")
{
DEF_CA
Float r;
Bool f;
NEXT_P0;
IF_TOS(sp[0] = TOS);
r = FTOS;
sp += -1;
fp += 1;
{
#line 1262 "./primitives"
f = FLAG(r!=0.);
#line 1262
}
NEXT_P1;
TOS = (Cell)f;
IF_FTOS(FTOS = fp[0]);
NEXT_P2;
}

I_f_zero_less:	/* f0< ( r -- f ) */
/*  */
NAME("f0<")
{
DEF_CA
Float r;
Bool f;
NEXT_P0;
IF_TOS(sp[0] = TOS);
r = FTOS;
sp += -1;
fp += 1;
{
#line 1262 "./primitives"
f = FLAG(r<0.);
#line 1262
}
NEXT_P1;
TOS = (Cell)f;
IF_FTOS(FTOS = fp[0]);
NEXT_P2;
}

I_f_zero_greater:	/* f0> ( r -- f ) */
/*  */
NAME("f0>")
{
DEF_CA
Float r;
Bool f;
NEXT_P0;
IF_TOS(sp[0] = TOS);
r = FTOS;
sp += -1;
fp += 1;
{
#line 1262 "./primitives"
f = FLAG(r>0.);
#line 1262
}
NEXT_P1;
TOS = (Cell)f;
IF_FTOS(FTOS = fp[0]);
NEXT_P2;
}

I_f_zero_less_or_equal:	/* f0<= ( r -- f ) */
/*  */
NAME("f0<=")
{
DEF_CA
Float r;
Bool f;
NEXT_P0;
IF_TOS(sp[0] = TOS);
r = FTOS;
sp += -1;
fp += 1;
{
#line 1262 "./primitives"
f = FLAG(r<=0.);
#line 1262
}
NEXT_P1;
TOS = (Cell)f;
IF_FTOS(FTOS = fp[0]);
NEXT_P2;
}

I_f_zero_greater_or_equal:	/* f0>= ( r -- f ) */
/*  */
NAME("f0>=")
{
DEF_CA
Float r;
Bool f;
NEXT_P0;
IF_TOS(sp[0] = TOS);
r = FTOS;
sp += -1;
fp += 1;
{
#line 1262 "./primitives"
f = FLAG(r>=0.);
#line 1262
}
NEXT_P1;
TOS = (Cell)f;
IF_FTOS(FTOS = fp[0]);
NEXT_P2;
}

I_d_to_f:	/* d>f ( d -- r ) */
/*  */
NAME("d>f")
{
DEF_CA
DCell d;
Float r;
NEXT_P0;
IF_FTOS(fp[0] = FTOS);
FETCH_DCELL(d, sp[1], TOS);
sp += 2;
fp += -1;
{
#line 1265 "./primitives"
#ifdef BUGGY_LONG_LONG
extern double ldexp(double x, int exp);
r = ldexp((Float)d.hi,CELL_BITS) + (Float)d.lo;
#else
r = d;
#endif
}
NEXT_P1;
FTOS = r;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_f_to_d:	/* f>d ( r -- d ) */
/*  */
NAME("f>d")
{
DEF_CA
Float r;
DCell d;
NEXT_P0;
IF_TOS(sp[0] = TOS);
r = FTOS;
sp += -2;
fp += 1;
{
#line 1273 "./primitives"
#ifdef BUGGY_LONG_LONG
d.hi = ldexp(r,-CELL_BITS) - (r<0);
d.lo = r-ldexp((Float)d.hi,CELL_BITS);
#else
d = r;
#endif
}
NEXT_P1;
STORE_DCELL(d, sp[1], TOS);
IF_FTOS(FTOS = fp[0]);
NEXT_P2;
}

I_f_store:	/* f! ( r f_addr -- ) */
/*  */
NAME("f!")
{
DEF_CA
Float r;
Float * f_addr;
NEXT_P0;
r = FTOS;
f_addr = (Float *) TOS;
sp += 1;
fp += 1;
{
#line 1281 "./primitives"
*f_addr = r;
}
NEXT_P1;
IF_FTOS(FTOS = fp[0]);
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_f_fetch:	/* f@ ( f_addr -- r ) */
/*  */
NAME("f@")
{
DEF_CA
Float * f_addr;
Float r;
NEXT_P0;
IF_FTOS(fp[0] = FTOS);
f_addr = (Float *) TOS;
sp += 1;
fp += -1;
{
#line 1284 "./primitives"
r = *f_addr;
}
NEXT_P1;
FTOS = r;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_d_f_fetch:	/* df@ ( df_addr -- r ) */
/*  */
NAME("df@")
{
DEF_CA
DFloat * df_addr;
Float r;
NEXT_P0;
IF_FTOS(fp[0] = FTOS);
df_addr = (DFloat *) TOS;
sp += 1;
fp += -1;
{
#line 1287 "./primitives"
#ifdef IEEE_FP
r = *df_addr;
#else
!! df@
#endif
}
NEXT_P1;
FTOS = r;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_d_f_store:	/* df! ( r df_addr -- ) */
/*  */
NAME("df!")
{
DEF_CA
Float r;
DFloat * df_addr;
NEXT_P0;
r = FTOS;
df_addr = (DFloat *) TOS;
sp += 1;
fp += 1;
{
#line 1294 "./primitives"
#ifdef IEEE_FP
*df_addr = r;
#else
!! df!
#endif
}
NEXT_P1;
IF_FTOS(FTOS = fp[0]);
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_s_f_fetch:	/* sf@ ( sf_addr -- r ) */
/*  */
NAME("sf@")
{
DEF_CA
SFloat * sf_addr;
Float r;
NEXT_P0;
IF_FTOS(fp[0] = FTOS);
sf_addr = (SFloat *) TOS;
sp += 1;
fp += -1;
{
#line 1301 "./primitives"
#ifdef IEEE_FP
r = *sf_addr;
#else
!! sf@
#endif
}
NEXT_P1;
FTOS = r;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_s_f_store:	/* sf! ( r sf_addr -- ) */
/*  */
NAME("sf!")
{
DEF_CA
Float r;
SFloat * sf_addr;
NEXT_P0;
r = FTOS;
sf_addr = (SFloat *) TOS;
sp += 1;
fp += 1;
{
#line 1308 "./primitives"
#ifdef IEEE_FP
*sf_addr = r;
#else
!! sf!
#endif
}
NEXT_P1;
IF_FTOS(FTOS = fp[0]);
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_f_plus:	/* f+ ( r1 r2 -- r3 ) */
/*  */
NAME("f+")
{
DEF_CA
Float r1;
Float r2;
Float r3;
NEXT_P0;
r1 = fp[1];
r2 = FTOS;
fp += 1;
{
#line 1315 "./primitives"
r3 = r1+r2;
}
NEXT_P1;
FTOS = r3;
NEXT_P2;
}

I_f_minus:	/* f- ( r1 r2 -- r3 ) */
/*  */
NAME("f-")
{
DEF_CA
Float r1;
Float r2;
Float r3;
NEXT_P0;
r1 = fp[1];
r2 = FTOS;
fp += 1;
{
#line 1318 "./primitives"
r3 = r1-r2;
}
NEXT_P1;
FTOS = r3;
NEXT_P2;
}

I_f_star:	/* f* ( r1 r2 -- r3 ) */
/*  */
NAME("f*")
{
DEF_CA
Float r1;
Float r2;
Float r3;
NEXT_P0;
r1 = fp[1];
r2 = FTOS;
fp += 1;
{
#line 1321 "./primitives"
r3 = r1*r2;
}
NEXT_P1;
FTOS = r3;
NEXT_P2;
}

I_f_slash:	/* f/ ( r1 r2 -- r3 ) */
/*  */
NAME("f/")
{
DEF_CA
Float r1;
Float r2;
Float r3;
NEXT_P0;
r1 = fp[1];
r2 = FTOS;
fp += 1;
{
#line 1324 "./primitives"
r3 = r1/r2;
}
NEXT_P1;
FTOS = r3;
NEXT_P2;
}

I_f_star_star:	/* f** ( r1 r2 -- r3 ) */
/* @i{r3} is @i{r1} raised to the @i{r2}th power */
NAME("f**")
{
DEF_CA
Float r1;
Float r2;
Float r3;
NEXT_P0;
r1 = fp[1];
r2 = FTOS;
fp += 1;
{
#line 1328 "./primitives"
r3 = pow(r1,r2);
}
NEXT_P1;
FTOS = r3;
NEXT_P2;
}

I_fnegate:	/* fnegate ( r1 -- r2 ) */
/*  */
NAME("fnegate")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1331 "./primitives"
r2 = - r1;
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_fdrop:	/* fdrop ( r -- ) */
/*  */
NAME("fdrop")
{
DEF_CA
Float r;
NEXT_P0;
r = FTOS;
fp += 1;
{
#line 1334 "./primitives"
}
NEXT_P1;
IF_FTOS(FTOS = fp[0]);
NEXT_P2;
}

I_fdup:	/* fdup ( r -- r r ) */
/*  */
NAME("fdup")
{
DEF_CA
Float r;
NEXT_P0;
r = FTOS;
fp += -1;
{
#line 1336 "./primitives"
}
NEXT_P1;
IF_FTOS(fp[1] = r;);
FTOS = r;
NEXT_P2;
}

I_fswap:	/* fswap ( r1 r2 -- r2 r1 ) */
/*  */
NAME("fswap")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = fp[1];
r2 = FTOS;
{
#line 1338 "./primitives"
}
NEXT_P1;
fp[1] = r2;
FTOS = r1;
NEXT_P2;
}

I_fover:	/* fover ( r1 r2 -- r1 r2 r1 ) */
/*  */
NAME("fover")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = fp[1];
r2 = FTOS;
fp += -1;
{
#line 1340 "./primitives"
}
NEXT_P1;
IF_FTOS(fp[1] = r2;);
FTOS = r1;
NEXT_P2;
}

I_frot:	/* frot ( r1 r2 r3 -- r2 r3 r1 ) */
/*  */
NAME("frot")
{
DEF_CA
Float r1;
Float r2;
Float r3;
NEXT_P0;
r1 = fp[2];
r2 = fp[1];
r3 = FTOS;
{
#line 1342 "./primitives"
}
NEXT_P1;
fp[2] = r2;
fp[1] = r3;
FTOS = r1;
NEXT_P2;
}

I_fnip:	/* fnip ( r1 r2 -- r2 ) */
/*  */
NAME("fnip")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = fp[1];
r2 = FTOS;
fp += 1;
{
#line 1344 "./primitives"
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_ftuck:	/* ftuck ( r1 r2 -- r2 r1 r2 ) */
/*  */
NAME("ftuck")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = fp[1];
r2 = FTOS;
fp += -1;
{
#line 1346 "./primitives"
}
NEXT_P1;
fp[2] = r2;
fp[1] = r1;
FTOS = r2;
NEXT_P2;
}

I_float_plus:	/* float+ ( f_addr1 -- f_addr2 ) */
/*  */
NAME("float+")
{
DEF_CA
Float * f_addr1;
Float * f_addr2;
NEXT_P0;
f_addr1 = (Float *) TOS;
{
#line 1348 "./primitives"
f_addr2 = f_addr1+1;
}
NEXT_P1;
TOS = (Cell)f_addr2;
NEXT_P2;
}

I_floats:	/* floats ( n1 -- n2 ) */
/*  */
NAME("floats")
{
DEF_CA
Cell n1;
Cell n2;
NEXT_P0;
n1 = (Cell) TOS;
{
#line 1351 "./primitives"
n2 = n1*sizeof(Float);
}
NEXT_P1;
TOS = (Cell)n2;
NEXT_P2;
}

I_floor:	/* floor ( r1 -- r2 ) */
/* round towards the next smaller integral value, i.e., round toward negative infinity */
NAME("floor")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1355 "./primitives"
/* !! unclear wording */
r2 = floor(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_fround:	/* fround ( r1 -- r2 ) */
/* round to the nearest integral value */
NAME("fround")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1360 "./primitives"
/* !! unclear wording */
#ifdef HAVE_RINT
r2 = rint(r1);
#else
r2 = floor(r1+0.5);
/* !! This is not quite true to the rounding rules given in the standard */
#endif
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_fmax:	/* fmax ( r1 r2 -- r3 ) */
/*  */
NAME("fmax")
{
DEF_CA
Float r1;
Float r2;
Float r3;
NEXT_P0;
r1 = fp[1];
r2 = FTOS;
fp += 1;
{
#line 1369 "./primitives"
if (r1<r2)
  r3 = r2;
else
  r3 = r1;
}
NEXT_P1;
FTOS = r3;
NEXT_P2;
}

I_fmin:	/* fmin ( r1 r2 -- r3 ) */
/*  */
NAME("fmin")
{
DEF_CA
Float r1;
Float r2;
Float r3;
NEXT_P0;
r1 = fp[1];
r2 = FTOS;
fp += 1;
{
#line 1375 "./primitives"
if (r1<r2)
  r3 = r1;
else
  r3 = r2;
}
NEXT_P1;
FTOS = r3;
NEXT_P2;
}

I_represent:	/* represent ( r c_addr u -- n f1 f2 ) */
/*  */
NAME("represent")
{
DEF_CA
Float r;
Char * c_addr;
UCell u;
Cell n;
Bool f1;
Bool f2;
NEXT_P0;
r = FTOS;
c_addr = (Char *) sp[1];
u = (UCell) TOS;
sp += -1;
fp += 1;
{
#line 1381 "./primitives"
char *sig;
Cell flag;
Cell decpt;
sig=ecvt(r, u, (int *)&decpt, (int *)&flag);
n=(r==0 ? 1 : decpt);
f1=FLAG(flag!=0);
f2=FLAG(isdigit(sig[0])!=0);
memmove(c_addr,sig,u);
}
NEXT_P1;
sp[2] = (Cell)n;
sp[1] = (Cell)f1;
TOS = (Cell)f2;
IF_FTOS(FTOS = fp[0]);
NEXT_P2;
}

I_to_float:	/* >float ( c_addr u -- flag ) */
/*  */
NAME(">float")
{
DEF_CA
Char * c_addr;
UCell u;
Bool flag;
NEXT_P0;
c_addr = (Char *) sp[1];
u = (UCell) TOS;
sp += 1;
{
#line 1391 "./primitives"
/* real signature: c_addr u -- r t / f */
Float r;
char *number=cstr(c_addr, u, 1);
char *endconv;
while(isspace(number[--u]) && u>0);
switch(number[u])
{
   case 'd':
   case 'D':
   case 'e':
   case 'E':  break;
   default :  u++; break;
}
number[u]='\0';
r=strtod(number,&endconv);
if((flag=FLAG(!(Cell)*endconv)))
{
   IF_FTOS(fp[0] = FTOS);
   fp += -1;
   FTOS = r;
}
else if(*endconv=='d' || *endconv=='D')
{
   *endconv='E';
   r=strtod(number,&endconv);
   if((flag=FLAG(!(Cell)*endconv)))
     {
	IF_FTOS(fp[0] = FTOS);
	fp += -1;
	FTOS = r;
     }
}
}
NEXT_P1;
TOS = (Cell)flag;
NEXT_P2;
}

I_fabs:	/* fabs ( r1 -- r2 ) */
/*  */
NAME("fabs")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1425 "./primitives"
r2 = fabs(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_facos:	/* facos ( r1 -- r2 ) */
/*  */
NAME("facos")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1428 "./primitives"
r2 = acos(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_fasin:	/* fasin ( r1 -- r2 ) */
/*  */
NAME("fasin")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1431 "./primitives"
r2 = asin(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_fatan:	/* fatan ( r1 -- r2 ) */
/*  */
NAME("fatan")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1434 "./primitives"
r2 = atan(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_fatan2:	/* fatan2 ( r1 r2 -- r3 ) */
/* @i{r1/r2}=tan@i{r3}. The standard does not require, but probably
intends this to be the inverse of @code{fsincos}. In gforth it is. */
NAME("fatan2")
{
DEF_CA
Float r1;
Float r2;
Float r3;
NEXT_P0;
r1 = fp[1];
r2 = FTOS;
fp += 1;
{
#line 1439 "./primitives"
r3 = atan2(r1,r2);
}
NEXT_P1;
FTOS = r3;
NEXT_P2;
}

I_fcos:	/* fcos ( r1 -- r2 ) */
/*  */
NAME("fcos")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1442 "./primitives"
r2 = cos(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_fexp:	/* fexp ( r1 -- r2 ) */
/*  */
NAME("fexp")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1445 "./primitives"
r2 = exp(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_fexpm1:	/* fexpm1 ( r1 -- r2 ) */
/* @i{r2}=@i{e}**@i{r1}@minus{}1 */
NAME("fexpm1")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1449 "./primitives"
#ifdef HAVE_EXPM1
extern double expm1(double);
r2 = expm1(r1);
#else
r2 = exp(r1)-1.;
#endif
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_fln:	/* fln ( r1 -- r2 ) */
/*  */
NAME("fln")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1457 "./primitives"
r2 = log(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_flnp1:	/* flnp1 ( r1 -- r2 ) */
/* @i{r2}=ln(@i{r1}+1) */
NAME("flnp1")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1461 "./primitives"
#ifdef HAVE_LOG1P
extern double log1p(double);
r2 = log1p(r1);
#else
r2 = log(r1+1.);
#endif
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_flog:	/* flog ( r1 -- r2 ) */
/* the decimal logarithm */
NAME("flog")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1470 "./primitives"
r2 = log10(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_falog:	/* falog ( r1 -- r2 ) */
/* @i{r2}=10**@i{r1} */
NAME("falog")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1474 "./primitives"
extern double pow10(double);
r2 = pow10(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_fsin:	/* fsin ( r1 -- r2 ) */
/*  */
NAME("fsin")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1478 "./primitives"
r2 = sin(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_fsincos:	/* fsincos ( r1 -- r2 r3 ) */
/* @i{r2}=sin(@i{r1}), @i{r3}=cos(@i{r1}) */
NAME("fsincos")
{
DEF_CA
Float r1;
Float r2;
Float r3;
NEXT_P0;
r1 = FTOS;
fp += -1;
{
#line 1482 "./primitives"
r2 = sin(r1);
r3 = cos(r1);
}
NEXT_P1;
fp[1] = r2;
FTOS = r3;
NEXT_P2;
}

I_fsqrt:	/* fsqrt ( r1 -- r2 ) */
/*  */
NAME("fsqrt")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1486 "./primitives"
r2 = sqrt(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_ftan:	/* ftan ( r1 -- r2 ) */
/*  */
NAME("ftan")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1489 "./primitives"
r2 = tan(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_fsinh:	/* fsinh ( r1 -- r2 ) */
/*  */
NAME("fsinh")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1494 "./primitives"
r2 = sinh(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_fcosh:	/* fcosh ( r1 -- r2 ) */
/*  */
NAME("fcosh")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1499 "./primitives"
r2 = cosh(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_ftanh:	/* ftanh ( r1 -- r2 ) */
/*  */
NAME("ftanh")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1504 "./primitives"
r2 = tanh(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_fasinh:	/* fasinh ( r1 -- r2 ) */
/*  */
NAME("fasinh")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1509 "./primitives"
r2 = asinh(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_facosh:	/* facosh ( r1 -- r2 ) */
/*  */
NAME("facosh")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1514 "./primitives"
r2 = acosh(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_fatanh:	/* fatanh ( r1 -- r2 ) */
/*  */
NAME("fatanh")
{
DEF_CA
Float r1;
Float r2;
NEXT_P0;
r1 = FTOS;
{
#line 1519 "./primitives"
r2 = atanh(r1);
}
NEXT_P1;
FTOS = r2;
NEXT_P2;
}

I_s_floats:	/* sfloats ( n1 -- n2 ) */
/*  */
NAME("sfloats")
{
DEF_CA
Cell n1;
Cell n2;
NEXT_P0;
n1 = (Cell) TOS;
{
#line 1525 "./primitives"
n2 = n1*sizeof(SFloat);
}
NEXT_P1;
TOS = (Cell)n2;
NEXT_P2;
}

I_d_floats:	/* dfloats ( n1 -- n2 ) */
/*  */
NAME("dfloats")
{
DEF_CA
Cell n1;
Cell n2;
NEXT_P0;
n1 = (Cell) TOS;
{
#line 1528 "./primitives"
n2 = n1*sizeof(DFloat);
}
NEXT_P1;
TOS = (Cell)n2;
NEXT_P2;
}

I_aligned:	/* aligned ( c_addr -- a_addr ) */
/*  */
NAME("aligned")
{
DEF_CA
Char * c_addr;
Cell * a_addr;
NEXT_P0;
c_addr = (Char *) TOS;
{
#line 1531 "./primitives"
a_addr = (Cell *)((((Cell)c_addr)+(sizeof(Cell)-1))&(-sizeof(Cell)));
}
NEXT_P1;
TOS = (Cell)a_addr;
NEXT_P2;
}

I_f_aligned:	/* faligned ( c_addr -- f_addr ) */
/*  */
NAME("faligned")
{
DEF_CA
Char * c_addr;
Float * f_addr;
NEXT_P0;
c_addr = (Char *) TOS;
{
#line 1536 "./primitives"
f_addr = (Float *)((((Cell)c_addr)+(sizeof(Float)-1))&(-sizeof(Float)));
}
NEXT_P1;
TOS = (Cell)f_addr;
NEXT_P2;
}

I_s_f_aligned:	/* sfaligned ( c_addr -- sf_addr ) */
/*  */
NAME("sfaligned")
{
DEF_CA
Char * c_addr;
SFloat * sf_addr;
NEXT_P0;
c_addr = (Char *) TOS;
{
#line 1541 "./primitives"
sf_addr = (SFloat *)((((Cell)c_addr)+(sizeof(SFloat)-1))&(-sizeof(SFloat)));
}
NEXT_P1;
TOS = (Cell)sf_addr;
NEXT_P2;
}

I_d_f_aligned:	/* dfaligned ( c_addr -- df_addr ) */
/*  */
NAME("dfaligned")
{
DEF_CA
Char * c_addr;
DFloat * df_addr;
NEXT_P0;
c_addr = (Char *) TOS;
{
#line 1546 "./primitives"
df_addr = (DFloat *)((((Cell)c_addr)+(sizeof(DFloat)-1))&(-sizeof(DFloat)));
}
NEXT_P1;
TOS = (Cell)df_addr;
NEXT_P2;
}

I_to_body:	/* >body ( xt -- a_addr ) */
/*  */
NAME(">body")
{
DEF_CA
Xt xt;
Cell * a_addr;
NEXT_P0;
xt = (Xt) TOS;
{
#line 1556 "./primitives"
a_addr = PFA(xt);
}
NEXT_P1;
TOS = (Cell)a_addr;
NEXT_P2;
}

I_to_code_address:	/* >code-address ( xt -- c_addr ) */
/* c_addr is the code address of the word xt */
NAME(">code-address")
{
DEF_CA
Xt xt;
Char * c_addr;
NEXT_P0;
xt = (Xt) TOS;
{
#line 1560 "./primitives"
/* !! This behaves installation-dependently for DOES-words */
c_addr = CODE_ADDRESS(xt);
}
NEXT_P1;
TOS = (Cell)c_addr;
NEXT_P2;
}

I_to_does_code:	/* >does-code ( xt -- a_addr ) */
/* If xt ist the execution token of a defining-word-defined word,
a_addr is the start of the Forth code after the DOES>;
Otherwise a_addr is 0. */
NAME(">does-code")
{
DEF_CA
Xt xt;
Cell * a_addr;
NEXT_P0;
xt = (Xt) TOS;
{
#line 1567 "./primitives"
a_addr = (Cell *)DOES_CODE(xt);
}
NEXT_P1;
TOS = (Cell)a_addr;
NEXT_P2;
}

I_code_address_store:	/* code-address! ( c_addr xt -- ) */
/* Creates a code field with code address c_addr at xt */
NAME("code-address!")
{
DEF_CA
Char * c_addr;
Xt xt;
NEXT_P0;
c_addr = (Char *) sp[1];
xt = (Xt) TOS;
sp += 2;
{
#line 1571 "./primitives"
MAKE_CF(xt, c_addr);
CACHE_FLUSH(xt,PFA(0));
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_does_code_store:	/* does-code! ( a_addr xt -- ) */
/* creates a code field at xt for a defining-word-defined word; a_addr
is the start of the Forth code after DOES> */
NAME("does-code!")
{
DEF_CA
Cell * a_addr;
Xt xt;
NEXT_P0;
a_addr = (Cell *) sp[1];
xt = (Xt) TOS;
sp += 2;
{
#line 1577 "./primitives"
MAKE_DOES_CF(xt, a_addr);
CACHE_FLUSH(xt,PFA(0));
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_does_handler_store:	/* does-handler! ( a_addr -- ) */
/* creates a DOES>-handler at address a_addr. a_addr usually points
just behind a DOES>. */
NAME("does-handler!")
{
DEF_CA
Cell * a_addr;
NEXT_P0;
a_addr = (Cell *) TOS;
sp += 1;
{
#line 1583 "./primitives"
MAKE_DOES_HANDLER(a_addr);
CACHE_FLUSH(a_addr,DOES_HANDLER_SIZE);
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_slash_does_handler:	/* /does-handler ( -- n ) */
/* the size of a does-handler (includes possible padding) */
NAME("/does-handler")
{
DEF_CA
Cell n;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 1588 "./primitives"
/* !! a constant or environmental query might be better */
n = DOES_HANDLER_SIZE;
}
NEXT_P1;
TOS = (Cell)n;
NEXT_P2;
}

I_threading_method:	/* threading-method ( -- n ) */
/* 0 if the engine is direct threaded. */
NAME("threading-method")
{
DEF_CA
Cell n;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 1593 "./primitives"
#if defined(DIRECT_THREADED)
n=0;
#else
n=1;
#endif
}
NEXT_P1;
TOS = (Cell)n;
NEXT_P2;
}

I_flush_icache:	/* flush-icache ( c_addr u -- ) */
/* Make sure that the instruction cache of the processor (if there is
one) does not contain stale data at @var{c_addr} and @var{u} bytes
afterwards. @code{END-CODE} performs a @code{flush-icache}
automatically. Caveat: @code{flush-icache} might not work on your
installation; this is usually the case if direct threading is not
supported on your machine (take a look at your @file{machine.h}) and
your machine has a separate instruction cache. In such cases,
@code{flush-icache} does nothing instead of flushing the instruction
cache. */
NAME("flush-icache")
{
DEF_CA
Char * c_addr;
UCell u;
NEXT_P0;
c_addr = (Char *) sp[1];
u = (UCell) TOS;
sp += 2;
{
#line 1609 "./primitives"
FLUSH_ICACHE(c_addr,u);
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_toupper:	/* toupper ( c1 -- c2 ) */
/*  */
NAME("toupper")
{
DEF_CA
Char c1;
Char c2;
NEXT_P0;
c1 = (Char) TOS;
{
#line 1612 "./primitives"
c2 = toupper(c1);
}
NEXT_P1;
TOS = (Cell)c2;
NEXT_P2;
}

I_fetch_local_number:	/* @local# ( -- w ) */
/*  */
NAME("@local#")
{
DEF_CA
Cell w;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 1616 "./primitives"
w = *(Cell *)(lp+(Cell)NEXT_INST);
INC_IP(1);
}
NEXT_P1;
TOS = (Cell)w;
NEXT_P2;
}

I_fetch_local_zero:	/* @local0 ( -- w ) */
/*  */
NAME("@local0")
{
DEF_CA
Cell w;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 1620 "./primitives"
w = *(Cell *)(lp+0*sizeof(Cell));
}
NEXT_P1;
TOS = (Cell)w;
NEXT_P2;
}

I_fetch_local_four:	/* @local1 ( -- w ) */
/*  */
NAME("@local1")
{
DEF_CA
Cell w;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 1623 "./primitives"
w = *(Cell *)(lp+1*sizeof(Cell));
}
NEXT_P1;
TOS = (Cell)w;
NEXT_P2;
}

I_fetch_local_eight:	/* @local2 ( -- w ) */
/*  */
NAME("@local2")
{
DEF_CA
Cell w;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 1626 "./primitives"
w = *(Cell *)(lp+2*sizeof(Cell));
}
NEXT_P1;
TOS = (Cell)w;
NEXT_P2;
}

I_fetch_local_twelve:	/* @local3 ( -- w ) */
/*  */
NAME("@local3")
{
DEF_CA
Cell w;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 1629 "./primitives"
w = *(Cell *)(lp+3*sizeof(Cell));
}
NEXT_P1;
TOS = (Cell)w;
NEXT_P2;
}

I_f_fetch_local_number:	/* f@local# ( -- r ) */
/*  */
NAME("f@local#")
{
DEF_CA
Float r;
NEXT_P0;
IF_FTOS(fp[0] = FTOS);
fp += -1;
{
#line 1632 "./primitives"
r = *(Float *)(lp+(Cell)NEXT_INST);
INC_IP(1);
}
NEXT_P1;
FTOS = r;
NEXT_P2;
}

I_f_fetch_local_zero:	/* f@local0 ( -- r ) */
/*  */
NAME("f@local0")
{
DEF_CA
Float r;
NEXT_P0;
IF_FTOS(fp[0] = FTOS);
fp += -1;
{
#line 1636 "./primitives"
r = *(Float *)(lp+0*sizeof(Float));
}
NEXT_P1;
FTOS = r;
NEXT_P2;
}

I_f_fetch_local_eight:	/* f@local1 ( -- r ) */
/*  */
NAME("f@local1")
{
DEF_CA
Float r;
NEXT_P0;
IF_FTOS(fp[0] = FTOS);
fp += -1;
{
#line 1639 "./primitives"
r = *(Float *)(lp+1*sizeof(Float));
}
NEXT_P1;
FTOS = r;
NEXT_P2;
}

I_laddr_number:	/* laddr# ( -- c_addr ) */
/*  */
NAME("laddr#")
{
DEF_CA
Char * c_addr;
NEXT_P0;
IF_TOS(sp[0] = TOS);
sp += -1;
{
#line 1642 "./primitives"
/* this can also be used to implement lp@ */
c_addr = (Char *)(lp+(Cell)NEXT_INST);
INC_IP(1);
}
NEXT_P1;
TOS = (Cell)c_addr;
NEXT_P2;
}

I_lp_plus_store_number:	/* lp+!# ( -- ) */
/* used with negative immediate values it allocates memory on the
local stack, a positive immediate argument drops memory from the local
stack */
NAME("lp+!#")
{
DEF_CA
NEXT_P0;
{
#line 1650 "./primitives"
lp += (Cell)NEXT_INST;
INC_IP(1);
}
NEXT_P1;
NEXT_P2;
}

I_minus_four_lp_plus_store:	/* lp- ( -- ) */
/*  */
NAME("lp-")
{
DEF_CA
NEXT_P0;
{
#line 1654 "./primitives"
lp += -sizeof(Cell);
}
NEXT_P1;
NEXT_P2;
}

I_eight_lp_plus_store:	/* lp+ ( -- ) */
/*  */
NAME("lp+")
{
DEF_CA
NEXT_P0;
{
#line 1657 "./primitives"
lp += sizeof(Float);
}
NEXT_P1;
NEXT_P2;
}

I_sixteen_lp_plus_store:	/* lp+2 ( -- ) */
/*  */
NAME("lp+2")
{
DEF_CA
NEXT_P0;
{
#line 1660 "./primitives"
lp += 2*sizeof(Float);
}
NEXT_P1;
NEXT_P2;
}

I_lp_store:	/* lp! ( c_addr -- ) */
/*  */
NAME("lp!")
{
DEF_CA
Char * c_addr;
NEXT_P0;
c_addr = (Char *) TOS;
sp += 1;
{
#line 1663 "./primitives"
lp = (Address)c_addr;
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_to_l:	/* >l ( w -- ) */
/*  */
NAME(">l")
{
DEF_CA
Cell w;
NEXT_P0;
w = (Cell) TOS;
sp += 1;
{
#line 1666 "./primitives"
lp -= sizeof(Cell);
*(Cell *)lp = w;
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_f_to_l:	/* f>l ( r -- ) */
/*  */
NAME("f>l")
{
DEF_CA
Float r;
NEXT_P0;
r = FTOS;
fp += 1;
{
#line 1670 "./primitives"
lp -= sizeof(Float);
*(Float *)lp = r;
}
NEXT_P1;
IF_FTOS(FTOS = fp[0]);
NEXT_P2;
}

I_up_store:	/* up! ( a_addr -- ) */
/*  */
NAME("up!")
{
DEF_CA
Cell * a_addr;
NEXT_P0;
a_addr = (Cell *) TOS;
sp += 1;
{
#line 1674 "./primitives"
up0=up=(char *)a_addr;
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_call_c:	/* call-c ( w -- ) */
/* Call the C function pointed to by @i{w}. The C function has to
access the stack itself. The stack pointers are exported in the gloabl
variables @code{SP} and @code{FP}. */
NAME("call-c")
{
DEF_CA
Cell w;
NEXT_P0;
w = (Cell) TOS;
sp += 1;
{
#line 1680 "./primitives"
/* This is a first attempt at support for calls to C. This may change in
   the future */
IF_FTOS(fp[0]=FTOS);
FP=fp;
SP=sp;
((void (*)())w)();
sp=SP;
fp=FP;
IF_TOS(TOS=sp[0]);
IF_FTOS(FTOS=fp[0]);
}
NEXT_P1;
IF_TOS(TOS = sp[0]);
NEXT_P2;
}

I_strerror:	/* strerror ( n -- c_addr u ) */
/*  */
NAME("strerror")
{
DEF_CA
Cell n;
Char * c_addr;
UCell u;
NEXT_P0;
n = (Cell) TOS;
sp += -1;
{
#line 1692 "./primitives"
c_addr = strerror(n);
u = strlen(c_addr);
}
NEXT_P1;
sp[1] = (Cell)c_addr;
TOS = (Cell)u;
NEXT_P2;
}

I_strsignal:	/* strsignal ( n -- c_addr u ) */
/*  */
NAME("strsignal")
{
DEF_CA
Cell n;
Char * c_addr;
UCell u;
NEXT_P0;
n = (Cell) TOS;
sp += -1;
{
#line 1696 "./primitives"
c_addr = strsignal(n);
u = strlen(c_addr);
}
NEXT_P1;
sp[1] = (Cell)c_addr;
TOS = (Cell)u;
NEXT_P2;
}

