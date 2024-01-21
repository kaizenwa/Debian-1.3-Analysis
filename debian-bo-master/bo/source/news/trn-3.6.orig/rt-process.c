/* $Id: rt-process.c,v 3.0 1992/12/14 00:14:13 davison Trn $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "intrp.h"
#include "trn.h"
#include "cache.h"
#include "bits.h"
#include "final.h"
#include "ng.h"
#include "ngdata.h"
#include "rcln.h"
#include "util.h"
#include "kfile.h"
#include "hash.h"
#include "rthread.h"
#include "rt-select.h"
#include "INTERN.h"
#include "rt-process.h"

extern HASHTABLE *msgid_hash;

static char *valid_message_id _((char*, char*));
static void link_child _((ARTICLE*));
static void unlink_child _((ARTICLE*));

/* This depends on art being set to the current article number.
*/
ARTICLE *
allocate_article(artnum)
ART_NUM artnum;
{
    register ARTICLE *article;

    /* create an new article */
    if (artnum >= absfirst) {
	if (artnum > lastart)
	    grow_cache(artnum);
	article = article_ptr(artnum);
    } else {
	article = (ARTICLE *)safemalloc(sizeof (ARTICLE));
	bzero((char*)article, sizeof (ARTICLE));
	article->flags |= AF_READ|AF_FAKE|AF_MISSING|AF_TMPMEM;
    }
    return article;
}

void
fix_msgid(msgid)
char *msgid;
{
    register char *cp;

    if ((cp = index(msgid, '@')) != Nullch) {
	while (*++cp) {
	    if (isupper(*cp)) {
		*cp = tolower(*cp);	/* lower-case domain portion */
	    }
	}
    }
}

int
msgid_cmp(key, keylen, data)
char *key;
int keylen;
HASHDATUM data;
{
    ARTICLE *article = (data.dat_ptr? (ARTICLE*)data.dat_ptr
				    : article_ptr(data.dat_len));
    /* We already know that the lengths are equal, just compare the strings */
    return bcmp(key, article->msgid, keylen);
}

SUBJECT *fake_had_subj; /* the fake-turned-real article had this subject */

bool
valid_article(article)
ARTICLE *article;
{
    ARTICLE *ap, *fake_ap;
    char *msgid = article->msgid;
    HASHDATUM data;

    if (msgid) {
	fix_msgid(msgid);
	data = hashfetch(msgid_hash, msgid, strlen(msgid));
	if ((fake_ap = (ARTICLE*)data.dat_ptr) == Nullart) {
	    if (!data.dat_len) {
		data.dat_len = article_num(article);
		hashstorelast(data);
		fake_had_subj = Nullsubj;
		return TRUE;
	    }
	    if (data.dat_len == article_num(article)) {
		fake_had_subj = Nullsubj;
		return TRUE;
	    }
	}

	/* Whenever we replace a fake art with a real one, it's a lot of work
	** cleaning up the references.  Fortunately, this is not often. */
	if (fake_ap) {
	    article->parent = fake_ap->parent;
	    article->child1 = fake_ap->child1;
	    article->sibling = fake_ap->sibling;
	    fake_had_subj = fake_ap->subj;
	    if (fake_ap->autofl) {
		article->autofl |= fake_ap->autofl;
		localkf_changes = 2;
	    }
	    if (curr_artp == fake_ap) {
		curr_artp = article;
		curr_art = article_num(article);
	    }
	    if (recent_artp == fake_ap) {
		recent_artp = article;
		recent_art = article_num(article);
	    }
	    if ((ap = article->parent) != Nullart) {
		if (ap->child1 == fake_ap)
		    ap->child1 = article;
		else {
		    ap = ap->child1;
		    goto sibling_search;
		}
	    } else if (fake_had_subj) {
		register SUBJECT *sp = fake_had_subj;
		if ((ap = sp->thread) == fake_ap) {
		    do {
			sp->thread = article;
			sp = sp->thread_link;
		    } while (sp != fake_had_subj);
		} else {
		  sibling_search:
		    while (ap->sibling) {
			if (ap->sibling == fake_ap) {
			    ap->sibling = article;
			    break;
			}
			ap = ap->sibling;
		    }
		}
	    }
	    for (ap = article->child1; ap; ap = ap->sibling)
		ap->parent = article;
	    clear_article(fake_ap);
	    free((char*)fake_ap);
	    data.dat_ptr = Nullch;
	    data.dat_len = article_num(article);
	    hashstorelast(data);
	    return TRUE;
	}
    }
    /* Forget about the duplicate message-id or bogus article. */
    uncache_article(article,TRUE);
    return FALSE;
}

/* Take a message-id and see if we already know about it.  If so, return
** the article, otherwise create a fake one.
*/
ARTICLE *
get_article(msgid)
char *msgid;
{
    register ARTICLE *article;
    HASHDATUM data;

    fix_msgid(msgid);

    data = hashfetch(msgid_hash, msgid, strlen(msgid));
    if (!(article = (ARTICLE *)data.dat_ptr)) {
	if (data.dat_len)
	    article = article_ptr(data.dat_len);
	else {
	    article = allocate_article(0);
	    data.dat_ptr = (char*)article;
	    article->msgid = savestr(msgid);
	    hashstorelast(data);
	}
    }
    return article;
}

/* Take all the data we've accumulated about the article and shove it into
** the article tree at the best place we can deduce.
*/
void
thread_article(article)
ARTICLE *article;
{
    register ARTICLE *ap, *last;
    register char *cp, *end;
    ARTICLE *kill_ap = ((article->autofl & AUTO_KILL)? article : Nullart);
    int art_chain_autofl = article->autofl
	| (article->subj->articles? article->subj->articles->autofl : 0);
    int art_thread_autofl;

    /* We're definitely not a fake anymore */
    article->flags = (article->flags & ~AF_FAKE) | AF_THREADED;

    /* If the article was already part of an existing thread, unlink it
    ** to try to put it in the best possible spot.
    */
    if (fake_had_subj) {
	ARTICLE *stopper;
	if (fake_had_subj->thread != article->subj->thread)
	    merge_threads(fake_had_subj, article->subj);
	/* Check for a real or shared-fake parent */
	ap = article->parent;
	while (ap && (ap->flags&AF_FAKE) == AF_FAKE && !ap->child1->sibling) {
	    last = ap;
	    ap = ap->parent;
	}
	stopper = ap;
	unlink_child(article);
	/* We'll assume that this article has as good or better references
	** than the child that faked us initially.  Free the fake reference-
	** chain and process our references as usual.
	*/
	for (ap = article->parent; ap != stopper; ap = last) {
	    unlink_child(ap);
	    last = ap->parent;
	    ap->date = 0;
	    ap->subj = 0;
	    ap->parent = 0;
	    /* don't free it until group exit since we probably re-use it */
	}
	article->parent = Nullart;		/* neaten up */
	article->sibling = Nullart;
    }

    /* If we have references, process them from the right end one at a time
    ** until we either run into somebody, or we run out of references.
    */
    if (*references) {
	last = article;
	ap = Nullart;
	end = references + strlen(references) - 1;
	while ((cp = rindex(references, '<')) != Nullch) {
	    while (end >= cp && ((unsigned char)*end <= ' ' || *end == ',')) {
		end--;
	    }
	    end[1] = '\0';
	    /* Quit parsing references if this one is garbage. */
	    if (!(end = valid_message_id(cp, end)))
		break;
	    /* Dump all domains that end in '.', such as "..." & "1@DEL." */
	    if (end[-1] == '.')
		break;
	    ap = get_article(cp);
	    *cp = '\0';
	    art_chain_autofl |= ap->autofl;
	    if (ap->autofl & AUTO_KILL)
		kill_ap = ap;

	    /* Check for duplicates on the reference line.  Brand-new data has
	    ** no date.  Data we just allocated earlier on this line has a
	    ** date but no subj.  Special-case the article itself, since it
	    ** does have a subj.
	    */
	    if ((ap->date && !ap->subj) || ap == article) {
		if ((ap = last) == article)
		    ap = Nullart;
		continue;
	    }
	    last->parent = ap;
	    link_child(last);
	    if (ap->subj)
		break;

	    ap->date = article->date;
	    last = ap;
	    end = cp-1;
	}
	if (!ap)
	    goto no_references;

	/* Check if we ran into anybody that was already linked.  If so, we
	** just use their thread.
	*/
	if (ap->subj) {
	    /* See if this article spans the gap between what we thought
	    ** were two different threads.
	    */
	    if (article->subj->thread != ap->subj->thread)
		merge_threads(ap->subj, article->subj);
	} else {
	    /* We didn't find anybody we knew, so either create a new thread
	    ** or use the article's thread if it was previously faked.
	    */
	    ap->subj = article->subj;
	    link_child(ap);
	}
	/* Set the subj of faked articles we created as references. */
	for (ap = article->parent; ap && !ap->subj; ap = ap->parent)
	    ap->subj = article->subj;

	/* Make sure we didn't circularly link to a child article(!), by
	** ensuring that we run off the top before we run into ourself.
	*/
	while (ap && ap->parent != article)
	    ap = ap->parent;
	if (ap) {
	    /* Ugh.  Someone's tweaked reference line with an incorrect
	    ** article-order arrived first, and one of our children is
	    ** really one of our ancestors. Cut off the bogus child branch
	    ** right where we are and link it to the thread.
	    */
	    unlink_child(ap);
	    ap->parent = Nullart;
	    link_child(ap);
	}
    } else {
      no_references:
	/* The article has no references.  Either turn it into a new thread
	** or re-attach the fleshed-out article to its old thread.
	*/
	link_child(article);
    }
    if (!(article->flags & AF_CACHED))
	cache_article(article);
    art_thread_autofl = art_chain_autofl;
    if (sel_mode == SM_THREAD && article == article->subj->articles) {
	SUBJECT *sp = article->subj->thread_link;
	while (sp != article->subj) {
	    if (sp->articles)
		art_thread_autofl |= sp->articles->autofl;
	    sp = sp->thread_link;
	}
    }
    if (art_thread_autofl & AUTO_SELECTALL) {
	if (sel_mode == SM_THREAD)
	    select_arts_thread(article, AUTO_SELECTALL);
	else
	    select_arts_subject(article, AUTO_SELECTALL);
    } else if (art_chain_autofl & AUTO_SELECT)
	select_subthread(article, AUTO_SELECT);
    if (art_thread_autofl & AUTO_KILLALL) {
	if (sel_mode == SM_THREAD)
	    kill_arts_thread(article, KF_ALL|KF_KILLFILE);
	else
	    kill_arts_subject(article, KF_ALL|KF_KILLFILE);
    }
    else if (kill_ap)
	kill_subthread(kill_ap, KF_ALL|KF_KILLFILE);
}

/* Check if the string we've found looks like a valid message-id reference.
*/
static char *
valid_message_id(start, end)
register char *start, *end;
{
    char *mid;

    if (start == end)
	return 0;

    if (*end != '>') {
	/* Compensate for space cadets who include the header in their
	** subsitution of all '>'s into another citation character.
	*/
	if (*end == '<' || *end == '-' || *end == '!' || *end == '%'
	 || *end == ')' || *end == '|' || *end == ':' || *end == '}'
	 || *end == '*' || *end == '+' || *end == '#' || *end == ']'
	 || *end == '@' || *end == '$') {
	    *end = '>';
	}
    } else if (end[-1] == '>') {
	*(end--) = '\0';
    }
    /* Id must be "<...@...>" */
    if (*start != '<' || *end != '>' || (mid = index(start, '@')) == Nullch
     || mid == start+1 || mid+1 == end) {
	return 0;
    }
    return end;
}

/* Remove an article from its parent/siblings.  Leave parent pointer intact.
*/
static void
unlink_child(child)
register ARTICLE *child;
{
    register ARTICLE *last;

    if (!(last = child->parent)) {
	register SUBJECT *sp = child->subj;
	if ((last = sp->thread) == child) {
	    do {
		sp->thread = child->sibling;
		sp = sp->thread_link;
	    } while (sp != child->subj);
	} else
	    goto sibling_search;
    } else {
	if (last->child1 == child)
	    last->child1 = child->sibling;
	else {
	    last = last->child1;
	  sibling_search:
	    while (last->sibling != child)
		last = last->sibling;
	    last->sibling = child->sibling;
	}
    }
}

/* Link an article to its parent article.  If its parent pointer is zero,
** link it to its thread.  Sorts siblings by date.
*/
static void
link_child(child)
register ARTICLE *child;
{
    register ARTICLE *ap;

    if (!(ap = child->parent)) {
	register SUBJECT *sp = child->subj;
	ap = sp->thread;
	if (!ap || child->date < ap->date) {
	    do {
		sp->thread = child;
		sp = sp->thread_link;
	    } while (sp != child->subj);
	    child->sibling = ap;
	} else
	    goto sibling_search;
    } else {
	ap = ap->child1;
	if (!ap || child->date < ap->date) {
	    child->sibling = ap;
	    child->parent->child1 = child;
	} else {
	  sibling_search:
	    while (ap->sibling && ap->sibling->date <= child->date)
		ap = ap->sibling;
	    child->sibling = ap->sibling;
	    ap->sibling = child;
	}
    }
}

/* Merge all of s2's thread into s1's thread.
*/
void
merge_threads(s1, s2)
SUBJECT *s1, *s2;
{
    register SUBJECT *sp;
    register ARTICLE *t1, *t2;

    t1 = s1->thread;
    t2 = s2->thread;
    /* Change all of t2's thread pointers to a common lead article */
    sp = s2;
    do {
	sp->thread = t1;
	sp = sp->thread_link;
    } while (sp != s2);

    /* Join the two circular lists together */
    sp = s2->thread_link;
    s2->thread_link = s1->thread_link;
    s1->thread_link = sp;

    /* If thread mode is set, ensure the subjects are adjacent in the list. */
    /* Don't do this if the selector is active, because it gets messed up. */
    if (sel_mode == SM_THREAD && mode != 't') {
	for (sp = s2; sp->prev && sp->prev->thread == t1; ) {
	    sp = sp->prev;
	    if (sp == s1)
		goto artlink;
	}
	while (s2->next && s2->next->thread == t1) {
	    s2 = s2->next;
	    if (s2 == s1)
		goto artlink;
	}
	/* Unlink the s2 chunk of subjects from the list */
	if (!sp->prev)
	    first_subject = s2->next;
	else
	    sp->prev->next = s2->next;
	if (!s2->next)
	    last_subject = sp->prev;
	else
	    s2->next->prev = sp->prev;
	/* Link the s2 chunk after s1 */
	sp->prev = s1;
	s2->next = s1->next;
	if (!s1->next)
	    last_subject = s2;
	else
	    s1->next->prev = s2;
	s1->next = sp;
    }

  artlink:
    /* Link each article that was attached to t2 to t1. */
    for (t1 = t2; t1; t1 = t2) {
	t2 = t2->sibling;
	link_child(t1);      /* parent is null, thread is newly set */
    }
}
