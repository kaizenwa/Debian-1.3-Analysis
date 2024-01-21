/*
 *	(c) Copyright 1990, Kim Fabricius Storm.  All rights reserved.
 *
 *	Response handling.
 */

#include "config.h"
#include "news.h"
#include "keymap.h"
#include "nn_term.h"
#include "options.h"
#include "regexp.h"
#include "chset.h"

/* answer.c */

#ifdef NNTP
# ifdef NNTP_MINI_INEWS_HEADER
static mini_inews_header __APROTO((FILE *t));
# endif
#endif
static subj_line __APROTO((FILE *t, int re, char *subj, char *prefix));
static void ng_line __APROTO((FILE *t, int use_follow));
static void ref_line __APROTO((FILE *t));
static to_line __APROTO((FILE *t, char *to));
static void alt_to_line __APROTO((FILE *t, char *to, int mask));
static void end_header __APROTO((FILE *t, register char *extra_headers));
static int reply_to __APROTO((FILE *t, char *address));
static void aux_param_bool __APROTO((FILE *f, char *name, int on));
static void aux_param_str __APROTO((FILE *f, char *name, char *str));
static void aux_param_int __APROTO((FILE *f, char *name, int i));
static aux_sh __APROTO((article_header *ah, char *script, char *prog, char *action, char *record, char *sent_fmt, int append_sig));
static void append_file __APROTO((char *name, register FILE *f));
static char *get_distr __APROTO((char *orig, char *distr));
static char *inet_name __APROTO((void));
static int check_sender __APROTO((char *sender));

extern char *temp_file;
extern char *news_lib_directory;

#ifndef DEFAULT_DISTRIB
#define DEFAULT_DISTRIB "world"
#endif

export char *default_distribution = DEFAULT_DISTRIB;
export char *distribution_follow = "always same default";
export char *distribution_post	= "ask default";
export char *extra_mail_headers	= NULL;
export char *extra_news_headers	= NULL;
export char *mail_record	= NULL;
export char *news_record	= NULL;
export char *mail_script	= NULL;
export char *news_script	= NULL;
export char *mailer_program	= REC_MAIL;
export int  mailer_pipe_input	= 1;
export char *inews_program	= NULL;
export int  inews_pipe_input	= 1;
export char *editor_program	= NULL;
export char *spell_checker	= NULL;
export char *mail_alias_expander = NULL;

export char *bug_address	= BUG_REPORT_ADDRESS;

export int include_art_id	= 0;
export int include_full_header	= 0;
export int orig_to_include_mask	= 0x3;
export int include_mark_blanks	= 0;

export int empty_answer_check	= 1; /* reject replies that are not edited */
export int response_check_pause = 0; /* time to wait for background cmds */
export char *response_dflt_answer = "send";

#ifdef APPEND_SIGNATURE
export int append_sig_mail = 1; /* mail does not append it */
#else
export int append_sig_mail = 0;
#endif
export int append_sig_post = 0;	/* inews should always do this */
export int query_signature = 1;	/* query user */


#define INCL_MARK_SIZE	10

export char included_mark[INCL_MARK_SIZE + 1] = ">";

import char delayed_msg[];
import int auto_select_rw;
import int ignore_fancy_select;

extern key_type help_key;

extern regexp *pg_regexp;
extern int pg_new_regexp;
import int use_mmdf_folders;
import int novice;
import char *pager;
import int data_bits;

static int ed_line;

#ifdef NNTP
#ifdef NNTP_MINI_INEWS_HEADER
#include <time.h>

extern struct tm *gmtime();
extern char *asctime();

static mini_inews_header(t)
FILE *t;
{
    time_t now;
    char *date, host[64];

    now = cur_time();
    date = asctime(gmtime(&now));
    date[3] = date[7] = date[10] = date[19] = date[24] = NUL;
#ifdef NNTP_PATH_HOSTNAME
    strncpy(host, NNTP_PATH_HOSTNAME, 64);

    if (host[0] == '/') {
	FILE *fph;
	if ((fph = open_file(host, OPEN_READ)) && fgets(host, 64, fph)) {
	    char *cp;
	    if (cp = strchr(host, '\n')) *cp = NUL;
	} else {
	    msg("Can't get outgoing hostname from file, using nn_gethostname");
	    nn_gethostname(host, 64);
	}
	if (fph) fclose(fph);
    }	
#else
    nn_gethostname(host, 64);
#endif

    fprintf(t, "Path: %s!%s\n", host, user_name());
    fprintf(t, "Date: %s %s %s %s GMT\n", date+8, date+4, date+22, date+11);
    fprintf(t, "Message-ID: <%s.%ld@%s>\n", user_name(), (long)now, host);
    ed_line += 3;
}
#endif
#endif

static int
subj_line(t, re, subj, prefix)
FILE *t;
int re;
char *subj, *prefix;
{
    if (subj == NULL) return 0;

    fputs("Subject: ", t);

    if (re >= 0)
	fputs("Re: ", t);

    if (prefix) {
	fputs(prefix, t);
	fputc(' ', t);
    }

    fputs(subj, t);
    fputc(NL, t);

    ed_line++;
    return 1;
}


static void
ng_line(t, use_follow)
FILE *t;
int use_follow;
{
    char *ng;

    ng = use_follow && news.ng_follow ? news.ng_follow : news.ng_groups;
    if (ng == NULL) return;
    
    fprintf(t, "Newsgroups: %s\n", ng);
    ed_line++;
}

static void
ref_line(t)
FILE *t;
{
    if (news.ng_ref == NULL && news.ng_ident == NULL) return;

    fputs("References:", t);
    if (news.ng_ref) fprintf(t, " %s", news.ng_ref);
    if (news.ng_ident) fprintf(t, " %s", news.ng_ident);
    putc(NL, t);
    ed_line++;
}


static int
to_line(t, to)
FILE *t;
char *to;
{
    if (to == NULL) return 0;

    fprintf(t, "To: %s\n", to);
    ed_line++;
    return 1;
}

static void
alt_to_line(t, to, mask)
FILE *t;
char *to;
int mask;
{
    if (to == NULL) return;
    if (mask && (orig_to_include_mask & mask) == 0) return;

    fprintf(t, "Orig-To: %s\n", to);
    ed_line++;
}

static void
end_header(t, extra_headers)
FILE *t;
register char *extra_headers;
{
    if (extra_headers != NULL && *extra_headers != NUL) {
	while (*extra_headers != NUL) {
	    if (*extra_headers == ';' || *extra_headers == NL) {
		if (*++extra_headers == NUL) break;
		fputc(NL, t);
		ed_line++;
	    } else {
		if (*extra_headers == '\\' && *++extra_headers == NUL)
		    break;
		fputc(*extra_headers++, t);
	    }
	}
	fputc(NL, t);
	ed_line++;
    }

    fputc(NL, t);
    ed_line++;
}


static int
reply_to(t, address)
FILE *t;
char *address;
{
    char route[512];

    if (address == NULL) return 0;

    if (reroute(route, address)) {
	to_line(t, route);
	return 1;
    }
    return 0;
}

/*
 *	open_purpose_file()
 *
 *	Open "newsgroups" file once - just rewind() otherwise.
 *	Caller must NOT close it!
 */

FILE *open_purpose_file()
{
    static FILE *f = NULL;
    static int is_open = 0;

    if (is_open) {
	if (f != NULL) rewind(f);
	return f;
    }
    is_open = 1;

#ifdef NNTP
    if (use_nntp) {
	f = nntp_get_newsgroups();
    } else
#endif
	f = open_file(relative(news_lib_directory, "newsgroups"), OPEN_READ);
    return f;
}

/*
 * display list of group purposes
 */

int
display_group_list(get_regexp)
int get_regexp;
{
    FILE *f;
    char line[512];
    char *expr = NULL;

    if (get_regexp) {
	prompt("\1/\1");
	expr = get_s((char *)NULL, (char *)NULL, (char *)NULL, NULL_FCT);
	if (expr == NULL || *expr == NUL) return 0;
    }

    if ((f = open_purpose_file()) == NULL) return 0;
    if (who_am_i == I_AM_POST) {
	gotoxy(0, prompt_line + 1);
	clrpage();
	pg_init(prompt_line + 1, 1);
    } else {
	home();
	clrdisp();
	pg_init(0, 1);
    }
    if (expr) pg_regexp = regcomp(expr);

    while (fgets(line, 512, f)) {
	if (pg_regexp && regexec_fold(pg_regexp, line) == 0) continue;
	if (pg_next() < 0) break;
	if (pg_new_regexp) {
	    pg_new_regexp = 0;	/* pg_next has cleared display */
	    rewind(f);
	    continue;
	}
	tprintf("%s", line);
    }
    return 1;
}

/*
 * invoke aux shell script with suitable arguments
 *
 * WARNING: record may be NULL, so it must be the last argument!!
 */

static void
aux_param_bool(f, name, on)
FILE *f;
char *name;
int on;
{
    fprintf(f, "%s=%s\n", name, on ? "true" : "false");
}

static void
aux_param_str(f, name, str)
FILE *f;
char *name, *str;
{
    int xport = (*name == '*');
    
    if (xport) name++;
    fprintf(f, "%s='%s'\n", name, str != NULL ? str : "");
    if (xport) fprintf(f, "export %s\n", name);
}

static void
aux_param_int(f, name, i)
FILE *f;
char *name;
int i;
{
    fprintf(f, "%s=%d\n", name, i);
}

static int
aux_sh(ah, script, prog, action, record, sent_fmt, append_sig)
article_header *ah;
char *script, *prog, *action, *record, *sent_fmt;
int append_sig;
{
    FILE *param;
    char *args[10], *fn;
    char route[512], *poster;
    register char **ap = args;

    if (strcmp(prog, "COMPLETE") == 0) goto no_params;

    param = open_file(relative(nn_directory, ".param"), OPEN_CREATE);
    if (param == NULL) {
	strcpy(delayed_msg, "cannot create .param file for aux script");
	return 1;
    }

    if (getenv("LOGNAME") == NULL)
	aux_param_str(param, "*LOGNAME", user_name());

    if (strcmp(prog, "cancel") == 0) {
	aux_param_str(param, "ART_ID", action);	/* article id for cancel */
	aux_param_str(param, "GROUP", record);	/* group name for cancel */
	if (news.ng_dist)
	  aux_param_str(param, "DIST", news.ng_dist); /* dist for cancel */
	else
	  aux_param_str(param, "DIST", "none"); /* dist for cancel */
    } else {
	aux_param_str(param, "FIRST_ACTION", action);
	aux_param_str(param, "RECORD", record);
	aux_param_str(param, "WORK", temp_file);
	aux_param_int(param, "ED_LINE", ed_line);

	aux_param_bool(param, "NOVICE", novice);
	aux_param_bool(param, "EMPTY_CHECK", empty_answer_check);
	aux_param_bool(param, "APPEND_SIG", append_sig);
	aux_param_bool(param, "QUERY_SIG", append_sig && query_signature);

	if (editor_program != NULL)
	    aux_param_str(param, "EDITOR", editor_program);
	aux_param_str(param, "SPELL_CHECKER", spell_checker);
	aux_param_str(param, "ALIAS_EXPANDER", mail_alias_expander);
	aux_param_str(param, "PAGER", pager);
	aux_param_str(param, "MAILER", mailer_program);
	aux_param_bool(param, "MAILER_PIPE", mailer_pipe_input);
	aux_param_int(param, "WAIT_PERIOD", response_check_pause);
	aux_param_str(param, "DFLT_ANSW", response_dflt_answer);
	aux_param_str(param, "POST", inews_program);
	aux_param_bool(param, "POST_PIPE", inews_pipe_input);
	aux_param_str(param, "MMDF_SEP", use_mmdf_folders ? "\1\1\1\1" : "");

	aux_param_str(param, "CHSET_NAME", curchset->cs_name);
	if (curchset->cs_width != 0)
	    aux_param_int(param, "CHSET_WIDTH", curchset->cs_width);
	else
	    aux_param_int(param, "CHSET_WIDTH", data_bits);

	if (current_group != NULL) {
	    aux_param_str(param, "*G", current_group->group_name);
	    if (ah == NULL)
		fn = NULL;
	    else
#ifdef NNTP
	    if (use_nntp)
		fn = nntp_get_filename(ah->a_number, current_group);
	    else
#endif
		fn = group_path_name;
	    aux_param_str(param, "*A", fn != NULL ? fn : "");
	}

	poster = NULL;
	if (ah != NULL && strcmp(prog, "follow") == 0) {
	    poster = news.ng_reply != NULL ? news.ng_reply : news.ng_from;
	    if (poster && reroute(route, poster)) 
		poster = route;
	    else
		poster = NULL;
	}
	aux_param_str(param, "POSTER_ADR", poster);
    }

    fclose(param);

 no_params:
    stop_usage();

    /* OBS: relative() returns ptr to static data below */
    *ap++ = "nnaux";
    *ap++ = script != NULL ? script : relative(lib_directory, "aux");
    *ap++ = prog;
    *ap++ = temp_file;
    *ap++ = NULL;

    if (execute(SHELL, args, 1)) {
	sprintf(delayed_msg, sent_fmt, " not");
	return 1;
    }
    sprintf(delayed_msg, sent_fmt, "");
    return 0;
}

static void
append_file(name, f)
char *name;
register FILE *f;
{
    register FILE *s;
    register int c;
    
    s = open_file(name, OPEN_READ);
    if (s == NULL) return;
    
    while ((c = getc(s)) != EOF) putc(c, f);
    fclose(s);
}

static char *get_distr(orig, distr)
char *orig, *distr;
{
    flag_type opts;
    int always, ask, same, dflt;
    char *str, *dd;

    if ((dd = default_distribution) == NULL) dd = DEFAULT_DISTRIB;

    if (distr == NULL) distr = "default";
    var_options(&distr, "always\0ask\0same\0default\0", &opts);
    always = (opts & FLAG(1));
    ask = (opts & FLAG(2));
    same = (opts & FLAG(3));
    dflt = (opts & FLAG(4));
    
    if (*distr == NUL || dflt) distr = dd;
    
    if (same && orig != NULL) {
	distr = orig;
	if (always) ask = 0;
    }
    
    if (!ask) return distr;
    
    prompt("Distribution: (default '%s') ", distr);
    str = get_s(NONE, NONE, NONE, NULL_FCT);
    if (str == NULL) return NULL;
    if (*str != NUL) distr = str;
    return distr;
}

int
answer(ah, command, incl)
article_header *ah;
int command;
int incl;	/* <0: ask, 0: don't include, >0: include article */
{
    register FILE *t, *art;
    char *pgm = NULL, *first_action, *record_file;
    int edit_message, append_sig;
    char *str, *script;
    news_header_buffer nhbuf, dhbuf;
    flag_type nn_st_flag = 0;

    if (file_exist(relative(nn_directory, "hold.work"), (char *)NULL)) {
	int ans;

	prompt("\1An uncompleted reponse exists\1 - complete now? ");
	if ((ans = yes(1)) < 0) return 0;
	if (ans) {
	    if (ah && ah->a_group) init_group(ah->a_group);
	    new_temp_file();
	    aux_sh(ah, (char *)NULL, "COMPLETE", (char *)NULL, (char *)NULL,
		   "Response%s posted", 0);
	    return 1;
	}
	prompt("Remove uncompleted reponse? ");
	if ((ans = yes(1)) < 0) return 0;
	if (ans) {
	    unlink(relative(nn_directory, "hold.work"));
	    unlink(relative(nn_directory, "hold.param"));
	}
    }

    first_action = "edit";
    edit_message = 1;
    append_sig = 0;

    if (incl < 0) {
	prompt("Include original article? ");
	if ((incl = yes(0)) < 0) return 0;
    }

    art = NULL;
    if (ah && ah->a_group) init_group(ah->a_group);

    if (incl || (command != K_MAIL_OR_FORWARD && command != K_BUG_REPORT)) {
	int open_modes;

	open_modes = FILL_NEWS_HEADER | GET_ALL_FIELDS | SKIP_HEADER;
	if (ah->flag & A_DIGEST) open_modes |= FILL_DIGEST_HEADER;

	art = open_news_article(ah, open_modes, nhbuf, dhbuf);
	if (art == NULL) {
	    msg("Can't find original article");
	    return 0;
	}

	if (ah->flag & A_DIGEST) {
	    if (digest.dg_from) {
		if (news.ng_reply) news.ng_from = news.ng_reply;
		news.ng_reply = digest.dg_from;
	    }
	    if (digest.dg_subj)
		news.ng_subj = digest.dg_subj;
	}
    } else
	ah = NULL;

    /* build header */
    new_temp_file();

    if ((t = open_file(temp_file, OPEN_CREATE)) == NULL) {
	msg("Can't create %s", temp_file);
	return 0;
    }

    ed_line = 0;

 follow_to_poster:

    record_file = mail_record;
    script = mail_script;

    if (command == K_REPLY) {
	pgm = "reply";
	append_sig = append_sig_mail;

	nn_st_flag = A_ST_REPLY;

	if (reply_to(t, news.ng_reply) ||
	    reply_to(t, news.ng_from) ||
	    reply_to(t, news.ng_path)) goto alt0;
	if (to_line(t, news.ng_reply)) goto alt1;
	if (to_line(t, news.ng_from)) goto alt2;
	if (to_line(t, news.ng_path)) goto alt3;
	goto err;

     alt0:
	alt_to_line(t, news.ng_reply, 0x1);
     alt1:
	alt_to_line(t, news.ng_from, 0x2);
     alt2:
	alt_to_line(t, news.ng_path, 0x4);
     alt3:

	if (news.ng_subj)
	    subj_line(t, ah->replies, ah->subject, (char *)NULL);
	else
	    subj_line(t, 0, current_group->group_name, "Your Article in");

	ng_line(t, 0);
	ref_line(t);

	end_header(t, extra_mail_headers);

	if (incl) {
	    if (current_group->group_flag & G_FOLDER)
		fprintf(t, "You write:\n");
	    else
		fprintf(t, "In %s you write:\n", current_group->group_name);
	    ed_line++;
	}
    }

    if (command == K_FOLLOW_UP) {
	if (news.ng_follow && strcmp(news.ng_follow, "poster") == 0) {
	    command = K_REPLY;
	    msg("Followup by reply to poster");
	    goto follow_to_poster;
	}

	pgm = "follow";
	record_file = news_record;
	script = news_script;
	append_sig = append_sig_post;

	nn_st_flag = A_ST_FOLLOW;

#ifdef NNTP
#ifdef NNTP_MINI_INEWS_HEADER
	mini_inews_header(t);
#endif
#endif
	ng_line(t, 1);

	if (news.ng_subj)
	    subj_line(t, ah->replies, ah->subject, (char *)NULL);
	else
	    if (!subj_line(t, 0, news.ng_from, "Babble from"))
		if (!subj_line(t, 0, news.ng_ident, "Article")) {
		    prompt("Subject: ");
		    str = get_s(NONE, NONE, NONE, NULL_FCT);
		    if (str == NULL) goto err;
		    subj_line(t, -1, str, (char *)NULL);
		}

	if (news.ng_keyw) {
	    fprintf(t, "Keywords: %s\n", news.ng_keyw);
	    ed_line++;
	}

	str = get_distr(news.ng_dist, distribution_follow);
	if (str == NULL) goto close_t;
	if (strcmp(str, "world")) {
	    fprintf(t, "Distribution: %s\n", str);
	    ed_line++;
	}

	ref_line(t);

	end_header(t, extra_news_headers);

	if (incl) {
	    if (news.ng_from) {
		if (include_art_id && news.ng_ident)
		    fprintf(t, "In%s %s ",
			    ah->flag & A_DIGEST ? " digest" : "",
			    news.ng_ident);
		fprintf(t, "%s writes:\n", news.ng_from);
		ed_line++;
	    } else
	    if (news.ng_ident) {
		fprintf(t, "In %s %s:\n",
			ah->flag & A_DIGEST ? "digest" : "article",
			news.ng_ident);
		ed_line++;
	    }
	}
    }

    if (command == K_MAIL_OR_FORWARD || command == K_BUG_REPORT) {
	pgm = incl ? "forward" : "mail";
	append_sig = append_sig_mail;

     m3_again:
	prompt("To: ");
	str = get_s(user_name(),
		    command == K_BUG_REPORT ? bug_address : NONE,
		    NONE, NULL_FCT);
	if (str == NULL) goto close_t;

	if (*str == NUL) str = user_name();
	if (*str == '?') goto m3_again;

	if (strcmp(str, user_name()) == 0)
	    record_file = NULL;	/* we will get this anyway,
				   there is so no need to save it */

/*	if (reply_to(t, str)) {	    alt_to_line(t, str, 0);	} else */
	to_line(t, str);

	do {
	    prompt("Subject: ");
	    str = get_s(incl ? ah->subject : NONE,
			command == K_BUG_REPORT ? "nn bug report" : NONE,
			NONE, NULL_FCT);
	    if (str == NULL) goto close_t;
	    if (*str == NUL && incl) str = ah->subject;
	} while (*str == NUL);

	subj_line(t, -1, str, (char *)NULL);

	end_header(t, extra_mail_headers);

	if (incl) {
	    prompt("\1Edit\1 forwarded message? ");
	    if ((edit_message = yes(0)) < 0) goto close_t;
	    if (!edit_message) {
		first_action = "send";
		fseek(art, ah->hpos, 0);
	    } else
		if (include_full_header && command == K_MAIL_OR_FORWARD)
		    fseek(art, ah->hpos, 0);
	}

	if (command == K_BUG_REPORT) {
	    fprintf(t, "\n=== configuration ===\n");
	    append_file(relative(lib_directory, "conf"), t);
	    fprintf(t, "=== variable settings ===\n");
	    print_variable_config(t, 0);
	    fprintf(t, "=== end ===\n");
	}
    }

    prompt("\1WAIT\1");

    if (incl) {
	register c, prevnl = 1;

	fputc(NL, t);
	ed_line++;

	while ((c = getc(art)) != EOF) {
	    if (c == NL) {
		putc(c, t);
		if (ftell(art) >= ah->lpos) break;
		prevnl++;
		if (!include_mark_blanks) continue;
	    }
	    if (prevnl) {
		if (command != K_MAIL_OR_FORWARD || ftell(art) < ah->fpos)
		    fputs(included_mark, t);
		prevnl = 0;
		if (c == NL) continue;
	    }
	    putc(c, t);
	}
    } else {
	putc(NL, t);
	ed_line++;
    }

    fclose(t);
    if (art) fclose(art);

    if (aux_sh(ah, script, pgm, first_action, record_file,
	       command == K_FOLLOW_UP ? "Article%s posted" : "Mail%s sent",
	       append_sig) == 0)
	if (ah) ah->flag |= nn_st_flag;

    return edit_message;

 err:
    msg("Can't build header for %s",
	command != K_FOLLOW_UP ? "letter" : "article");

 close_t:
    fclose(t);
    unlink(temp_file);
    if (art) fclose(art);

    return 0;
}


/*
 *	inet_name: return "<user_name()>@<host_name()>"
 */

static char *inet_name()
{
    static char *inetname = NULL;
    char hname[100], *un;

    if (inetname == NULL) {
	nn_gethostname(hname, 100);
	un = user_name();
	inetname = newstr(strlen(hname) + strlen(un) + 2);
	sprintf(inetname, "%s@%s", un, hname);
    }
    return inetname;
}

/*
 *  check_sender: If sender is "root", "news", the full name or the internet
 *  name of the user, return 1 otherwise 0
 */

static int check_sender(sender)
char *sender;
{
    return strcmp(user_name(), "root") == 0
	|| strcmp(user_name(), "news") == 0
	|| strmatch(full_name(), sender)
	|| strmatch(inet_name(), sender);
}

int
cancel(ah)
article_header *ah;
{
    news_header_buffer nhbuf;
    FILE *f;

    if (ah->a_group) init_group(ah->a_group);

    if (ah->flag & A_DIGEST) {
	tprintf("\rCancel entire digest ? "); clrline();
	if (yes(1) > 0)
	    ah->flag &= ~A_DIGEST;
	else {
	    msg("Can only cancel entire digests (yet?)");
	    return 2;
	}
    }

    f = open_news_article(ah, FILL_NEWS_HEADER|GET_ALL_FIELDS, nhbuf, (char *)NULL);
    if (f == NULL) {
	msg("Article not found");
	return 2;
    }
    fclose(f);

    if  (! check_sender(news.ng_from)) {
	msg("You can only cancel your own articles!");
	return 1;
    }

    prompt("Confirm cancel: '%s: %.30s'",
	   ah->sender ? ah->sender : "",
	   ah->subject ? ah->subject : "");
    if (yes(1) <= 0) return 1;

    tprintf("\rCancelling article %s in group %s",
	   news.ng_ident, current_group->group_name);
    clrline();

    ed_line = -1;

    new_temp_file();
    if (aux_sh(ah, (char *)NULL, "cancel", news.ng_ident, current_group->group_name,
	       "Article%s cancelled", 0))
	return -1;

    return 0;
}

static char *post_distribution = NULL;
static char *post_subject = NULL;
static char *post_summary = NULL;
static char *post_keywords = NULL;
static char *post_source_file = NULL;
static int post_no_edit = 0;
static char *post_to_groups = NULL;

Option_Description(post_options) {
    'd', String_Option(post_distribution),
    'f', String_Option(post_source_file),
    'k', String_Option(post_keywords),
    's', String_Option(post_subject),
    'y', String_Option(post_summary),
    'p', Bool_Option(post_no_edit),
    '\0', 0,
};

void
do_nnpost(argc, argv)
int argc;
char *argv[];
{
    int ngroups, i;
    char newsgroups[FILENAME*2];

    init_term(0);
    visit_init_file(0, (char *)NULL);
    init_answer();
    current_group = NULL;

    ngroups =
	parse_options(argc, argv, (char *)NULL, post_options,
		      " newsgroup...");

    if (post_no_edit && post_source_file == NULL)
	nn_exitmsg(1, "Must specify a source file with -p\n");

    if (ngroups > 0) {
	strcpy(newsgroups, argv[1]);
	for (i = 2; i <= ngroups; i++) {
	    strcat(newsgroups, ",");
	    strcat(newsgroups, argv[i]);
	}
	post_to_groups = newsgroups;
    }

    if (ngroups > 0 && post_no_edit && post_subject && post_distribution) {
	if (!post_summary) post_summary = "";
	if (!post_keywords) post_keywords = "";
	post_menu();
	goto no_dialogue;
    }
    
    init_term(1);

    nn_raw();
    clrdisp();
    prompt_line = 0;
    if (post_menu() == 2) clrdisp();
    tputc(CR); tputc(NL);
    unset_raw();

 no_dialogue:
    if (*delayed_msg)
	nn_exitmsg(0, "%s", delayed_msg);
    else
        nn_exit(0);
}

int
post_menu()
{
    register FILE *t, *src = NULL;
    register int c;
    int must_redraw = 0;
    char brk_chars[4];
    char *str, *tail;
    group_header gh;
    char group_name[FILENAME], subject[FILENAME],
	 distribution[FILENAME], keywords[FILENAME], summary[FILENAME];

    if (post_source_file) {
	src = open_file(post_source_file, OPEN_READ);
	if (src == NULL)
	    nn_exitmsg(1, "File %s not found\n", post_source_file);
    }

    if (post_to_groups)
	strcpy(group_name, post_to_groups);
    else {
	group_name[0] = NUL;

     again_group:

	prompt(who_am_i == I_AM_POST ? "Newsgroups: " : "\1POST to group\1 ");

	strcpy(brk_chars, " /?");
	brk_chars[0] = help_key;
	str = get_s(current_group ? current_group->group_name : NONE,
		    group_name, brk_chars, group_completion);
	if (str == NULL) goto no_post;
	if (*str == '?' || (key_type)(*str) == help_key || *str == '/') {
	    if (display_group_list(*str == '/'))
		must_redraw = 2;
	    else
		msg("No group list is available");
	    goto again_group;
	}
	if (*str == NUL) {
	    if (current_group == NULL || (current_group->group_flag & G_FAKED))
		goto no_post;
	    str = current_group->group_name;
	}
	strcpy(group_name, str);

	for (str = group_name; str; str = tail) {
	    tail = strchr(str, ',');
	    if (tail) *tail = NUL;

	    if (lookup(str) == NULL) {
		msg("unknown group: %s", str);
		*str = NUL;
		goto again_group;
	    }

	    if (tail) *tail++ = ',';
	}
	if (who_am_i == I_AM_POST) {
	    prompt_line++;
	    if (must_redraw) {
		gotoxy(0, prompt_line);
		clrpage();
		must_redraw = 1;
	    }
	}
    }

    if ((str = post_subject) == NULL) {
	prompt("Subject: ");
	str = get_s(NONE, NONE, NONE, NULL_FCT);
	if (str == NULL || *str == NUL) goto no_post;
	if (who_am_i == I_AM_POST) prompt_line++;
    }
    strcpy(subject, str);

    if ((str = post_keywords) == NULL) {
	prompt("Keywords: ");
	str = get_s(NONE, NONE, NONE, NULL_FCT);
	if (str == NULL) goto no_post;
	if (who_am_i == I_AM_POST) prompt_line++;
    }
    strcpy(keywords, str);

    if ((str = post_summary) == NULL) {
	prompt("Summary: ");
	str = get_s(NONE, NONE, NONE, NULL_FCT);
	if (str == NULL) goto no_post;
	if (who_am_i == I_AM_POST) prompt_line++;
    }
    strcpy(summary, str);

    if ((str = post_distribution) == NULL) {
	str = get_distr(NULL, distribution_post);
	if (str == NULL) goto no_post;
	if (who_am_i == I_AM_POST) prompt_line++;
    }
    strcpy(distribution, str);

    new_temp_file();
    if ((t = open_file(temp_file, OPEN_CREATE)) == NULL) {
	msg("Can't create %s", temp_file);
	goto no_post;
    }

    if (!post_no_edit)
	prompt("\1WAIT\1");

    ed_line = 3;
#ifdef NNTP
#ifdef NNTP_MINI_INEWS_HEADER
    mini_inews_header(t);
#endif
#endif
    fprintf(t, "Newsgroups: %s\n", group_name);
    if (strcmp(distribution, "world")) {
	fprintf(t, "Distribution: %s\n", distribution);
	ed_line++;
    }
    fprintf(t, "Subject: %s\n", subject);
    if (*summary) {
	fprintf(t, "Summary: %s\n", summary);
	ed_line++;
    }
    if (*keywords) {
	fprintf(t, "Keywords: %s\n", keywords);
	ed_line++;
    }

    end_header(t, extra_news_headers);

    if (post_source_file) {
	while ((c = getc(src)) != EOF) putc(c, t);
	fclose(src);
    } else
	fputc(NL, t);

    fclose(t);

    if (auto_select_rw && !ignore_fancy_select) {
        tail = strchr(group_name, ',');
        if (tail) *tail = NUL;
        gh.group_name = group_name;
        enter_kill_file(&gh,subject,6,60);
        if (tail) *tail = ',';
    }

    aux_sh((article_header *)NULL, news_script, "post",
	   post_no_edit ? "send" : "edit", news_record,
	   "Article%s posted", append_sig_post);
    must_redraw = 1;

 no_post:
    return must_redraw;
}

void
init_answer()
{
    char buf[FILENAME + 5];

#ifndef INEWS_PATH
#define INEWS_PATH relative(news_lib_directory, "inews")
#endif

    if (inews_program == NULL) {
	sprintf(buf, "%s -h", INEWS_PATH);
	inews_program = copy_str(buf);
    }
}
