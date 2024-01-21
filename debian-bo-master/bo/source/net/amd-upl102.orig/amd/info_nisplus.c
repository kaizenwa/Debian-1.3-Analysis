/*
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	%W% (Berkeley) %G%
 *
 * $Id: info_nisplus.c,v 5.2.2.1 1992/02/09 15:08:32 jsp beta $
 *
 */

/*
 * Get info from NIS+ (version 3) map
 */

#include "am.h"

/* if the machine has nisplus maps, it also supports NIS */
#ifdef HAS_NISPLUS

#ifndef HAS_NISPLUS_MAPS
#define HAS_NISPLUS_MAPS
#endif

#endif

#ifdef HAS_NISPLUS_MAPS

#ifndef	HAS_NIS_MAPS
#define HAS_NIS_MAPS
#endif

#include <string.h>
#ifdef	HAS_NISPLUS
#include <rpcsvc/nis.h>
#endif

#ifdef HAS_NIS_MAPS
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>

/*
 * Figure out the nis domain name
 */
static int determine_nis_domain(P_void)
{
static	int nis_not_running = 0;

	char default_domain[YPMAXDOMAIN];

	if (nis_not_running)
		return ENOENT;

	if (getdomainname(default_domain, sizeof(default_domain)) < 0) {
		nis_not_running = 1;
		plog(XLOG_ERROR, "getdomainname: %m");
		return EIO;
	}

	if (!*default_domain) {
		nis_not_running = 1;
		plog(XLOG_WARNING, "NIS domain name is not set.  NIS ignored.");
		return ENOENT;
	}

	domain = strdup(default_domain);

	return 0;
}


#ifdef HAS_NIS_RELOAD
struct nis_callback_data {
	mnt_map *ncd_m;
	char *ncd_map;
	void (*ncd_fn)();
};

/*
 * Callback from yp_all
 */
static int nis_callback(status, key, kl, val, vl, data)
int status;
char *key;
int kl;
char *val;
int vl;
struct nis_callback_data *data;
{
	if (status == YP_TRUE) {
		/*
		 * Add to list of maps
		 */
		char *kp = strnsave(key, kl);
		char *vp = strnsave(val, vl);
		(*data->ncd_fn)(data->ncd_m, kp, vp);

		/*
		 * We want more ...
		 */
		return FALSE;
	} else {
		/*
		 * NOMORE means end of map - otherwise log error
		 */
		if (status != YP_NOMORE) {
			/*
			 * Check what went wrong
			 */
			int e = ypprot_err(status);

#ifdef DEBUG
			plog(XLOG_ERROR, "yp enumeration of %s: %s, status=%d, e=%d",
					data->ncd_map, yperr_string(e), status, e);
#else
			plog(XLOG_ERROR, "yp enumeration of %s: %s", data->ncd_map, yperr_string(e));
#endif
		}

		return TRUE;
	}
}

#ifdef BROKEN_YP_ALL
static int yp_all_fixed P((char *, char *, struct ypall_callback *));
static int yp_all_fixed(indomain, inmap, incallback)
     char *indomain;
     char *inmap;
     struct ypall_callback *incallback;
{ int i, j;
  char *outkey, *outval;
  int outkeylen, outvallen;
  char *outkey_old;
  int outkeylen_old;

  i = yp_first(indomain, inmap, &outkey, &outkeylen, &outval, &outvallen);
  if (i) {
    plog(XLOG_ERROR, "yp_first() returned error: %s\n", yperr_string(i));
  }
  do {
    j = (incallback->foreach)(YP_TRUE, outkey, outkeylen, outval, outvallen,
			      incallback->data);
    if (j != FALSE)			/* terminate loop */
      break;
    outkey_old = outkey;
    outkeylen_old = outkeylen;
    i = yp_next(indomain, inmap, outkey_old, outkeylen_old,
		 &outkey, &outkeylen, &outval, &outvallen);
    
  } while (!i);
#ifdef DEBUG
  if (i) {
    dlog("yp_next() returned error: %s\n", yperr_string(i));
  }
#endif /* DEBUG */
  if (i == YPERR_NOMORE)
    return 0;
  return i;
}
#endif /* BROKEN_YP_ALL */

int nis_reload P((mnt_map *m, char *map, void (*fn)()));
int nis_reload(m, map, fn)
mnt_map *m;
char *map;
void (*fn)();
{
	struct ypall_callback cbinfo;
	int error;
	struct nis_callback_data data;

	if (!domain) {
		error = determine_nis_domain();
		if (error)
			return error;
	}

	data.ncd_m = m;
	data.ncd_map = map;
	data.ncd_fn = fn;
	cbinfo.data = (voidp) &data;
	cbinfo.foreach = nis_callback;

#ifdef BROKEN_YP_ALL
	/*
	 * If you are using NIS and your yp_all function is "broken", use
	 * an alternate code which avoids a bug in yp_all().  The bug in
	 * yp_all() is that it does not close a TCP connection to ypserv,
	 * and this ypserv runs out of open filedescriptors, getting into an
	 * infinite loop, thus all YP clients enevtually unbind and hang
	 * too.
	 * 	-Erez Zadok <ezk@cs.columbia.edu>
	 * 	-James Tanis <jtt@cs.columbia.edu>
	 */

	error = yp_all_fixed(domain, map, &cbinfo);
#else
	error = yp_all(domain, map, &cbinfo);
#endif /* BROKEN_YP_ALL */

	if (error)
		plog(XLOG_ERROR, "error grabbing nis map of %s: %s", map, yperr_string(ypprot_err(error)));

	return error;
}
#endif /* HAS_NIS_RELOAD */

/*
 * Try to locate a key using NIS.
 */
struct nis_search_callback_data {
	char *ncd_map;
	char *key;
	char *vp;
};

/*
 * Callback from yp_all in nis_search/nis_init
 */

static int nis_search_callback(status, key, kl, val, vl, data)
int status;
char *key;
int kl;
char *val;
int vl;
struct nis_search_callback_data *data;
{
	if (status == YP_TRUE) {
		if (data->key == NULL) {
			/* nought to look for so terminate search */
			return TRUE;
		} else if (kl == strlen(data->key)
			&& strncmp(key, data->key, kl) == 0) {
			/*
		 	* remember value found (nis_search uses this)
		 	*/
			data->vp = strnsave(val, vl);

#ifdef	DEBUG
			dlog("yp found %s, value <%s>", data->key, val);
#endif
			/*
		 	* finished search
		 	*/
			return TRUE;
		} else {
			return FALSE;	/* key not found yet */
		}
	} else {
		/*
		 * NOMORE means end of map - otherwise log error
		 */
		if (status != YP_NOMORE) {
			/*
			 * Check what went wrong
			 */
			int e = ypprot_err(status);

#ifdef DEBUG
			plog(XLOG_ERROR, "yp enumeration of %s: %s, status=%d, e=%d",
					data->ncd_map, yperr_string(e), status, e);
#else
			plog(XLOG_ERROR, "yp enumeration of %s: %s", data->ncd_map, yperr_string(e));
#endif
		}

		return TRUE;
	}
}

int nis_search P((mnt_map *m, char *map, char *key, char **val, time_t *tp));
int nis_search(m, map, key, val, tp)
mnt_map *m;
char *map;
char *key;
char **val;
time_t *tp;
{
	int outlen;
	struct ypall_callback cbinfo;
	int error;
	struct nis_search_callback_data data;

	/*
	 * Make sure domain initialised
	 */
	if (!domain) {
		int error = determine_nis_domain();
		if (error)
			return error;
	}

	/*
	 * Lookup key - can't use yp_match as it doesn't work to a NIS+ server
	 */
	data.ncd_map = map;
	data.key = key;
	data.vp = NULL;
	cbinfo.data = (voidp) &data;
	cbinfo.foreach = nis_search_callback;

#ifdef BROKEN_YP_ALL
	/*
	 * If you are using NIS and your yp_all function is "broken", use
	 * an alternate code which avoids a bug in yp_all().  The bug in
	 * yp_all() is that it does not close a TCP connection to ypserv,
	 * and this ypserv runs out of open filedescriptors, getting into an
	 * infinite loop, thus all YP clients enevtually unbind and hang
	 * too.
	 * 	-Erez Zadok <ezk@cs.columbia.edu>
	 * 	-James Tanis <jtt@cs.columbia.edu>
	 */

	error = yp_all_fixed(domain, map, &cbinfo);
#else
	error = yp_all(domain, map, &cbinfo);
#endif /* BROKEN_YP_ALL */

	if (error)
		plog(XLOG_ERROR, "NIS search: error grabbing nis map of %s: %s", map, yperr_string(ypprot_err(error)));

	/*
	 * Do something interesting with the return code
	 */
	switch (error) {
	case 0:
		if (data.vp == NULL) {
#if	defined(DEBUG)
			dlog("NIS search for %s: <%s> found <nothing>",
				map,
				key);
#endif
			return ENOENT;
		}
		*val = data.vp;
		return 0;

	case YPERR_KEY:
		return ENOENT;

	default:
		plog(XLOG_ERROR, "%s: %s", map, yperr_string(error));
		return EIO;
	}
}

int nis_init P((char *map, time_t *tp));
int nis_init(map, tp)
char *map;
time_t *tp;
{
	int order = 0;
	struct ypall_callback cbinfo;
	int error;
	struct nis_search_callback_data data;

	if (!domain) {
		int error = determine_nis_domain();
		if (error)
			return error;
	}

	/*
	 * To see if the map exists, use yp_all - we can't use yp_order
	 * as it doesn't work to a NIS+ server!
	 */

	data.ncd_map = map;
	data.key = NULL;
	data.vp = NULL;
	cbinfo.data = (voidp) &data;
	cbinfo.foreach = nis_search_callback;

#ifdef BROKEN_YP_ALL
	/*
	 * If you are using NIS and your yp_all function is "broken", use
	 * an alternate code which avoids a bug in yp_all().  The bug in
	 * yp_all() is that it does not close a TCP connection to ypserv,
	 * and this ypserv runs out of open filedescriptors, getting into an
	 * infinite loop, thus all YP clients enevtually unbind and hang
	 * too.
	 * 	-Erez Zadok <ezk@cs.columbia.edu>
	 * 	-James Tanis <jtt@cs.columbia.edu>
	 */

	error = yp_all_fixed(domain, map, &cbinfo);
#else
	error = yp_all(domain, map, &cbinfo);
#endif /* BROKEN_YP_ALL */

	if (error)
		plog(XLOG_ERROR, "NIS init: error grabbing nis map of %s: %s", map, yperr_string(ypprot_err(error)));

	if (error != 0 && error != YPERR_KEY)
		return ENOENT;

	*tp = (time_t) order;
#ifdef DEBUG
	dlog("NIS master for %s@%s has order %d", map, domain, order);
#endif
	return 0;
}
#endif /* HAS_NIS_MAPS */

#ifdef	HAS_NISPLUS
#define NISPLUS_KEY "key="
#define NISPLUS_ORGDIR ".org_dir"

static int amd_nisplus_callback(key, value, data)
nis_name key;
nis_object* value;
struct nis_callback_data *data;
{

	char *kp = strnsave(ENTRY_VAL(value, 0), ENTRY_LEN(value, 0));
	char *vp = strnsave(ENTRY_VAL(value, 1), ENTRY_LEN(value, 1));
#if	defined(DEBUG)
	dlog("NISplus callback for <%s,%s>", kp, vp);
#endif

	(*data->ncd_fn)(data->ncd_m, kp, vp);

	/*
	 * We want more ...
	*/
	return FALSE;
}

int amd_nisplus_reload P((mnt_map *m, char *map, void (*fn)()));
int amd_nisplus_reload(m, map, fn)
mnt_map *m;
char *map;
void (*fn)();
{
	int error = 0;
	struct nis_callback_data data;
	nis_result* result;
	char* org;	/* if map does not have ".org_dir" then append it */
	nis_name map_name;

	org = strstr(map, NISPLUS_ORGDIR);
	if (org == NULL)
		org = NISPLUS_ORGDIR;
	else
		org = "";

	/* make some room for the NIS map_name */
	map_name = xmalloc(strlen(map)
			+ sizeof(NISPLUS_ORGDIR)
			);
	if (map_name == NULL) {
		plog(XLOG_ERROR,
			"Unable to create map_name %s: %s",
			map,
			strerror(ENOMEM));
		return ENOMEM;
	}

	sprintf(map_name, "%s%s", map, org);

	data.ncd_m = m;
	data.ncd_map = map_name;
	data.ncd_fn = fn;

#if	defined(DEBUG)
	dlog("NISplus reload for %s", map);
#endif

	result = nis_list(map_name,
			EXPAND_NAME | FOLLOW_LINKS | FOLLOW_PATH,
			amd_nisplus_callback,
			&data);

	/* free off the NIS map_name */
	free(map_name);

	if (result->status != NIS_SUCCESS && result->status != NIS_CBRESULTS)
		error = 1;

	if (error)
		plog(XLOG_ERROR, "error grabbing nisplus map of %s: %s",
			map,
			nis_sperrno(result->status));

	nis_freeresult(result);
	return error;
}

struct amd_nisplus_search_callback_data {
	nis_name	key;
	char*		value;
};

static int amd_nisplus_search_callback(key, value, data)
nis_name key;
nis_object* value;
struct amd_nisplus_search_callback_data* data;
{
#if	defined(DEBUG)
	dlog("NISplus search callback for <%s>", ENTRY_VAL(value, 0));
	dlog("NISplus search callback value <%s>", ENTRY_VAL(value, 1));
#endif

	data->value = strnsave(ENTRY_VAL(value, 1), ENTRY_LEN(value, 1));
	return TRUE;
}

/*
 * Try to locate a key using NIS+.
 */
int amd_nisplus_search P((mnt_map *m, char *map, char *key, char **val, time_t *tp));
int amd_nisplus_search(m, map, key, val, tp)
mnt_map *m;
char *map;
char *key;
char **val;
time_t *tp;
{
	nis_error status;
	netobj cookie;
	int found = 0;
	nis_result* result;
	int error = 0;
	struct amd_nisplus_search_callback_data data;
	nis_name index;
	char* org;	/* if map does not have ".org_dir" then append it */

	org = strstr(map, NISPLUS_ORGDIR);
	if (org == NULL)
		org = NISPLUS_ORGDIR;
	else
		org = "";

	/* make some room for the NIS index */
	index = xmalloc(sizeof('[')	/* for opening selection criteria */
			+ sizeof(NISPLUS_KEY)
			+ strlen(key)
			+ sizeof(']')	/* for closing selection criteria */
			+ sizeof(',')	/* + 1 for , separator */
			+ strlen(map)
			+ sizeof(NISPLUS_ORGDIR)
			);
	if (index == NULL) {
		plog(XLOG_ERROR,
			"Unable to create index %s: %s",
			map,
			strerror(ENOMEM));
		return ENOMEM;
	}

#ifdef	COMMENT
	if (key == NULL || *key == '\0')
		sprintf(index,"%s%s", map, org);
	else
#endif
		sprintf(index,"[%s%s],%s%s", NISPLUS_KEY, key, map, org);

	data.key = key;
	data.value = NULL;

#if	defined(DEBUG)
	dlog("NISplus search for %s", index);
#endif

	result = nis_list(index,
			EXPAND_NAME | FOLLOW_LINKS | FOLLOW_PATH,
			amd_nisplus_search_callback,
			&data);

	/* free off the NIS index */
	free(index);

	if (result == NULL) {
		plog(XLOG_ERROR, "%s: %s", map, strerror(ENOMEM));
		return ENOMEM;
	}

	/*
	 * Do something interesting with the return code
	 */
	switch (result->status) {
	case NIS_SUCCESS:
	case NIS_CBRESULTS:

		if (data.value == NULL) {
			nis_object* value = result->objects.objects_val;
#if	defined(DEBUG)
			dlog("NISplus search found <nothing>");
			dlog("NISplus search for %s: %s(%d)",
				map,
				nis_sperrno(result->status),
				result->status);
#endif

			if (value != NULL)
				data.value = strnsave(ENTRY_VAL(value, 1),
						ENTRY_LEN(value, 1));
		}

		*val = data.value;

		if (*val) {
			error = 0;
#if	defined(DEBUG)
			dlog("NISplus search found %s", *val);
#endif
		} else {
			error = ENOENT;
#if	defined(DEBUG)
			dlog("NISplus search found nothing");
#endif
		}
		
		*tp = 0;
		break;

	case NIS_NOSUCHNAME:
#if	defined(DEBUG)
		dlog("NISplus search returned %d", result->status);
#endif
		error = ENOENT;
		break;

	default:
		plog(XLOG_ERROR, "%s: %s", map, nis_sperrno(result->status));
		error = EIO;
		break;
	}
	nis_freeresult(result);
	return error;
}

int amd_nisplus_init P((char *map, time_t *tp));
int amd_nisplus_init(map, tp)
char *map;
time_t *tp;
{
	nis_result* result;
	char* org;	/* if map does not have ".org_dir" then append it */
	nis_name map_name;
	int error = 0;

	org = strstr(map, NISPLUS_ORGDIR);
	if (org == NULL)
		org = NISPLUS_ORGDIR;
	else
		org = "";

	/* make some room for the NIS map_name */
	map_name = xmalloc(strlen(map)
			+ sizeof(NISPLUS_ORGDIR)
			);
	if (map_name == NULL) {
		plog(XLOG_ERROR,
			"Unable to create map_name %s: %s",
			map,
			strerror(ENOMEM));
		return ENOMEM;
	}

	sprintf(map_name, "%s%s", map, org);

	result = nis_lookup(map_name, (EXPAND_NAME | FOLLOW_LINKS | FOLLOW_PATH));

	/* free off the NIS map_name */
	free(map_name);

	if (result == NULL) {
		plog(XLOG_ERROR, "NISplus init <%s>: %s",
			map,
			strerror(ENOMEM));
		return ENOMEM;
	}

	if (result->status != NIS_SUCCESS) {
#if	defined(DEBUG)
		dlog("NISplus init <%s>: %s (%d)",
			map,
			nis_sperrno(result->status),
			result->status);
#endif

		error = ENOENT;

	}

	*tp = 0;	/* no time */
	nis_freeresult(result);
	return error;
}

#endif  /* HAS_NISPLUS */
#endif  /* HAS_NISPLUS_MAPS */
