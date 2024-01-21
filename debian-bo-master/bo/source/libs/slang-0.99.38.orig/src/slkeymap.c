/* Keymap routines for SLang.  The role of these keymap routines is simple:
 * Just read keys from the tty and return a pointer to a keymap structure.  
 * That is, a keymap is simple a mapping of strings (keys from tty) to 
 * structures.  Also included are routines for managing the keymaps. 
 */
/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */


#include "config.h"

#include <stdio.h>
#include <string.h>


#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "slang.h"
#include "_slang.h"

/* We need a define a rule for upperand lower case chars that user cannot
   change!  This could be a problem for international chars! */

#define UPPER_CASE_KEY(x) (((x) >= 'a') && ((x) <= 'z') ? (x) - 32 : (x))
#define LOWER_CASE_KEY(x) (((x) >= 'A') && ((x) <= 'Z') ? (x) + 32 : (x))

int SLang_Key_TimeOut_Flag = 0;	       /* true if more than 1 sec has elapsed
                                          without key in multikey sequence */

int SLang_Last_Key_Char;




SLKeyMap_List_Type SLKeyMap_List[SLANG_MAX_KEYMAPS];   /* these better be inited to 0! */

static SLang_Key_Type *malloc_key(unsigned char *str)
{
   SLang_Key_Type *neew;

   if (NULL == (neew = (SLang_Key_Type *) SLMALLOC(sizeof(SLang_Key_Type))))
     {
	SLang_Error = SL_MALLOC_ERROR;
	return NULL;
     }
   SLMEMSET ((char *) neew, 0, sizeof (SLang_Key_Type));
   SLMEMCPY((char *) neew->str, (char *) str, (unsigned int) *str);
   return(neew);
}


static SLKeyMap_List_Type *add_keymap(char *name, SLang_Key_Type *map)
{
   int i;

   for (i = 0; i < SLANG_MAX_KEYMAPS; i++)
     {
	if (SLKeyMap_List[i].keymap == NULL)
	  {
	     SLKeyMap_List[i].keymap = map;
	     strncpy((char *) SLKeyMap_List[i].name, (char *) name, MAX_KEYMAP_NAME_LEN);
	     SLKeyMap_List[i].name[MAX_KEYMAP_NAME_LEN] = 0; 
	     return &SLKeyMap_List[i];
	  }
     }
   SLang_Error = UNKNOWN_ERROR;
   /* SLang_doerror ("Keymap quota exceeded."); */
   return NULL;
}

#ifdef SLKEYMAP_OBSOLETE
VOID_STAR 
#else
FVOID_STAR
#endif
SLang_find_key_function(char *name, SLKeyMap_List_Type *keymap)
{
   SLKeymap_Function_Type *fp = keymap -> functions;
   
   while ((fp != NULL) && (fp->name != NULL))
     {
	if ((*fp->name == *name) && !strcmp(fp->name, name)) 
	  {
#ifdef SLKEYMAP_OBSOLETE
	     return (VOID_STAR) fp->f;
#else
	     return (FVOID_STAR) fp->f;
#endif
	  }
	fp++;
     }
   return(NULL);
}

#ifdef REAL_UNIX_SYSTEM
/* Expand termcap string specified by s.  s as passed will have the format:
 *   "XY)..."  where XY represents a termcap keyname.
 */
static char *process_termcap_string (char *s, char *str, int *ip, int imax)
{
   char c[3], *val;
   int i;
   
   if ((0 == (c[0] = s[0]))
       || (0 == (c[1] = s[1]))
       || (s[2] != ')'))
     {
	_SLdo_error ("setkey: ^(%s is badly formed.", s);
	return NULL;
     }
   s += 3;
   
   c[2] = 0;
   if ((NULL == (val = SLtt_tgetstr (c)))
       || (*val == 0))
     return NULL;
   
   i = *ip;
   while ((i < imax) && (*val != 0))
     {
	str[i++] = *val++;
     }
   *ip = i;
   
   return s;
}
#endif

/* convert things like "^A" to 1 etc... The 0th char is the strlen INCLUDING
 * the length character itself.
 */
char *SLang_process_keystring(char *s)
{
   static char str[32];
   unsigned char ch;
   int i;

   i = 1;
   while (*s != 0)
     {
	ch = (unsigned char) *s++;
	if (ch == '^')
	  {
	     ch = *s++;
	     if (ch == 0)
	       {
		  if (i < 32)
		    str[i++] = '^';
		  break;
	       }
#ifdef REAL_UNIX_SYSTEM
	     if (ch == '(')
	       {
		  s = process_termcap_string (s, str, &i, 32);
		  if (s == NULL)
		    {
		       str[0] = 1;
		       return str;
		    }
		  continue;
	       }
#endif
	     ch = UPPER_CASE_KEY(ch);
	     if (ch == '?') ch = 127; else ch = ch - 'A' + 1;
	  }
	
	if (i >= 32) break;
	str[i++] = ch;
     }
   str[0] = i;
   return(str);
}


static int key_string_compare (unsigned char *a, unsigned char *b, unsigned int len)
{
   unsigned char *amax = a + len;
   int cha, chb, cha_up, chb_up;
   
   while (a < amax)
     {
	cha = *a++;
	chb = *b++;
	
	if (cha == chb) continue;
	
	cha_up = UPPER_CASE_KEY(cha);
	chb_up = UPPER_CASE_KEY(chb);
	
	if (cha_up == chb_up)
	  {
	     /* Use case-sensitive result. */
	     return cha - chb;
	  }
	/* Use case-insensitive result. */
	return cha_up - chb_up;
     }
   return 0;
}

	    
static char *Define_Key_Error = "Inconsistency in define key.";

/* This function also performs an insertion in an ordered way. */
static int find_the_key (char *s, SLKeyMap_List_Type *kml, SLang_Key_Type **keyp)
{
   unsigned char ch;
   unsigned int str_len;
   SLang_Key_Type *key, *last, *neew;
   unsigned char *str;

   *keyp = NULL;
   
   str = (unsigned char *) SLang_process_keystring(s);
   
   if (1 == (str_len = str[0]))
     return 0;

   ch = str[1];
   key = kml->keymap + ch;

   if (str_len == 2)
     {
	if (key->next != NULL)
	  {
	     SLang_doerror (Define_Key_Error);
	     return -2;
	  }
	
	/* copy keymap uses the pointers so do not free these if copied */
	if ((SLKeyMap_List[1].keymap == NULL)
	    && (key->type == SLKEY_F_INTERPRET))
	  {
	     SLFREE (key->f.s);
	  }
	key->str[0] = str_len;
	key->str[1] = ch;
	
	*keyp = key;
	return 0;
     }

   /* insert the key definition */
   while (1)
     {
	int cmp;
	unsigned int key_len, len;
	
	last = key;
	key = key->next;
	
	if ((key != NULL) && (key->str != NULL))
	  {
	     len = key_len = key->str[0];
	     if (len > str_len) len = str_len;
	     
	     cmp = key_string_compare (str + 1, key->str + 1, len - 1);

	     if (cmp > 0)
	       continue;
	     
	     if (cmp == 0)
	       {
		  if (key_len != str_len)
		    {
		       SLang_doerror (Define_Key_Error); 
		       return -2;
		    }

		  /* copy keymap uses the pointers so do not free these if copied */
		  if ((SLKeyMap_List[1].keymap == NULL)
		      && (key->type == SLKEY_F_INTERPRET))
		    {
#ifdef SLKEYMAP_OBSOLETE	
		       SLFREE(key->f);
#else
		       SLFREE(key->f.s);
#endif
		    }
		  
		  *keyp = key;
		  return 0;
	       }
	     /* Drop to cmp < 0 case */
	  }
	
	if (NULL == (neew = malloc_key(str))) return -1;
	     
	neew -> next = key;
	last -> next = neew;
	
	*keyp = neew;
	return 0;
     }
}

/* returns -2 if inconsistent, -1 if malloc error, 0 upon success */
#ifdef SLKEYMAP_OBSOLETE
int SLang_define_key1 (char *s, VOID_STAR f, unsigned int type, SLKeyMap_List_Type *kml)
#else
int SLkm_define_key (char *s, FVOID_STAR f, SLKeyMap_List_Type *kml)
#endif
{
   SLang_Key_Type *key;
#ifndef SLKEYMAP_OBSOLETE
   unsigned int type = SLKEY_F_INTRINSIC;
#endif
   int ret;
   
   ret = find_the_key (s, kml, &key);
   if ((ret != 0) || (key == NULL))
     return ret;

   key->type = type;
#ifdef SLKEYMAP_OBSOLETE
   key->f = f;
#else
   key->f.f = f;
#endif
   return 0;
}

int SLang_define_key(char *s, char *funct, SLKeyMap_List_Type *kml)
{
   SLang_Key_Type *key;
#ifdef SLKEYMAP_OBSOLETE
   VOID_STAR f;
#else
   FVOID_STAR f;
#endif
   int ret;
   
   ret = find_the_key (s, kml, &key);
   if ((ret != 0) || (key == NULL))
     return ret;

   f = SLang_find_key_function(funct, kml);

   if (f == NULL)                      /* assume interpreted */
     {
	char *str = SLmake_string (funct);
	if (str == NULL) return -1;
	key->type = SLKEY_F_INTERPRET;
#ifdef SLKEYMAP_OBSOLETE
	key->f = (VOID_STAR) str;
#else
	key->f.s = str;
#endif
     }
   else 
     {
	key->type = SLKEY_F_INTRINSIC;
#ifdef SLKEYMAP_OBSOLETE
	key->f = f;
#else
	key->f.f = f;
#endif
     }
   return 0;
}

#ifndef SLKEYMAP_OBSOLETE
int SLkm_define_keysym (char *s, unsigned int keysym, SLKeyMap_List_Type *kml)
{
   SLang_Key_Type *key;
   int ret;
   
   ret = find_the_key (s, kml, &key);

   if ((ret != 0) || (key == NULL))
     return ret;

   key->type = SLKEY_F_KEYSYM;
   key->f.keysym = keysym;
   return 0;
}
#endif

SLang_Key_Type *SLang_do_key(SLKeyMap_List_Type *kml, int (*getkey)(void))
{
   register SLang_Key_Type *key, *next, *kmax;
   unsigned int len;
   unsigned char input_ch;
   register unsigned char chup, chlow;
   unsigned char key_ch = 0;
   
   SLang_Last_Key_Char = (*getkey)();
   SLang_Key_TimeOut_Flag = 0;
   
   if (SLANG_GETKEY_ERROR == (unsigned int) SLang_Last_Key_Char)
     return NULL;
   
   input_ch = (unsigned char) SLang_Last_Key_Char;
   
   key = (SLang_Key_Type *) &((kml->keymap)[input_ch]);

   /* if the next one is null, then we know this MAY be it. */
   while (key->next == NULL) 
     {
	if (key->type != 0)
	  return key;
	
	/* Try its opposite case counterpart */
	chlow = LOWER_CASE_KEY(input_ch);
	if (input_ch == chlow)
	  input_ch = UPPER_CASE_KEY(input_ch);
	
	key = kml->keymap + input_ch;
	if (key->type == 0)
	  return NULL;
     }
   
   /* It appears to be a prefix character in a key sequence. */
   
   len = 1;			       /* already read one character */
   key = key->next;		       /* Now we are in the key list */
   kmax = NULL;			       /* set to end of list */
   
   while (1)
     {	
	SLang_Key_TimeOut_Flag = 1;   
	SLang_Last_Key_Char = (*getkey)();
	SLang_Key_TimeOut_Flag = 0;
	
	len++;
	
	if ((SLANG_GETKEY_ERROR == (unsigned int) SLang_Last_Key_Char)
	    || SLKeyBoard_Quit)
	  break;
	
	input_ch = (unsigned char) SLang_Last_Key_Char;

	chup = UPPER_CASE_KEY(input_ch); chlow = LOWER_CASE_KEY(input_ch);

	while (key != kmax)
	  {
	     if (key->str[0] > len)
	       {
		  key_ch = key->str[len];
		  if (chup == UPPER_CASE_KEY(key_ch))
		    break;
	       }
	     key = key->next;
	  }
	
	if (key == kmax) break;

	/* If the input character is lowercase, check to see if there is
	 * a lowercase match.  If so, set key to it.  Note: the 
	 * algorithm assumes the sorting performed by key_string_compare.
	 */
	if (input_ch != key_ch)
	  {
	     next = key->next;
	     while (next != kmax)
	       {
		  if (next->str[0] > len)
		    {
		       unsigned char next_ch = next->str[len];
		       if (next_ch == input_ch)
			 {
			    key = next;
			    break;
			 }
		       if (next_ch != chup)
			 break;
		    }
		  next = next->next;
	       }
	  }
	
	/* Ok, we found the first position of a possible match.  If it
	 * is exact, we are done.
	 */
	if ((unsigned int) key->str[0] == len + 1)
	  return key;
	
	/* Apparantly, there are some ambiguities. Read next key to resolve
	 * the ambiguity.  Adjust kmax to encompass ambiguities.
	 */
	
	next = key->next;
	while (next != kmax)
	  {
	     if ((unsigned int) next->str[0] > len)
	       {
		  key_ch = next->str[len];
		  if (chup != UPPER_CASE_KEY(key_ch))
		    break;
	       }
	     next = next->next;
	  }
	kmax = next;
     }

   return NULL;
}



void SLang_undefine_key(char *s, SLKeyMap_List_Type *kml)
{
   int n, i;
   SLang_Key_Type *key, *next, *last, *key_root, *keymap;
   unsigned char *str;

   keymap = kml -> keymap;
   str = (unsigned char *) SLang_process_keystring(s);

   if (0 == (n = *str++ - 1)) return;
   i = *str;
   
   last = key_root = (SLang_Key_Type *) &(keymap[i]);
   key = key_root->next;
   
   while (key != NULL)
     {
	next = key->next;
	if (0 == SLMEMCMP ((char *)(key->str + 1), (char *) str, n))
	  {
	     if ((SLKeyMap_List[1].keymap == NULL)
		 && (key->type == SLKEY_F_INTERPRET))
	       {
#ifdef SLKEYMAP_OBSOLETE
		  SLFREE(key->f);
#else
		  SLFREE (key->f.s);
#endif
	       }
	     SLFREE(key);
	     last->next = next;
	  }
	else last = key;
	key = next;
     }

   if (n == 1)
     {
	*key_root->str = 0;
#ifdef SLKEYMAP_OBSOLETE
	key_root->f = NULL;
#else
	key_root->f.f = NULL;
#endif
	key_root->type = 0;
     }
}

char *SLang_make_keystring(unsigned char *s)
{
   static char buf[40];
   char *b;
   int n;

   b = buf;
   n = *s++ - 1;
   while (n--)
     {
	if (*s < 32)
	  {
	     *b++ = '^';
	     *b++ = *s + 'A' - 1;
	  }
	else *b++ = *s;
	s++;
     }
   *b = 0;
   return(buf);
}




static SLang_Key_Type *copy_keymap(SLKeyMap_List_Type *kml)
{
   int i;
   SLang_Key_Type *neew, *old, *new_root, *km;

   if (NULL == (new_root = (SLang_Key_Type *) SLCALLOC(256, sizeof(SLang_Key_Type))))
     {
	SLang_Error = SL_MALLOC_ERROR;
	return NULL;
     }

   if (kml == NULL) return new_root;
   km = kml->keymap;

   
   for (i = 0; i < 256; i++)
     {
	old = &(km[i]);
	neew = &(new_root[i]);
#ifdef SLKEYMAP_OBSOLETE
	neew->f = old->f;
#else
#if 0
	if (old->type == SLKEY_F_INTERPRET)
	  neew->f.s = old->f.s;
	else 
	  neew->f.f = old->f.f;
#else
	SLMEMCPY((char *) &neew->f, (char *) &old->f, sizeof (neew->f));
#endif
#endif
	neew->type = old->type;
	SLMEMCPY((char *) neew->str, (char *) old->str, (unsigned int) *old->str);

	old = old->next;
	while (old != NULL)
	  {
	     neew->next = malloc_key((unsigned char *) old->str);
	     neew = neew->next;
#ifdef SLKEYMAP_OBSOLETE
	     neew->f = old->f;
#else
# if 0
	     if (old->type == SLKEY_F_INTERPRET)
	       neew->f.s = old->f.s;
	     else
	       neew->f.f = old->f.f;
# else
	     SLMEMCPY((char *) &neew->f, (char *) &old->f, sizeof (neew->f));
# endif
#endif
	     neew->type = old->type;
	     old = old->next;
	  }
	neew->next = NULL;
     }
   return(new_root);
}


SLKeyMap_List_Type *SLang_create_keymap(char *name, SLKeyMap_List_Type *map)
{
   SLang_Key_Type *neew;
   SLKeyMap_List_Type *new_map;
   
   if ((NULL == (neew = copy_keymap(map)))
       || (NULL == (new_map = add_keymap(name, neew)))) return NULL;
   
   if (map != NULL) new_map -> functions = map -> functions;
   
   return new_map;
}

SLKeyMap_List_Type *SLang_find_keymap(char *name)
{
   int i;

   for (i = 0; i < SLANG_MAX_KEYMAPS; i++)
     {
	if (!strncmp(SLKeyMap_List[i].name, name, 8)) return &SLKeyMap_List[i];
     }
   return(NULL);
}
