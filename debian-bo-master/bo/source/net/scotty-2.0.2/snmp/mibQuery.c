/*
 * mibQuery.c
 *
 * Provides functions to search in the MIB tree.
 *
 * Copyright (c) 1994, 1995
 *
 * Sven Schmidt, J. Schoenwaelder
 * TU Braunschweig, Germany
 * Institute for Operating Systems and Computer Networks
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that this copyright
 * notice appears in all copies.  The University of Braunschweig
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#include "snmp.h"
#include "mib.h"

/*
 * Global variables of the MIB extension:
 */

char *mib_FileName = NULL;		/* Current MIB file name loaded. */
MIB_Node *mib_Tree = NULL;		/* The root of the MIB tree.	 */
MIB_TextConv *mib_TCList = NULL;	/* List of textual conventions.	 */
MIB_TextConv *mib_TCSaveMark = NULL;	/* The first already saved	 */
					/* element of mib_TCList	 */

/*
 * A private buffer that is used to assemble object identifier in 
 * dottet notation.
 */

static char oidBuffer[OID_MAXLEN * 8];

/*
 * Forward declarations for procedures defined later in this file:
 */

static char *
FormatOctetTC		_ANSI_ARGS_((char *val, char *fmt));

static char *
FormatIntTC		_ANSI_ARGS_((char *val, char *fmt));

static char *
FormatTimeTicks		_ANSI_ARGS_((char *val));

static char *
ScanOctetTC		_ANSI_ARGS_((char *val, char *fmt));

static char *
ScanIntTC		_ANSI_ARGS_((char *val, char *fmt));

static char *
ScanTimeTicks		_ANSI_ARGS_((char *val));


static void
GetPath			_ANSI_ARGS_((MIB_Node *nodePtr, char *soid));

/*
 * GetPath() writes the path to the given node into soid. This is
 * done recursively using the pointers to the parent node.
 */

static void
GetPath (nodePtr, s) 
     MIB_Node *nodePtr;
     char *s;
{
    if (! nodePtr) return;
    if (nodePtr->parentPtr) {
	GetPath (nodePtr->parentPtr, s);
	sprintf (s + strlen(s), ".%u", nodePtr->subid);
    } else {
	sprintf (s, "%u", nodePtr->subid);
    }
}


/*
 * MIB_Oid() returns the oid that belongs to label. The exact switch
 * turns exact matching on, which only allows object identifier which
 * exactly match an object type definition.
 */

char*
MIB_Oid (label, exact)
     char *label;
     int exact;
{
    char *expanded = ASN1_Hex2Oid (label);
    MIB_Node *nodePtr;
    int offset = -1;

    if (expanded) label = expanded ;
    if ((nodePtr = MIB_FindNode (label, &offset, exact))) {
	if (ASN1_IsOid (label)) {
	    return label;
	}
	GetPath (nodePtr, oidBuffer);
	if (offset > 0) {
	    strcat (oidBuffer, label+offset);
	}
	return oidBuffer;
    }

    return NULL;
}

/*
 * MIB_Name() returns the name that belongs to label. This is the 
 * reverse operation to MIB_Oid().
 */

char*
MIB_Name (label, exact)
     char *label;
     int exact;
{
    char *expanded = ASN1_Hex2Oid (label);
    MIB_Node *nodePtr;
    int offset = -1;
    
    if (expanded) label = expanded;
    if ((nodePtr = MIB_FindNode (label, &offset, exact))) {
	if (offset > 0) {
	    strcpy (oidBuffer, nodePtr->label);
	    strcat (oidBuffer, label+offset);
	    return oidBuffer;
	} else {
	    return nodePtr->label;
	}
    }

    return NULL;
}

/*
 * MIB_Description() extracts the description of a MIB object.
 * White spaces following newline characters are removed so that
 * the resulting text looks like the text in the MIB definition.
 * Note however, that all leading white spaces are removed which
 * may make problems in some rare situations.
 */

char*
MIB_Description (name, exact)
     char *name;
     int exact;
{
    FILE *fp;
    int	ch;
    char line[81];
    int len = 0;
    MIB_Node *nodePtr = MIB_FindNode (name, NULL, exact);
    static Tcl_DString *result = NULL;
    
    if (result == NULL) {
	result = (Tcl_DString *) ckalloc (sizeof(Tcl_DString));
	Tcl_DStringInit (result);
    } else {
	Tcl_DStringFree (result);
    }
    
    line[0] = '\0';
    if (nodePtr) {
	if (nodePtr->fileOffset > 0 && nodePtr->fileName != NULL) {
	    fp = fopen (nodePtr->fileName, "r");
	    if (fp == NULL) {
		perror (nodePtr->fileName);
		return "";
	    }
	    if (fseek (fp, nodePtr->fileOffset, 0) < 0) {
                perror (nodePtr->fileName);
                return "";
	    }

	    while ((ch = getc (fp)) != EOF) {
		if (ch == '"') break;
	    }

	    len = 0;
            line[0] = '\0';
	    while ((ch = getc (fp)) != EOF) {
		if (ch == '"') break;
		line[len++] = ch;
		if (ch == '\n' || len == 80) {
		    line[len] = '\0';
		    Tcl_DStringAppend (result, line, len);
		    len = 0;
		    if (ch == '\n') {
			while ((ch = getc (fp)) != EOF) {
			    if (!isspace (ch)) break;
			}
			if (ch == EOF || ch == '"') break;
			line[len++] = ch;
		    }
		}
	    }
	    if (len != 0) {
		line[len] = '\0';
		Tcl_DStringAppend (result, line, len);
	    }
	    
	    fclose (fp);
	    return (Tcl_DStringValue (result));
	} else {
	    return "";
	}
    }
    return NULL;
}

/*
 * MIB_Succ() returns the labels or object identifiers of all direct 
 * successors of a MIB node.
 */

char*
MIB_Succ (name)
     char *name;
{
    MIB_Node *nodePtr;
    int retoid;
    static Tcl_DString *result = NULL;
    char buf[20];
    char *expanded = ASN1_Hex2Oid(name);

    if (expanded) name = expanded;
    nodePtr = MIB_FindNode(name, NULL, 1);
    retoid = ASN1_IsOid(name);
    
    if (result == NULL) {
	result = (Tcl_DString *) ckalloc(sizeof(Tcl_DString));
	Tcl_DStringInit(result);
    } else {
	Tcl_DStringFree(result);
    }
    
    if (nodePtr) {
	if (nodePtr->childPtr) {
	    for (nodePtr = nodePtr->childPtr; 
		 nodePtr; 
		 nodePtr = nodePtr->nextPtr) {
		if (retoid) {
		    Tcl_DStringAppendElement(result, name);
		    sprintf(buf, ".%u", nodePtr->subid);
		    Tcl_DStringAppend(result, buf, -1);
		} else {
		    Tcl_DStringAppendElement(result, nodePtr->label);
		}
	    }
	}
	return Tcl_DStringValue(result);
    }

    return NULL;
}


/*
 * MIB_Syntax() returns the syntax as defined in the OBJECT-TYPE
 * definition. This is used to implement the mib syntax command.
 */

char*
MIB_Syntax (name, exact)
     char *name;
     int exact;
{
    MIB_Node *nodePtr = MIB_FindNode(name, NULL, exact);

    if (nodePtr) {
	if (nodePtr->tc && nodePtr->tc->name) {
	    if (*nodePtr->tc->name == '_')
	      return ASN1_Sntx2Str(nodePtr->tc->syntax);
	    else 
	      return nodePtr->tc->name;
	}
	return ASN1_Sntx2Str(nodePtr->syntax);
    }

    return NULL;
}


/*
 * MIB_ASN1() returns the transfer syntax actually used to encode a
 * value. This may be different from the syntax defined in the
 * OBJECT-TYPE macro as textual conventions are based on a set of
 * primitive types. Note: We have sometimes more than one MIB syntax
 * tag for an ASN.1 type, so we must make sure to find the right
 * syntax tag.
 */

int
MIB_ASN1 (name, exact)
     char *name;
     int exact;
{
    int syntax = ASN1_OTHER;
    MIB_Node *nodePtr = MIB_FindNode(name, NULL, exact);

    if (nodePtr) {
	if (nodePtr->tc && nodePtr->tc->name) {
	    syntax = nodePtr->tc->syntax;
	} else {
	    syntax = nodePtr->syntax;
	}
	switch (syntax) {
	  case Integer32: syntax = ASN1_INTEGER; break;
	  case Counter:	  syntax = ASN1_Counter32; break;
	  case Gauge:	  syntax = ASN1_Gauge32; break;
	}
    }

    return syntax;
}

/*
 * MIB_Access() return the max access field of the OBJECT-TYPE
 * macro.
 */

char*
MIB_Access (name, exact)
     char *name;
     int exact;
{
    MIB_Node *nodePtr = MIB_FindNode(name, NULL, exact);

    if (nodePtr) {
	switch (nodePtr->access) {
	  case M_NOACCESS:   return ("not-accessible");
	  case M_READONLY:   return ("read-only");
	  case M_READCREATE: return ("read-create");
	  case M_READWRITE:  return ("read-write");
	  case M_WRITEONLY:  return ("write-only");
	  case M_FORNOTIFY:  return ("accessible-for-notify");
	}
    }
    
    return NULL;
}


/*
 * FormatOctetTC() formats val according to textual convention stored in
 * fmt. It returns the parameter or a pointer to static buffer space 
 * (0 terminated), or 0 on error. Parts marked XXX are untested.
 */

static char *
FormatOctetTC (val, fmt)
     char *val;
     char *fmt;
{
    int pfx, have_pfx;			/* counter prefix */
    char *last_fmt;			/* save ptr to last seen fmt */
    static char *ret = 0;
    static int ret_len = 0;
    int idx;

    /* sanity check; */
    if (! fmt)
      return val;

    /* a simple one: */
    if (! strcmp (fmt, "1x:"))
      return val;

    if (! ret)
      ret = ckalloc (ret_len = 100);
    idx = 0;			/* idx into ret buffer */

    while (*fmt && *val)
      {
	  last_fmt = fmt;	/* save for loops */

	  /* scan prefix: */
	  have_pfx = pfx = 0;
	  while (*fmt && isdigit(*fmt))
	    pfx = pfx * 10 + *fmt - '0', have_pfx = 1, fmt++;
	  if (! have_pfx)
	    pfx = 1;

	  /* scan format: */
	  if (*fmt == 'a')
	    {
		while (*val && pfx > 0) {
		    char c = *val++ & 0xff;
		    int v = c >= 'a' ?  c - 87 : (c >= 'A' ? c - 55 : c - 48);
		    if (! *val)
		      break;
		    c = *val++ & 0xff;
		    v = (v << 4) + (c >= 'a' ?  c - 87 :
				    (c >= 'A' ? c - 55 : c - 48));
		    if (idx + 100 >= ret_len)
		      ret = ckrealloc (ret, ret_len += 100);
		    ret [idx++] = v;
		    pfx--;
		    if (*val == ':')
		      val++;
		}
		fmt++;
	    }
	  else if (*fmt == 'd' || *fmt == 'o' || *fmt == 'b')
	    {
		char format = *fmt;
		int c, vv = 0, v;

		fmt++;

		/* collect octets and format: */
		while (pfx > 0)
		  { 
		      if (! *val)
			break;
		      /* collect next byte from octal buffer and move to vv: */
		      c = *val++ & 0xff;
		      v = c >= 'a' ?  c - 87 : 
					(c >= 'A' ? c - 55 : c - 48);
		      if (! *val)
			break;
		      c = *val++ & 0xff;
		      v = (v << 4) + (c >= 'a' ?  c - 87 :
					(c >= 'A' ? c - 55 : c - 48));
		      vv = vv * 256 + v;

		      if (*val == ':')
			val++;

		      pfx--;
		  }
		
		if (idx + 100 >= ret_len)
		  ret = ckrealloc (ret, ret_len += 100);
		if (format == 'd')
		  {
		      sprintf (ret + idx, "%d", vv);
		      idx += strlen (ret + idx);
		  }
		else if (format == 'o')
		  {
		      /* XXX: completely untested */
		      sprintf (ret + idx, "%o", vv);
		      idx += strlen (ret + idx);
		  }
		else if (format == 'b')
		  {
		      /* XXX: completely untested */
		      int i; 
		      for (i = (sizeof (int) * 8 - 1); i >= 0
			   && ! (vv & (1 << i)); i--);
		      for (; i >= 0; i--)
			ret [idx++] = vv & (1 << i) ? '1' : '0';
		  }
	    }
	  else if (*fmt == 'x')
	    {
		/* a simple one: copy the following hex-digits: */
		while (pfx > 0)
		  {		
		      if (! *val || ! val [1])
			break;
		      if (idx + 100 >= ret_len)
			ret = ckrealloc (ret, ret_len += 100);
		      ret [idx++] = *val++;
		      ret [idx++] = *val++;
		      if (*val == ':')
			val++;
		      pfx--;
		  }
		fmt++;
	    }
	  else {
	      fprintf (stderr, "scotty: oerg ? unknown textual-format `%c'\n",
		       *fmt); 
	      fmt++;
	    }

	  /* look about a separator: */
	  if (*fmt && ! isdigit(*fmt) && *fmt != '*')
	    {
		if (have_pfx && *val)
		  {
		      if (idx + 100 >= ret_len)
			ret = ckrealloc (ret, ret_len += 100);
		      ret [idx++] = *fmt;
		  }
		fmt++;
	    }

	  /* repeat with last format, if datas avail: */
	  if (! *fmt && *val)
	    fmt = last_fmt;
      }

    ret [idx] = 0;

    return ret;
}


/*
 * FormatIntTC() formats val according to textual convention stored in
 * fmt. It returns the parameter or a pointer to static buffer space 
 * (0 terminated), or 0 on error. Parts marked XXX are untested.
 */

static char *
FormatIntTC (val, fmt)
     char *val;
     char *fmt;
{
    static char *ret = 0;
    static int ret_len = 0;
    int i, idx, val_len;
    int dpt = -1;				/* decimal point */
    char format;

   /* sanity check and simple case: */
    if (! fmt)
      return (char *) 0;
    else 
      format = *fmt;

    if (*fmt == 'd' && ! fmt [1])
      return val;

    /* must be one of these: */
    if (*fmt != 'd' && *fmt != 'x' && *fmt != 'o' && *fmt != 'b')
      return 0;
    
    /* init return buffer: */
    if (! ret)
      ret = ckalloc (ret_len = 100);
    idx = 0;				/* idx into ret buffer */
	  
    /* `d' format may have the form: d-[0-9]+ */
    if (*fmt == 'd' && fmt [1] == '-' 
	&& fmt [2] >= '0' && fmt [2] <= '9')
      {
	  format = *fmt;
	  dpt = 0;
	  for (fmt += 2; *fmt >= '0' && *fmt <= '9'; fmt++)
	    dpt = dpt * 10 + *fmt - '0';
      }
    else if (fmt [1])
      return 0;					/* invalid */

    /* check integer value; */
    for (val_len = 0; val [val_len]; val_len++)
      if (! ((! val_len && val [val_len] == '-') || isdigit (val[val_len])))
	return 0;
    
    if (dpt >= 0)
      {
	  /* now we have to format val_len digits with decimal-point 
	     at dpt: */
	  if (dpt + val_len + 2 >= ret_len)	/* don't care */
	    ret = ckrealloc (ret, ret_len = dpt + val_len + 2);

	  if (format == 'd')
	    {
		/* monadic - always first: */
		if (*val == '-')
		  ret [idx++] = '-', val++, val_len--;
		if (dpt >= val_len)
		  {
		      ret [idx++] = '0', ret [idx++] = '.';
		      for (i = 0; i < dpt - val_len; i++)
			ret [idx++] = '0';
 		      strcpy (ret + idx, val);
		      return ret;
		  }
		for (i = 0; i < val_len - dpt; i++)
		  ret [idx++] = val [i];
		ret [idx++] = '.';
		for (; i < val_len; i++)
		  ret [idx++] = val [i];
	    }
	  else
	    /* error: */
	    return val;
      }
    else if (format == 'o')
      {
	  /* XXX: completely untested */
	  sprintf (ret, "%o", atoi (val));
	  return ret;
      }
    else if (format == 'x')
      {
	  /* XXX: completely untested */
	  sprintf (ret, "%x", atoi (val));
	  return ret;
      }
    else if (format == 'b')
      {
	  int vv = atoi (val);
	  /* XXX: completely untested */
	  for (i = (sizeof (int) * 8 - 1); i > 0
	       && ! (vv & (1 << i)); i--);
	  for (; i >= 0; i--)
	    ret [idx++] = vv & (1 << i) ? '1' : '0';
      }

    ret [idx] = 0;
    return ret;
}


/*
 * FormatTimeTicks() converts an unsigned number representing a
 * TimeTicks value into a string representation. The result is alway a
 * pointer into static buffer space so there is no need to worry about
 * freeing it. 
 */

static char *
FormatTimeTicks	(value)
     char *value;
{
    u_int d, h, m, s, f;
    static char buf[80];

    d = 0;
    while (isdigit (*value)) {
	d = 10 * d + *value - '0';
	value++;
    }
    f = d % 100; d = d /100;
    s = d % 60;  d = d / 60;
    m = d % 60;  d = d / 60;
    h = d % 24;  d = d / 24;
    sprintf (buf, "%3dd %2d:%02d:%02d.%02d", d, h, m, s, f);
    return buf;
}


/*
 * ScanOctetTC scans a value from val using the textual convention stored in
 * fmt. It returns the parameter or a pointer to static buffer space 
 * (0 terminated), or 0 on error. Parts marked XXX are untested.
 */

static char *
ScanOctetTC (val, fmt)
     char *val;
     char *fmt;
{
    int pfx, have_pfx;			/* counter prefix */
    char *last_fmt;			/* save ptr to last seen fmt */
    static char *ret = 0;
    static int ret_len = 0;
    int idx;

    /* sanity check; */
    if (! fmt)
      return val;

    if (! ret)
      ret = ckalloc (ret_len = 100);

    *ret = '\0';
    idx = 0;			/* idx into ret buffer */

    /* a simple one: */
    if (! strcmp (fmt, "1x:"))
      return val;
    
    /* quick ``255a'' formatting: */
    if (! strcmp (fmt, "255a"))
      {
	  while (*val)
	    {
		if (idx + 100 >= ret_len)
		  ret = ckrealloc (ret, ret_len += 100);
		sprintf (ret + idx, "%02x", *val & 0xff);
		idx += 2;
		if (*++val)
	    ret [idx++] = ':';
	    }
	  return ret;
      }


    /* and now to something different: */

    while (*fmt && *val)
      {
	  last_fmt = fmt;	/* save for loops */
	  /* scan prefix: */
	  have_pfx = pfx = 0;
	  while (*fmt && isdigit(*fmt))
	    pfx = pfx * 10 + *fmt - '0', have_pfx = 1, fmt++;
	  if (! have_pfx)
	    pfx = 1;

	  /* scan format: */
	  if (*fmt == 'a')
	    {
		while (*val && pfx > 0) {
		    char c = *val;
		    int c1 = (c & 0xf0) >> 4;
		    int c2 = c & 0x0f;
		    if ((c1 += '0') > '9') c1 += 7;
		    if ((c2 += '0') > '9') c2 += 7;
		    
		    if (idx + 100 >= ret_len)
		      ret = ckrealloc (ret, ret_len += 100);

		    ret [idx++] = c1, ret [idx++] = c2;
		    if (*++val)
		      ret [idx++] = ':';
		    pfx--;
		}
		fmt++;
	    }
	  else if (*fmt == 'd' || *fmt == 'o' || *fmt == 'b')
	    {
		int vv = 0, v, got_val = 0;

		if (*fmt == 'd' && 1 == sscanf (val, "%d", &vv))
		  {
		      got_val = 1;
		      while (isdigit(*val))
			val++;
		  }
		else if (*fmt == 'o' && 1 == sscanf (val, "%o", &vv))
		  {
		      /* XXX: completely untested */
		      got_val = 1;
		      while (*val >= '0' && *val <= '7')
			val++;
		  }
		else if (*fmt == 'b')
		  {
		      /* XXX: completely untested */
		      while (*val == '0' || *val == '1')
			{
			    got_val = 1;
			    vv = (vv << 1) | (*val - '0');
			    val++;
			}
		  }

		/* XXX: tell more about unscanable strings !!! */
		if (! got_val)
		  break;
		
		/* now put pfx-num bytes to the output buffer: */
		while (pfx > 0)
		  {
		      int c1, c2;

		      v = vv >> ((pfx - 1) * 8);
		      
		      if (idx + 100 >= ret_len)
			ret = ckrealloc (ret, ret_len += 100);
		      
		      c1 = (v & 0xf0) >> 4;
		      c2 = v & 0x0f;
		      if ((c1 += '0') > '9') c1 += 7;
		      if ((c2 += '0') > '9') c2 += 7;
		      		      
		      ret [idx++] = c1, ret [idx++] = c2;
		      if (*val)
			ret [idx++] = ':';
		      pfx--;
		  }
		
		fmt++;
	    }
	  else if (*fmt == 'x')
	    {
		/* a simple one: copy the following hex-digits: */
		while (pfx > 0)
		  {		
		      if (! *val || ! val [1])
			break;
		      if (idx + 100 >= ret_len)
			ret = ckrealloc (ret, ret_len += 100);
		      ret [idx++] = *val++;
		      ret [idx++] = *val++;
		      if (*val == ':')
			val++, ret [idx++] = ':';
		      pfx--;
		  }
		fmt++;
	    }
	  else {
	      fprintf (stderr, "scotty: oerg ? unknown textual-format `%c'\n",
		       *fmt); 
	      fmt++;
	    }

	  /* look about a separator: */
	  if (*fmt && ! isdigit(*fmt) && *fmt != '*')
	    {
		if (have_pfx && *val)
                      val++;
		fmt++;
	    }

	  /* repeat with last format, if datas avail: */
	  if (! *fmt && *val)
	    fmt = last_fmt;
      }

    ret [idx] = 0;
    return ret;
}



/*
 * ScanIntTC scans a value from val using the textual convention stored in
 * fmt. It returns the parameter or a pointer to static buffer space 
 * (0 terminated), or 0 on error. Parts marked XXX are untested.
 */

static char *
ScanIntTC (val, fmt)
     char *val;
     char *fmt;
{
    static char ret [100];
    int dpt = -1;				/* decimal point */
    char format;

    /* sanity check and simple case: */
    if (! fmt)
      return (char *) 0;
    else
      format = *fmt;

    if (*fmt == 'd' && ! fmt [1])
      return val;

    /* must be one of these: */
    if (*fmt != 'd' && *fmt != 'o' && *fmt != 'b')
      return 0;
    
    /* `d' format may have the form: d-[0-9]+ */
    if (*fmt == 'd' && fmt [1] == '-' 
	&& fmt [2] >= '0' && fmt [2] <= '9')
      {
	  format = *fmt;
	  dpt = 0;
	  for (fmt += 2; *fmt >= '0' && *fmt <= '9'; fmt++)
	    dpt = dpt * 10 + *fmt - '0';
      }
    else if (fmt [1])
      return 0;					/* wrong */

    if (dpt >= 0)
      {
	  int idx = 0, do_copy = 0;

	  for (; *val; val++)
	    {
		if (*val == '.')
		  continue;
		else if (*val != '0' || do_copy)
		  ret [idx++] = *val;
		else if (*val != '0' && ! do_copy)
		  do_copy = 1;
	    }

	  ret [idx] = 0;

	  return ret;
      }
    else if (format == 'o')
      {
	  int vv;
	  if (1 == sscanf (val, "%o", &vv))
	    {
		/* XXX: completely untested */
		sprintf (ret, "%d", vv);
		return ret;
	    }
      }
    else if (format == 'b')
      {
	  int vv = 0, got_val = 0;
	  
	  /* XXX: completely untested */
	  while (*val == '0' || *val == '1')
	    {
		got_val = 1;
		vv = (vv << 1) | (*val - '0');
		val++;
	    }
	  
	  if (got_val)
	    {
		sprintf (ret, "%d", vv);
		return ret;
	    }
      }

    return 0;
}


/*
 * ScanTimeTicks() converts a string into a unsigned number representing
 * a TimeTicks value. The result is alway a pointer into static buffer
 * space so there is no need to worry about freeing it.
 */

static char *
ScanTimeTicks (value)
     char *value;
{
    u_int d, h, m, s, f, n;
    static char buf[20];

    n = sscanf (value, "%dd %d:%d:%d.%d", &d, &h, &m, &s, &f);
    if (n == 5) {
	sprintf (buf, "%u", d * 8640000 
		 + h * 360000 + m * 6000 + s * 100 + f);
    } else {
	d = 0;
	while (isdigit (*value)) {
	    d = 10 * d + *value - '0';
	    value++;
	}
	sprintf (buf, "%u", d);
    }

    return buf;
}


/*
 * MIB_Format() converts value given any textual conventions that are
 * defined for name. Value will be returned unmodified if no
 * convention exists. The result is alway a pointer into static buffer
 * space so there is no need to worry about freeing it.
 */

char *
MIB_Format (name, exact, value)
     char *name;
     int exact;
     char *value;
{
    MIB_Node *nodePtr = MIB_FindNode (name, NULL, exact);
   
    if (nodePtr) {
	if (nodePtr->tc) {

	    /*
	     * Check if we have an enumeration for this value.
	     */

	    MIB_IntEnum *i;
	    for (i = nodePtr->tc->enumList; i; i = i->nextPtr) {
		if (strcmp (i->value, value) == 0) {
		    return i->name;
		}
	    }

	    /* 
	     * Check if we can apply a display hint.
	     */

	    if (nodePtr->tc->displayHint) {
		char *ret = NULL;
		if (nodePtr->syntax == ASN1_OCTET_STRING) {
		    ret = FormatOctetTC (value, nodePtr->tc->displayHint);
		} else if (nodePtr->syntax == ASN1_INTEGER 
			   || nodePtr->syntax == Integer32) {
                    ret = FormatIntTC (value, nodePtr->tc->displayHint);
                }
		if (ret) {
		    return ret;
		}
	    }
	}
	if (nodePtr->syntax == ASN1_TimeTicks) {
	    return FormatTimeTicks (value);
	}
	return value;
    }

    return NULL;
}

/*
 * MIB_Scan() converts value given any textual conventions that are
 * defined for name. Value will be returned unmodified if no
 * convention exists. This is the counterpart to MIB_Format().
 */

char*
MIB_Scan (name, exact, value)
     char *name;
     int exact;
     char *value;
{
    MIB_Node *nodePtr = MIB_FindNode (name, NULL, exact);
    
    if (nodePtr) {
	if (nodePtr->tc) {

	    /*
	     * Check if we have an enumeration for this value.
	     */

	    MIB_IntEnum *i;
	    for (i = nodePtr->tc->enumList; i; i = i->nextPtr) {
		if (strcmp (i->name, value) == 0) {
		    return i->value;
		}
	    }
	    
	    /* 
	     * Check if we can apply a display hint.
	     */

            if (nodePtr->tc->displayHint) {
		char *ret = NULL;
		if (nodePtr->syntax == ASN1_OCTET_STRING) {
		    ret = ScanOctetTC (value, nodePtr->tc->displayHint);
		} else if (nodePtr->syntax == ASN1_INTEGER) {
		    ret = ScanIntTC (value, nodePtr->tc->displayHint);
		}
                if (ret) {
		    return ret;
		}
            }
	}
	if (nodePtr->syntax == ASN1_TimeTicks) {
	    return ScanTimeTicks (value);
	}
	return value;
    }

    return NULL;
}

/*
 * MIB_TC() returns the textual convention that applies to an object.
 */

char*
MIB_TC (name, exact)
     char *name;
     int exact;
{
    MIB_Node *nodePtr = MIB_FindNode (name, NULL, exact);
    static Tcl_DString *result = NULL;

    if (result == NULL) {
        result = (Tcl_DString *) ckalloc (sizeof(Tcl_DString));
        Tcl_DStringInit (result);
    } else {
	Tcl_DStringFree (result);
    }

    if (nodePtr) {
        if (nodePtr->tc) {
	    MIB_IntEnum *desc;
	    if (*nodePtr->tc->name != '_') {
		Tcl_DStringAppendElement (result, nodePtr->tc->name);
	    } else {
		Tcl_DStringAppendElement (result, "");
	    }
	    Tcl_DStringAppendElement (result, 
				      ASN1_Sntx2Str (nodePtr->tc->syntax));
	    Tcl_DStringAppendElement (result, nodePtr->tc->displayHint);
	    Tcl_DStringStartSublist (result);
	    for (desc = nodePtr->tc->enumList; desc; desc = desc->nextPtr) {
	        Tcl_DStringStartSublist (result);
		Tcl_DStringAppendElement (result, desc->name);
		Tcl_DStringAppendElement (result, desc->value);
	        Tcl_DStringEndSublist (result);
	    }
	    Tcl_DStringEndSublist (result);
	}
	return Tcl_DStringValue (result);
    }
    
    return NULL;
}


/*
 * MIB_File() returns the file name which contains the definition
 * the object.
 */

char*
MIB_File (name, exact)
     char *name;
     int exact;
{
    MIB_Node *nodePtr = MIB_FindNode (name, NULL, exact);

    if (nodePtr) {
	return nodePtr->fileName ? nodePtr->fileName : "";
    }

    return NULL;
}


/*
 * MIB_Index() returns the index variables that identify the instances
 * in a conceptual table. Note, we accept the table as well as the entry
 * node.
 */

char*
MIB_Index (name, exact)
     char *name;
     int exact;
{
    MIB_Node *nodePtr;
    char *expanded = ASN1_Hex2Oid (name);

    if (expanded) name = expanded;
    nodePtr = MIB_FindNode (name, NULL, exact);
    if (nodePtr) {
	if (nodePtr->syntax == ASN1_SEQUENCE_OF && nodePtr->childPtr) {
	    nodePtr = nodePtr->childPtr;
	}
	if (nodePtr->syntax == ASN1_SEQUENCE && nodePtr->index) {
	    return nodePtr->index;
	} else {
	    return "";
	}
    }

    return NULL;
}

/*
 * MIB_Parent() returns the parent of a MIB node. Note, this version
 * completely ignores any trailing instance identifier.
 */

char*
MIB_Parent (name, exact)
     char *name;
     int exact;
{
    MIB_Node *nodePtr;
    char *expanded = ASN1_Hex2Oid (name);

    if (expanded) name = expanded;
    nodePtr = MIB_FindNode (name, NULL, exact);
    if (nodePtr) {
	if (nodePtr->parentPtr && nodePtr->parentPtr->label) {
	    if (ASN1_IsOid(name)) {
		GetPath (nodePtr->parentPtr, oidBuffer);
		return oidBuffer;
	    } else {
		return nodePtr->parentPtr->label;
	    }
	} else {
	    return "";
	}
    }

    return NULL;
}

/*
 * MIB_DefVal() returns the parent of a MIB node.
 */

char*
MIB_DefVal (name, exact)
     char *name;
     int exact;
{
    MIB_Node *nodePtr = MIB_FindNode (name, NULL, exact);

    if (nodePtr) {
	if (nodePtr->index && nodePtr->syntax != ASN1_SEQUENCE_OF 
	    && nodePtr->syntax != ASN1_SEQUENCE ) {
	    return nodePtr->index;
	} else {
	    return "";
	}
    }

    return NULL;
}
