% syntax.sl		-*- SLang -*-
% The functions here are used to manipulate the syntax keyword tables

define syntax_strcmp ()
{
   strcmp ((), ()) < 0;
}

%!% Prototype: String add_keywords (String tbl, String kws, Int len, Int n);
%!%
%!% Adds a set of keywords `kws', each of length `len', to the already
%!% existing syntax table `tbl'.  For convenience of the user, the function
%!% does alphabetical sorting and removes duplicate entries.
%!%
%!% The previous list of keywords is returned.
%!% Related Functions: @define_keywords_n@, @create_syntax_table@, @add_keyword_n@
define add_keywords (tbl, kws, len, n)
{
   variable okws, a, i, j, num, idx;

   % old keywords
   okws = define_keywords_n (tbl, Null_String, len, n);
   kws = strcat (okws, kws);

   num = strlen (kws) / len;
   !if (num) return Null_String;

   a = create_array ('s', num, 1);	% array to sort
   num--;				% maximum array index

   _for (0, num, 1)
     {
	i = ();
	a [i] = substr (kws, 1 + i * len, len);
     }

   % remove duplicate entries
   _for (0, num, 1)
     {
	i = ();
	kws = a [i];
	i++;
	_for (i, num, 1)
	  {
	     j = ();
	     !if (strcmp (kws, a [j]))
	       a [j] = Null_String;
	  }
     }

   idx = array_sort (a, "syntax_strcmp");
   kws = Null_String;
   _for (0, num, 1)
     {
	i = ();
	kws = strcat (kws, a [idx [i]]);
     }

   () = define_keywords_n (tbl, kws, len, n);
   okws;
}

%!% Prototype: Void add_keyword_n (String tbl, String kw, Int n);
%!%
%!% Adds a single keyword `kw' to the already existing syntax table `tbl'.
%!% Related Functions: @define_keywords_n@, @create_syntax_table@, @add_keywords@
define add_keyword_n (tbl, kw, n)
{
   variable len = strlen (kw);
   !if (len) return;
   () = add_keywords (tbl, kw, len, n);
}

%!% Prototype: Void add_keyword (String tbl, String kw);
%!%
%!% Adds a single keyword `kw' to the already existing syntax table `tbl'.
%!% Related Functions: @define_keywords_n@, @create_syntax_table@, @add_keyword_n@
define add_keyword ()
{
   add_keyword_n (0);
}

%!% Prototype: String remove_keywords (String tbl, String kws, Int len, Int n);
%!%
%!% Removes a set of keywords `kws', each of length `len', from the already
%!% existing syntax table `tbl'.
%!%
%!% The previous list of keywords is returned.
%!% Related Functions: @add_keywords@, @define_keywords_n@, @create_syntax_table@, @add_keyword_n@
define remove_keywords (tbl, kws, len, n)
{
   variable okws, num, nrem, a, i, rm;

   EXIT_BLOCK
     {
	() = define_keywords_n (tbl, kws, len, n);
	okws;
     }

   % the old keywords
   okws = define_keywords_n (tbl, Null_String, len, n);
   num = strlen (okws) / len;

   nrem = strlen (kws) / len;
   !if (nrem)
     {
	kws = okws;
	return;
     }

   a = create_array ('s', num, 1);	% tmp array
   num--;				% maximum array index

   _for (0, num, 1)
     {
	i = ();
	a [i] = substr (okws, 1 + i * len, len);
     }

   % remove duplicate entries
   _for (0, nrem, 1)
     {
	i = ();
	rm = substr (kws, 1 + i * len, len);
	_for (0, num, 1)
	  {
	     i = ();
	     !if (strcmp (rm, a [i]))
	       a [i] = Null_String;
	  }
     }

   kws = Null_String;
   _for (0, num, 1)
     {
	i = ();
	kws = strcat (kws, a [i]);
     }
}
