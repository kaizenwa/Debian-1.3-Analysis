%%
%%  rot13.sl---- rotates text by 13 characters
%%
define rot13 ()
{
   variable i, j;
   % Initialize the array
   _for (0, 255, 1)
     {
	i = ();
	TRANSLATE_ARRAY [i] = i;
     }
   _for ('A', 'M', 1)
     {
	i = ();
	TRANSLATE_ARRAY [i] = i + 13;
	% Now take care of lower case ones
	i = i | 0x20;
	TRANSLATE_ARRAY [i] = i + 13;
     }
   
   _for ('N', 'Z', 1)
     {
	i = ();
	TRANSLATE_ARRAY [i] = i - 13;
	% Now take care of lower case ones
	i = i | 0x20;
	TRANSLATE_ARRAY [i] = i - 13;
     }
   translate_region ();
}
