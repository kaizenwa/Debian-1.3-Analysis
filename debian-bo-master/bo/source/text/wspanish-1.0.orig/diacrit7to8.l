/*
 * diacrit7to8.l: convert characters with postfix diacritics to Latin1.
 *
 * by Kristoffer Høgsbro Rose <kris@diku.dk>, December 1, 1993.
 *
 * This programs comes without any warranty whatsowever.
 */

%%

"A`"	putchar('À'); /* postfix accents */
"A'"	putchar('Á');
"A^"	putchar('Â');
"A~"	putchar('Ã');
"A\""	putchar('Ä');
"C/"	putchar('Ç');
"E`"	putchar('È');
"E'"	putchar('É');
"E^"	putchar('Ê');
"E\""	putchar('Ë');
"I`"	putchar('Ì');
"I'"	putchar('Í');
"I^"	putchar('Î');
"I\""	putchar('Ï');
"N~"	putchar('Ñ');
"O`"	putchar('Ò');
"O'"	putchar('Ó');
"O^"	putchar('Ô');
"O~"	putchar('Õ');
"O\""	putchar('Ö');
"U`"	putchar('Ù');
"U'"	putchar('Ú');
"U^"	putchar('Û');
"U\""	putchar('Ü');
"Y'"	putchar('Ý');
"a`"	putchar('à');
"a'"	putchar('á');
"a^"	putchar('â');
"a~"	putchar('ã');
"a\""	putchar('ä');
"c/"	putchar('ç');
"e`"	putchar('è');
"e'"	putchar('é');
"e^"	putchar('ê');
"e\""	putchar('ë');
"i`"	putchar('ì');
"i'"	putchar('í');
"i^"	putchar('î');
"i\""	putchar('ï');
"n~"	putchar('ñ');
"o`"	putchar('ò');
"o'"	putchar('ó');
"o^"	putchar('ô');
"o~"	putchar('õ');
"o\""	putchar('ö');
"u`"	putchar('ù');
"u'"	putchar('ú');
"u^"	putchar('û');
"u\""	putchar('ü');
"y'"	putchar('ý');

^\n	/* skip empty lines */;
