#define PATCHLEVEL 3
/*
    patch history:

    7/2/91	original version with bugs preventing running as
		nntp client on Sun-3/50.
    13/2/91	patch #1
		-correct an error in uncontrolled use of 'tolower'
		which solved the problem running as nntp client on
		a SUN-3/50.
		-Also truncate long lines in menu to prevent screen
		overflow caused by wrapped lines (controlled by symbol
		TRUNCATE).
		-Add debug value iVleavetmp.
    20/2/91	patch #2
		-Alan P. Barrett <barrett@dalsy.ee.und.ac.za> reworked
		and contributed the code in Printcharter to fit lines
		on the screen, not rudely truncate them.
		-Present the many found groups after fuzzy match via
		submenu's, don't ask one by one.
    6/1/92	patch #3
		create a Makefile and include conditional compilation
		to disinguish betwen BSD and Systm V (-DSYSV)
*/
