#ifndef JED_FEATURES_H
#define JED_FEATURES_H

/* If you want folding capibility, you MUST set JED_HAS_LINE_ATTRIBUTES to 1.
 * This adds an additional 4 bytes per line overhead.  In addition, you must
 * also set JED_HAS_SAVE_NARROW to 1.  It is also a good idea to set
 * JED_HAS_BUFFER_LOCAL_VARS to 1 since that will allow fold marks to vary on 
 * a buffer-by-buffer basis instead of a mode-by-mode basis.
 * Summary: for folding, set the next 3 variables to 1.
 */
#define JED_HAS_LINE_ATTRIBUTES		0
#define JED_HAS_BUFFER_LOCAL_VARS	0


#define JED_HAS_SAVE_NARROW		1

/* Double/Triple click support.  This is currently supported by:
 *
 *   X Windows
 *   GPM Mouse Driver (Linux)
 *   DJGPP version of jed
 */
#if defined(__unix__) || defined(VMS) || defined(__GO32__) || defined(MSWINDOWS)
# define JED_HAS_MULTICLICK		1
#else
# define JED_HAS_MULTICLICK		0
#endif

/*  Asynchronous subprocess support.  This is only available for Unix systems.
 */
#ifdef REAL_UNIX_SYSTEM
# define JED_HAS_SUBPROCESSES		1
#else
# define JED_HAS_SUBPROCESSES		0
#endif

/* Enhanced syntax highlighting support.  This is a much more sophisticated
 * approach based on regular expressions.  Experimental.
 */
#define JED_HAS_DFA_SYNTAX		0

/* Set JED_HAS_ABBREVS to 1 for the abbreviation feature. */
#define JED_HAS_ABBREVS			1

/* This should not be used on 16 bit systems. */
#ifndef pc_system
# define JED_HAS_DISPLAY_TABLE		1
#endif

#define JED_HAS_COLOR_COLUMNS		1
#define JED_HAS_LINE_MARKS		1

#endif
