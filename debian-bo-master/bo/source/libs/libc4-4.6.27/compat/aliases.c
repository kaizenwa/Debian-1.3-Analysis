#include <gnu-stabs.h>

/* Although we keep those functions in the jump table. But we
 * have removed the entry point to outside. So we need the 
 * alias to bypass the jump table.
 */
#if 0
symbol_alias (__old_re_search__LOCAL__,__old_re_search);
symbol_alias (__old_re_search_2__LOCAL__,__old_re_search_2);
symbol_alias (__old_re_match__LOCAL__,__old_re_match);
symbol_alias (__old_re_match_2__LOCAL__,__old_re_match_2);
symbol_alias (__old_re_compile_fastmap__LOCAL__,__old_re_compile_fastmap);
symbol_alias (__old_re_compile_pattern__LOCAL__,__old_re_compile_pattern);
symbol_alias (__old_re_set_registers__LOCAL__,__old_re_set_registers);
#endif

/* For curses, gdbm and curses. */
#if 0
symbol_alias (dbm_close__LOCAL__,dbm_close);
symbol_alias (dbm_delete__LOCAL__,dbm_delete);
symbol_alias (dbm_dirfno__LOCAL__,dbm_dirfno);
symbol_alias (dbm_fetch__LOCAL__,dbm_fetch);
symbol_alias (dbm_firstkey__LOCAL__,dbm_firstkey);
symbol_alias (dbm_nextkey__LOCAL__,dbm_nextkey);
symbol_alias (dbm_open__LOCAL__,dbm_open);
symbol_alias (dbm_pagfno__LOCAL__,dbm_pagfno);
symbol_alias (dbm_store__LOCAL__,dbm_store);
symbol_alias (dbminit__LOCAL__,dbminit);
symbol_alias (delete__LOCAL__,delete);
symbol_alias (fetch__LOCAL__,fetch);
symbol_alias (firstkey__LOCAL__,firstkey);
symbol_alias (gdbm_close__LOCAL__,gdbm_close);
symbol_alias (gdbm_delete__LOCAL__,gdbm_delete);
symbol_alias (gdbm_fetch__LOCAL__,gdbm_fetch);
symbol_alias (gdbm_firstkey__LOCAL__,gdbm_firstkey);
symbol_alias (gdbm_nextkey__LOCAL__,gdbm_nextkey);
symbol_alias (gdbm_open__LOCAL__,gdbm_open);
symbol_alias (gdbm_reorganize__LOCAL__,gdbm_reorganize);
symbol_alias (gdbm_store__LOCAL__,gdbm_store);
symbol_alias (nextkey__LOCAL__,nextkey);
symbol_alias (store__LOCAL__,store);
#endif

#if 0
symbol_alias (tgetent__LOCAL__,tgetent);
symbol_alias (tgetflag__LOCAL__,tgetflag);
symbol_alias (tgetnum__LOCAL__,tgetnum);
symbol_alias (tgetstr__LOCAL__,tgetstr);
symbol_alias (tgoto__LOCAL__,tgoto);
symbol_alias (tparam__LOCAL__,tparam);
symbol_alias (tputs__LOCAL__,tputs);
#endif

symbol_alias (box__LOCAL__,box);
symbol_alias (delwin__LOCAL__,delwin);
symbol_alias (endwin__LOCAL__,endwin);
symbol_alias (getcap__LOCAL__,getcap);
symbol_alias (gettmode__LOCAL__,gettmode);
symbol_alias (idlok__LOCAL__,idlok);
symbol_alias (initscr__LOCAL__,initscr);
symbol_alias (longname__LOCAL__,longname);
symbol_alias (mvcur__LOCAL__,mvcur);
symbol_alias (mvprintw__LOCAL__,mvprintw);
symbol_alias (mvscanw__LOCAL__,mvscanw);
symbol_alias (mvwin__LOCAL__,mvwin);
symbol_alias (mvwprintw__LOCAL__,mvwprintw);
symbol_alias (mvwscanw__LOCAL__,mvwscanw);
symbol_alias (newwin__LOCAL__,newwin);
symbol_alias (overlay__LOCAL__,overlay);
symbol_alias (overwrite__LOCAL__,overwrite);
symbol_alias (printw__LOCAL__,printw);
symbol_alias (scanw__LOCAL__,scanw);
symbol_alias (scroll__LOCAL__,scroll);
symbol_alias (setterm__LOCAL__,setterm);
symbol_alias (subwin__LOCAL__,subwin);
symbol_alias (touchline__LOCAL__,touchline);
symbol_alias (touchwin__LOCAL__,touchwin);
symbol_alias (waddbytes__LOCAL__,waddbytes);
symbol_alias (waddch__LOCAL__,waddch);
symbol_alias (waddstr__LOCAL__,waddstr);
symbol_alias (wclear__LOCAL__,wclear);
symbol_alias (wclrtobot__LOCAL__,wclrtobot);
symbol_alias (wclrtoeol__LOCAL__,wclrtoeol);
symbol_alias (wdelch__LOCAL__,wdelch);
symbol_alias (wdeleteln__LOCAL__,wdeleteln);
symbol_alias (werase__LOCAL__,werase);
symbol_alias (wgetch__LOCAL__,wgetch);
symbol_alias (wgetstr__LOCAL__,wgetstr);
symbol_alias (winsch__LOCAL__,winsch);
symbol_alias (winsertln__LOCAL__,winsertln);
symbol_alias (wmove__LOCAL__,wmove);
symbol_alias (wprintw__LOCAL__,wprintw);
symbol_alias (wrefresh__LOCAL__,wrefresh);
symbol_alias (wscanw__LOCAL__,wscanw);
symbol_alias (wstandend__LOCAL__,wstandend);
symbol_alias (wstandout__LOCAL__,wstandout);
