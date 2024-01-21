/* Copyright 1989 GROUPE BULL -- See license conditions in file COPYRIGHT
 * Copyright 1989 Massachusetts Institute of Technology
 */
/**************************************************************************\
* 									   *
* 			   @@@@@@  @       @ @       @			   *
* 			  @      @ @       @ @@     @@			   *
* 			 @         @   @   @ @ @   @ @			   *
* 			 @    @@@@ @  @ @  @ @  @ @  @			   *
* 			 @       @ @ @   @ @ @   @   @			   *
* 			  @     @@ @@     @@ @       @			   *
* 			   @@@@@ @ @       @ @       @			   *
* 									   *
*  _    ,								   *
* ' )  /      _/_                   /)					   *
*  /--/ o _   /  __,_  , ,     ____//     __  _ , __o  _   o ______   _	   *
* /  (_(_/_)_(__(_) (_(_/_    (_) /__    / (_(<_\/ (__/_)_(_(_) /) )_/_)_  *
*                      /         />					   *
*                     '         </					   *
* 									   *
\**************************************************************************/

static char           *RCS_Version = "$GwmVersion: 1.8c $";

/* Here is the log of all revisions 
 * NOTE: Revision numbers are non-significant, use Version numbers in all mail.
 */
/* $Log: gwm.shar,v $
 * Revision 1.115  1995/12/08 07:51:55  colas
 * ********************
 * *** Version 1.8c ***
 * ********************
 *
 * Revision 1.114  1995/12/08 07:35:42  colas
 * "Henry S. Thompson" <ht@cogsci.ed.ac.uk>: patches to compile on freebsd
 * pack icon menu in standard profile raises icons too
 *
 * Revision 1.113  1995/09/22 14:13:27  colas
 * ********************
 * *** Version 1.8b ***
 * ********************
 *
 * Revision 1.112  1995/09/22 13:14:34  colas
 * typo in aho email corrected in mwm-win.gwm
 *
 * aho: virtual-pan.gwm float.gwm:
 *     patches to fix xlock problems (float windows race condition)
 *
 * aho: vtwm-menu.gwm vtwm-window.gwm:
 *     treatment of color focus, which were not so good before
 *
 * Naoki Hamada <nao@sbl.cl.nec.co.jp>: but, zrt_put was called (by
 *     wool_hostname_get in SetUpDefaults) before initialisation by zrt_init
 *
 * Revision 1.111  1995/08/16 16:36:22  colas
 * some more traces in debug mode
 *
 * Revision 1.110  1995/07/25  16:52:55  colas
 * contrib/gwmsend: now appends property instead of deleting it
 * patches 004, 005, 006 applied
 * TO DOC: timeout-win
 *
 * Revision 1.109  1995/07/20  15:46:34  colas
 * new deco: timeout-win. needs debugging and documentation.
 *
 * Revision 1.108  1995/07/17  16:09:56  colas
 * data/simple-ed-win.gwm: old code was limiting the size of plugs in titlebar.
 * data/wallpaper.gwm new file (old but forgot to include in distrib) code to add
 *         wallpaper effects to the dvrooms package
 *
 * C code: new functionality:
 *   window-icon-name can now be set to!
 *
 * Revision 1.107  1995/07/03  09:24:22  colas
 * *** Version 1.8a ***
 *
 * logging by sending an udppacket is now disablable at run
 * time by setting the environment variables NO_GWM_LOG or NO_KOALA_SPY
 *
 * Revision 1.106  1995/06/30  08:45:20  colas
 * *******************
 * *** Version 1.8 ***
 * *******************
 *
 * Revision 1.105  1995/06/29  17:02:06  colas
 * modifications to the virtual package of Anders:
 *  - the door-mrg had itself as menu that could mess things up if popping a
 *    menu on it (it has itself as menu, and this recursion messed things)
 *  - added a property on doors: 'action that can contain executable code
 *    triggered after a move via a door
 *  - on end and restart, first move home
 *
 * default .profile.gwm updated to my current profile state
 * new std-virtual.gwm package
 *
 * check if icon windows or pixmaps are not invalid (nulls) to work around
 * emacs19.29 bug
 *
 * Revision 1.104  1995/06/27  13:46:42  colas
 * *** Version 1.7p_beta_5 ***
 *
 * Imake options for RS6000 corrected
 * patch by anders for small X errors
 * patch by anders, menus refused to change shape
 *
 * Revision 1.103  1995/06/07  20:02:23  colas
 * *** Version 1.7p_beta_4 ***
 *
 * Revision 1.102  1995/06/07  12:57:31  colas
 * Revision 1.101  1995/06/07  12:39:23  colas
 *
 * Anders Holst <aho@pdc.kth.se>: fixes the random crashes when using GWM_EXECUTE
 * updates to vtwm
 * 
 * LaTeX doc updated. doc in postscript format included in distrib
 *
 * user.c: some roundoff errors occured when resizing windows at screen 
 * boundaries when confine-windows was set
 *
 * Revision 1.100  1995/05/29  15:56:57  colas
 * simple-win.gwm: new parameters:
 *     label like simple-icon
 *     legend to place the label on sides of window
 *     lpad and rpad: number of () to pad the label with stretchable space
 * bar-max-widths set by default to 1000
 *
 * John Carr <jfc@MIT.EDU>: patches to supress warnings on AIX/RS_6000/xlc
 * rxterm install fixed once more
 *
 * Revision 1.99  1995/05/17  11:23:18  colas
 * *** Version 1.7p_beta_3 ***
 *
 * border-on-shaped new global variable to allow border on shaped windows
 * doc on new extension (to be added to doc) in gwm-1.7p_beta_3.README
 *
 * Revision 1.98  1995/05/17  09:57:07  colas
 *
 * traps bogus clients with a WM_STATE_Withdrawn as its initial_map field in
 * their WM_HINTS property, prints a warning but map them nonetheless
 *
 * Revision 1.97  1995/05/16  16:16:36  colas
 * contrib/scripts/find-bar-nils
 *
 * bar can have abitrary shaped backgrounds (shaped tiles)
 *
 * found (bar-make ()) constructs and replaced them:
 * via the script find-bar-nils
 *
 * autopan.gwm:119: 1 occurences in: (defun make-pan-list (x y xs ys) (with (
 * em-widgets.gwm:137: 1 occurences in: (: widget:scrollbar-make (with (fsm widg
 * mwm-icon.gwm:279: 1 occurences in: (: icon-right-bar (with (cursor frame-cu
 * mwm-icon.gwm:300: 1 occurences in: (: icon-left-bar (with (cursor frame-cur
 * mwm-win.gwm:340: 1 occurences in: (: border-v-bar (with (fsm border.fsm bo
 * pick.gwm:52: 1 occurences in: (defun pick-menu () (with (fsm pick-fsm
 * virtual-pan.gwm:120: 1 occurences in: (defun make-pan-list (x y xs ys) (with (
 * vtwm-window.gwm:277: 1 occurences in: (defun vtwm-borderbar () (with (borderwi
 * widgets.gwm:97: 1 occurences in: (: widget:scrollbar-make (with (fsm widg
 *
 * simple-window modified to take the new customizer simple-icon.stretch, if set
 * to t makes "stretching" legends
 *
 * patches from aho applied (untouched)
 *
 * typo in logging
 *
 * Revision 1.95  1995/05/11  17:06:56  colas
 * better spy
 *
 * Revision 1.94  1995/04/27  16:14:40  colas
 * added some code to ensure window is put back on screen if an error happens
 * while decorating it or its icon
 *
 * spy.c: new function, sends an UDP packet to track GWM usage. you can supress
 * this mecanism if you want by defining -DNO_GWM_LOG. The packet sends only the
 * host name as info, once as start, and once every day. If you dont want the
 * info to appear you can define the string that will be given instead of the
 * host name by the GWM_LOG_ID which must be a string.
 *
 * Revision 1.93  1995/04/26  16:34:51  colas
 * Makefile added in distrib
 *
 * simple-icon.gwm:
 *
 *     - customize item "legend" can now be instead of () or t the strings:
 *       "top" "base" "right" "left" for the positions where you want the string
 *       to appear
 *       e.g: (customize simple-icon any XTerm legend "left")
 *
 *     - new customization item "label" to provide either a fixed string or a
 *       lambda which will be used to filter the label
 *       must return a non-empty string otherwise the unfiltered label is used
 *       e.g: to supress the Netscape: in netscape icon titles
 *       (customize simple-icon any Netscape
 *           label (lambdaq (s) (match "Netscape: \\(.*\\)$" s 1))
 *       )
 *
 * iconify a window doesnt not loose the window anymore in case of error in wool
 * code
 *
 * Revision 1.92  1995/04/25  14:31:09  colas
 * *** Version 1.7p_beta_2 ***
 *
 * Revision 1.91  1995/04/25  08:30:41  colas
 * ejb@ERA.COM (E. Jay Berkenbilt): patches for time on SYSV (posix?)
 *
 * at last: last memory leak bugs fixed:
 *    - one on xpm reading
 *    - one on lambda deallocations
 *    - one on new menu creations (vscreen)
 *
 * Revision 1.90  1995/04/24  17:04:09  colas
 * portability fixes, compiles on solris, irix,...
 *
 * Revision 1.89  1995/04/21  15:23:47  colas
 * ejb@ERA.COM (E. Jay Berkenbilt): patches for time on SYSV (posix?)
 * bug in list-of-windows
 *
 * Revision 1.88  1995/04/20  13:55:51  colas
 * pacth to function param evaluation by Anders Holst <aho@nada.kth.se>
 *
 * problem with XV
 *     believe it or else, but XV thinks that if the position of the client window
 *     is at 0,0 in the WM frame, it thinks that it runs OLWM or MWM and proceed to
 *     look into the window attributes x and y fields to know what is the frame size
 *     OLWM apparently puts there the offsets, and MWM 0,0. (this appears to be
 *     a wrong assumption by looking at MWM source code, BTW)
 *     See xv code: file xvevent.c, HandleEvent, in the case ReparentNotify:
 *
 * Revision 1.87  1995/04/19  15:19:03  colas
 * Michael Welsh Duggan <md5i+@andrew.cmu.edu>: corrections to gwm doc
 * a lot of XQueryTree calls were not followed by XFrees
 *
 * Revision 1.87  1995/04/12  16:24:32  colas
 * Vinnie Shelton <shelton@grind.ICD.Teradyne.COM>:
 *     patches included for better compilation on SVR4 systems
 *     distrib is now a tar of gwm-version_number
 *
 * In makefile: new var EXTRA_INCLUDE for misc -I directives
 *
 * Revision 1.86  1995/04/05  14:43:19  colas
 * *** Version 1.7p_beta_1 ***
 *
 * Revision 1.85  1995/04/05  12:51:45  colas
 * Michael A. Patton <MAP@BBN.COM>:
 *     Sample "diagonal" definitions included in placements.gwm
 *     'rows.diag-ul and 'rows.diag-ur
 *
 * Revision 1.84  1995/04/03  11:13:45  colas
 * XPM files not included anymore!
 * If they are not included in standard, you must in your Make.xxx file
 * gwm-parsers/ fixed
 *
 * ------------------------------------------------------------------------------
 * Anders Holst  (aho@nada.kth.se): <9412121556.AA25326@sans02.nada.kth.se>
 *     There is a small bug in the MWM profile that makes it freeze if
 *     either the variable 'passSelectButton' is set to nil, or if someone
 *     tries to set focus to a window in some other way than clicking in it
 *     files: mwm-icon.gwm mwm-win.gwm
 *
 * ------------------------------------------------------------------------------
 * Anders Holst  (aho@nada.kth.se): <9412121602.AA25335@sans02.nada.kth.se>
 *     Now there is a new version (0.3b) of the VTWM profile for GWM.
 *     Together with it follows a virtual screen package, and an icon manager
 *     package that can be used in other GWM profiles.
 *
 *     In addition to several small bug-fixes and enhencements, these are the
 *     main changes from the last version (0.1):
 *
 *     * All user specifications can now be given in vtwmrc.gwm, which is
 *       thoroughly cleaned up, and contain a lot of comments and
 *       descriptions on how to set most of the variables in the profile.
 *
 *     * Construction of menus is *not* done in a separate file any more,
 *       but is moved into vtwmrc.gwm like all other user specifications.
 *       Several useful tools for construction of menus are added.
 *
 *     * The icon manager is upgraded a lot, and can now also handle normal
 *       icons.
 *
 *     Instead of sending all files out, I have tar:ed and gzip:ed them and
 *     made available through ftp (if you can't reach them there by some
 *     reason, please email me and I will email it back):
 *       thalamus.sans.kth.se:aho/gwm/vtwm-profile-0.2.tar.gz
 *     FILES:
 *     hilite2.xbm
 *     iconify2.xbm
 *     load-icon-mgr.gwm
 *     load-virtual.gwm
 *     menu.xbm
 *     pick.gwm
 *     resize2.xbm
 *     std-func.gwm
 *     virtual-door.gwm
 *     virtual-pan.gwm
 *     virtual.gwm
 *     vtwm-icon-mgr.gwm
 *     vtwm-menu.gwm
 *     vtwm-window.gwm
 *     vtwm-zoom.gwm
 *     vtwm.gwm
 *     vtwmrc.gwm
 *     zoom.xbm
 *     README.icon-mgr
 *     README.virtual
 *     README.vtwm
 *
 * ------------------------------------------------------------------------------
 * Anders Holst  (aho@nada.kth.se): <199501161024.LAA18031@sans02.nada.kth.se>
 *     Here are two more bugfix suggestions. Both concerns the file wops.c
 *     and a patch is included below.
 *
 *     The first one concerns the use of 'window-starts-iconic' to make
 *     windows iconified when they are opened. This collides with the code
 *     for redecorating a window. It says:
 *                 ...
 *     	    ClientWindowInitialMap(cw);
 *     	    if (was_mapped) {
 *     		ClientWindowMap(cw);
 *     		wool_process_unmap_events(window);
 *     	    }
 *                 ...
 *     If the window was visible before restarting, then it may be first
 *     iconified from ClientWindowInitialMap because 'window-starts-iconic'
 *     is set in 'opening', and then immediately mapped with ClientWindowMap
 *     because it was visible before. This causes both the icon and the
 *     window to be visible at the same time.
 *
 *     I suggest that ClientWindowInitialMap is called only when
 *     ClientWindowMap is not. This fixes this problem. (It does not fix all
 *     problems however. When icon-groups or rooms or an icon manager is
 *     used, "iconifying" a window may rather make it withdrawn, which
 *     confuses ClientWindowInitialMap. This seems however very hard to do
 *     anything about...)
 *
 *
 *     The other bug has to do with shaped windows. They always caused an
 *     "X_error: bad pixmap", either when deleted or redecorated. (Try the
 *     xclock icon for example. Get its pixmap with (window-icon-pixmap) a
 *     few times. Then kill the xclock again). This is because the hint in
 *     the window for the pixmap itself is carefully copied to a new pixmap,
 *     but the hint for the shape is taken directly. When the pixmap is
 *     freed, both the pixmap and the shape mask are freed, causing a new
 *     call to (window-icon-pixmap) to use a freed shape mask. I guess that
 *     the shape mask has to be copied to a new pixmap too.
 *
 *     I try to copy the pixmap below, but I am not sure I do it the right
 *     way. The code seems to work, but surely there must be a less
 *     complicated way to copy a pixmap in X11 !!! Please look at it, if you
 *     can do it with less X11 calls (and please tell me how).
 *
 *     FILES: wops.c
 *
 * ------------------------------------------------------------------------------
 * Anders Holst <aho@nada.kth.se>: <199501161045.LAA18054@sans02.nada.kth.se>
 *     Regarding the problem with '(on geometry-change ...)' , I found why
 *     this caused the window to be unmapped when refreshed. This is because
 *     enabling ConfigureNotify events is done with SubstructureNotify,
 *     which causes GWM to get a whole plethora of other events, like for
 *     example UnmapNotify events. These are expected by GWM from enabling
 *     StructureNotify, and therefore the refreshed window itself is
 *     obediently unmapped by GWM when the UnmapNotify event for the subwindow
 *     that was created above it arrives.
 *
 *     The code below checks that the UnmapNotify comes from a
 *     StructureNotify and not SubstructureNotify. It also makes only *one*
 *     of the possibly many ConfigureNotify events that seem to be generated
 *     from a resize or move to trigger the WOOL code i question.
 *
 *     FILES: client.c
 *
 * ------------------------------------------------------------------------------
 * Anders Holst <aho@nada.kth.se>: <199501161320.OAA18188@sans02.nada.kth.se>
 *
 *     additions to the GWM documentation for vtwm and gwmsend/gwmchat
 *
 * ------------------------------------------------------------------------------
 * Anders Holst <aho@nada.kth.se>: <199501221234.NAA10696@sans02.nada.kth.se>
 *
 *     Two more minor bugfixes, both concerning the file "client.c":
 *
 *     First, sometimes the pull-down menus in Emacs 19 get displaced, by an
 *     amount approximately equal to the size of the window decoration (what
 *     about Epoch or Lucid ?). This occurs only for an emacs that was on
 *     screen before gwm, and before the window is moved. I suggest that
 *     'DecorateWindow' (and 'UnDecorateWindow') should do a
 *     'SendSyntheticMoveEvent', since the decoration may actually move the
 *     client window.
 *
 *     Second, the decoration bars around shaped windows get their border
 *     invisible on the right and under them (but not above and to the
 *     left. (That is, the borders of the *bars*, not of the whole window, I
 *     know that the latter should disappear on shaped windows). It seems
 *     that one has to compensate oneself for the bars borders when the shape
 *     is combined.
 *
 * ------------------------------------------------------------------------------
 * Anders Holst <aho@nada.kth.se>: <199501231236.NAA12961@sans02.nada.kth.se>
 *     'rotate-cut-buffers' causes bus error
 *
 * ------------------------------------------------------------------------------
 * Anders Holst <aho@nada.kth.se>: <199502080902.KAA14661@sans02.nada.kth.se>
 *     QUESTION: Isn't 'delete-nth' supposed to always affect all references
 *     to the same list that it removes elements from ?
 *
 *     Currently there are some situations in which the list is first copied,
 *     and only the copy is changed, for example if you write
 *      (setq tmp1 '(a b c d))
 *      (setq tmp2 tmp1)
 *      (delete-nth 'a 'tmp1)
 *     but not if you do
 *      (delete-nth 'a tmp1)
 *
 *     This copying is what did not work, since it tries to evaluate the
 *     elements of the list to copy. The copying can be made
 *     non-evaluating,
 *
 * ------------------------------------------------------------------------------
 * Anders Holst <aho@nada.kth.se>: <199502080945.KAA14709@sans02.nada.kth.se>
 *
 *     'window-wm-state-update' never worked the way it was done. The only
 *     case when it is useful is when there is a user icon, but since this
 *     function calls Update_XA_WM_STATE which does nothing in that case, it
 *     never had any effect.
 *
 *     Further, when using eg. an icon manager, (or icon groups or rooms) the
 *     window may actually be withdrawn when it is "iconified" so it would be
 *     nice to be able to specify which state I want the window to believe it
 *     is in, with an optional argument 'window, 'icon, or () to
 *     'window-wm-state-update'.
 *
 * ------------------------------------------------------------------------------
 * Anders Holst <aho@nada.kth.se>: <199502080952.KAA14717@sans02.nada.kth.se>
 *
 *     This is not a bugfix actually, but a suggestion for an added function.
 *
 *     I was in desperate need for a function to find out the inner
 *     borderwidth of a window. Otherwise I can not place a new menu in
 *     exactly the same place as an old menu (if the borderwidth when I
 *     create the menu is different from the inner-borderwidth when the menu
 *     gets decorated. This caused the window manager to drift slightly when
 *     used with the MWM-profile).
 *
 *     It turned out it almost was there already, it was just a matter of
 *     declaring it.
 *
 * ------------------------------------------------------------------------------
 * Anders Holst <aho@nada.kth.se>: <199502081015.LAA14723@sans02.nada.kth.se>
 *
 *     Here are three small fixes in the file client.c :
 *
 *     * Some programs made GWM crash during opening, since a MapNotify
 *     event was received for the window currently being decorated.This
 *     caused the opening code to be called twice, the second time with
 *     cw->opening being a NULL pointer...
 *
 *     * Other programs made GWM crash during closing, if process-events
 *     happened to be called from 'closing'. The same reason as above, but
 *     because of DestroyNotify on the window or its icon.
 *
 *     * Windows using 'geometry-change' events could cause a crash during
 *     closing because of an DestroyNotify event from the cw->hook mistaken
 *     as one from cw->client.
 *
 * ------------------------------------------------------------------------------
 * Anders Holst <aho@nada.kth.se>: <199502081027.LAA14733@sans02.nada.kth.se>
 *
 *     By some reason clients using a window-icon-window (like xmailwatcher)
 *     caused GWM to create a 'ghost'-window with the icon in when restarted
 *     (that is, a window (in the case of xmailwatcher called "iconShell")
 *     separate from the icon, which sometimes could live on when
 *     xmailwatcher had died, and then was impossible to kill).
 *
 *     I checked how (the real) MWM handled this, and the only difference I
 *     found was that it made sure the icon window was *unmapped* when MWM
 *     finished. I removed the condition in the corresponding GWM code, and
 *     the problem went away.
 *
 *     This makes more sense too. Why let the icon be mapped just because the
 *     client has to be remapped ?
 *
 * ------------------------------------------------------------------------------
 * Anders Holst <aho@nada.kth.se>: <199502131240.NAA00661@sans02.nada.kth.se>
 *     FILE: data/README.vtwm
 *     Some changes in the VTWM-profile from version 0.2 to 0.3 require
 *     changes to the user customizable file "vtwmrc.gwm". To make it easier
 *     to update "vtwmrc.gwm" for users who have modified it, this file
 *     contains a patch with the most important changes.
 *
 * ------------------------------------------------------------------------------
 * Anders Holst <aho@nada.kth.se>: <199503011701.SAA23377@sans02.nada.kth.se>
 *     I have had problems with xv. Its main window drifts away when new pictures
 *     are loaded. TWM did not have the same problem with xv, so I checked that
 *     code to see what was different.
 *
 *     It turned out that this had to do with XConfigureWindow and window
 *     gravity: According to TWM (line 2359 in events.c in the
 *     twm that comes with X11R5), the ICCCM says that the window gravity
 *     should be followed on a ConfigureRequest. Second, it seems that the
 *     default window gravity should be NorthWest, since when you dont say
 *     anything, the win_gravity in the window attributes is set to 1 but the
 *     gravity mask bit in the WM_NORMAL_HINTS are set to zero. And this is
 *     also what both TWM and MWM assumes.
 *
 *     So, when I changed both these things: If no gravity is given the
 *     window gravity is NorthWest, and the window gravity is followed when
 *     obeying a ConfigureRequest, then xv works perfect with GWM too.
 *
 * ------------------------------------------------------------------------------
 * Anders Holst <aho@nada.kth.se>:
 *
 *     set of patches to handle properly windows without gravity hint
 *     set. behavior based upon TWM.
 *
 * ------------------------------------------------------------------------------
 * Anders Holst <aho@nada.kth.se>: <199503061004.LAA29425@sans02.nada.kth.se>
 *
 *     Alvin C Shelton <acs@world.std.com> wrote:
 *     > I've noticed a weird thing with pop-up
 *     > menus: the first time I select an item from a menu the inverting works
 *     > fine, but the next time any items above the one I selected won't be
 *     > inverted, unless I drag past the item I previously selected and back
 *     > up.
 *
 *     Checking the code in std-popups.gwm, and diffing with the older
 *     version from the time when this worked, it seems that someone tried to
 *     make possible different colors for different items. However,
 *     invert-color is not updated often enough, causing this bug as well as
 *     that different colors wouldn't work anyway.
 *
 * ------------------------------------------------------------------------------
 * Anders Holst <aho@nada.kth.se>: <199503061018.LAA29431@sans02.nada.kth.se>
 *     bug report:
 *     (setq tmp 1) (with (tmp 3 tmp 2) tmp)  => tmp is 3 !!! (should be 1)
 *
 *     Fix by colas to wl_func.c
 *
 * Revision 1.84  1995/03/10  07:14:03  colas
 * XPM files not included anymore! you need to get xpm3.4c library (will work
 *      with any Xpm v3 library)
 * If they are not included in standard, you must in your Make.xxx file
 * gwm-parsers/ fixed, automatically used
 * bugs, compiles fixes for linux
 *
 * Revision 1.83  1994/12/09  13:36:59  colas
 * DISTRIBUTION:
 *     all non-xpm or .gwm files moved from data/ into the new contrib/
 *     subdirectory, including:
 *         emacs-mode/  gwmchat/     gwmsh/       rxterm/
 *         gwm-buffer/  gwmsend/     lisp-modes/  widgets/
 * 	command_menus/
 *
 * Anders Holst  <aho@sans.kth.se>:
 *     patches to mwm-utils.gwm for minor bugs
 *     In the distribution there is a
 *     small typo in "obscured-by" that makes it sometimes return the wrong
 *     result. In f.raise_lower is a parenthesis error, that actually makes it
 *     first raise all other windows, before raising the intended window
 *     (which makes it look like the window is lowered before it is
 *     raised...).
 *
 * Anders Holst  <aho@sans.kth.se>:
 *     I wanted a twm-like resize-window, so I began looking in the c-code
 *     for how to do it. Then I discovered that it was enough with a very
 *     small modification to the mwm-like resize-window-style, to make it
 *     handle twm-like resize as well. In the mwm-style, one can "catch" a
 *     corner when dragging along a side. The only thing needed in a twm-like
 *     style, is the same phenomenon when coming from within the window and
 *     hitting a side.
 *
 *     All the code was there already, it was just to relax two if-conditions
 *     a little, to allow treatment of this case. The only difference to the
 *     behavior of the mwm-like resize-style is that if the resize is issued
 *     from eg. a pop-up menu where the pointer ends up inside the window, it
 *     is possible to catch a side by walking out to it. This was not the
 *     case before.
 *
 *     At the same time I discovered a small bug in the mwm-resize-style.
 *     Suppose the standard MWM profile is loaded, and
 *     mwm-resize-style-catch-corners is set to 1. When eg. dragging along
 *     the left side of an xterm or emacs window and hitting the upper left
 *     corner, the window size is not correctly confined to the size-hints,
 *     ie. it will be a fractional number of characters high. The next time a
 *     resize is issued and released without moving, the window will
 *     immediately take a correct size again. This bug was more serious after
 *     having made the above change, so I fixed this too.
 *
 *     After having applied the diff below to the c-code (user.c), this is
 *     what I use as a twm-like resize:
 *
 *     (defun twm-resize-window ()
 *       (with (resize-style 1
 *              mwm-resize-style-corner-size 1
 *              mwm-resize-style-catch-corners 1
 *              cursor (cursor-make 52)
 *              cursor-NW cursor
 *              cursor-NE cursor
 *              cursor-SW cursor
 *              cursor-SE cursor
 *              cursor-N cursor
 *              cursor-W cursor
 *              cursor-S cursor
 *              cursor-E cursor)
 *         (resize-window)))
 *
 * Anders Holst  <aho@sans.kth.se>:
 *     data/gwmsend.c
 *
 * Chan Wilson <cwilson@snarf.engr.sgi.com>:
 *     fixes to clients.c: Update_XA_WM_NAME and Update_XA_WM_ICON_NAME to
 *     display correctly internationalized texts (compounds strings) in window and
 *     icon names
 *
 * Anders Holst  <aho@sans.kth.se>:
 *    "ClientWindowUnmap" was unmapping only the frame, not the client window.
 *
 * Anders Holst  <aho@sans.kth.se>:
 *     It is possible in WOOL to get an empty list that is not equal (nor
 *     "eq") to nil. This caused me complete confusion for hours.
 *     (A related problem, which is fixed at the same time below, is that
 *       (context-save ())
 *     crashes, ie. gives a "WOOL ERROR: Internal error: bus error".)
 *
 * Anders Holst  <aho@sans.kth.se>:
 *     In the gwm-1.7o distribution there is a small bug in "dvrooms.gwm"
 *     causing dvrooms.font to come out as (), instead of as a valid font as
 *     intended. This is because '(defname-in-screen-to () ...)' is used
 *     instead of '(declare-screen-dependent ...)'.
 *
 * Revision 1.82  1994/05/24  14:56:54  colas
 * compiles and runs in R6.
 *
 * Revision 1.81  1994/05/24  14:28:08  colas
 * xpm 3.4b
 *
 * Revision 1.80  1994/05/17  12:28:34  colas
 * David W. Schuler <schuld@vnet.IBM.COM>:
 *     patches to the Imakefile for AIX 1.3.0
 * Eyvind Ness <eyvind.ness@hrp.no>:
 *     could crash on NULL pointer (opening a window which somehow has its
 *     opening field NULL
 * better icon sorter: pack-icons
 * Sven Wischnowsky <oberon@cs.tu-berlin.de>:
 *     patch for the gwm.c file which adds two new options:
 *     -I: this one makes gwm read its stdin (and, of course, evaluate it)
 *     -P: together with -I makes gwm print a simple prompt showing the
 *         number of currently open parentheses
 *     see file data/emacs-mode.readme for details
 *
 * Revision 1.79  1993/09/03  09:37:36  colas
 * *** Version 1.7o ***
 *
 * Revision 1.78  1993/09/03  08:21:52  colas
 * correct tracking of icon pixmap change even from wool with non-rectangular
 *     icons
 * xpm-closeness attributes (defaults to 40000). 0 disables it
 * icon mask hint supported
 * f.exec fixed to call /bin/sh to execute string if only one was given
 *
 * Revision 1.77  1993/08/24  12:30:40  colas
 * icon pixmaps can be non-rectangular (if loaded by xpm)
 * GWM_Propagate_user_events was not reset after send (bruno vasselle)
 *
 * Revision 1.76  1993/07/26  15:28:46  colas
 * Xpm v 3.2g integrated
 *
 * Revision 1.75  1993/07/26  15:06:33  colas
 * Calvin Clark <ckclark@MIT.EDU>:
 *     error in grab lists should not abort decoration
 * mwm: Meta-X no longer maximizes the window by default, since it is annoying
 *     for emacs users (ed.duomo@lambada.oit.unc.edu)
 * more fixes to WM_TAKE_FOCUS Protocol: fix by David Hogan <dhog@cs.su.oz.au>,
 *     reported by John Mackin <john@civil.su.oz.au>
 * simple-win.gwm: typo was preventing color chnage of label with focus-in
 *     bug fix by: Tom Yu <tlyu@MIT.EDU>
 * wbrooms.gwm: new package (like dvrooms) by
 *     William R Burdick <burdick@ecn.purdue.edu>
 *
 * Revision 1.74  1993/05/19  11:52:54  colas
 * *** Version 1.7o (beta 3) ***
 * caret@sword.eng.pyramid.com (Neil Russell):
 * 	Syntax error in wool.c; Should not #include inside a function.
 * rjc@cogsci.edinburgh.ac.uk (Richard Caley):
 * 	should send both WM_TAKE_FOCUS and set focus for clients (lemacs)
 * 	requesting it
 * "Henry S. Thompson" <ht@cogsci.edinburgh.ac.uk>
 * 	(pixmap-make width heigth) now take into account tile if it exists
 * "Mike Marques" <mike@ccs.yorku.ca>:
 * 	calls in the Imakefile contain spaces
 * kenw%bilbo@ihs.com (Ken Weinert)
 * 	def_bitmap.h: charcaters should be unsigned
 * dwight@toolucky.llnl.gov ( Dwight Shih )
 * 	color problems in dvroom name editing
 * Julian Gosnell <jules@tomen-ele.co.jp>
 * 	patches to fix the print-window-info problem.
 *
 * Revision 1.73  1993/02/24  15:37:11  colas
 * more memoryh leaks fixed?
 *
 * Revision 1.72  1993/02/05  16:03:22  colas
 * -k and -K options:
 * Dan Griscom <griscom@media.mit.edu>:
 *     It's the "-k pid" option, which takes a process id as an
 *     argument. When gwm has finished initializing, it sends a signal (SIGALRM
 *     by default) to the given process. So, if you do the following lines:
 *         sleep 15 & pid=$!
 * 	gwm -k $pid &
 * 	wait $pid
 *     then your init shell script will pause until gwm has finished initializing.
 *     -K signal_num sets the signal number to signal_num instead of SIGALRM (14)
 * Steven Charlton <steven@cs.ualberta.ca>:
 *     problems on multi-screen servers (multiple menu items, etc...)
 * Reg Quinton <reggers@julian.uwo.ca:
 *     resource menuShadeColor is not set for mono screens with Mmw profile
 * memory leak seems fixed
 *
 * Revision 1.71  1993/01/14  14:23:36  colas
 * Glen Whitney <gwhitney@math.ucla.edu>:
 *     diffs to correct a couple of bugs in Version 1.1 of the
 *     Mwm emulator for gwm. The bugs were:
 *          1. Mwm profile does not work on monochrome displays.
 *          2. Mwm profile leaves "closed" windows hanging on the screen,
 *             useless. (Thanks to Rodney McDuff, mcduff@newton.physics.uq.oz.au)
 * Olaf Kirch <okir@mathematik.th-darmstadt.de>:
 *     patches for compilation with flex instead of lex, and clean-ups of the
 *     code. You must use the provided wool.flex intead of wool.lex then.
 *     Olaf writes:
 *     When compiling gwm under Linux, I came across the problem that
 *     flex didn't produce the analyzer gwm expected. I therefore
 *     rewrote it to work with flex.
 *
 *     However, there is a little kludge here. You use input redirection
 *     to parse include files and the like. However, currently the
 *     workings of lex are not very well hidden, you always do
 *
 *     	old_type = wool_input_redirect(....,old_buffer);
 *     	/* parse new stream * /
 *     	wool_input_redirect(old_type, 0, NULL);
 *     	wool_unput(old_buffer);
 *
 *     This seems to be too much tailored to the customs of lex. How
 *     about
 *
 *     	wool_input_push(new_type, new_stream);
 *     	/* parse new stream * /
 *     	wool_pop();
 *
 *     That's what I did to make flex work, sort of. I didn't want to
 *     start changing wool.c, so instead, I use the old wool_input_redirect(),
 *     and check if old_buffer is NULL or not. When it isn't, I push the
 *     old buffer onto a stack, and substitute the new one. Else, I pop
 *     the latest buffer from the stack. However, you can easily modify
 *     the code to return a 'buffer handle' when substituting a new stream,
 *     and write wool_replace_stream().
 *
 *     There are a couple of other minor changes I made to make it compile
 *     on Linux. It runs fine (although a bit slow, since I only have 4M at the
 *     moment).
 *
 * Revision 1.70  92/12/09  15:34:13  colas
 * Michael A. Patton <MAP@lcs.mit.edu> : bug when executing unmap+map in wool
 * 
 * Revision 1.69  1992/08/28  12:44:10  colas
 * could do a bus error in idraw popups
 * patch to compile wl_event.c offset code on mips compilers:
 *       -DNO_STRUCTURE_OFFSETS to be added
 * move-opaque.gwm: call to process-exposes to avoid crashes
 *       tsm@cs.brown.edu (Timothy Miller)
 * some windows could be left unmapped
 *
 * Revision 1.68  1992/08/11  09:36:22  colas
 * *** Version 1.7m ***
 * data/mwm-utils.gwm was lacking a newline at the end of file!
 *
 * Revision 1.67  1992/08/10  12:49:57  colas
 * gwm-mode.el renamed into gwm-buffer.el
 *
 * Revision 1.66  1992/08/04  09:10:03  colas
 * *** Version 1.7m ***
 * -DUSE_STANDARD_MALLOC compilation flag for running with purify
 * some bugs and memory leaks corrected by running under PURIFY
 *
 * Revision 1.65  1992/07/29  08:22:23  colas
 * never use CurrentTime in icccm protocols for buggy clients
 *
 * Revision 1.64  1992/07/27  11:08:51  colas
 * map-on-raise was always applied!
 * licence ==> license
 * cstruble@gnu.ai.mit.edu (Craig Struble) Short/int comparison pb
 * load: on absolute pathnames, do not add extensions
 *       check that extension is not already there before adding it
 * active-labels in simple-win corrected
 * blk@vanity.mitre.org (Brian L. Kahn): drop-menus packages for use with epoch
 *     files in data/:
 *     drop-menus.doc drop-menus.gwm style.gwm widgets.el widgets.gwm
 * Calvin Clark <ckclark@mit.edu>: bad handling of the sibling field in
 *     ConfigureRequest events
 * Doug Bogia <bogia@cs.uiuc.edu>: no more button overriding by mon-keys.gwm
 * Mike Fletcher <fletch@cad.gatech.edu>: data/gwm-mode.el interactive buffer
 *     under epoch for remotely executing gwm code
 * Glen Whitney <gwhitney@math.ucla.edu>: new version of the mwm emulator.
 *     mwm files overriden, but saved in mwm-emul-1.0.tar.Z in ftp sites
 *     see mwm-emulation.txt
 * New context flag for windows: ignore-take-focus (default ()). If set, the
 *     WM_TAKE_FOCUS protocol is never used for the window, even if it is set by
 *     the window
 *
 * Revision 1.63  1992/03/02  11:22:06  colas
 * *** Version 1.7l ***
 * fast.gwm (for gwm -f fast), to make a very primitive, fast starting WM
 *     ejb@era.com (Jay Berkenbilt)
 * bugs to twm profile corrected (would core dump after a while) by
 *     Dag Diesen <dagdis@ifi.uio.no>
 * UseInstalledXpm in Imakefile to use already installed XPM library
 *     klute@irb.informatik.uni-dortmund.de (Rainer Klute)
 * epoch.gwm updated.
 * never-warp-pointer variable to disable all calls to warp pointer
 *     tim@stanley.cis.brown.edu (Timothy Miller)
 * some changes for compiling on stellar (Timothy Miller)
 * map-on-raise flag interpreted by decoration to know if we must de-iconify a
 *     window if the client raises it (FrameMaker)
 * button mapping removed from mon-keys.gwm
 *     Doug Bogia <bogia@cs.uiuc.edu>
 * (setq confine-windows t) was overriden by unconf-move in .profile.gwm
 *     Doug Bogia <bogia@cs.uiuc.edu>
 * gwm now ignores directories when loading files
 *
 * Revision 1.62  1992/02/12  14:50:23  colas
 * *** Version 1.7k ***
 * 'background key in meter function was ignored
 *     (bug report from "Mike Marques" <mike@apex.yorku.ca>)
 *
 * Revision 1.61  1992/02/11  15:25:18  colas
 * *** Version 1.7k ***
 * Philippe Kaplan's additions of hooks for external window management (not yet
 *     documented...)
 * (property-change any) is now possible (a stupid check was preventing it)
 *
 * Revision 1.60  1992/02/11  14:43:39  colas
 * *** Version 1.7j ***
 * new resources for simple-icon:
 *     simple-icon.plug-name forces the icon name
 *     simple-icon.no-center-plug forces icon to ignore application pixmap
 *
 * Revision 1.59  1992/02/11  10:01:26  colas
 * *** Version 1.7j ***
 * Michael Bloom <mb@ttidca.tti.com> patches for interviews
 * data/en-recover.gwm utility code from Eyvind Ness <eyvind@hrp.no>
 * new simple-icon.plug-name argument to simple-icon
 * window-group has now sanity checks to always return a list or ()
 * potential bug in GC fixed
 *
 * Revision 1.58  1991/10/21  09:32:13  colas
 * *** Version 1.7i ***
 * traps idraw bad hints
 *
 * Revision 1.57  1991/10/04  13:37:58  colas
 * *** Version 1.7h ***
 * ***** R5 TAPE VERSION *****
 * uses xpm v3.0
 * icons with no center plug resize when bars resize Mike Liang <mike@lsil.com>
 *
 * Revision 1.56  1991/10/02  17:04:16  colas
 * *** Version 1.7g ***
 * bug in error message in undefined key (Philippe Kaplan)
 * xpm/Imakefile corrections by Pekka.Nikander@ngs.fi
 * bug with some false window-group hints
 * data/amc-lisp.el copyright changed to X one, with author agreement
 * Doug Bogia patches for resize into upper-left of screen
 * confined resize problems fixed
 * blk@zoot.cca.cr.rockwell.com (Barry L. Kaplan) GWM-Lisp major mode for Emacs
 *   included in distrib
 * Hugues.Leroy@irisa.fr (Hugues Leroy) additions of rxcmdtool to rxterm.script
 * PROBLEMS file for listing hints & tips
 *
 * Revision 1.55  1991/09/13  08:21:32  colas
 * *** Version 1.7f ***
 * (send-key-to-window "aB@#" alone) works as expected
 *
 * Revision 1.54  1991/09/13  08:12:58  colas
 * *** Version 1.7f ***
 * Upgrade of xpm (3.0b) to read xpm v2 pixmaps
 * Do trap bad transient-for windows
 * blk@zoot.cca.cr.rockwell.com (Barry L. Kaplan) bug in submenu execution in mwm
 *
 * Revision 1.53  1991/09/10  12:43:52  colas
 * small bug in doc
 *
 * Revision 1.51  1991/09/10  12:21:38  colas
 * XPM v3.0a library included
 *
 * Revision 1.50  1991/09/10  09:17:51  colas
 * malloc.c from wool.
 * no more resource manager bugs. were due to ...
 * no more XIOError trapping.
 *
 * Revision 1.49  1991/09/06  15:28:39  colas
 * xpm/FILES included
 * color-make-rgb function added by dab@berserkly.cray.com (David Borman)
 * do not use bzero anymore on sparc
 *
 * Revision 1.48  1991/08/29  11:24:56  colas
 * *** Version 1.7d ***
 * Transient-for windows buggy
 *
 * Revision 1.47  1991/08/28  15:10:52  colas
 * *** Version 1.7c ***
 * Compiles with R5
 * John Carr <jfc@ATHENA.MIT.EDU> patches for IBM RT and RS/6000, X11R5, ansiC
 * more careful during window decoration (for xrn crashes)
 * Sun4 bugs corrected
 * window-to-client and client-to-window functions
 *
 * Revision 1.46  1991/08/22  07:28:48  colas
 * machine names truncated to first dot
 *
 * Revision 1.45  1991/08/20  08:14:32  colas
 * bug in flushing grab events queue
 *
 * Revision 1.44  1991/08/15  16:44:15  colas
 * patches to README by MAP@lcs.mit.edu (Michael A. Patton) + revised myself
 * window-icon? documented and coded in C
 *
 * Revision 1.43  1991/08/15  14:40:10  colas
 * *** Version 1.7a ***
 * window-is-valid & wob-is-valid functions
 * data/Imakefile repaired
 * NOXEHTYPE flag for pre-R4 includes
 * make wool makes a command-line interpreter version
 * dvrooms additions by Dwight Shih <dwight@s1.gov>
 *
 * Revision 1.42  1991/08/14  09:37:31  colas
 * *** 1.7 ***
 * distrib details
 *
 * Revision 1.41  1991/08/13  17:11:45  colas
 * *** 1.7 ***
 * small bugs
 *
 * Revision 1.40  1991/08/07  14:54:20  colas
 * (draw-text pixmap x y font text)
 * bug in redecorating windows in groups fixed
 * profile-colas.gwm included
 *
 * Revision 1.39  1991/07/29  14:57:30  colas
 * *** 1.7_epsilon ***
 *
 * Revision 1.38  1991/07/29  13:35:11  colas
 * place-button with 3D-look
 * null string for the window-client-name badly matched (patch by Doug Bogia)
 * no-set-focus not always set properly (patch by Doug Bogia)
 * amc-lisp.el emacs style included
 * data/Imakefile automatically generated
 * fixed crash when client quits immediately after a colormap change request
 *     (problem showed by Frank Mangin <mangin@sa.inria.fr>)
 * no more use of makedepend in makefile
 * draw-rectangle
 * color-free
 * no need to put pressed button as modifier for release events anymore
 * double-buttonpress was bugged: acted as a buttonpress
 *
 * Revision 1.37  1991/06/24  11:50:08  colas
 * madler@apollo.com (Michael Adler) dims_outer_to_inner() in client.c:
 *     failed to account for the border width of the decoration
 * Eyvind Ness <eyvind@hrp.no> near-mouse placement policy included in file
 *     near-mouse.gwm
 * mods to isolate wool from gwm
 * client moving only one coord would move to 0
 * xid-to-wob function
 * delete-read-properties pointer to delete x properties after reading them
 * GWM_EXECUTE gets deleted after being read
 * client-name context var was ignored in place-menu
 * client-info menu wasn't movable
 * new place-button in utils.gwm
 *
 * Revision 1.36  1991/04/29  16:17:03  colas
 * window-icon checks for window existence
 *
 * Revision 1.35  1991/04/11  15:02:25  colas
 * (= "foo" 'foo) didn't work!
 * good resource for host names with dots in them DGREEN@IBM.COM (Dan R.Greening)
 * proper use of * in resource specs
 * uses xpm library v3.0a
 * tar archive includes gwm dir (tar c gwm instead of tar c .)
 *
 * Revision 1.34  1991/04/05  14:25:17  colas
 * William.Lott@ARDATH.SLISP.CS.CMU.EDU: (wob-borderpixel <pixmap>) broken
 * restart code fixed also in mwm.gwm and twm.gwm
 * keys works in vscreen window now
 * no-decoration.gwm wasn't included
 * John Carr patches for IBM PC-RTs under BSD (AOS), not AIX
 * Mike Newton patches for building on DG AViiON
 * Dan R. Greening patches to Imakefile for IBM RS-6000
 * sven@tde.lth.se (Sven Mattisson) patches for Sequent Symmetry DYNIX 3.0.17
 * meters stay in place
 *
 * Revision 1.33  91/01/11  17:03:13  colas
 * John Carr <jfc@ATHENA.MIT.EDU>: fixes for correct argument types in functions
 * 
 * Revision 1.32  91/01/09  14:25:35  colas
 * Jay Berkenbilt <qjb@ATHENA.MIT.EDU>: closing code executed at the end/restart
 *     of gwm was just value of closing, causing random crashes
 * icon-groups-old.gwm  simple-icon-old.gwm  simple-win-old.gwm files were
 *     missing from distrib (for mwm and twm profiles)
 * 
 * Revision 1.31  91/01/08  18:30:58  colas
 * av@cs.uta.fi (Arto V. Viitanen) ids too long for appolo C compiler
 * Mike Newton: new version of mon-keys.gwm
 * iconification of clients not matched by describe-window failed
 * Mike Newton: pop menu on root with any modifier on menu button
 * Mike Newton: patches to gwm_utils & compile flags for Data General AViiON
 * tom@srl.mew.mei.co.jp (Tom Borgstrom): OBJS1 in Imakefile
 * Malloc Bug in xpm librairy for monochrome pixmaps
 * 
 * Revision 1.30  91/01/02  16:08:15  colas
 * *** Version 1.7_gamma ***
 * basebars on shaped windows was buggy
 * John Mackin <john@cs.su.oz.au> corrections to the man page
 * send-key-to-window adds automatically shift as modifier if letter is uppercase
 *     Frank Mangin <Frank.Mangin@mirsa.inria.fr>
 * CHANGES file updated (no doc yet)
 * 
 * Revision 1.29  90/12/27  18:47:43  colas
 * new events enter-window-not-from-grab & leave-window-not-from-grab
 * default action for menus now works
 * random bus error bug in redecorate window fixed
 * menu labels can now be pixmaps or lists (evaluated)
 * "\xXX" was bugged
 * new color-components function
 * 
 * Revision 1.28  90/12/26  20:40:04  colas
 * 1.7_beta version
 * 
 * Revision 1.27  90/12/26  20:17:57  colas
 * icon-groups-old.gwm included in distrib
 * quick & dirty patches to twm & mwm modes to run with 1.7 standard profile
 * reparse-standard-behaviors simplified
 * xpm v2.8 library included
 * colors in pixmap-load can be either string or pixel value (number)
 * qjb vscreen package
 * now you must include standard-behavior in your fsms, is not included anymore
 *     in window/icon-behavior
 * 
 * Revision 1.26  90/12/13  18:48:51  colas
 * rxterm.script recognize full internet adresses
 * rotate-cut-buffer was bugged
 * 
 * Revision 1.25  90/12/11  17:58:07  colas
 * Wool bug: (boundp '()) was returning ()!
 * new "customize" function to generically redefine decos by context
 * set-icon now works again
 * "client info" menu item pops up a window
 * try to trap bus errors in current-mouse-pos (in move-opaque)
 * clients with window-icon started iconic before gwm starts work now OK
 *   (bug report: John Mackin <john@cs.su.oz.au>, David Hogan <dhog@cs.su.oz.au>)
 * 
 * Revision 1.24  90/11/29  17:10:45  colas
 * setting window-size bugged on windows which diddn't have resize incr hints
 * warning: float package makes redecorate on an icon crash
 * (pixmap-load filename [symbol1 color1 ... symbolN colorN])
 * icon-groups add iconify others in group
 * icon boxes of phk
 * all resources uses the class.name.name.machine resource spec
 * trace package included
 * raise-on-move, -resize and -iconify
 * make-string-usable-for-resource-key traps now blanks as well as . *
 * decos are evaluated ONLY at decoration time, not at profile read time
 * decos are recursively evaluated till obtaining a window description
 * do not call opening code on unmapped window group leaders
 * 
 * Revision 1.23  90/11/13  16:44:21  colas
 * the propagate argument to send-user-event stayed set after function invocation
 * reads xpm v2 pixmap files
 * 
 * Revision 1.22  90/11/08  19:03:23  colas
 * menu-make skips nil entries
 * float up or down
 * was mis-interpreting some WM hints
 * didn't handle resizes or move too early after initial mapping (this one was
 *        a tought one!)
 * multi-item entries in menus
 * menu code reorganized
 * simple-icon and term-icon customizable
 * traps Program sizes <= 1 for this brain-dead motif 1.1!
 * could crash if window was destroyed too fast
 * default menu action triggered if clicked too fast for menu to pop
 * 
 * Revision 1.21  90/10/22  11:52:47  colas
 * removed Scott Blachowicz's modifs to placement.gwm (bus errors?)
 * duanev@mcc.com (Duane Voth) buttonrelease inst. of press on icon of s-ed-ed-win
 * simple-win title stayed in gray
 * Rod Whitby: floating windows (was called auto-raise) (float)
 * 	    unconfined-move (unconf-move)
 * 	    F5 and F7 (suntools-keys)
 * 
 * Revision 1.20  90/08/02  14:39:35  colas
 * Scott Blachowicz  new patches to placements.gwm
 * 
 * Revision 1.19  90/07/26  18:16:02  colas
 * Carl Witty <cwitty@portia.stanford.edu> patches included:
 *     confined windows could get 1 pixel off the right & down of screen
 *     mwm profile:
 *         f.maximize corrected:
 *             If executed from a menu, it didn't correctly change the
 * 	    state of the window's zoom button
 * 	    It did set the zoom button's pixmap incorrectly, although
 * 	    this was corrected as soon as a leave-window event is generated.
 * 	keyboard menu traversal more sensible
 * 	new functions:
 * 	    (f.eval (? "Hello")) will print hello when it is executed.
 * 	    (f.identify) pops up a window which giving information about the
 * 	    current window.
 * 	    (f.delete) uses the WM_DELETE_WINDOW protocol to delete a window.
 * 	    (f.load "mwm") will reload the mwm profile.
 * 	    (f.raise_lower) is my version, which doesn't raise a window unless
 * 	    another on-screen window actually obscures it.
 * 	    (f.raise_move) is obvious.
 * 	    (f.refresh_win) didn't do anything before; now it does.
 * 	    (f.warpto "emacs") warps the mouse to the first emacs window found
 * 	    and brings it to the front.  I implemented it for a friend who 
 * 	    likes twm.
 * 	I didn't like the look of disabled menu items in the mwm profile, with
 * 	    that line drawn through them, so I added some modifications to 
 * 	    allow disabled menu items to be displayed in a different color
 * 	    instead.
 *     Here are some patches to make the icon manager in the twm profile act
 *         more like the one in the X11R4 twm.  That is, when the cursor is in
 * 	the icon manager, the focus is set to the window whose bar the cursor
 * 	is over.  The patches also change it so that the icon manager doesn't
 * 	resize when a window is iconified or deiconified.
 * 
 * Revision 1.18  90/07/26  17:10:08  colas
 * John Carr <jfc@ATHENA.MIT.EDU>: redecorating windows have no background
 *     (was not added last time)
 * dvroom.auto-add
 * XGetProperties were not tested enough for failure: pb of iconification
 * (exit insts...) were evaluating inst after restoring old context!
 * exit out of a for didn't restore correcetly the old values
 * screen-tile & icon-pixmap are defaulted-to intead of set
 * select for events sooner on windows, should not loose early resize events
 * casey@gauss.llnl.gov (Casey Leedom): namespace doc entries out of order
 * Scott Blachowicz <scott%grlab%hpubvwa@beaver.cs.washington.edu>
 *     patch to placements.gwm for decreasing lists
 * 
 * Revision 1.17  90/05/28  16:04:36  colas
 * *** Version 1.6b ***
 * Jay Berkenbilt <qjb@ATHENA.MIT.EDU> east gravities bugged
 * MW_STATE now overrides initial_state, even in "normal" case
 * John Carr <jfc@ATHENA.MIT.EDU>: redecorating windows have no background
 * 
 * Revision 1.16  90/05/23  18:06:18  colas
 * duanev@mcc.com (Duane Voth): dvrooms maintain a GWM_ROOM window property 
 *     on window to put them back in room managers 
 * mwm profile: closing a xmh sub window was generating X errors
 * button 2 in icon of simple-ed-win works now 
 *     (Andrew Simms <ams@acm.Princeton.EDU>)
 * tmiller@suna0.cs.uiuc.edu withdrawing a client having an "user-icon" set
 *     didn't work (such as a dvroom)
 * 
 * Revision 1.15  90/05/21  10:57:55  colas
 * Rod Whitby <rwhitby@austek.oz.au> added the variable "confine-windows" to keep
 *     interactive moves and resizes confined to screen boundaries.
 * added confine-windows support to move-opaque
 * More Muller patches to mwm emulation code
 * Rod Whitby <rwhitby@austek.oz.au> xterm-list more defaultable
 * cwitty@portia.stanford.edu reallocating when adding strings could fail if
 *     new_size was > 2*old_size
 * when match fails, returns () when caleed with 2args, "" otherwise.
 * Doug Bogia <bogia@suna0.cs.uiuc.edu> The no-set-focus did not allow the 
 *     autocolormap to work.
 * new global values class-name & client-name for class of placed menus
 * 
 * Revision 1.14  90/05/09  15:18:47  colas
 * *** Version 1.6a ***
 * icon-groups: new members always started iconic if leader had -iconic flag
 * deplibs added in Imakefile (hleroy@irisa.fr (Hugues Leroy))
 * compil without USER_DEBUG dacseg@uts.amdahl.com (Scott E. Garfinkle)
 * icon description evalution was done too early
 * bus errors trapped Ray Nickson <Ray.Nickson@comp.vuw.ac.nz>
 * mult-screen confusion fixed <eirik@elf.TN.Cornell.EDU>
 * map on an alerady mapped leader bug <carroll@suna0.cs.uiuc.edu>
 * WM_CLIENT_MACHINE was atom inst.of string Carl Witty <cwitty@cs.stanford.edu>
 * Dana Chee <dana@bellcore.com> bitmaps stipple were offset
 * Dana Chee blank pixmaps in mwm icons
 * Jay Berkenbilt <qjb@ATHENA.MIT.EDU> replacing menu inside screen was
 *     forgetting borders
 * could drop resize events Simon Kaplan <kaplan@cs.uiuc.edu>
 * bug in destroying placed menus fixed Carl Witty <cwitty@portia.stanford.edu>
 * (pop-menu 'here) was buggy not work (--> no cascading mwm menus)
 * kill gwm was doing a fatal error, quits now.
 * 
 * Revision 1.13  90/03/30  15:45:55  colas
 * *** Version 1.6 ***
 * could unmap group leader on startup
 * DecorateWindow was resetting TargetWob
 * bar with nothing didn't took size of pixmap (Gilles.Muller@irisa.fr)
 * icon group works on clienst iconified on stratup
 * iconify on startup calls (iconify-window)
 * icon-groups.excluded
 * 
 * Revision 1.12  90/03/28  18:42:11  colas
 * Gilles.Muller@irisa.fr: mwm error when closing xmh acc. window
 * mwm bugs when closing xmh sub-windows
 * can put a 'expr. in set-icon (Steve Anderson <anderson@sapir.cog.jhu.edu>)
 * do not map withdrawn windows on ending
 * doc was saying 'fixed instead of 'here. doc corrected.
 * deltabutton
 * 
 * Revision 1.11  90/03/23  18:54:18  colas
 * bug repaired in ref count: Vincent Bouthors <vincent@mirsa.inria.fr>
 *    (: a (:b 20)) (with (a 0) (: b 0)) would free the number 20
 * OpenWindow could modify the current window
 * withdrawing a window could destroy it
 * no user-positioning for transient windows cwitty@CSLI.STANFORD.EDU(Carl Witty)
 * traps bogus inc hints
 * 
 * Revision 1.10  90/03/20  16:10:56  colas
 * bar-min-width was ignored (Carl Witty <cwitty@portia.stanford.edu>)
 * shaped windows were offset by window borderwidth
 *     (Carl Witty <cwitty@portia.stanford.edu>)
 * simple-icon wasn't working in multi screen mode (found by
 *     Doug Bogia (bogia@cs.uiuc.edu))
 * changing the name after editing window name left in black/black
 *     Doug Bogia (bogia@cs.uiuc.edu)
 * window-icon-pixmap-id
 * patch to the README file by casey@gauss.llnl.gov (Casey Leedom)
 * or returned an un-evaluted argument!
 * new sort-icons and (rows.limits 'sort func)
 * reparse-standard-behaviors
 * action-button, select-button, menu-button
 * current-event-... return 0 (instead of ()) if event wasn't relevant
 * 
 * Revision 1.9  90/03/12  17:34:08  colas
 * Doug Bogia <bogia@cs.uiuc.edu>: patches to the no-set-focus code
 * 
 * Revision 1.8  90/03/07  18:14:51  colas
 * *** Version 1.5e ***
 * zoom-win.gwm renamed as mwm-zoom-win.gwm
 * GetWMNormalHints used
 * no more use of obsolete flags
 * 
 * Revision 1.7  90/02/28  21:52:02  colas
 * *** Version 1.5d ***
 * arup's patches to twm
 * doc of simple-icon.legend (false is now default)
 * if placed a menu before wool_host_name was referenced, crash
 * move-opaque part of distrib
 * bugs in mwm
 * better mwm menus
 * mwm de-iconify raises
 * 
 * Revision 1.6  90/02/21  15:18:06  colas
 * *** Version 1.5c ***
 * window-icon checked for existence
 * machine name set on menus
 * new hostname active value
 * elapsed-time returns real time in milliseconds
 * mwm was asking for placement of user-positioned windows
 * window-icon? was bugged
 * mwm didn't set colormaps
 * 
 * Revision 1.5.1.9  90/02/15  19:54:36  colas
 * *** Version 1.5b ***
 * Rod Whitby <rwhitby@adl.austek.oz.au> : visibility events
 * new naming with $Version: (in file gwm.shar,n)
 * GWM_EXECUTE built in
 * make-string-usable-for-resource-key (no more in window-name)
 * GWM_RUNNING property and GwmWindow hidden window
 * mwm profile gets resources by class and show window icon names on icons
 * 
 * Revision 1.5.1.8  90/02/14  17:59:40  colas
 * icon-groups could make windows disappear
 * restart with icon windows works now.
 * -r for retrying, waiting for you to kill other wm
 * menu popping could break (parent kept being set to old window)
 * do not take into account icon windows in window groups
 *     Michael Urban <urban%rcc@rand.org>
 * only check for WM_STATE on startup rjt@sedist.cray.com (Randy Thomas)
 * nice man page by casey@gauss.llnl.gov (Casey Leedom) (gwm.man)
 * delete-window no longer have the side-effect of changing current wob
 * standard profile: icons update names on window-icon-name change
 * window-icon-name on root was crashing gwm
 * all register decls removed
 * 
 * Revision 1.5.1.7  90/02/01  14:52:27  colas
 * re-install colormap if client changes its WM_COLORMAP_WINDOWS list
 * casey@gauss.llnl.gov (Casey Leedom) patches to the Imakefile
 * dummy man page
 * 
 * 
 * Revision 1.5.1.6  90/02/01  11:16:07  colas
 * patch for interactive386 by drich%dialogic@uunet.UU.NET (Dan Rich)
 * patch from John Carr <jfc@ATHENA.MIT.EDU> for keeping same screen
 *     during opening & close 
 * term-icon as idraw drawings
 * juha@tds.kth.se (Juha Sarlin): bugs in RCSRevNum in doc
 * 
 * Revision 1.5.1.5  89/12/20  19:05:27  colas
 * december release of arup's twm package
 * menu min/max width was ignored when resized
 * process-events nary
 * flag reenter_on_opening
 * 
 * Revision 1.5.1.3  89/12/20  15:25:08  colas
 * 2 patches from dab@berserkly.cray.com (David Borman) for DEC BSD4.4 (-DSTUPID)
 * x, y, gravity, borderwidth & borderpixel in "meter"
 * meter returns list of previous values that can be re-given to meter
 * Redecorating sometimes unmapped the window
 * meter nows shrinks and grows (was only growing before)
 * window-creep fixed
 * Dlists package cleaned up
 * 
 * Revision 1.5.1.2  89/12/07  17:41:09  colas
 * map-notify event
 * 
 * Revision 1.5.1.1  89/12/05  13:36:20  colas
 * new rcs system
 * was sending 2 icccm events on resize+move
 * on withdraw request, undecorate the window to start afresh on new map.
 * menu-max-width & menu-min-width
 * 
 * Revision 1.5  89/11/27 18:10:00 colas
 * X11R4 RELEASE	
 * - is now n-ary (- n1 (+ n2 n3 n4))
 */

#include "wool.h"
DECLARE_strchr;

char GWM_version_name[20];

GWM_print_banner()
{
    strncpy(GWM_version_name, strchr(RCS_Version, ' ') + 1,
	    strchr(RCS_Version + 1, '$') - strchr(RCS_Version, ' ') - 2);
    printf("GWM %s %s\n", GWM_version_name,
#ifdef DEBUG
	   " (debug)"			/* debug-mode banner */
#else					/* DEBUG */
	   " (gwm@mirsa.inria.fr)"
#endif					/* DEBUG */
	   );
}

/* WARNING: This is an internal rcs number for my eyes only. The official
 * one is found in RCS_Version!
 */

static char           *RCS_Header = 
"$Id: gwm.shar,v 1.115 1995/12/08 07:51:55 colas Exp $";
