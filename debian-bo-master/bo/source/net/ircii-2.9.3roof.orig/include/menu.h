/*
 * Here we define how our menus are held
 *
 * @(#)$Id: menu.h,v 1.9 1995/09/03 13:45:20 mrg Exp $
 */

#ifndef __menu_h_
#define __menu_h_

#define IRCII_MENU_H

#define	SMF_ERASE	0x0001
#define	SMF_NOCURSOR	0x0002
#define	SMF_CURSONLY	0x0004
#define	SMF_CALCONLY	0x0008

/* Below are our known menu functions */
	void	menu_previous _((char *));	/* Go to previous menu */
	void	menu_submenu _((char *));	/* Invoke a submenu */
	void	menu_exit _((char *));		/* Exit the menu */
	void	menu_channels _((char *));	/* List of channels menu */
	void	menu_command _((char *));	/* Invoke an IRCII command */
	void	menu_key _((char));
	void	load_menu _((char *));
	int	ShowMenu _((char *));
	int	ShowMenuByWindow _((Window *, int));
	void	enter_menu _((unsigned char, char *));
	void	set_menu _((char *));

#endif /* __menu_h_ */
