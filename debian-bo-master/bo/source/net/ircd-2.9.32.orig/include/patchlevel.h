/************************************************************************
 *   IRC - Internet Relay Chat, include/patchlevel.h
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 * PATCHes
 *
 * Only put here ADDED special stuff, for instance: ".mu3" or ".ban"
 * Please start the patchlevel with a '.'
 *
 * IMPORTANT: Since u2.9 there is a new format of this file. The reason
 * is that this way it shouldn't be needed anymore for the user to edit
 * this manually !!!
 * If you do, be sure you know what you are doing!
 * 
 * For patch devellopers:
 * To make a diff of your patch, edit any of the below lines containing
 * a "" (an EMPTY string). Your patch will then succeed, with only an
 * offset, on the first empty place in the users patchlevel.h.
 * Do not change anyother line, the '\' are to make sure that the 'fuzz'
 * will stay 0. --Run
 *
 */

#define PATCH1	\
		\
		\
		\
		".32"

/*
 Deliberate empty lines
*/

#define PATCH2	\
		\
		\
		\
		""

/*
 Deliberate empty lines
*/

#define PATCH3	\
		\
		\
		\
		""

/*
 Deliberate empty lines
*/

#define PATCH4	\
		\
		\
		\
		""

/*
 Deliberate empty lines
*/

#define PATCH5	\
		\
		\
		\
		""

/*
 Deliberate empty lines
*/

#define PATCH6	\
		\
		\
		\
		""

/*
 Deliberate empty lines
*/

#define PATCH7	\
		\
		\
		\
		""

/*
 Deliberate empty lines
*/

#define PATCH8	\
		\
		\
		\
		""

/*
 Deliberate empty lines
*/

#ifdef TESTNET
#define PATCH9	\
		\
		\
		\
		".testnet"
#else
#define PATCH9 ""
#endif

/*
 Deliberate empty lines
*/

/* Do NOT edit those: */

#ifndef BASE_VERSION
#define BASE_VERSION "u2.9"
#endif 

#ifndef MAJOR_PROTOCOL
#define MAJOR_PROTOCOL "09"
#endif
