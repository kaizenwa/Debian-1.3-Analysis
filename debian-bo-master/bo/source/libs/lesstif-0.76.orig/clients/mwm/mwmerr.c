/**
 *
 * $Id: mwmerr.c,v 1.2 1996/10/04 03:35:28 miers Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

/*
 * MLM - THIS FILE WILL PROBABLY GO AWAY.
 */
#include "mwm.h"

static char *
error_text[] = {
    "Unable to warp to unmanaged screen %d",
    "Another window manager is running on screen %d",
    "failed to load font: %.100s",
    "cannot find an appropriate font: %.100s",
    "%s: %s on line %d of configuration file \"%s\"",
    "%s: %s on line %d of specification string",
    "[XmbTextPropertyToTextList]:",
    "     Locale (%.100s) not supported. (Check $LANG).",
    "Insufficient memory for client window framing",
    "I/O error on display:",
    "Cannot restart the window manager",
    "Insufficient memory for graphics data",
    "Could not make icon to go in icon box",
    "Insufficient memory for icon creation",
    "Invalid icon bitmap",
    "Invalid root for icon bitmap",
    "Insufficient memory to bevel icon image",
    "Insufficient memory for bitmap %s",
    "Unable to read bitmap file %s",
    "Invalid bitmap file %s",
    "Insufficient memory to create icon box data",
    "Could not open display.",
    "Insufficient memory for Screen data",
    "Unable to manage any screens on display.",
    "Cannot configure X connection",
    "Insufficient memory for Workspace data",
    "Insufficient memory for displayString",
    "Insufficient memory for window manager data",
    "Insufficient memory for screen names",
    "Insufficient memory for displayString",
    "Insufficient memory for icon placement",
    "Insufficient memory to create icon box data",
    "Menu specification %s not found",
    "Insufficient memory for menu %s",
    "Menu specification %s not found",
    "Menu recursion detected for %s",
    "Insufficient memory for menu %s",
    "Insufficient memory for menu %s",
    "Insufficient memory for window management data",
    "Insufficient memory for window manager flags",
    "Insufficient memory to XInternAtom _MOTIF_WM_QUERY_nn",
    "Failed to own _MOTIF_WM_QUERY_nn selection",
    "Insufficient memory for window manager data",
    "Lost _MOTIF_WM_QUERY_nn selection",
    "Insufficient memory to convert _MOTIF_WM_QUERY_nn selection",
    "Insufficient memory for window manager data",
    "Key bindings %s not found, using builtin key bindings",
    "Button bindings %s not found, using builtin button bindings",
    "Cannot open configuration file",
    "Insufficient memory for menu accelerators",
    "Insufficient memory to get LANG environment variable.",
    "Insufficient memory for menu",
    "Expected '{' after menu name",
    "Insufficient memory for menu item",
    "Invalid mnemonic specification",
    "Insufficient memory for accelerator specification",
    "Invalid accelerator specification",
    "Insufficient memory",
    "Missing group specification",
    "Invalid group specification",
    "Invalid number specification",
    "Expected '{' after button set name",
    "Insufficient memory for button specification",
    "Invalid button specification",
    "Invalid button context",
    "Expected '{' after key set name",
    "Insufficient memory for key specification",
    "Invalid key specification",
    "Invalid key context",
    "Insufficient memory for screen names",
    "Insufficient memory for client data",
    "Couldn`t make icon box",
    "mwm cannot convert property",
    "as clientTitle/iconTitle: XmbTextPropertyToTextList.",
    "insufficient memory to convert property",
    "as clientTitle/iconTitle: XmbTextPropertyToTextList",
    "mwm receives unknown property as clientTitle/iconName :",
    "property ignored.",
    "Retrying - using builtin window menu"
};
