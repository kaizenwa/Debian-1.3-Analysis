/**
 *
 * $Id: Protocols.c,v 1.9 1996/12/17 03:24:02 miers Exp $
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

static char rcsid[] = "$Id: Protocols.c,v 1.9 1996/12/17 03:24:02 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/ProtocolsP.h>
#include <Xm/VendorSEP.h>

#include <XmI/DebugUtil.h>

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void _XmProtocolHandler(Widget w, XEvent *evp, String *params, Cardinal *nparams);

/*
 * resources 
 */
#define Offset(field) XtOffsetOf(XmProtocolRec, protocol.field)
static XtResource resources[] = {
    {
	XmNextensionType, XmCExtensionType, XmRExtensionType,
	sizeof(unsigned char), XtOffsetOf(XmProtocolRec, ext.extensionType),
	XmRImmediate, (XtPointer)XmPROTOCOL_EXTENSION
    },
    {
        XmNprotocolCallback, XmCProtocolCallback, XmRCallback,
        sizeof(XtCallbackList), Offset(callbacks),
        XmRImmediate, (XtPointer)NULL
    }
};

XmProtocolClassRec xmProtocolClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &xmExtClassRec,
        /* class_name            */ "XmProtocolClass",
	/* widget_size           */ sizeof(XmProtocolRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ FALSE,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ NULL,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ 0,
	/* compress_exposure     */ 0,
	/* compress_enterleave   */ 0,
	/* visible_interest      */ 0,
	/* destroy               */ destroy,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ NULL,
	/* get_values_hook       */ _XmExtGetValuesHook,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
        /* display_accelerator   */ NULL,
	/* extension             */ NULL
    },
    /* XmExtObject part */
    {
        /* syn_resources      */ NULL,
        /* num_syn_resources  */ 0,
        /* extension          */ NULL
    },
    /* Protocol Class part */
    {
        /* extension             */ NULL
    }
};

WidgetClass xmProtocolClass = (WidgetClass)&xmProtocolClassRec;

static void
class_initialize()
{
    XdbDebug(__FILE__, NULL, "Protocol ClassInitialize\n");
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XdbDebug(__FILE__, NULL, "Protocol ClassPartInitialize\n");
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "Protocol Initialize\n");

    Protocol_PreHook(new_w).callback = NULL;
    Protocol_PostHook(new_w).callback = NULL;
}

static void
destroy(Widget w)
{
    XdbDebug(__FILE__, w, "Protocol Destroy\n");
}

static Boolean
set_values(Widget current,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "Protocol SetValues\n");

    return True; /* FIX ME */
}

static XmAllProtocolsMgr
__XmGetAllMgr(Widget shell)
{
    XmAllProtocolsMgr newman;

    if (shell == NULL || !XmIsVendorShell(shell))
	return NULL;

    newman = (XmAllProtocolsMgr)_XmGetWidgetExtData(shell,
						    XmPROTOCOL_EXTENSION);

    XdbDebug(__FILE__, shell, "Get AllMgr: %08x %08x\n", shell, newman);

    return newman;
}

static XmProtocolMgr
__XmAddProperty(XmAllProtocolsMgr mgrs, Atom property)
{
    if (mgrs->max_protocol_mgrs == 0) {
	mgrs->max_protocol_mgrs = 8;
	mgrs->protocol_mgrs =
		(XmProtocolMgr *)XtMalloc(mgrs->max_protocol_mgrs *
					  sizeof(XmProtocolMgr));
    }
    if ((mgrs->num_protocol_mgrs + 1) == mgrs->max_protocol_mgrs) {
	mgrs->max_protocol_mgrs *= 2;
	mgrs->protocol_mgrs =
		(XmProtocolMgr *)XtRealloc((char *)mgrs->protocol_mgrs,
					   mgrs->max_protocol_mgrs *
					   sizeof(XmProtocolMgr));
    }
    mgrs->protocol_mgrs[mgrs->num_protocol_mgrs] =
		(XmProtocolMgr)XtCalloc(1, sizeof(XmProtocolMgrRec));
    mgrs->protocol_mgrs[mgrs->num_protocol_mgrs]->property = property;
    mgrs->num_protocol_mgrs++;

    return mgrs->protocol_mgrs[mgrs->num_protocol_mgrs - 1];
}

static XmProtocolMgr
__XmFindProperty(XmAllProtocolsMgr mgrs, Atom property)
{
    int i;

    for (i = 0; i < mgrs->num_protocol_mgrs; i++) {
	if (mgrs->protocol_mgrs[i]->property == property)
	    return mgrs->protocol_mgrs[i];
    }

    return NULL;
}

static XmProtocol
__XmAddProtocol(XmAllProtocolsMgr mgrs, Atom property, Atom protocol)
{
    XmProtocolMgr mgr;
    XmProtocol w;
    Widget ve;

    mgr = __XmFindProperty(mgrs, property);

    if (!mgr)
	mgr = __XmAddProperty(mgrs, property);

    if (mgr->max_protocols == 0) {
	mgr->max_protocols = 8;
	mgr->protocols =
		(XmProtocolList)XtMalloc(mgrs->max_protocol_mgrs *
					sizeof(XmProtocolMgr));
    }
    if ((mgr->num_protocols + 1) == mgr->max_protocols) {
	mgr->max_protocols *= 2;
	mgr->protocols =
		(XmProtocolList)XtRealloc((char *)mgr->protocols,
					 mgrs->max_protocol_mgrs *
					 sizeof(XmProtocolMgr));
    }

    ve = _LtFindVendorExt(mgrs->shell);
    if (!ve)
	_XmError(NULL, "Shell has no extension!\n");

    w = (XmProtocol)XtCreateWidget("protocol_ext", xmProtocolClass,
				   ve, NULL, 0);

    Protocol_Atom(w) = protocol;
    mgr->protocols[mgr->num_protocols] = w;
    mgr->num_protocols++;

    return w;
}

static XmProtocol
__XmFindProtocol(XmAllProtocolsMgr mgrs, Atom property, Atom protocol)
{
    XmProtocolMgr mgr;
    int i;

    mgr = __XmFindProperty(mgrs, property);
    if (!mgr)
	return NULL;

    for (i = 0; i < mgr->num_protocols; i++) {
	if (Protocol_Atom(mgr->protocols[i]) == protocol)
	    return mgr->protocols[i];
    }

    return NULL;
}

static XtActionsRec	actions[] = {
    { "XmProtocolHandler", _XmProtocolHandler },
};

void
_XmInitProtocols(Widget w)
{
    XmAllProtocolsMgr newman;

    newman = (XmAllProtocolsMgr)XtCalloc(1, sizeof(XmAllProtocolsMgrRec));
    newman->shell = w;
    _XmPushWidgetExtData(w, (XmWidgetExtData)newman, XmPROTOCOL_EXTENSION);

    XdbDebug(__FILE__, w, "Init Protocols: %08x %08x\n", w, newman);

    XtAppAddActions(XtWidgetToApplicationContext(w),
		    actions, XtNumber(actions));
}

/*
 * this ONLY gets called from VendorShellExt's realize callback
 */
void
_XmInstallProtocols(Widget w)
{
    XmAllProtocolsMgr mgrs;
    XmProtocolMgr mgr;
    Atom *protocols;
    int i, j;

    XdbDebug(__FILE__, w, "_XmInstallProtocols\n");

    if ((mgrs = __XmGetAllMgr(w)) == NULL) {
	_XmWarning(w, "No XmProtocolManager for shell %s\n", XtName(w));
	return;
    }

    for (i = 0; i < mgrs->num_protocol_mgrs; i++) {
	mgr = mgrs->protocol_mgrs[i];

	protocols = (Atom *)XtMalloc(sizeof(Atom) * mgr->num_protocols);

	for (j = 0; j < mgr->num_protocols; j++)
	    protocols[j] = Protocol_Atom(mgr->protocols[j]);

	XChangeProperty(XtDisplay(w), XtWindow(w),
			mgr->property, XA_ATOM, 32, PropModeReplace,
			(unsigned char *)protocols, mgr->num_protocols);

	XtFree((char *)protocols);
    }
}

static void
_XmProtocolHandler(Widget w, XEvent *evp, String *params, Cardinal *nparams)
{
    Atom property, protocol;
    XmAllProtocolsMgr mgrs;
    XmProtocol p;
    XmAnyCallbackStruct cd;

    XdbDebug(__FILE__, w, "_XmProtocolHandler\n");

    cd.reason = XmCR_WM_PROTOCOLS;
    cd.event = evp;

    if (evp->type != ClientMessage) {
	XdbDebug(__FILE__, w, "XmProtocolHandler(not a client message)\n");
	return;
    }

    property = evp->xclient.message_type;
    protocol = evp->xclient.data.l[0];

    if ((mgrs = __XmGetAllMgr(w)) == NULL) {
	XdbDebug(__FILE__, w,
		 "XmProtocolHandler(couldn't find manager struct)\n");
	return;
    }

    if ((p = __XmFindProtocol(mgrs, property, protocol)) == NULL) {
	XdbDebug(__FILE__, w,
		 "XmProtocolHandler(couldn't find protocol)\n");
	return;
    }

    if (Protocol_Active(p)) {
	XdbDebug(__FILE__, w,
		 "XmProtocolHandler(calling callbacks)\n");

	if (Protocol_PreHook(p).callback)
	    (Protocol_PreHook(p).callback)(mgrs->shell,
					   Protocol_PreHook(p).closure,
					   (XtPointer)&cd);

	XtCallCallbackList(mgrs->shell,
			   Protocol_Callbacks(p),
			   (XtPointer)&cd);

	if (Protocol_PostHook(p).callback)
	    (Protocol_PostHook(p).callback)(mgrs->shell,
					    Protocol_PostHook(p).closure,
					    (XtPointer)&cd);
    }
    else
	XdbDebug(__FILE__, w,
		 "XmProtocolHandler(protocol not active)\n");
}

void
XmActivateProtocol(Widget shell,
		   Atom property,
		   Atom protocol)
{
    XmAllProtocolsMgr mgrs;
    XmProtocol p;

    XdbDebug(__FILE__, shell, "XmActivateProtocol\n");

    if ((mgrs = __XmGetAllMgr(shell)) == NULL)
	return;

    if ((p = __XmFindProtocol(mgrs, property, protocol)) == NULL)
	return;

    Protocol_Active(p) = True;
}

void
XmDeactivateProtocol(Widget shell,
		   Atom property,
		   Atom protocol)
{
    XmAllProtocolsMgr mgrs;
    XmProtocol p;

    XdbDebug(__FILE__, shell, "XmDeactivateProtocol\n");

    if ((mgrs = __XmGetAllMgr(shell)) == NULL)
	return;

    if ((p = __XmFindProtocol(mgrs, property, protocol)) == NULL)
	return;

    Protocol_Active(p) = False;
}

#ifdef	XmActivateWMProtocol
#undef	XmActivateWMProtocol
#endif
void
XmActivateWMProtocol(Widget shell,
		     Atom protocol)
{
    XmActivateProtocol(shell,
		       XM_WM_PROTOCOL_ATOM(shell),
		       protocol);
}

void
XmAddProtocols(Widget shell,
	       Atom property,
	       Atom *protocol,
	       Cardinal num_protocols)
{
    char *tbl, *a;
    XtTranslations ctbl;
    int i;
    XmAllProtocolsMgr mgrs;
    XmProtocol p;

    XdbDebug(__FILE__, shell, "XmAddProtocols\n");

    if ((mgrs = __XmGetAllMgr(shell)) == NULL) {
	XdbDebug(__FILE__, shell,
		 "XmAddProtocols: Can't find Management structure: not a shell?\n");
	return;
    }

    a = XGetAtomName(XtDisplay(shell), property);
    tbl = (char *)XtMalloc(32 + strlen(a));
    strcpy(tbl, "<Message>");		/* len 9 */
    strcat(tbl, a);
    strcat(tbl, ": XmProtocolHandler()\n");	/* len 21 */

    ctbl = XtParseTranslationTable(tbl);
    XtAugmentTranslations(shell, ctbl);

    XtFree(tbl);
    XFree(a);

    for (i = 0; i < num_protocols; i++) {
	if ((p = __XmFindProtocol(mgrs, property, protocol[i])) == NULL)
	    __XmAddProtocol(mgrs, property, protocol[i]);
    }

    if (XtIsRealized(shell))
    {
	_XmInstallProtocols(shell);
    }

    /*
     * Automatically Activate
     */
    for (i=0; i < num_protocols; i++)
	XmActivateProtocol(shell, property, protocol[i]);
}

#ifdef	XmAddWMProtocols
#undef	XmAddWMProtocols
#endif

void
XmAddWMProtocols(Widget shell,
		 Atom *protocols,
		 Cardinal num_protocols)
{
    XmAddProtocols(shell,
		   XM_WM_PROTOCOL_ATOM(shell),
		   protocols,
		   num_protocols);
}

#ifdef	XmAddWMProtocolCallback
#undef	XmAddWMProtocolCallback
#endif

void
XmAddProtocolCallback(Widget shell,
		      Atom property,
		      Atom protocol,
		      XtCallbackProc callback,
		      XtPointer closure)
{
    XmAllProtocolsMgr mgrs;
    XmProtocol p;

    XdbDebug(__FILE__, shell, "XmAddProtocolCallback\n");

    if ((mgrs = __XmGetAllMgr(shell)) == NULL) {
	XdbDebug(__FILE__, shell,
		 "XmAddProtocolCallback: Can't find Management structure: not a shell?\n");
	return;
    }

    if ((p = __XmFindProtocol(mgrs, property, protocol)) == NULL) {
	XmAddProtocols(shell, property, &protocol, 1);
	p = __XmFindProtocol(mgrs, property, protocol);
    }

    /*
     * Add Callback
     */
    XtAddCallback((Widget)p, XmNprotocolCallback, callback, closure);
}

void 
XmAddWMProtocolCallback(Widget shell,
			Atom protocol,
			XtCallbackProc callback,
			XtPointer closure)
{
    XmAddProtocolCallback(shell, 
			  XM_WM_PROTOCOL_ATOM(shell),
			  protocol,
			  callback,
			  closure);
}

void
XmRemoveProtocolCallback(Widget shell,
			 Atom property,
			 Atom protocol,
			 XtCallbackProc callback,
			 XtPointer closure)
{
    XmAllProtocolsMgr mgrs;
    XmProtocol p;

    XdbDebug(__FILE__, shell, "XmRemoveProtocolCallback\n");

    if ((mgrs = __XmGetAllMgr(shell)) == NULL) {
	XdbDebug(__FILE__, shell,
		 "XmRemoveProtocolCallback: Can't find Management structure: not a shell?\n");
	return;
    }

    if ((p = __XmFindProtocol(mgrs, property, protocol)) == NULL) {
	XmAddProtocols(shell, property, &protocol, 1);
	p = __XmFindProtocol(mgrs, property, protocol);
    }

    /*
     * Remove Callback
     */
    XtRemoveCallback((Widget)p, XmNprotocolCallback, callback, closure);
}

#ifdef	XmRemoveWMProtocolCallback
#undef	XmRemoveWMProtocolCallback
#endif

void
XmRemoveWMProtocolCallback(Widget shell,
			   Atom protocol,
			   XtCallbackProc callback,
			   XtPointer closure)
{
    XmRemoveProtocolCallback(shell,
			     XM_WM_PROTOCOL_ATOM(shell),
			     protocol,
			     callback,
			     closure);
}

void
XmRemoveProtocols(Widget shell,
		  Atom property,
		  Atom *protocols,
		  Cardinal num_protocols)
{
    XmAllProtocolsMgr mgrs;
    XmProtocol p;
    int i;

    XdbDebug(__FILE__, shell, "XmRemoveProtocols\n");

    if ((mgrs = __XmGetAllMgr(shell)) == NULL)
	return;

    for (i = 0; i < num_protocols; i++) {
	if ((p = __XmFindProtocol(mgrs, property, protocols[i])) == NULL)
	    return;

	Protocol_Active(p) = False;
    }
}

#ifdef	XmRemoveWMProtocols
#undef	XmRemoveWMProtocols
#endif

void
XmRemoveWMProtocols(Widget shell,
		    Atom *protocols,
		    Cardinal num_protocols)
{
    XmRemoveProtocols(shell,
		      XM_WM_PROTOCOL_ATOM(shell),
		      protocols,
		      num_protocols);
}

void
XmSetProtocolHooks(Widget shell,
	Atom		property,
	Atom		protocol,
	XtCallbackProc	prehook,
	XtPointer	pre_closure,
	XtCallbackProc	posthook,
	XtPointer	post_closure)
{
    XmAllProtocolsMgr mgrs;
    XmProtocol p;

    XdbDebug(__FILE__, shell, "XmSetProtocolHooks\n");

    if ((mgrs = __XmGetAllMgr(shell)) == NULL) {
	XdbDebug(__FILE__, shell,
		 "XmSetProtocolHooks: Can't find Management structure: not a shell?\n");
	return;
    }

    if ((p = __XmFindProtocol(mgrs, property, protocol)) == NULL) {
	XmAddProtocols(shell, property, &protocol, 1);
	p = __XmFindProtocol(mgrs, property, protocol);
    }

    Protocol_PreHook(p).callback = prehook;
    Protocol_PreHook(p).closure = pre_closure;
    Protocol_PostHook(p).callback = posthook;
    Protocol_PostHook(p).closure = post_closure;
}

#ifdef	XmSetWMProtocolHooks
#undef	XmSetWMProtocolHooks
#endif
void
XmSetWMProtocolHooks(Widget shell,
	Atom	protocol,
	XtCallbackProc	prehook,
	XtPointer	pre_closure,
	XtCallbackProc	posthook,
	XtPointer	post_closure)
{
    XmSetProtocolHooks(shell,
		       XM_WM_PROTOCOL_ATOM(shell),
		       protocol,
		       prehook,
		       pre_closure,
		       posthook,
		       post_closure);
}
