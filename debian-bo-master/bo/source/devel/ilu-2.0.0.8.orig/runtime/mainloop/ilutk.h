/* ilutk.h */
/* Last edited by Mike Spreitzer October 19, 1995 8:01 am PDT */

extern void     IluTk_Init(void);
/*
 * Integrate the ILU and Tk main loops.  After this call, ILU uses
 * Tk's event handling mechanism; Tk_DoOneEvent will dispatch to ILU
 * as well as Tk events (one at a time, of course).  Recall that ILU
 * uses its main loop recursively; thus this integrated main loop
 * can be called while doing an RPC, which means that your
 * application has to be re-entrant.
 * 
 * Call this procedure at least once before (a) using the ILU main loop
 * directly, (b) calling across any ISL-specified interface, or (c)
 * creating any non-local ports for any ILU servers (which is
 * bundled into server creation in some language-specific runtimes).
 */
