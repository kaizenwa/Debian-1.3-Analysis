/*
 * Definitions of functions.
 */

/*
 * These are the functions in the standard repertoire.
 */

FncDef(abs,1)
FncDef(any,4)
FncDef(args,1)
FncDef(bal,6)
FncDef(center,3)
FncDef(char,1)
FncDef(chdir,1)
FncDef(close,1)
FncDef(collect,2)
FncDef(copy,1)
FncDef(cset,1)

#if UNIX || VMS || OS2_32 || NT || SCCX_MX
FncDef(delay,1)
#endif					/* UNIX || VMS || OS2_32 || NT ... */

FncDef(delete,2)
FncDefV(detab)
#ifdef MultiThread
FncDef(display,3)
#else					/* MultiThread */
FncDef(display,2)
#endif					/* MultiThread */
FncDefV(entab)
FncDef(errorclear,0)
#ifdef ConsoleWindow
#undef exit
#endif
FncDef(exit,1)
#ifdef ConsoleWindow
#define exit c_exit
#endif
FncDef(find,4)
FncDef(flush,1)
FncDef(function,0)
FncDef(get,2)
FncDef(getenv,1)
FncDef(iand,2)
FncDef(icom,1)
FncDef(image,1)
FncDef(insert,3)
FncDef(integer,1)
FncDef(ior,2)
FncDef(ishift,2)
FncDef(ixor,2)
FncDef(key,2)
FncDef(left,3)
FncDef(list,2)
FncDef(many,4)
FncDef(map,3)
FncDef(match,4)
FncDef(member,1)
FncDef(move,1)
#ifdef MultiThread
FncDef(name,2)
#else						/* MultiThread */
FncDef(name,1)
#endif						/* MultiThread */
FncDef(numeric,1)

#ifdef Graphics
FncDefV(open)
#else					/* Graphics */
FncDef(open,3)
#endif					/* Graphics */

FncDef(ord,1)
FncDef(pop,1)
FncDef(pos,1)
#ifdef MultiThread
FncDef(proc,3)
#else					/* MultiThread */
FncDef(proc,2)
#endif					/* MultiThread */
FncDef(pull,1)
FncDefV(push)
FncDefV(put)
FncDef(read,2)
FncDef(reads,2)
FncDef(real,1)
FncDef(remove,2)
FncDef(rename,1)
FncDef(repl,2)
FncDef(reverse,1)
FncDef(right,3)
FncDefV(runerr)
FncDef(seek,2)
FncDef(seq,2)
FncDef(set,1)
FncDef(sort,2)
FncDef(sortf,2)
FncDefV(stop)
FncDef(string,1)
FncDef(tab,1)
FncDef(table,1)
FncDef(trim,2)
FncDef(type,1)
FncDef(upto,4)
#ifdef MultiThread
FncDef(variable,3)
#else						/* MultiThread */
FncDef(variable,1)
#endif						/* MultiThread */
FncDef(where,1)
FncDefV(write)
FncDefV(writes)

/*
 * System function.
 */

#ifdef SystemFnc
FncDef(system,1)
#endif					/* SystemFnc */

/*
 * Dynamic loading.
 */

#ifdef LoadFunc
FncDef(loadfunc,2)
#endif					/* LoadFunc */

/*
 * Executable images.
 */

#ifdef ExecImages
FncDef(save,1)
#endif					/* ExecImages */

/*
 * External functions.
 */
#ifdef ExternalFunctions
FncDefV(callout)
#endif					/* ExternalFunctions */

/*
 * Math functions.
 */

FncDef(acos,1)
FncDef(asin,1)
FncDef(atan,2)
FncDef(cos,1)
FncDef(dtor,1)
FncDef(exp,2)
FncDef(log,1)
FncDef(rtod,1)
FncDef(sin,1)
FncDef(sqrt,1)
FncDef(tan,1)

#ifdef KeyboardFncs
FncDef(getch,0)
FncDef(getche,0)
FncDef(kbhit,0)
#endif					/* KeyboardFncs */

/*
 * Functions for MS-DOS.
 */

#ifdef DosFncs
FncDef(Int86,1)
FncDef(Peek,1)
FncDef(Poke,1)
FncDef(GetSpace,1)
FncDef(FreeSpace,1)
FncDef(InPort,1)
FncDef(OutPort,1)
#endif					/* DosFncs */

/*
 * Functions for the Archimedes.
 */

#ifdef ArmFncs
FncDef(Swi,2)
FncDef(Peek,2)
FncDef(Poke,2)
FncDef(GetSpace,1)
FncDef(FreeSpace,1)
#endif					/* ArmFncs */

/*
 * Functions to support program visualization.
 */

#ifdef Visualization
FncDef(serial,1)
FncDef(structure,1)
#endif					/* Visualization */

/*
 * Event processing functions.
 */
#ifdef EventMon
FncDef(EvGet,2)
FncDef(event,3)
#endif					/* EventMon */

#ifdef Graphics
FncDef(Active,0)
FncDefV(Alert)
FncDefV(Bg)
FncDefV(Clip)
FncDefV(Clone)
FncDefV(Color)
FncDefV(ColorValue)
FncDefV(CopyArea)
FncDefV(Couple)
FncDefV(DrawArc)
FncDefV(DrawCircle)
FncDefV(DrawCurve)
FncDefV(DrawImage)
FncDefV(DrawLine)
FncDefV(DrawPoint)
FncDefV(DrawPolygon)
FncDefV(DrawRectangle)
FncDefV(DrawSegment)
FncDefV(DrawString)
FncDefV(EraseArea)
FncDefV(Event)
FncDefV(Fg)
FncDefV(FillArc)
FncDefV(FillCircle)
FncDefV(FillPolygon)
FncDefV(FillRectangle)
FncDefV(Font)
FncDefV(FreeColor)
FncDefV(GotoRC)
FncDefV(GotoXY)
FncDefV(Lower)
FncDefV(NewColor)
FncDefV(PaletteChars)
FncDefV(PaletteColor)
FncDefV(PaletteKey)
FncDefV(Pattern)
FncDefV(Pending)
FncDefV(Pixel)
FncDef(QueryPointer,1)
FncDefV(Raise)
FncDefV(ReadImage)
FncDefV(TextWidth)
FncDef(Uncouple,1)
FncDefV(WAttrib)
FncDefV(WDefault)
FncDefV(WFlush)
FncDef(WSync,1)
FncDefV(WriteImage)
#endif					/* Graphics */

#ifdef MultiThread
FncDef(cofail,1)
FncDefV(load)
FncDef(parent,1)
FncDef(eventmask,2)
FncDef(opmask,2)
FncDef(globalnames,1)
FncDef(keyword,2)
FncDef(localnames,2)
FncDef(staticnames,2)
FncDef(paramnames,2)
FncDef(fieldnames,1)
#endif					/* MultiThread */
