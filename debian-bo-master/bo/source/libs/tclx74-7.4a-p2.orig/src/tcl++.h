/*
 * tcl++.h --
 *
 * This file defines a C++ classes that can be used to access a Tcl
 * interpreter. If tcl.h is not already included, it includes it. Tcl.h has
 * macros that allow it to work with K&R C, ANSI C and C++.
 *
 * This class maybe instantiated in one of two ways.  In one method, an
 * interpreter is created and owned by the object.  When the object is deleted,
 * the interpreter is deleted.  In the other method, the interpreter is passed
 * to the constructor and is referenced by the object, but not owned by it.
 * When the object is deleted, the interpreter will not be deleted.
 *-----------------------------------------------------------------------------
 * Important note:
 *   Some of the string arguments specified as const in this class are actually
 * temporarily modified by Tcl while code is being evaulated.  The string
 * will be restored to its original state on exit. This can cause problems if
 * the string is aliased or is in read-only memory, which will cause a access
 * fault.  The gcc compiler will store string constants in read-only memory
 * unless unless you use -fwritable-strings.  One trick to get around this is
 * to have an non-const variable that the string is assigned to:
 *
 *   char cmd [] = "some command to pass to Tcl_Eval";
 *
 * The parameters that have this problem are marked with "WRITTEN" in this
 * header file.
 *-----------------------------------------------------------------------------
 * Copyright 1991-1995 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * Based on Tcl C++ classes developed by Parag Patel.
 *-----------------------------------------------------------------------------
 * $Id: tcl++.h,v 5.0 1995/07/25 05:59:02 markd Rel $
 *-----------------------------------------------------------------------------
 */

#ifndef _TCL_PLUS_PLUS_H
#define _TCL_PLUS_PLUS_H

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#ifndef TCLEXTEND_H
#    include "tclExtend.h"
#endif

class TclInterp_cl
{
    friend class TclTrace_cl;

  private:
    Tcl_Interp *interp;
    int         ownInterp;

    char *
    CatVarArgs (va_list  argPtr);

  public:
    /*
     * Constructors, destructors and basic access members.
     */
    inline 
    TclInterp_cl () 
    {
        interp = Tcl_CreateInterp ();
        XInit ();
        ownInterp = 1;
    }

    inline 
    TclInterp_cl (Tcl_Interp *passedInterp)
    {
        interp = passedInterp;
        ownInterp = 0;
    }

    inline 
    ~TclInterp_cl () 
    { 
        if (ownInterp)
            Tcl_DeleteInterp (interp);
    }

    inline char *
    Result () 
    { 
        return interp->result; 
    }

    inline int 
    ErrorLine () 
    { 
        return interp->errorLine;
    }

    inline Tcl_Interp *
    GetInterp () 
    {
        return interp;
    }

   /*
    * Exported Tcl procedures:
    */

    inline int
    AsyncInvoke (int  code)
    {
        return Tcl_AsyncInvoke (interp, code);
    }

    inline void
    AppendElement (const char *string)
    { 
        Tcl_AppendElement (interp, (char *) string);
    }

    void
    AppendResult (const char *p,
                  ...);

    inline int
    AppInit ()
    {
        return Tcl_AppInit (interp);
    }

    inline void
    AddErrorInfo (const char *message)
    {
        Tcl_AddErrorInfo (interp, (char *) message);
    }

    inline void
    CallWhenDeleted (Tcl_InterpDeleteProc *proc,
                     ClientData            clientData)
    {
        Tcl_CallWhenDeleted (interp, proc, clientData);
    }

    inline void 
    CreateCommand (const char        *cmdName,
                   Tcl_CmdProc       *proc, 
                   ClientData         data, 
                   Tcl_CmdDeleteProc *deleteProc)
    { 
        Tcl_CreateCommand (interp, (char*) cmdName, proc, data, deleteProc);
    }

    inline void
    CreateMathFunc (const char    *name,
                    int            numArgs,
                    Tcl_ValueType *argTypes,
                    Tcl_MathProc  *proc,
                    ClientData     clientData)
    {
        Tcl_CreateMathFunc (interp, (char *) name, numArgs, argTypes,
                            proc,  clientData);
    }

    inline int
    CreatePipeline (int    argc, 
                    char **argv, 
                    int  **pidArrayPtr,
                    int   *inPipePtr, 
                    int   *outPipePtr,
                    int   *errFilePtr)
    {
        return Tcl_CreatePipeline (interp, argc, argv, pidArrayPtr, inPipePtr, 
                                   outPipePtr, errFilePtr);
    }

    inline Tcl_Trace 
    CreateTrace (int               level, 
                 Tcl_CmdTraceProc *proc, 
                 ClientData        data)
    {
        return Tcl_CreateTrace (interp, level, proc, data);
    }

    inline void
    DStringResult (Tcl_DString *dsPtr)
    {
        Tcl_DStringResult (interp, dsPtr);
    }

    inline void 
    DeleteCommand (const char *cmdName)
    { 
        Tcl_DeleteCommand (interp, (char *)cmdName);
    }

    inline void 
    DeleteTrace (Tcl_Trace trace)
    { 
        Tcl_DeleteTrace(interp, trace);
    }

    inline void
    DetachPids (int  numPids, 
                int *pidPtr)
    {
        Tcl_DetachPids (numPids, pidPtr);
    }

    inline void
    EnterFile (FILE  *file,
               int    permissions)
    {
        Tcl_EnterFile (interp, file, permissions);
    }

    inline int 
    Eval (char  *cmd)  /* WRITTEN */
    {
        return Tcl_Eval (interp, cmd);
    }

    inline int
    EvalFile (const char *fileName)
    { 
        return Tcl_EvalFile (interp, (char *) fileName);
    }

    inline int
    ExprBoolean (const char *string,
                 int        *ptr)
    {
        return Tcl_ExprBoolean (interp, (char *) string, ptr);
    }

    inline int
    ExprDouble (const char *string,
                double     *ptr)
    { 
        return Tcl_ExprDouble (interp, (char *) string, ptr);
    }

    inline int
    ExprLong (const char *string,
              long       *ptr)
    {
        return Tcl_ExprLong (interp, (char *) string, ptr);
    }

    inline int
    ExprString (const char *string)
    {
        return Tcl_ExprString (interp, (char *) string);
    }

    inline int
    GetBoolean (const char *string,
                int        *boolPtr)
    { 
        return Tcl_GetBoolean (interp, (char *) string, boolPtr);
    }

    inline int
    GetCommandInfo (const char  *cmdName,
                    Tcl_CmdInfo *infoPtr)
    {
        return Tcl_GetCommandInfo (interp, (char *) cmdName, infoPtr);
    }

    inline int
    GetDouble (const char *string,
               double     *doublePtr)
    { 
        return Tcl_GetDouble (interp, (char *) string, doublePtr);
    }

    inline int
    GetInt (const char *string,
            int        *intPtr)
    { 
        return Tcl_GetInt (interp, (char *) string, intPtr);
    }

    inline int
    GetOpenFile (const char *string,
                 int         write,
                 int         checkUsage,
                 FILE      **filePtr)
    {
        return Tcl_GetOpenFile (interp, (char *) string, write, checkUsage,
                                filePtr);
    }

    inline const char *
    GetVar (const char *varName, 
            int         flags = 0)
    { 
        return Tcl_GetVar (interp, (char *) varName, flags);
    }

    inline const char *
    GetVar2 (const char *part1, 
             const char *part2,
             int         flags = 0)
    { 
        return Tcl_GetVar2 (interp, (char *) part1, (char *) part2, flags); 
    }

    inline int
    GlobalEval (const char *command)
    {
        return Tcl_GlobalEval (interp, (char *) command);
    }

    inline int
    Init ()
    {
        return Tcl_Init (interp);
    }

    inline void
    InitMemory ()
    {
        Tcl_InitMemory (interp);
    }

    inline int
    LinkVar (const char *varName,
             char       *addr,
             int         type)
    {
        return Tcl_LinkVar (interp, (char *) varName, addr, type);
    }

    inline char *
    ParseVar (const char  *string, 
              char       **termPtr)
    {
        return Tcl_ParseVar (interp, (char *) string, termPtr);
    }

    inline const char *
    PosixError ()
    {
        return Tcl_PosixError (interp);
    }

    inline void
    PrintDouble (double  value,
                 char   *dst)
    {
        Tcl_PrintDouble (interp, value, dst);
    }

    inline int 
    RecordAndEval (const char *cmd, 
                   char        flags)
    {
        return Tcl_RecordAndEval (interp, (char *) cmd, flags);
    }

    inline int
    RegExpMatch (char *string,
                 char *pattern)
    {
        return Tcl_RegExpMatch (interp, string, pattern);
    }

    inline void
    ResetResult ()
    {
        Tcl_ResetResult (interp);
    }

    inline int
    SetCommandInfo (const char  *cmdName,
                    Tcl_CmdInfo *infoPtr)
    {
        return Tcl_SetCommandInfo (interp, (char *) cmdName, infoPtr);
    }

    void
    SetErrorCode (char *p, 
                  ...);

    inline int
    SetRecursionLimit (int depth)
    {
        return Tcl_SetRecursionLimit (interp, depth);
    }

    inline void 
    SetResult (const char *string)
    {
        Tcl_SetResult (interp, (char *) string, TCL_VOLATILE);
    }

    inline void 
    SetResult (const char   *string, 
               Tcl_FreeProc *freeProc)
    { 
        Tcl_SetResult (interp, (char *) string, freeProc);
    }

    inline const char *
    SetVar (const char  *varName,      /* WRITTEN */
            const char  *newValue,
            int          global = 0)
    { 
        return Tcl_SetVar (interp, (char *) varName, (char *) newValue, 
                           global);
    }

    inline const char *
    SetVar2 (const char *part1, 
             const char *part2, 
             const char *newValue, 
             int         global)
    {
         return Tcl_SetVar2 (interp, (char *) part1, (char *) part2, 
                             (char *) newValue, global);
    }

    inline int 
    SplitList (const char   *list,
               int          &argcPtr,
               char       **&argvPtr)
    {
        return Tcl_SplitList (interp, (char *) list, &argcPtr, &argvPtr);
    }

    inline char *
    TildeSubst (const char  *name,
                Tcl_DString *bufferPtr)
    {
        return Tcl_TildeSubst (interp, (char *) name, bufferPtr);
    }

    int
    TraceVar (const char       *varName,
              int               flags,
              Tcl_VarTraceProc *proc,
              ClientData        clientData)
    {
         return Tcl_TraceVar (interp, (char *) varName, flags, proc,
                              clientData);
    }

    inline int
    TraceVar2 (const char       *part1, 
               const char       *part2,
               int               flags, 
               Tcl_VarTraceProc *proc, 
               ClientData        clientData)
    {
         return Tcl_TraceVar2 (interp, (char *) part1, (char *) part2, flags,
                               proc, clientData); 
    }

    inline void
    UnlinkVar (const char *varName)
    {
        Tcl_UnlinkVar (interp, (char *) varName);
    }

    inline void
    UnsetVar (const char *varName,
              int         global)
    {
        Tcl_UnsetVar (interp, (char *) varName, global);
    }

    inline void
    UnsetVar2 (const char *part1, 
               const char *part2, 
               int         global)
    {
        Tcl_UnsetVar2 (interp, (char *) part1, (char *) part2, global);
    }

    inline void
    UntraceVar (const char       *varName, 
                int               flags,
                Tcl_VarTraceProc *proc, 
                ClientData        clientData)
    {
        Tcl_UntraceVar (interp, (char *) varName, flags, proc, clientData);
    }


    inline void
    UntraceVar2 (const char       *part1,
                 const char       *part2,
                 int               flags, 
                 Tcl_VarTraceProc *proc, 
                 ClientData        clientData)
    { 
        Tcl_UntraceVar2 (interp, (char *) part1, (char *) part2, flags, proc,
                         clientData);
    }

    int
    VarEval (const char *p,
              ...);

    inline ClientData
    VarTraceInfo (const char       *varName,
                  int               flags,
                  Tcl_VarTraceProc *procPtr,
                  ClientData        prevClientData)
    { 
        return Tcl_VarTraceInfo (interp, (char *) varName, flags, procPtr,
                                 prevClientData);
    }

    inline ClientData
    VarTraceInfo2 (const char       *part1, 
                   const char       *part2, 
                   int               flags,
                   Tcl_VarTraceProc *procPtr,
                   ClientData        prevClientData)
    { 
        return Tcl_VarTraceInfo2 (interp, (char *) part1, (char *) part2, 
                                  flags, procPtr, prevClientData);
    }

    /*
     * Exported Extended Tcl initialization functions.
     */

    inline int
    XInit ()
    {
        return TclX_Init (interp);
    }

    inline int
    XCmdInit ()
    {
        return TclXCmd_Init (interp);
    }

    inline int
    XLibInit ()
    {
        return TclXLib_Init (interp);
    }

    inline void
    XErrorExit (int  exitCode)
    {
        TclX_ErrorExit (interp, exitCode);
    }

    /*
     * Exported Extended Tcl utility functions.
     */
    inline int
    GetLong (const char *string,
             long       *longPtr)
    {
        return Tcl_GetLong (interp, string, longPtr);
    }

    inline int
    GetUnsigned (const char *string,
                 unsigned   *unsignedPtr)
    {
         return Tcl_GetUnsigned (interp, string, unsignedPtr);
    }

    /*
     * Exported keyed list manipulation functions.
     */
    inline char *
    DeleteKeyedListField (const char  *fieldName,
                          const char  *keyedList)
    {
        return Tcl_DeleteKeyedListField (interp, fieldName, keyedList);
    }

    inline int
    GetKeyedListField (const char  *fieldName,
                       const char  *keyedList,
                       char       **fieldValuePtr)
    {
        return Tcl_GetKeyedListField (interp, fieldName, keyedList,
                                      fieldValuePtr);
    }

    inline int
    GetKeyedListKeys (const char  *subFieldName,
                      const char  *keyedList,
                      int         *keyesArgcPtr,
                      char      ***keyesArgvPtr)
    {

        return Tcl_GetKeyedListKeys (interp, subFieldName, keyedList,
                                     keyesArgcPtr, keyesArgvPtr);
    }

    inline char *
    SetKeyedListField (const char  *fieldName,
                       const char  *fieldvalue,
                       const char  *keyedList)
    {
        return Tcl_SetKeyedListField (interp, fieldName, fieldvalue,
                                      keyedList);
    }

};

class TclTrace_cl
{
    Tcl_Trace   trace;
    Tcl_Interp *interp;

  public:
    inline 
    TclTrace_cl (TclInterp_cl     &interpCl, 
                 int               level, 
                 Tcl_CmdTraceProc *proc, 
                 ClientData        data)
    { 
        trace = Tcl_CreateTrace (interp = interpCl.interp, level, proc, data);
    }

    inline ~TclTrace_cl () 
    {
        Tcl_DeleteTrace (interp, trace);
    }
};

class TclHandleTbl_cl
{
    void_pt headerPtr;

public:
    inline
    TclHandleTbl_cl (const char *handleBase,
                     int         entrySize,
                     int         initEntries)

    {
        headerPtr = Tcl_HandleTblInit (handleBase, entrySize, initEntries);
    }

    inline
    ~TclHandleTbl_cl ()
    {
        Tcl_HandleTblRelease (headerPtr);
    }

    inline void_pt  
    HandleAlloc (char *handlePtr)
    {
        return Tcl_HandleAlloc (headerPtr, handlePtr);
    }

    inline void 
    HandleFree (void_pt  entryPtr)
    {
        Tcl_HandleFree (headerPtr, entryPtr);
    }

    inline int
    HandleTblUseCount (int amount)
    {
        return Tcl_HandleTblUseCount (headerPtr, amount);
    }

    inline void_pt
    HandleWalk (int *walkKeyPtr)
    {
        return Tcl_HandleWalk (headerPtr, walkKeyPtr);
    }

    inline void_pt
    HandleXlate (Tcl_Interp  *interp,
                 const  char *handle)
    {
        return Tcl_HandleXlate (interp, headerPtr, handle);
    }

    inline void
    WalkKeyToHandle (int   walkKey,
                     char *handle)
    {
        Tcl_WalkKeyToHandle (headerPtr, walkKey, handle);
    }
};

#endif /* _TCL_PLUS_PLUS_H */
 

