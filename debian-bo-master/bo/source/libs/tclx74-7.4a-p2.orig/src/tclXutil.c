/*
 * tclXutil.c
 *
 * Utility functions for Extended Tcl.
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
 * $Id: tclXutil.c,v 5.0 1995/07/25 05:59:00 markd Rel $
 *-----------------------------------------------------------------------------
 */

#include "tclExtdInt.h"

#ifndef _tolower
#  define _tolower tolower
#  define _toupper toupper
#endif

/*
 * Prototypes of internal functions.
 */
static int
ReturnOverflow _ANSI_ARGS_((Tcl_Interp *interp));

static int
CallEvalErrorHandler _ANSI_ARGS_((Tcl_Interp  *interp));

/*
 * Used to return argument messages by most commands.
 */
char *tclXWrongArgs = "wrong # args: ";

/*
 *-----------------------------------------------------------------------------
 *
 * ReturnOverflow --
 *    
 *   Return an error message about an numeric overflow.
 *
 * Parameters:
 *   o interp (O) - Interpreter to set the error message in.
 * Returns:
 *   TCL_ERROR;
 *-----------------------------------------------------------------------------
 */
static int
ReturnOverflow (interp)
    Tcl_Interp *interp;
{
    interp->result = "integer value too large to represent";
    Tcl_SetErrorCode(interp, "ARITH", "IOVERFLOW",
                     interp->result, (char *) NULL);
    return TCL_ERROR;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_StrToLong --
 *      Convert an Ascii string to an long number of the specified base.
 *
 * Parameters:
 *   o string (I) - String containing a number.
 *   o base (I) - The base to use for the number 8, 10 or 16 or zero to decide
 *     based on the leading characters of the number.  Zero to let the number
 *     determine the base.
 *   o longPtr (O) - Place to return the converted number.  Will be 
 *     unchanged if there is an error.
 *
 * Returns:
 *      Returns 1 if the string was a valid number, 0 invalid.
 *-----------------------------------------------------------------------------
 */
int
Tcl_StrToLong (string, base, longPtr)
    CONST char *string;
    int         base;
    long       *longPtr;
{
    char *end, *p;
    long  i;

    /*
     * Note: use strtoul instead of strtol for integer conversions
     * to allow full-size unsigned numbers, but don't depend on strtoul
     * to handle sign characters;  it won't in some implementations.
     */

    errno = 0;
    for (p = (char *) string; isspace(UCHAR(*p)); p++) {
        /* Empty loop body. */
    }
    if (*p == '-') {
        p++;
        i = -strtoul(p, &end, base);
    } else if (*p == '+') {
        p++;
        i = strtoul(p, &end, base);
    } else {
        i = strtoul(p, &end, base);
    }
    if (end == p) {
        return FALSE;
    }
    if (errno == ERANGE) {
        return FALSE;
    }
    while ((*end != '\0') && isspace(UCHAR(*end))) {
        end++;
    }
    if (*end != '\0') {
        return FALSE;
    }
    *longPtr = i;
    return TRUE;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_StrToInt --
 *      Convert an Ascii string to an number of the specified base.
 *
 * Parameters:
 *   o string (I) - String containing a number.
 *   o base (I) - The base to use for the number 8, 10 or 16 or zero to decide
 *     based on the leading characters of the number.  Zero to let the number
 *     determine the base.
 *   o intPtr (O) - Place to return the converted number.  Will be 
 *     unchanged if there is an error.
 *
 * Returns:
 *      Returns 1 if the string was a valid number, 0 invalid.
 *-----------------------------------------------------------------------------
 */
int
Tcl_StrToInt (string, base, intPtr)
    CONST char *string;
    int         base;
    int        *intPtr;
{
    char *end, *p;
    int   i;

    /*
     * Note: use strtoul instead of strtol for integer conversions
     * to allow full-size unsigned numbers, but don't depend on strtoul
     * to handle sign characters;  it won't in some implementations.
     */

    errno = 0;
    for (p = (char *) string; isspace(UCHAR(*p)); p++) {
        /* Empty loop body. */
    }
    if (*p == '-') {
        p++;
        i = -strtoul(p, &end, base);
    } else if (*p == '+') {
        p++;
        i = strtoul(p, &end, base);
    } else {
        i = strtoul(p, &end, base);
    }
    if (end == p) {
        return FALSE;
    }
    if (errno == ERANGE) {
        return FALSE;
    }
    while ((*end != '\0') && isspace(UCHAR(*end))) {
        end++;
    }
    if (*end != '\0') {
        return FALSE;
    }
    *intPtr = i;
    return TRUE;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_StrToUnsigned --
 *      Convert an Ascii string to an unsigned int of the specified base.
 *
 * Parameters:
 *   o string (I) - String containing a number.
 *   o base (I) - The base to use for the number 8, 10 or 16 or zero to decide
 *     based on the leading characters of the number.  Zero to let the number
 *     determine the base.
 *   o unsignedPtr (O) - Place to return the converted number.  Will be 
 *     unchanged if there is an error.
 *
 * Returns:
 *      Returns 1 if the string was a valid number, 0 invalid.
 *-----------------------------------------------------------------------------
 */
int
Tcl_StrToUnsigned (string, base, unsignedPtr)
    CONST char *string;
    int         base;
    unsigned   *unsignedPtr;
{
    char *end, *p;
    unsigned i;

    errno = 0;
    for (p = (char *) string; isspace(UCHAR(*p)); p++) {
        /* Empty loop body. */
    }
    i = strtoul(p, &end, base);
    if (end == p) {
        return FALSE;
    }
    if (errno == ERANGE) {
        return FALSE;
    }
    while ((*end != '\0') && isspace(UCHAR(*end))) {
        end++;
    }
    if (*end != '\0') {
        return FALSE;
    }
    *unsignedPtr = i;
    return TRUE;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_StrToDouble --
 *   Convert a string to a double percision floating point number.
 *
 * Parameters:
 *   string (I) - Buffer containing double value to convert.
 *   doublePtr (O) - The convert floating point number.
 * Returns:
 *   TRUE if the number is ok, FALSE if it is illegal.
 *-----------------------------------------------------------------------------
 */
int
Tcl_StrToDouble (string, doublePtr)
    CONST char *string;
    double     *doublePtr;
{
    char   *end, *p;
    double  i;

    errno = 0;
    for (p = (char *) string; isspace(UCHAR(*p)); p++) {
        /* Empty loop body. */
    }
    i = strtod (string, &end);
    if (end == p) {
        return FALSE;
    }
    *doublePtr = i;
    return TRUE;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_StrToOffset --
 *      Convert an Ascii string to an off_t number of the specified base.
 *
 * Parameters:
 *   o string (I) - String containing a number.
 *   o base (I) - The base to use for the number 8, 10 or 16 or zero to decide
 *     based on the leading characters of the number.  Zero to let the number
 *     determine the base.
 *   o offsetPtr (O) - Place to return the converted number.  Will be 
 *     unchanged if there is an error.
 *
 * Returns:
 *      Returns 1 if the string was a valid number, 0 invalid.
 *-----------------------------------------------------------------------------
 */
int
Tcl_StrToOffset (string, base, offsetPtr)
    CONST char *string;
    int         base;
    off_t      *offsetPtr;
{
    char *end, *p;
    off_t i;

    /*
     * Note: use strtoul instead of strtol for integer conversions
     * to allow full-size unsigned numbers, but don't depend on strtoul
     * to handle sign characters;  it won't in some implementations.
     */

    errno = 0;
    for (p = (char *) string; isspace(UCHAR(*p)); p++) {
        /* Empty loop body. */
    }
    if (*p == '-') {
        p++;
        i = -strtoul(p, &end, base);
    } else if (*p == '+') {
        p++;
        i = strtoul(p, &end, base);
    } else {
        i = strtoul(p, &end, base);
    }
    if (end == p) {
        return FALSE;
    }
    if (errno == ERANGE) {
        return FALSE;
    }
    while ((*end != '\0') && isspace(UCHAR(*end))) {
        end++;
    }
    if (*end != '\0') {
        return FALSE;
    }
    *offsetPtr = i;
    return TRUE;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_DownShift --
 *     Utility procedure to down-shift a string.  It is written in such
 *     a way as that the target string maybe the same as the source string.
 *
 * Parameters:
 *   o targetStr (I) - String to store the down-shifted string in.  Must
 *     have enough space allocated to store the string.  If NULL is specified,
 *     then the string will be dynamicly allocated and returned as the
 *     result of the function. May also be the same as the source string to
 *     shift in place.
 *   o sourceStr (I) - The string to down-shift.
 *
 * Returns:
 *   A pointer to the down-shifted string
 *-----------------------------------------------------------------------------
 */
char *
Tcl_DownShift (targetStr, sourceStr)
    char       *targetStr;
    CONST char *sourceStr;
{
    register char theChar;

    if (targetStr == NULL)
        targetStr = ckalloc (strlen ((char *) sourceStr) + 1);

    for (; (theChar = *sourceStr) != '\0'; sourceStr++) {
        if (isupper (theChar))
            theChar = _tolower (theChar);
        *targetStr++ = theChar;
    }
    *targetStr = '\0';
    return targetStr;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_UpShift --
 *     Utility procedure to up-shift a string.
 *
 * Parameters:
 *   o targetStr (I) - String to store the up-shifted string in.  Must
 *     have enough space allocated to store the string.  If NULL is specified,
 *     then the string will be dynamicly allocated and returned as the
 *     result of the function. May also be the same as the source string to
 *     shift in place.
 *   o sourceStr (I) - The string to up-shift.
 *
 * Returns:
 *   A pointer to the up-shifted string
 *-----------------------------------------------------------------------------
 */
char *
Tcl_UpShift (targetStr, sourceStr)
    char       *targetStr;
    CONST char *sourceStr;
{
    register char theChar;

    if (targetStr == NULL)
        targetStr = ckalloc (strlen ((char *) sourceStr) + 1);

    for (; (theChar = *sourceStr) != '\0'; sourceStr++) {
        if (ISLOWER (theChar))
            theChar = _toupper (theChar);
        *targetStr++ = theChar;
    }
    *targetStr = '\0';
    return targetStr;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_DStringGets --
 *
 *    Reads a line from a file into a dynamic string.  The string will be
 * expanded, if necessary and reads are done until EOL or EOF is reached.
 * The line is appended to any data already in the string.
 *
 * Parameter
 *   o filePtr (I) - File to read from.
 *   o dynStrPtr (I) - String to return the data in.
 * Returns:
 *    o TCL_BREAK - EOF
 *    o TCL_OK - If data was transfered.
 *    o TCL_ERROR - An error occured.
 *-----------------------------------------------------------------------------
 */
int
Tcl_DStringGets (filePtr, dynStrPtr)
    FILE         *filePtr;
    Tcl_DString  *dynStrPtr;
{
    char           buffer [128];
    register char *bufPtr, *bufEnd;
    register int   readVal;
    int            startLength = dynStrPtr->length;

    bufPtr = buffer;
    bufEnd = buffer + sizeof (buffer) - 1;

    clearerr (filePtr);  /* Clear previous error/EOF */

    while (TRUE) {
        readVal = getc (filePtr);
        if (readVal == '\n')      /* Is it a new-line? */
            break;
        if (readVal == EOF)
            break;
        *bufPtr++ = readVal;
        if (bufPtr > bufEnd) {
            Tcl_DStringAppend (dynStrPtr, buffer, sizeof (buffer));
            bufPtr = buffer;
        }
    }
    if ((readVal == EOF) && ferror (filePtr))
        return TCL_ERROR;   /* Error */

    if (bufPtr != buffer) {
        Tcl_DStringAppend (dynStrPtr, buffer, bufPtr - buffer);
    }

    if ((readVal == EOF) && dynStrPtr->length == startLength)
        return TCL_BREAK;
    else
        return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_GetLong --
 *
 *      Given a string, produce the corresponding long value.
 *
 * Results:
 *      The return value is normally TCL_OK;  in this case *longPtr
 *      will be set to the integer value equivalent to string.  If
 *      string is improperly formed then TCL_ERROR is returned and
 *      an error message will be left in interp->result.
 *
 * Side effects:
 *      None.
 *
 *-----------------------------------------------------------------------------
 */
int
Tcl_GetLong(interp, string, longPtr)
    Tcl_Interp *interp;
    CONST char *string;
    long       *longPtr;
{
    char *end, *p;
    long i;

    errno = 0;
    for (p = (char *) string; isspace(UCHAR(*p)); p++) {
        /* Empty loop body. */
    }
    i = strtol(p, &end, 0);
    if (end == p) {
        badInteger:
        Tcl_AppendResult(interp, "expected integer but got \"", string,
                "\"", (char *) NULL);
        return TCL_ERROR;
    }
    if (errno == ERANGE) {
        return ReturnOverflow (interp);
    }
    while ((*end != '\0') && isspace(UCHAR(*end))) {
        end++;
    }
    if (*end != '\0') {
        goto badInteger;
    }
    *longPtr = i;
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_GetUnsigned --
 *
 *      Given a string, produce the corresponding unsigned integer value.
 *
 * Results:
 *      The return value is normally TCL_OK;  in this case *unsignedPtr
 *      will be set to the integer value equivalent to string.  If
 *      string is improperly formed then TCL_ERROR is returned and
 *      an error message will be left in interp->result.
 *
 * Side effects:
 *      None.
 *
 *-----------------------------------------------------------------------------
 */
int
Tcl_GetUnsigned(interp, string, unsignedPtr)
    Tcl_Interp *interp;
    CONST char *string;
    unsigned   *unsignedPtr;
{
    char          *end, *p;
    unsigned long  i;

    /*
     * Since some strtoul functions don't detect negative numbers, check
     * in advance.
     */
    errno = 0;
    for (p = (char *) string; isspace(UCHAR(*p)); p++) {
        /* Empty loop body. */
    }
    if (*p == '-')
        goto badUnsigned;
    if (*p == '+') {
        p++;
    }
    i = strtoul(p, &end, 0);
    if (end == p) {
        goto badUnsigned;
    }
    if (errno == ERANGE) {
        return ReturnOverflow (interp);
    }
    while ((*end != '\0') && isspace(UCHAR(*end))) {
        end++;
    }
    if (*end != '\0') {
        goto badUnsigned;
    }

    *unsignedPtr = i;
    return TCL_OK;

  badUnsigned:
    Tcl_AppendResult (interp, "expected unsigned integer but got \"", 
                      string, "\"", (char *) NULL);
    return TCL_ERROR;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_GetTime --
 *
 *      Given a string, produce the corresponding time_t value.
 *
 * Results:
 *      The return value is normally TCL_OK;  in this case *timepPtr
 *      will be set to the integer value equivalent to string.  If
 *      string is improperly formed then TCL_ERROR is returned and
 *      an error message will be left in interp->result.
 *
 * Side effects:
 *      None.
 *
 *-----------------------------------------------------------------------------
 */
int
Tcl_GetTime(interp, string, timePtr)
    Tcl_Interp *interp;
    CONST char *string;
    time_t     *timePtr;
{
    char          *end, *p;
    unsigned long  i;

    /*
     * Since some strtoul functions don't detect negative numbers, check
     * in advance.
     */
    errno = 0;
    for (p = (char *) string; isspace(UCHAR(*p)); p++) {
        /* Empty loop body. */
    }
    if (*p == '-')
        goto badTime;
    if (*p == '+') {
        p++;
    }
    i = strtoul(p, &end, 0);
    if (end == p) {
        goto badTime;
    }
    if (errno == ERANGE) {
        return ReturnOverflow (interp);
    }
    while ((*end != '\0') && isspace(UCHAR(*end))) {
        end++;
    }
    if (*end != '\0') {
        goto badTime;
    }

    *timePtr = (time_t) i;
    if (*timePtr != i)
        goto badTime;
    return TCL_OK;

  badTime:
    Tcl_AppendResult (interp, "expected unsigned time but got \"", 
                      string, "\"", (char *) NULL);
    return TCL_ERROR;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_GetOffset --
 *
 *      Given a string, produce the corresponding off_t value.
 *
 * Results:
 *      The return value is normally TCL_OK;  in this case *timepPtr
 *      will be set to the integer value equivalent to string.  If
 *      string is improperly formed then TCL_ERROR is returned and
 *      an error message will be left in interp->result.
 *
 * Side effects:
 *      None.
 *
 *-----------------------------------------------------------------------------
 */
int
Tcl_GetOffset(interp, string, offsetPtr)
    Tcl_Interp *interp;
    CONST char *string;
    off_t      *offsetPtr;
{
    char *end, *p;
    long i;

    errno = 0;
    for (p = (char *) string; isspace(UCHAR(*p)); p++) {
        /* Empty loop body. */
    }
    i = strtol(p, &end, 0);
    if (end == p) {
        goto badOffset;
    }
    if (errno == ERANGE) {
        return ReturnOverflow (interp);
    }
    while ((*end != '\0') && isspace(UCHAR(*end))) {
        end++;
    }
    if (*end != '\0') {
        goto badOffset;
    }
    *offsetPtr = (off_t) i;
    if (*offsetPtr != i)
        goto badOffset;
    return TCL_OK;

  badOffset:
    Tcl_AppendResult (interp, "expected integer offset but got \"", 
                      string, "\"", (char *) NULL);
    return TCL_ERROR;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_RelativeExpr --
 *
 *    Evaluate an expression that may start with the magic words "end" or
 * "len".  These strings are replaced with either the end offset or the
 * length that is passed in.
 *
 * Parameters:
 *   o interp (I) - A pointer to the interpreter.
 *   o cstringExpr (I) - The expression to evaludate.
 *   o stringLen (I) - The length of the string.
 *   o exprResultPtr (O) - The result of the expression is returned here.
 * Returns:
 *   TCL_OK or TCL_ERROR.
 *-----------------------------------------------------------------------------
 */
int
Tcl_RelativeExpr (interp, cstringExpr, stringLen, exprResultPtr)
    Tcl_Interp  *interp;
    char        *cstringExpr;
    long         stringLen;
    long        *exprResultPtr;
{
    
    char *buf;
    int   exprLen, result;
    char  staticBuf [64];

    if (!(STRNEQU (cstringExpr, "end", 3) ||
          STRNEQU (cstringExpr, "len", 3))) {
        return Tcl_ExprLong (interp, cstringExpr, exprResultPtr);
    }

    sprintf (staticBuf, "%ld",
             stringLen - ((cstringExpr [0] == 'e') ? 1 : 0));
    exprLen = strlen (staticBuf) + strlen (cstringExpr) - 2;

    buf = staticBuf;
    if (exprLen > sizeof (staticBuf)) {
        buf = (char *) ckalloc (exprLen);
        strcpy (buf, staticBuf);
    }
    strcat (buf, cstringExpr + 3);

    result = Tcl_ExprLong (interp, buf, exprResultPtr);

    if (buf != staticBuf)
        ckfree (buf);
    return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_GetOpenFileStruct --
 *
 *    Convert a file handle to a pointer to the internal Tcl file structure.
 *
 * Parameters:
 *   o interp (I) - Current interpreter.
 *   o handle (I) - The file handle to convert.
 * Returns:
 *   A pointer to the open file structure for the file, or NULL if an error
 * occured.
 *-----------------------------------------------------------------------------
 */
OpenFile *
Tcl_GetOpenFileStruct (interp, handle)
    Tcl_Interp *interp;
    char       *handle;
{
    FILE   *filePtr;

    if (Tcl_GetOpenFile (interp, handle,
                         FALSE, FALSE,  /* No checking */
                         &filePtr) != TCL_OK)
        return NULL;

    return tclOpenFiles [fileno (filePtr)];
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_SetupFileEntry --
 *
 * Set up an entry in the Tcl file table for a file number, including the stdio
 * FILE structure.
 *
 * Parameters:
 *   o interp (I) - Current interpreter.  The file handle is NOT returned
 *     in result, it is cleared.  This is done because the file handles
 *     are often returned in different ways.
 *   o fileNum (I) - File number to set up the entry for.
 *   o permissions (I) - Flags consisting of TCL_FILE_READABLE,
 *     TCL_FILE_WRITABLE.
 * Returns:
 *   A pointer to the FILE structure for the file, or NULL if an error
 * occured.
 *
 * Notes:
 *   Use great care setting up an entry for both read and write access.
 * For non-seakable files like sockets, this can cause strange errors.
 *-----------------------------------------------------------------------------
 */
FILE *
Tcl_SetupFileEntry (interp, fileNum, permissions)
    Tcl_Interp *interp;
    int         fileNum;
    int         permissions;
{
    Interp   *iPtr = (Interp *) interp;
    char     *mode;
    FILE     *filePtr;

    /*
     * Set up a stdio FILE control block for the new file.
     */
    switch (permissions) {
      case TCL_FILE_READABLE:
        mode = "r";
        break;
      case TCL_FILE_WRITABLE:
        mode = "w";
        break;
      case TCL_FILE_READABLE | TCL_FILE_WRITABLE:
        mode = "r+";
        break;
      default:
        mode = NULL;  /* Cause a core dump on invalid mode */
    }

    filePtr = fdopen (fileNum, mode);
    if (filePtr == NULL) {
        iPtr->result = Tcl_PosixError (interp);
        return NULL;
    }
    
    Tcl_EnterFile (interp, filePtr, permissions);
    Tcl_ResetResult (interp);

    return filePtr;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_SetupFileEntry2 --
 *
 * Set up an entrues in the Tcl file table for a pair of file numbers, one
 * for read, the other for write.
 *
 * Parameters:
 *   o interp (I) - Current interpreter.  The file handle is NOT returned
 *     in result, it is cleared.  This is done because the file handles
 *     are often returned in different ways.
 *   o readFileNum (I) - Read file number to set up the entry for.
 *   o writeFileNum (I) - Write file number to set up the entry for.
 *   o writeFilePtrPtr (O) - Write FILE structure ptr is returned here.
 *     If NULL, the pointer is not returned.
 * Returns:
 *   A pointer to the FILE structure for read file, or NULL if an error
 * occured.
 *-----------------------------------------------------------------------------
 */
FILE *
Tcl_SetupFileEntry2 (interp, readFileNum, writeFileNum, writeFilePtrPtr)
    Tcl_Interp *interp;
    int         readFileNum;
    int         writeFileNum;
    FILE      **writeFilePtrPtr;
{
    FILE     *readFilePtr, *writeFilePtr;
    OpenFile *tclFilePtr;

    /*
     * Set up dual FILE structs.
     */
    readFilePtr = fdopen (readFileNum, "r");
    if (readFilePtr == NULL) {
        interp->result = Tcl_PosixError (interp);
        return NULL;
    }
    
    writeFilePtr = fdopen (writeFileNum, "w");
    if (writeFilePtr == NULL) {
        interp->result = Tcl_PosixError (interp);
        fclose (readFilePtr);
        return NULL;
    }

    /*
     * Add these into the Tcl file table as a single file.
     */
    Tcl_EnterFile (interp, readFilePtr, TCL_FILE_READABLE);

    tclFilePtr = tclOpenFiles [fileno (readFilePtr)];
    tclFilePtr->permissions |= TCL_FILE_WRITABLE;
    tclFilePtr->f2 = writeFilePtr;

    Tcl_ResetResult (interp);

    if (writeFilePtrPtr != NULL)
        *writeFilePtrPtr = writeFilePtr;
    return readFilePtr;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_CloseForError --
 *
 *   Close a file number on error.  If the file is in the Tcl file table, clean
 * it up too. The variable errno, and interp->result and the errorCode variable
 * will be saved and not lost.
 *
 * Parameters:
 *   o interp (I) - Current interpreter.
 *   o fileNum (I) - File number to close.
 *-----------------------------------------------------------------------------
 */
void
Tcl_CloseForError (interp, fileNum)
    Tcl_Interp *interp;
    int         fileNum;
{
    static char *ERROR_CODE = "errorCode";
    int          saveErrNo = errno;
    Interp      *iPtr = (Interp *) interp;
    char        *saveResult, *errorCode, *saveErrorCode, *argv [2], buf [32];

    saveResult = ckstrdup (interp->result);

    if (iPtr->flags & ERROR_CODE_SET) {
        errorCode = Tcl_GetVar (interp, ERROR_CODE, TCL_GLOBAL_ONLY);
        saveErrorCode = ckstrdup (errorCode);
    } else {
        saveErrorCode = NULL;
    }

    sprintf (buf, "file%d", fileNum);

    argv [0] = "close";
    argv [1] = buf;
    Tcl_CloseCmd (NULL, interp, 2, argv);
    Tcl_ResetResult (interp);

    if (saveErrorCode != NULL) {
        Tcl_SetVar (interp, ERROR_CODE, saveErrorCode, TCL_GLOBAL_ONLY);
        iPtr->flags |= ERROR_CODE_SET;
        free (saveErrorCode);
    }
    Tcl_SetResult (interp, saveResult, TCL_DYNAMIC);

    close (fileNum);  /* In case Tcl didn't have it open */
    
    errno = saveErrNo;
}
     

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_TicksToMS --
 *
 *   Convert clock ticks to milliseconds.
 *
 * Parameters:
 *   o numTicks (I) - Number of ticks.
 * Returns:
 *   Milliseconds.
 *-----------------------------------------------------------------------------
 */
clock_t
Tcl_TicksToMS (numTicks)
     clock_t numTicks;
{
    static clock_t msPerTick = 0;

    /*
     * Some systems (SVR4) implement CLK_TCK as a call to sysconf, so lets only
     * reference it once in the life of this process.
     */
    if (msPerTick == 0)
        msPerTick = CLK_TCK;

    if (msPerTick <= 100) {
        /*
         * On low resolution systems we can do this all with integer math. Note
         * that the addition of half the clock hertz results in appoximate
         * rounding instead of truncation.
         */
        return (numTicks) * (1000 + msPerTick / 2) / msPerTick;
    } else {
        /*
         * On systems (Cray) where the question is ticks per millisecond, not
         * milliseconds per tick, we need to use floating point arithmetic.
         */
        return ((numTicks) * 1000.0 / msPerTick);
    }
}

/*
 *-----------------------------------------------------------------------------
 *
 * CallEvalErrorHandler --
 *
 *   Call the error handler specified in tclx_errorHandler.
 *
 * Parameters:
 *   o interp (I) - A pointer to the interpreter.
 *   o options (I) - Options controling the evaluation:
 *     o TCLX_EVAL_GLOBAL - Evaulate in the global context.
 *     o TCLX_EVAL_ERR_HANDLER - Call the user-specified error callback 
 *       specified in the global variable tclx_errorHandler if an error
 *       occurs.
 *   o cmd (I) - The command to evaluate.
 * Returns:
 *   The Tcl result code from the handler.  TCL_ERROR is returned and
 * result unchanged if not handler is available.
 *-----------------------------------------------------------------------------
 */
static int
CallEvalErrorHandler (interp)
    Tcl_Interp  *interp;
{
    char         *errorHandler, *msgArg;
    char         *msgArgv [2];
    Tcl_DString   command;
    int           result;

    errorHandler = Tcl_GetVar (interp, "tclx_errorHandler", TCL_GLOBAL_ONLY);
    if (errorHandler == NULL)
        return TCL_ERROR;
    
    Tcl_DStringInit (&command);
    Tcl_DStringAppend (&command, errorHandler, -1);

    /*
     * Use list merge to quote the argument, as it probably contains spaces.
     */
    msgArgv [0] = interp->result;
    msgArgv [1] = NULL;
    msgArg = Tcl_Merge (1, msgArgv);

    Tcl_DStringAppend (&command, " ", -1);
    Tcl_DStringAppend (&command, msgArg, -1);
    ckfree (msgArg);

    result = Tcl_GlobalEval (interp, Tcl_DStringValue (&command));
    if (result == TCL_ERROR) {
        Tcl_AddErrorInfo (interp,
                          "\n    (while processing tclx_errorHandler)");
    }

    Tcl_DStringFree (&command);
    return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * TclX_Eval --
 *
 *   Evaluate a Tcl command string with various options.
 *
 * Parameters:
 *   o interp (I) - A pointer to the interpreter.
 *   o options (I) - Options controling the evaluation:
 *     o TCLX_EVAL_GLOBAL - Evaulate in the global context.
 *     o TCLX_EVAL_FILE - Treat the string as the name of a file to eval.
 *     o TCLX_EVAL_ERR_HANDLER - Call the user-specified error callback 
 *       specified in the global variable tclx_errorHandler if an error
 *       occurs.
 *   o string (I) - The command or name of file to evaluate.
 * Returns:
 *   The Tcl result code.
 *-----------------------------------------------------------------------------
 */
int
TclX_Eval (interp, options, string)
    Tcl_Interp  *interp;
    unsigned     options;
    char        *string;
{
    Interp      *iPtr = (Interp *) interp;
    CallFrame   *savedVarFramePtr;
    int          result;

    if (options & TCLX_EVAL_GLOBAL) {
        savedVarFramePtr = iPtr->varFramePtr;
        iPtr->varFramePtr = NULL;
    }

    if (options & TCLX_EVAL_FILE) {
        result = Tcl_EvalFile (interp, string);
    } else {
        result = Tcl_Eval (interp, string);
    }

    if ((result == TCL_ERROR) && (options & TCLX_EVAL_ERR_HANDLER)) {
        result = CallEvalErrorHandler (interp);
    }

    if (options & TCLX_EVAL_GLOBAL) {
        iPtr->varFramePtr = savedVarFramePtr;
    }
    return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * TclX_VarEval --
 *
 *   Evaluate a Tcl command string with various options.
 *
 * Parameters:
 *   o interp (I) - A pointer to the interpreter.
 *   o options (I) - Options controling the evaluation, see TclX_Eval.
 *   o str, ... (I) - String arguments, terminated by a NULL.  They will
 *     be concatenated together to form a single string.
 *-----------------------------------------------------------------------------
 */
int
TclX_VarEval (va_alist)
    va_dcl
{
    va_list      argList;
    Tcl_Interp  *interp;
    unsigned     options;
    char        *str;
    Tcl_DString  cmdBuffer;
    int          result;

    Tcl_DStringInit (&cmdBuffer);

    va_start (argList);
    interp = va_arg (argList, Tcl_Interp *);
    options = va_arg (argList, unsigned);

    while (1) {
        str = va_arg (argList, char *);
        if (str == NULL)
            break;
        Tcl_DStringAppend (&cmdBuffer, str, -1);
    }

    result = TclX_Eval (interp, options, Tcl_DStringValue (&cmdBuffer));
    Tcl_DStringFree (&cmdBuffer);
    
    return result;
}
