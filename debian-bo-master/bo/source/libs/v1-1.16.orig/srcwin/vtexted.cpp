//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//===============================================================
// vtexted.cxx - vTextEditor class functions - based on vTextCanvas
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//                                 
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//
// This code is based on Bruce Wampler's See editor
//===============================================================

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <v/vkeys.h>

#include <v/vtexted.h>
#include <v/vfinddlg.h>

#include <fstream.h>    /// @@@ just temp!

// Working notes:

    const static int lineListSize = 10000;

    // one static find pattern
    int vTextEditor::findCaseSensitive = 0;
    char vTextEditor::theFindPat[MAX_LINE + 2] = "";

// =========================>>> vTextEditor::vTextEditor <<<================
  vTextEditor::vTextEditor(vBaseWindow* parent)
  {

    _parent = parent;                   // remember who my parent was

    // default state flags

    state.findAtBeginning =             // don't leave find at beginning of pat
    state.fixed_scroll = 0;             // Not fixed scrolling
    state.tabspc = 8;

    // switches:
    state.readOnly =
    state.wraplm =                      // wrap limit
    use_wild = 0;                       // use wild cards in match

    oldcol = 1;

    state.ins_mode = 1;                 // insert (vs. overtype)

    b_lastln =
    leftmg = xoutcm =
    tvdlin = state.echof = lastLine = 1;

    linptr = 0;

    mouseCol = mouseRow = -1;   // no mouse stuff yet
    scroll_lin = -1;

    _lines = 0;                 // make these null for smarter destructor

    initBuff()  ;               // initialize buffer routines
    resetBuff();
  }

//=========================>>> vTextEditor::~vTextEditor <<<================
  vTextEditor::~vTextEditor()
  {
    if (_lines)
      {
        for (long ix = 0 ; ix < _nextLine ; ++ix)       // delete contents
            delete [] _lines[ix];
        delete [] _lines;       // delete if created
      }
  }

// #########################################################################
//
// buffer methods
//
// These are designed to be easily overriden to allow more complex
// buffer management -- it should be possible to easily design a
// package that allows unlimited file size, for example.
//
// #########################################################################

//===========================>>> vTextEditor::initBuff <<<==================
  void vTextEditor::initBuff(void)
  {
    // initialize buffer routines - handled as a method so
    // can override

    _lines  = new char*[lineListSize];
    _maxLines = lineListSize - 1;       // safety factor

    _lines[0] = new char[2]; strcpy(_lines[0],"\n");
    _lines[1] = new char[2]; strcpy(_lines[1],"\n");
    _nextLine = 1;
  }

// =======================>>> vTextEditor::resetBuff <<<====================
  void vTextEditor::resetBuff()                         // open the buffer
  {

    // values used by buffer management

    if (_lines)
      {
        delete [] _lines[0];
        delete [] _lines[1];
        for (long ix = 2 ; ix < _nextLine ; ++ix)       // delete contents
            delete [] _lines[ix];
        _lines[0] = new char[2]; strcpy(_lines[0],"\n");
        _lines[1] = new char[2]; strcpy(_lines[1],"\n");
      }
    else
      {
        _lines  = new char*[lineListSize];
        _maxLines = lineListSize - 1;   // safety factor

        _lines[0] = new char[2]; strcpy(_lines[0],"\n");
        _lines[1] = new char[2]; strcpy(_lines[1],"\n");
        _nextLine = 1;
      }

    _curLineBF =
    _nextLine = 1;

    wasColCmd = 0;

    // values used by editor code

    mark.beg_lin = mark.end_lin =       /* manually clear mark range */
    mark.beg_col = mark.end_col = 0;    /* DO NOT CALL ClearMarkRange() */
    mark.beg_chr = mark.end_chr = 0;

    state.cmdCount = 1;
    curlin = 1;

    curchr = _lines[1];

    last_col_out =
    state.counter =
    state.changes = 0;

    dsplin = ddline = 1;
    state.ins_mode = 1;                 // insert (vs. overtype)
  }

// =========================>>> vTextEditor::addLine <<<====================
  int vTextEditor::addLine(char* line)
  {
    // This method adds the line to the end of the working buffer.
    // line numbers start at 1! This is logical - it corresponds
    // to how you would really number a text file. It does not
    // correspond to C 0 based array indexing. So it goes.

    if (!line)
        return 0;

    if (_nextLine >= _maxLines)         // check for max lines
        return 0;

    BUFFPTR bp = appendToBuff(line);

    if (!bp)
        return 0;

    _lines[_nextLine++] = bp;   // point line array to beginning of line


    return 1;
  }

// ======================>>> vTextEditor::appendToBuff <<<==================
  BUFFPTR vTextEditor::appendToBuff(char* line)
  {
    // add text to the buffer -- allocate the spaced right here!

    BUFFPTR buff;
    BUFFPTR bp;
    int len = 0;
    char* cp;

    for (cp = line ; *cp && *cp != '\n' && *cp != '\r' ; ++cp) // break on eos or eol
      {
        ++len;
      }

    buff = (BUFFPTR) new char[len+2];   // the space for the line

    if (!buff)                          // did we get the space?
        return 0;

    for (bp = buff, cp = line ; *cp && *cp != '\n' && *cp != '\r' ; ) // break on eos or eol
      {
        *bp++ = *cp++;
      }
     *bp++ = '\n';
     *bp++ = 0;

    return buff;
  }

// =====================>>> vTextEditor::getFirstLine <<<===================
  int vTextEditor::getFirstLine(char* line, int maxChars)
  {
    // return the first line in the buffer

    return getLine(line, maxChars,1);
  }

// =========================>>> vTextEditor::getNextLine <<<================
  int vTextEditor::getNextLine(char* line, int maxChars)
  {
    // return the next line in the text buffer, not including end of line
    //    returns the number of characters, -1 if at end of file

    return getLine(line, maxChars, _curLineBF +1);
  }

// =========================>>> vTextEditor::getLine <<<===================
  int vTextEditor::getLine(char* line, int maxChars, long lineNum)
  {
    // return the next line in the text buffer, not including end of line
    //    returns the number of characters, -1 if at end of file

    if (lineNum >= _nextLine || lineNum < 1)            // at end!
        return -1;

    _curLineBF = lineNum;                       // Set _curlineBF
    int len = 0;

    for (char* ix = _lines[_curLineBF] ; *ix != 0 && *ix != '\n' && len < maxChars ; )
      {
        line[len++] = *ix++;    // copy & track length
      }
    line[len] = 0;                      // terminate
    return len;
  }

// ===================>>> vTextEditor::displayBuff <<<======================
  void vTextEditor::displayBuff(long lineNum)
  {
    // All lines added, so now display the buffer

    ShowVScroll(1);

    if (lineNum >= _nextLine)
        lineNum = _nextLine - 1;
    if (lineNum < 1)
        lineNum = 1;

    curlin = lineNum;   // Internal stuff begins with line 1
    curchr = GLine(curlin);     /* first char of buffer */
    lastLine = lastLineBF();
    Verify();
    setScrollBar();
  }

// =========================>>> vTextEditor::GLine <<<======================
  BUFFPTR vTextEditor::GLine(long lineNum)
  {
    if (lineNum < 1 || lineNum >= _nextLine)
        return 0;

    return _lines[lineNum];
  }

// =====================>>> vTextEditor::deleteCharBF <<<===================
  BUFFPTR vTextEditor::deleteCharBF(BUFFPTR charNum, long lineNum)
  {
    // First, shift everything up to next end of line left
    // This routine must not assume that lines are stored contiguously


    BUFFPTR ix;
    BUFFPTR newCurChar = charNum;

    char combine[MAX_LINE*2+2];

    char chrDeleted = GCh(charNum);

    if (lastLineBF() < 1)
        return 0;

    if (IsEndLine(chrDeleted))          // combine two lines?
      {
        if (lineNum < (_nextLine - 1))  // There IS a next line
          {
            BUFFPTR bp;
            int to;
            int offset;

            // combine this line with the next line
            to = 0;
            for (bp = _lines[lineNum] ; *bp && !IsEndLine(*bp) ; )
                combine[to++] = *bp++;

            offset = to;                // track where we were

            for (bp = _lines[lineNum+1] ; *bp && !IsEndLine(*bp) ; )
                combine[to++] = *bp++;
            *bp = 0;
            combine[to] = 0;

            // create buffer for it
            bp = appendToBuff(combine);
            if (!bp)
                return 0;

            newCurChar = bp + offset;   // this is new cur char

            // free the old space
            delete [] _lines[lineNum];
            delete [] _lines[lineNum+1];
            _lines[lineNum] = bp;

            // shift up the rest of the lines
            for (long lp = lineNum + 1 ; lp < _nextLine ; ++lp)
                _lines[lp] = _lines[lp + 1];
            --_nextLine;
          }
      }
    else
      {
        newCurChar = charNum;
        for (ix = charNum + 1 ; *ix && !IsEndLine(*ix) ; ++ix)
            *(ix-1) = *ix;
        *(ix-1) = *ix;
        *ix = 0;
      }
    return newCurChar;
  }

// ===================>>> vTextEditor::deleteLinesBF <<<====================
  long vTextEditor::deleteLinesBF(long start, long end)
  {
    // delete the lines in the specified range,
    // return new last line of file

    long to, from;

    if (lastLineBF() < 1)
        return 0;

    for (to = start ; to <= end ; ++to)
        delete[] _lines[to];

    for (to = start, from = end+1 ; from < _nextLine ; ++to, ++from)
      {
        _lines[to] = _lines[from];  // shift
      } 
    _lines[from] = 0;
    _nextLine -= (end - start + 1);

    if (_nextLine <= 1)
      {
        _lines[1] = "\n";
        return 1;
      }

    return _nextLine - 1;
  }

// =====================>>> vTextEditor::insertCharBF <<<===================
  int vTextEditor::insertCharBF(int chr, BUFFPTR& curchr, long& curlin)
  {
    // insert a char into the buffer at curlin, curchr.
    // return new curchr, curlin

    char ln1[MAX_LINE+2];
    char ln2[MAX_LINE+2];
    char* newLine;
    BUFFPTR origLine = _lines[curlin];  // original full line
    int offSet;
    int ix;
    BUFFPTR fp;


    if (chr == 0)               // don't allow this case
        return 0;

    origLine = _lines[curlin];  // original line
        
    offSet = 0;
    for (BUFFPTR bp = origLine ; bp != curchr ; ++bp, ++offSet)
        ;

    if (chr != '\n')
      {
        for (ix = 0, fp = origLine ; *fp != 0 && ix < MAX_LINE ; )
          {
             if (ix == offSet)
                ln1[ix++] = chr;                // add the new char
             ln1[ix++] = *fp++;                 // copy original line
          }
        if (ix == offSet)
            ln1[ix++] = chr;            // add the new char
        ln1[ix] = 0;
        newLine = appendToBuff(ln1);
        if (!newLine)                           // see if got the space
            return 0;
        delete [] _lines[curlin];               // free the old space
        _lines[curlin] = newLine;               // point line to new space
        curchr = newLine + offSet + 1;          // update curchr
      }
    else                                        // adding newline
      {
        // we are inserting a newline!
        for (ix = 0, fp = origLine ; *fp != 0 && ix < MAX_LINE ; )
          {
            if (ix == offSet)
                ln1[ix++] = chr;                // add the new char
            ln1[ix++] = *fp++;                  // copy original line
          }
        if (ix == offSet)
            ln1[ix++] = chr;                    // add the new char
        ln1[ix] = 0;

        // ln1 now has two lines in it, split em up.

        char* cp;
        for (cp = ln1 ; *cp != '\n' ; ++cp)     // find first part
            ;
        *cp++ = 0;

        char *tp;
        for (tp = ln2 ; *cp ; )                 // copy second part
            *tp++ = *cp++;
        *tp = 0;

        newLine = appendToBuff(ln1);
        if (!newLine)                           // see if got the space
            return 0;
        delete [] _lines[curlin];               // free the old space
        _lines[curlin] = newLine;               // point line to new space

        if (_nextLine >= _maxLines)             // check for max lines
            return 0;

        long lx;
        for (lx = _nextLine ; lx > curlin ; --lx)
            _lines[lx] = _lines[lx - 1];

        newLine = appendToBuff(ln2);
        if (!newLine)                           // see if got the space
            return 0;

        ++curlin; ++_nextLine;

        _lines[curlin] = newLine;               // point line to new space

        curchr = _lines[curlin];                // point to line beginning
      }
    return 1;
  }

// ==========================>>> vTextEditor::lineLenBF <<<=================
  int vTextEditor::lineLenBF(long lineNum)
  {
    if (lineNum < 1 || lineNum >= (_nextLine - 1))
        return 0;

    int len = 0;
    for (BUFFPTR ix = _lines[lineNum] ; *ix != '\n' && *ix != 0 ; ++ix)
        ++len;
    return len;
  }

// #########################################################################
//
// Command Interface methods
//
// #########################################################################

// ===================>>> vTextEditor::EditCommand <<<======================
  int vTextEditor::EditCommand(int id, long val)
  {
    // returns 1 if command succeeded, 0 if failed
    // or -1 if not processed

    int retval = 1;             // assume OK
    switch (id)
      {
        case edVerify:          // repaint screen
            Verify();
            break;

        case edFind:            // find
          {
            vFindDialog fdlg(_parent);
	    if (!fdlg.AskFindPat(theFindPat, MAX_LINE, findCaseSensitive))
                return 0;
	    retval = Find(theFindPat,findCaseSensitive,0,0);
            break;
          }

        case edFindNext:        // find again
          {
            if (*theFindPat)            // something to find
		retval = FindNext(findCaseSensitive,0,0);
            else
                retval = 0;
            break;
          }

        case edBufferBottom:    // move to bottom of file (no val)
            bufferBottom();
            break;

        case edCharDelete:      // delete val chars
            retval = charDelete(val);
            break;

        case edCharFoldCase:    // swap case of val letters
            retval = charFoldCase(val);
            break;

        case edCharInsert:      // insert char val
            retval = charInsert(val);
            break;

        case edCharRight:       // move val chars right
            retval = charRight(val);
            break;

        case edLineBeginning:   // move to line beginning (no val)
            lineBeginning();
            break;

        case edLineDown:        // move down val lines in column
            retval = lineDown(val);
            break;

	case edLineDownBeg:     // move down val lines
            retval = lineDownBeg(val);
            break;

        case edLineDelete:      // delete val lines
            lineDelete(val);
            break;

        case edLineDeleteFront: // delete to beginning of line (no val)
            retval = lineDeleteFront();
            break;

        case edLineDeleteToEnd: // delete to end of line (no val)
            retval = lineDeleteToEnd();
            break;

        case edLineEnd:         // move to end of line (no val)
            lineEnd();
            break;

        case edLineGoto:        // move cursor to line val
            retval = lineGoto(val);
            break;

        case edLineOpen:        // open val new blank lines
            retval = lineOpen(val);
            break;

        case edWordRight:       // move cursor val words right
            wordRight(val);
            break;

        case edBalMatch:        // find matching paren (up to val lines away)
            retval = BalMatch(val);
            break;

        case edScrollDown:      // scroll val lines without changing cursor
            scrollDown(val);
            break;

        default:                        // editor can't handle
            retval = -1;
            break;
      }
    return retval;
  }

// ======================>>> vTextEditor::EditKeyIn <<<=====================
  int vTextEditor::EditKeyIn(vKey key, unsigned int shift)
  {
     // process keystrokes -- return 1 if processed, 0 if
     // command fails, and -1 if not recognized

    if (vk_IsModifier(key))             // ignore modifiers
        return -1;

    int retval = 1;
    switch (key)
      {
        case vk_Left:
        case vk_KP_Left:
          {
            if (shift & VKM_Ctrl ||shift & VKM_Shift)
                wordRight(-state.cmdCount);
            else
                retval = charRight(-state.cmdCount,1);
            break;
          }

        case vk_Up:
        case vk_KP_Up:
          {
            if (shift & VKM_Shift)
                retval = lineDownBeg(-state.cmdCount);
            else
                retval = lineDown(-state.cmdCount);
            break;
	  }

        case vk_Right:
        case vk_KP_Right:
          {
            if (shift & VKM_Ctrl ||shift & VKM_Shift)
                wordRight(state.cmdCount);
            else
                retval = charRight(state.cmdCount,1);
            break;
          }

        case vk_KP_Down:
        case vk_Down:
          {
            if (shift & VKM_Shift)
                retval = lineDownBeg(state.cmdCount);
            else
                retval = lineDown(state.cmdCount);
            break;
          }

        case vk_BackSpace:
          {
            retval = charDelete(-state.cmdCount);
            break;
          }

        case vk_Tab:
          {
            retval = defaultKeyIn('\t',shift);
            break;
          }

        case vk_Linefeed:
          {
            break;
          }

        case vk_Return:
        case vk_KP_Enter:
          {
            retval = defaultKeyIn('\n',shift);
            break;
          }

        case vk_Escape:
          {
            break;
          }

        case vk_Delete:
        case vk_KP_Delete:
          {
            if (shift & VKM_Shift || shift & VKM_Ctrl)
                lineDelete(state.cmdCount);
            else
                retval = charDelete(state.cmdCount);
            break;
          }

        case vk_Home:
        case vk_KP_Home:
          {
            if (shift & VKM_Shift || shift & VKM_Ctrl)
                retval = lineGoto(1);
            else
                lineBeginning();
            break;
          }

        case vk_Page_Up:
        case vk_KP_Page_Up:
          {
            if (shift & VKM_Ctrl)
                scrollDown(-state.cmdCount * GetRows());
            else
                retval = lineDown((long)(-state.cmdCount * GetRows()));
            break;
          }

        case vk_Page_Down:
        case vk_KP_Page_Down:
          {
            if (shift & VKM_Ctrl)
                scrollDown(state.cmdCount * GetRows());
            else
                retval = lineDown((long) minl((state.cmdCount * GetRows()), (long)(lastLine - curlin + 1)));
            break;
          }

        case vk_End:
        case vk_KP_End:
          {
            if (shift & VKM_Shift || shift & VKM_Ctrl)
                bufferBottom();
            else
                lineEnd();
            break;
          }

        case vk_KP_Insert:
        case vk_Insert:
          {
            state.ins_mode = !state.ins_mode;
            ChangeInsMode(state.ins_mode);
            break;
          }

        default:
          {
            retval = defaultKeyIn(key,shift);
            break;
          }
      }

    state.cmdCount = 1;
    return retval;
  }

// ===================>>> vTextEditor::defaultKeyIn <<<=====================
  int vTextEditor::defaultKeyIn(vKey key, unsigned int shift)
  {
    // this is a virtual function so that it would be possible
    // to override this method to implement a modal editor like
    // vi or see.
    // Returns -1 if not processed, 1 if successful, 0 if
    // command didn't succeed.

    int chr = (int)key;

    if (shift & VKM_Alt || shift & VKM_Ctrl)
        return -1;

    if (chr < ' ' && chr != '\r' && chr != '\t' && chr != '\n')
        return -1;

    if (chr > 0xFE)     // ignore function and alt keys
        return -1;

    return charInsert(key);
  }

// =======================>>> vTextEditor::Find <<<=========================
  int vTextEditor::Find(char* pat, int caseSensitive, int Down, int Wrap)
  {
    // Copy a new pattern to the pattern buffer

    char *fp = pat;
    int ix;
    for (ix = 0 ; *fp && ix < MAX_LINE ; )
	theFindPat[ix++] = *fp++;
    theFindPat[ix] = 0;

    return FindNext(caseSensitive, Down, Wrap); // now use findNext
  }

// =====================>>> vTextEditor::FindNext <<<=======================
  int vTextEditor::FindNext(int caseSensitive, int Down, int Wrap)
  {
    // find a pattern = return 1 if found, 0 if not

    BUFFPTR findBP = curchr;
    char* foundAt;
    int ix;
    int lineOffset = 0;

    char workPat[MAX_LINE + 2]; // working pattern
    char workLine[MAX_LINE + 2];        // working line

    long findLine = curlin;
    long origLine = curlin;

    if (lastLineBF() < 1)
        return 0;

    int patLen = strlen(theFindPat);

    for (ix = 0 ; theFindPat[ix] != 0 ; ++ix)
      {
        if (caseSensitive)
            workPat[ix] = theFindPat[ix];
        else
            workPat[ix] = tolower(theFindPat[ix]);
      }
    workPat[ix] = 0;

    //@@@  just down for now...

    for ( ; ; )
      {
        lineOffset = findBP - GLine(findLine);  // if not at beginning
        for (ix = 0 ; ix < MAX_LINE && GCh(findBP) != 0 ; ++findBP, ++ix)
          {
            if (caseSensitive)
                workLine[ix] = GCh(findBP);
            else
                workLine[ix] = tolower(GCh(findBP));
          }
        workLine[ix] = 0;

        foundAt = strstr(workLine,workPat);             // see if in there

        if (foundAt != 0)                       // We found it!
          {
            ClearMarkRange();                   /* no range now */
            curlin = findLine;
            int offset = foundAt - &workLine[0];

            curchr = GLine(curlin) + offset + lineOffset;

            // set up mark range
            mark.beg_lin = mark.end_lin = curlin;
            mark.beg_col = col_pos(curchr,0);
            mark.end_col = col_pos(curchr+patLen,0);
            mark.beg_chr = curchr;
            mark.end_chr = curchr + patLen;

            if (!state.findAtBeginning)
                curchr += patLen;               // leave at end

            // need intelligent screen updating
            update(curlin - origLine);
            return 1;
          }
        if (findLine >= lastLine)
          {
            StatusMessage("Pattern Not Found");
            return 0;
          }
        else
          {
            findLine++;
            findBP = GLine(findLine);
          }

      }
    StatusMessage("Pattern Not Found");
    return 0;
  }

// =======================>>> vTextEditor::HPage <<<========================
  void vTextEditor::HPage(int shown, int top)
  {
    vTextCanvasPane::HPage(shown, top);
  }

// =======================>>> vTextEditor::VPage <<<========================
  void vTextEditor::VPage(int shown, int top)
  {

    if (lastLineBF() < 1 || lastLine <= GetRows())
      {
        setScrollBar();
        return;
      }
    if (top >= 100)
        curlin = lastLine;
    else if (top < 1)
        curlin = 1;
    else
        curlin = ((long)top * lastLine) / 100;

    if (curlin < 1)
        curlin = 1;
    if (curlin > lastLine)
        curlin = lastLine;

    curchr = GLine(curlin);             // have to update this, too!

    // Some tricky stuff to put the cursor in the middle.
    int old_ddline = ddline;
    if (lastLine > GetRows())
        ddline = GetRows() / 2;
    else
        ddline = lastLine / 2;
    if (ddline < 1)
        ddline = 1;
    newscr();
    tvxy(findX(),tvy);          /* reset cursor to current position */
    ddline = old_ddline;

    setScrollBar();                     // MUST do this!
    ChangeLoc(curlin,col_pos(curchr,0));
  }

// =====================>>> vTextEditor::HScroll <<<========================
  void vTextEditor::HScroll(int step)
  {
    vTextCanvasPane::HScroll(step);
  }

// =====================>>> vTextEditor::VScroll <<<========================
  void vTextEditor::VScroll(int step)
  {
    if (step < 0)
        scrollDown(-1);
    else
        scrollDown(1);
  }

// =====================>>> vTextEditor::FontChanged <<<====================
  void vTextEditor::FontChanged(vFont& newFont)
  {
    vTextCanvasPane::FontChanged(newFont);
  }

// ====================>>> vTextEditor::TextMouseDown <<<===================
  void vTextEditor::TextMouseDown(int row, int col, int button)
  {
    mouseRow = row + 1;                 // convert to 1,1 corner
    mouseCol = col + 1;
  }

// ======================>>> vTextEditor::TextMouseUp <<<===================
  void vTextEditor::TextMouseUp(int r, int c, int button)
  {
    long origLine = curlin;

    if (mouseRow <= 0 || mouseCol <= 0)
        return;
    
    if (curlin < 1 || GetCols() < 1 || lastLineBF() < 1)
        return;

    int row = r + 1;                    // convert to 1,1 corner
    int col = c + 1;

#ifdef SELECT_AREA
    if (mouseRow == row && mouseCol == col)     // just a move to
      {
#endif
        if (scroll_lin < 0)                     // regular move
          {
            int cnt = row - tvdlin;
            if (cnt != 0)                       // difference
                lineDownBeg((long)cnt,0);               // changing
            else
                lineBeginning();                // same row
          }
        else                    // switching position to scroll window
          {
             curlin = maxl((long)1,minl(scroll_lin + row - 1,lastLine));
             curchr = GLine(curlin);
          }
        // now move to column

        BUFFPTR startx = curchr;                // beginning of line

        while (!IsEndLine(GCh(curchr)) && col > findX() )
          {
            ++curchr;
          }
        if (scroll_lin < 0)                     /* regular move */
          {
            tvhdln();
          }
        else                        /* need to verify since things can be off */
          {
            scroll_lin = -1;
            long old_ddline = ddline;   // trick - terrible awful coding, but...
            ddline = row;
            newscr();
            int xf = findX();
            tvxy(xf,tvy);       /* reset cursor to current position */
            ddline = old_ddline;
          }
        b_lastln = curlin; 
        ChangeLoc(curlin,col_pos(curchr,0));
#ifdef SELECT_AREA
      }
    else                                        // select an area
      {
        // @@@ This will eventually allow selection of an area of text.
        // Until V implements cut and paste, we will intentionally not
        // support it.
      }
#endif

    setScrollBar();
  }

// ======================>>> vTextEditor::TextMouseMove <<<=================
  void vTextEditor::TextMouseMove(int row, int col, int button)
  {
    vTextCanvasPane::TextMouseMove(row,col,button);
  }

// =========================>>> vTextEditor::Redraw <<<=====================
  void vTextEditor::Redraw(int x, int y, int w, int h)
  {
    vTextCanvasPane::Redraw(x,y,w,h);
  }

// ======================>>> vTextEditor::ResizeText <<<====================
  void vTextEditor::ResizeText(const int rows, const int cols)
  {
    Verify();
  }

// #########################################################################
//
// screen update stuff - maps old see/tv to vTextCanvasPane methods
//
// #########################################################################

// =========================>>> vTextEditor::tvxy <<<=======================
  void vTextEditor::tvxy(int ix, int iy)
  {
    // tvxy - position cursor at position x,y 
    //          x=1 is left most column
    //          y=1 is top most line
 
    tvx = ix;
    tvy = iy;
    if (!state.echof)
        return;
    GotoRC(iy-1,ix-1);  // Convert to 0,0 based coords
  }

// =========================>>> vTextEditor::tvplin <<<=====================
  void vTextEditor::tvplin(long lineNum, BUFFPTR chrptr, int whole_line, 
                           int hibeg, int hiend)
  { /* tvplin - put line beginning at chrptr
                will only type what will fit on screen (using xout) */
 
    char tmp;
    int linlen, origx, need_hi_end;

    int hiStart = -1;
    int hiLast = -1;

    BUFFPTR i;

    char linout[MAX_LINE+1];
    char drawout[MAX_LINE+1];

    need_hi_end = 0;

    last_col_out = linptr = 0;
    origx = xoutcm;                     /* save x so can get true linelen */
    for (i =  chrptr; !IsEndLine(GCh(i)) && xoutcm < MAX_LINE; ++i)
      {
        /* xoutcm has current column.  If a hilite goes here,
           add to the linout array as 0x01 or 0x02.  
        */
        if (xoutcm == hibeg)
          {
            hiStart = linptr;           // where highlight starts
            need_hi_end = 1;            /* will need a follow */
          }
        if (xoutcm == hiend)
          {
            hiLast = linptr;            // where highlight ends
            need_hi_end = 0;            /* don't need a follow now */
          }
        if (GCh(i) < ' ' && GCh(i) >= 0)        /* control character? */
          {
            if (GCh(i) == '\t')
              {
                if (state.tabspc > 0)
                  {
                    do 
                      {
                        linout[linptr++] = ' '; /* replace with blanks */
                        ++xoutcm;
                      }
                    while ( ((xoutcm-1) % state.tabspc) != 0);
                  }
                else
                  {
                    linout[linptr++] = '^';
                    linout[linptr++] = 'I';
                    xoutcm += 2;
                    linout[linptr++] = '*';
                    ++xoutcm;
                  }
                continue;
              }
            else                /*  other control character */
              {
                linout[linptr++] = '^';
                ++xoutcm;
                if (xoutcm == GetCols() && !(IsEndLine(GCh(i))) )
                    continue;
                tmp = GCh(i) + '@';
                linout[linptr++] = tmp;

              }
          } /*# end if control character */
        else 
          {
            linout[linptr++] = GCh(i);
          }
        ++xoutcm;
      }

    if (need_hi_end)
      {
        hiLast = linptr;
        need_hi_end = 0;        /* don't need a follow now */
      }

    linout[linptr] = 0;         // terminate the line

    int lineStart =  0;

    if (whole_line)             /* write whole line */
      {
        last_col_out = linlen = mint(GetCols(),linptr-leftmg+1);
        lineStart = leftmg - 1;
      }
    else                        // just part of line
      {
        linlen = mint(GetCols()-origx+1,linptr);
        last_col_out = linlen + origx - 1;
        lineStart = 0;
      }

    paintLine(linout, lineStart, hiStart, hiLast, lineNum);

  }
 
// ======================>>> vTextEditor::paintLine <<<=====================
  void vTextEditor::paintLine(char* linout, int lineStart, 
        int hiStart, int hiLast, long lineNum)
  {
    // paint a line. This can be overridden (e.g., for syntax highlighting).
    // linout: the line to output with tabs converted to spaces, etc.
    // lineStart: where to begin printing the line (for hoiz. scrolling)
    // hiStart, hiLast: reverse text attribute
    // lineNum: the real line number in the buffer this is. This is unused
    //          normally, but can be used for syntax highlighting to get
    //          surrounding context.

    int linlen = strlen(linout);
    if (linlen > 0)             // only draw if there!
      {
        char tmpChr = 0;                // to hold char

        if (hiStart < lineStart && hiLast < lineStart)  // no highlight
          {
            DrawText(&linout[lineStart]);       // simple case
          }
        else                    // has highlighting
          {
            if (hiStart > lineStart)    // highlight from beginning
              {
                tmpChr = linout[hiStart];       // remember char
                linout[hiStart] = 0;
                DrawText(&linout[lineStart]);  // normal part
                linout[hiStart] = tmpChr;       // replace
              }

            tmpChr = linout[hiLast];    // remember char
            linout[hiLast] = 0;         // make end of string
            DrawAttrText(&linout[hiStart],ChReverse); // highlight part
            linout[hiLast] = tmpChr;    // replace

            if (linlen > hiLast)        // rest of line
                DrawText(&linout[hiLast]);
          }
      }
  }

// ====================>>> vTextEditor::type_lines <<<======================
  void vTextEditor::type_lines(long ibeg, int icnt)
  { /* type_lines - type icnt lines starting at lines[ibeg]
                no cr/lf on the last line */
 
    long i, lim;
    int hibeg, hiend;
    BUFFPTR start;

    if (!state.echof)
        return;
    if (ibeg < 1 || ibeg > lastLine)
        return;
    xoutcm = tvx;
    lim = ibeg+icnt-1;

    for (i = ibeg ; i <= lim && i <= lastLine ; ++i)
      {
        /* simple check for whole line highlighting for now */
        if (i == mark.beg_lin)
          {
            hibeg = mark.beg_col;
            if (i == mark.end_lin)
                hiend = mark.end_col;
            else
                hiend = 1000;
          }
        else if (i >= mark.beg_lin && i <= mark.end_lin)
          {
            hibeg = 1;
            if (i == mark.end_lin)
                hiend = mark.end_col;
            else
                hiend = 1000;
          }
        else
          {
            hibeg = hiend = -1;
          }

        start = GLine(i);
        tvplin(i,start,1,hibeg, hiend); /* type out a wole line */
        xoutcm = 1;
        if (last_col_out < GetCols())
            tvelin();   /* erase rest of line */
        if ( i != lim )
            tvxy(1,++tvy);
      }
  }

// =========================>>> vTextEditor::Verify <<<=====================
  void vTextEditor::Verify(void)
  { // Verify - rewrite the screen or type current line with cursor

    int xf, old_ddline;
 
    if (lastLineBF() < 1)
      {
        tvclr();
        return;
      }
    old_ddline = ddline;
    ddline = dsplin;
    newscr();
    xf = findX();
    tvxy(xf,tvy);       /* reset cursor to current position */
    ddline = old_ddline;
    ChangeLoc(curlin,col_pos(curchr,0));
  }

// =====================>>> vTextEditor::tvbotb <<<=========================
  void vTextEditor::tvbotb(int n)
  {  // tvbotb - make n blank lines at the bottom of the screen
 
    if (!state.echof)
        return;
    if (n >= GetRows())
      {
        tvclr();
      }
    else 
      {
        tvxy(1,GetRows());      /* go to real last line */

        ScrollText(n);
        int j = GetRows()-n+1;  /* home to virtual last line */
        tvxy(1,j);      /* position at first new blank line */
      }
  }
 
// ==========================>>> vTextEditor::tvclr  <<<==========================
  void vTextEditor::tvclr(void)
  {  // tvclr - clear the entire screen and home

    if (!state.echof)
        return;
    Clear();
    tvxy(1,1);
    tvx = tvy = 1;
  }
 
// ========================>>> vTextEditor::tvelin <<<======================
  void vTextEditor::tvelin(void)
  {  // tvelin - erase the rest of the current line 
 
    if (!state.echof)
        return;
    int r,c;
    GetRC(r,c);
    ClearRow(r, c);
  }

// ========================>>> vTextEditor::tvelin <<<======================
  void vTextEditor::tvescr(void)
  {  // tvelin - erase from cursor to end of screen
 
    if (!state.echof)
        return;
    ClearToEnd(tvy-1, tvx - 1);
  }

// ==========================>>> vTextEditor::tvtopb <<<====================
  void vTextEditor::tvtopb(int n)
  {  // tvtopb - create n blank lines at the top of the screen
 
    if (!state.echof)
        return;

    tvxy(1,1);          /* home first */
    if ( n >= GetRows())
      {
        tvescr();       /* simply erase the screen */
      }
    else
      {
        ScrollText(-n);
      }
    tvxy(1,1);          /* home first */
  }
 
// =======================>>> vTextEditor::bufferBottom <<<=================
  void vTextEditor::bufferBottom(void)
  { // bufferBottom - move cursor to bottom of current buffer

    if (lastLineBF() < 1)
        return;

    ClearMarkRange();           /* no range now */
    wasColCmd = 0;

    curlin = lastLine;          /* the last real line of text */
    curchr = GLine(curlin);     /* the first char of that line */
    lineEnd();                  /* goto end of the line */
    newscr();                   /* update the screen */
  }

// #########################################################################
//
// char methods
//
// #########################################################################

// =======================>>> vTextEditor::charDelete <<<===================
  int vTextEditor::charDelete(long cnt)
  {  // charDelete - delete next cnt characters

    static char chdel;
    int abscnt,newx;
    BUFFPTR to;
    char ans[2];
    int i;

    if (state.readOnly || lastLineBF() < 1)
        return 0;

    checkIfScrolled();
    if (RemoveMarkRange())      /* there was a range to kill */
        return 1;
    ClearMarkRange();           /* no range now */
    wasColCmd = 0;

    abscnt = (cnt > 0) ? cnt : (-cnt);  /* remember absolute value of cnt */

    state.changes += abscnt;                    /* track changes */

    if (cnt < 0)
        charRight(cnt,0);

    for (i = 0 ; i < abscnt ; ++i)
      {
        chdel = GCh(curchr); /* remember the char we are deleting */
        curchr = deleteCharBF(curchr,curlin);
        if (curchr == 0)
            return 0;
        if (!IsEndLine(chdel))
          {
            tvelin();           /* erase rest of the line */
            tvtyln(curlin,curchr,curchr == GLine(curlin));
            newx = findX();             /* where cursor will go */
            tvxy(newx,tvy);             /* reposition cursor */
          }
        else
          {
            lastLine = lastLineBF();
            if (tvdlin < dsplin)        // not at end
              {
                tvescr();               /* erase rest of screen */
                tvxy(1,tvy);            /* fix it up */
                type_lines(curlin,mint((int)(GetRows() - tvdlin+1),
                                       (int)(lastLine - curlin))  );
                newx = findX();         /* where cursor will go */
                tvxy(newx,tvy);         /* reposition cursor */
              }
            else                        /* at end of buffer */
                Verify(); 
          }
      }

    ChangeLoc(curlin,col_pos(curchr,0));
    return 1;
  }

//=====================>>> vTextEditor::charFoldCase <<<====================
  int vTextEditor::charFoldCase(long cnt)
  {
        /* fold from upper to lower or lower to upper case if letter */
    int ni;
    char fchr;

    if (state.readOnly)
        return 0;

    ClearMarkRange();           /* no range now */
    wasColCmd = 0;

    for (ni = 0 ; ni < cnt ; ++ni)
      {
        fchr = GCh(curchr);     /* get current character */
        if (fchr >= 'a' && fchr <= 'z')
            fchr = cupper(fchr);
        else if (fchr >= 'A' && fchr <= 'Z')
            fchr = clower(fchr);
        if (IsEndLine(fchr))
            charRight((long)1,1);
        else
          {
            if (!charDelete((long) 1))          /* delete cur character */
                return 0;
            if (!charInsert((int)fchr))         /* and put back */
                return 0;
          }
      }
    return 1;
  }

// ====================>>> vTextEditor::charInsert <<<======================
  int vTextEditor::charInsert(int ival)
  {
    long to, from;
    int limit, ix, start_next, line_begin, line_end;
    int nocins, ityp, xf;
    int force_tidy;
    BUFFPTR abvchr;

    int chr;

    if (state.readOnly)
        return 0;
    ClearMarkRange();                   /* no range now */
    wasColCmd = 0;

    chr = ival;                         /* use the passed value */

    ++state.changes;                    /* count changes */

    if (lastLineBF() == 0)              // First actual insert!
      {
        char ln1[4];
        resetBuff();
        if (chr == '\n')        // this effecitvely is two lines!
          {
            ln1[0] = 0;
            addLine(ln1);
            addLine(ln1);
            tvdlin = 2;
            displayBuff(1);             // get a proper display
          }
        else
          {
            ln1[0] = chr;
            ln1[1] = 0;
            addLine(ln1);
            if (!_lines[1])
                return 0;
            curlin = 1;
            curchr = _lines[1] + 1;
            tvdlin = 1;
            displayBuff(1);             // get a proper display
            lineEnd();          // move to end of the line
          }
        setScrollBar();
        return 1;
      }

    if (!state.ins_mode)                        // overtype mode?
      {
        if (!charDelete(1))             // delete nextchar
           return 0;
      }

    if (chr == '\r')
        chr = '\n';                     // this is our convention

    if (chr == 0)
        chr = ' ';
    
    if (!insertCharBF(chr, curchr, curlin))
        return 0;
    lastLine = lastLineBF();            // update this one

    if (chr != '\n')
      {
        tvelin();               /* erase rest of the line */
        tvtyln(curlin,curchr-1,curchr == GLine(curlin));
      }
    else
      {
        long dummy1; int dummy2;
        FindDispLine(dummy1,dummy2);
        if (tvdlin < dsplin)    // not at end
          {
            tvescr();               /* erase rest of screen */
            tvxy(1,tvy);    /* fix it up */
            type_lines(curlin,mint((int)(GetRows() - tvdlin + 1),
            (int)(lastLine - curlin))  );
          }
        else                        /* at end of buffer */
            Verify(); 
        setScrollBar();
      }

    tvhdln();   /* home to display line */
    xf = findX();
    tvxy(xf,tvy);       /* reset cursor to current position */
    ChangeLoc(curlin,col_pos(curchr,0));

    return 1;
 }

// ====================>>> vTextEditor::charRight  <<<======================
  int vTextEditor::charRight(long cnt, int clear_mark)
  {  // charRight: move cursor right cnt characters
     // newlines count as one character

    long change;
    int i, rv;

    if (lastLineBF() < 1)
        return 0;

    checkIfScrolled();
    if (clear_mark)
        ClearMarkRange();               /* no range now */

    wasColCmd = 0;

    rv = 1;                     /* assume success */
    change = 0;                 /* no change yet */
    if (cnt > 0)                /* moving right */
      {
        for (i = 1 ; i <= cnt ; ++i)
          {
            if (IsEndLine(GCh(curchr))) /* at end of line */
              {
                if (curlin >= lastLine)
                  {
                    rv = 0;
                    break;              /* don't go beyond end! */
                  }
                ++curlin;
                ++change;               /* we've gone down one line */
                curchr = GLine(curlin);
              }
            else
                ++curchr;
          }
      }
    else if (cnt < 0)           /* moving left */
      {
        cnt = (-cnt);           /* fix this */
        for (i = 1 ; i <= cnt ; ++i)
          {
            if (curchr == GLine(curlin)) /* at front */
              {
                if (curlin > 1) /* can only go up on >= line 2 */
                  {
                    --curlin;
                    --change;
                    for (curchr = GLine(curlin) ;
                         !IsEndLine(GCh(curchr)) ;
                         ++curchr)
                        ;       /* bump curchr to end of the line */
                  }
                else
                  {
                    rv = 0;
                    break;              /* don't go beyond front! */
                  }
              }
            else
                --curchr;
          }
      }
    if (change != 0)            /* don't do unnecessary change */
        update(change);
    if (clear_mark)
        ChangeLoc(curlin,col_pos(curchr,0));
    tvhdln();
    return rv;
  }

// #########################################################################
//
// line methods
//
// #########################################################################

// ===================>>> vTextEditor::lineBeginning <<<====================
  void vTextEditor::lineBeginning()
  {  /* lineBeginning - move cursor to beginning of current line */

    int xf;

    if (lastLineBF() < 1)
        return;

    ClearMarkRange();           /* no range now */
    wasColCmd = 0;

    curchr = GLine(curlin);     /* point to current character */
    xf = findX();       /* this line needed to make the next */
                        /* call eval order independent, if you wondered */
    tvxy(xf,tvy);       /* and move cursor */
    ChangeLoc(curlin,col_pos(curchr,0));
  }

// =====================>>> vTextEditor::lineDelete <<<=====================
  void vTextEditor::lineDelete(long cnt)
  { // lineDelete - delete cnt lines

    int i;
    int ityp;
    int s_curlin, s_echo, s_dlin;
    int killed_last_line;

    long line_1, last_line;     /* range of lines to kill */
    long istrt;

    if (state.readOnly || lastLineBF() < 1)
        return;
    ClearMarkRange();           /* no range now */
    wasColCmd = 0;

    if (cnt == 0)
        return;

    if (cnt < 0)                /* negative kill */
      {
        cnt = minl(-cnt,curlin-1);      /* all upwards? */
        lineDownBeg(-cnt,0);            /* go up that far */
      }

    if (cnt != 0)
      {
        killed_last_line = (curlin == lastLine) && (cnt == 1);
        range(cnt,&line_1,&last_line);  /* calculate the line numbers to kill */

        curlin = line_1;                /* remember new current line */

        SaveKillLine(last_line);        /* save one line */

        ++state.changes;                        /* count changes */
        lastLine = deleteLinesBF(line_1,last_line);     /* delete the lines from the buffer(s) */

        if (lastLine < curlin)
            curlin = lastLine;          /* don't go past end */

        curchr = GLine(curlin); /* remember new current character */

        if (cnt >= 0 && curlin+(GetRows() - tvdlin) <= lastLine &&
          tvdlin < GetRows())   /* killing down */
          {
            tvxy(1,tvy);        /* get to start of line */
            ityp = (int) minl((long)(GetRows()-tvdlin+1),lastLine - curlin + 1);
            tvescr();   /* erase the screen */
            istrt = curlin;
            type_lines(istrt, ityp);
            tvhdln();   /* home to display line */
          }
        else 
            Verify();                           /* kill up, retype screen */
      }
    ChangeLoc(curlin,col_pos(curchr,0));
  }

// =====================>>> vTextEditor::lineDeleteFront  <<<===============
  int vTextEditor::lineDeleteFront(void)
  { // lineDeleteFront - delete from cursor to beginning of line

    int chrs;

    if (state.readOnly || lastLineBF() < 1)
        return 0;
    ClearMarkRange();           /* no range now */
    wasColCmd = 0;

    SaveKillLine(curlin);                               /* save one line */
    chrs = curchr - GLine(curlin);      /* how much to delete */
    if (chrs > 0)
        return charDelete((long)(-chrs));       /* won't cause a combine, so don't worry */
    return 1;
  }

// =====================>>> vTextEditor::lineDeleteToEnd  <<<===============
  int vTextEditor::lineDeleteToEnd(void)
  { // lineDeleteToEnd:
    //       kill the rest of the line, not including cursor and endLine

    int chrs;
    BUFFPTR i;

    if (state.readOnly || lastLineBF() < 1)
        return 0;
    ClearMarkRange();           /* no range now */
    wasColCmd = 0;

    SaveKillLine(curlin);               /* save one line */
    chrs = 0;
    for (i = curchr; !IsEndLine(GCh(i)) ; ++i)
        ++chrs;                 /* count how much to delete */
    if (chrs > 0)
        return charDelete((long) chrs); /* won't cause combine, so don't worry */
    return 1;
  }

// ========================>>> vTextEditor::lineDown <<<====================
  int vTextEditor::lineDown(long cnt)
  { /* lineDown - move down in column */

    int curcol,l,oldef,needns,ic,ix,lim,rv;

    if (lastLineBF() < 1)
        return 0;
    ClearMarkRange();           /* no range now */

    oldef = state.echof;
    needns = 0;
    if (leftmg > 1)             /* handle right virtual screen different */
      {
        needns = 1;
      }

    if (wasColCmd)      // going up/down columnwise
        curcol = oldcol;
    else
        oldcol = curcol = col_pos(curchr,1);    /* calculate the current column */

    rv = lineDownBeg(cnt,0);    /* go down given lines, update screen */

    state.echof = 0;

    if (curlin >= 1 && curlin <= lastLine && curcol > 1)        /* not at ends? */
      {
        l = lineLenBF(curlin);
        lim = mint(curcol-1,l);
        for (ix = 0, ic = col_pos(curchr,1) ; ix < lim && ic < curcol ;
                ++ix, ic = col_pos(curchr,1))
          ;
        charRight((long)ix,0);
      }

    state.echof = oldef;
    if (needns)                 /* needed new screen */
      {
        Verify();
      }
    else
        tvhdln();
    ChangeLoc(curlin,col_pos(curchr,0));
    wasColCmd = 1;
    return rv;
  }

// ====================>>> vTextEditor::lineDownBeg <<<=====================
  int vTextEditor::lineDownBeg(long cnt, int notify)
  { /* lineDownBeg - move dot down cnt lines */

    long oldlin,change;

    if (lastLineBF() < 1)
        return 0;
    ClearMarkRange();           /* no range now */
    wasColCmd = 0;

    if (curlin == lastLine && cnt > 0)  /* down from last line? */
      {
        lineEnd();
        return 0;               /* make loops fail */
      }
    oldlin = curlin;            /* remember where we started from */
    if (curlin + cnt < 1)
        curlin = 1;
    else if (curlin + cnt > lastLine)
        curlin = lastLine;
    else
        curlin = curlin + cnt;
    curchr = GLine(curlin);     /* point to the current character */
    change = curlin-oldlin;     /* calculate how many lines changed */
    update(change);             /* update the screen */
    if (notify)
        ChangeLoc(curlin,col_pos(curchr,0));
    return change != 0;
  }

// =====================>>> vTextEditor::lineEnd <<<========================
  void vTextEditor::lineEnd()
  { // lineEnd - move cursor to end of the line

    int knt;
    BUFFPTR i;

    if (lastLineBF() < 1)
        return;
    ClearMarkRange();           /* no range now */
    wasColCmd = 0;

    knt = 0;
    for (i = curchr ; !IsEndLine(GCh(i)) ; ++i) /* find end of line */
        ++knt;
    charRight((long)knt,0);             /* move to end of line */
    ChangeLoc(curlin,col_pos(curchr,0));
  }

// ======================>>> vTextEditor::lineOpen <<<======================
  int vTextEditor::lineOpen(long cnt)
  {  // lineOpen - open a new line

    int i;
    long pcnt;

    if (state.readOnly || lastLineBF() < 1)
        return 0;
    pcnt = cnt >= 0 ? cnt : (-cnt);     /* only allow positive opens */

    for (i=1; i<=pcnt; ++i)
      {
        if (!charInsert('\n'))          /* insert right number of newlines */
            return 0;
      }

    lineDownBeg(-pcnt,0);               /* and goto beginning of the opened line */

    lineEnd();
    return 1;
  }

// ======================>>> vTextEditor::lineGoto <<<======================
  int vTextEditor::lineGoto(long cnt)
  { // lineGoto: move cursor to line cnt

    if (cnt < 0 || lastLineBF() < 1)
        return 0;
    ClearMarkRange();           /* no range now */
    wasColCmd = 0;

    curlin = maxl(minl(cnt,lastLine),(long) 1); /* move to lines */
    curchr = GLine(curlin);     /* point to the current character */
    Verify();
    setScrollBar();
    return 1;
  }

// =====================>>> vTextEditor::lineAutoFill <<<===================
  void vTextEditor::lineAutoFill(void)
  {
    BUFFPTR bx, start_chr, startx;
    int lines, chr, old_ef, old_y, ityp, go_right, cp;
    long start_line, istrt;

    if (state.readOnly || lastLineBF() < 1)
        return;

    if (state.wraplm < 1)
        return;                 /* no auto-wrap going on */

    /*  1. Check if current line needs to be wrapped */

    startx = GLine(curlin);     /* first char of line */
    bx = lastBX(curlin);        /* index of last char of line */
    if (startx == bx)
        return;                 /* blank line, can't need tidy */
    if (col_pos(bx,0) <= state.wraplm) /* is last char beyond limit? */
        return;                 /* line doesn't need tidy */
   
    /*  2. If it does, figure out where the cursor is. */

    go_right = curchr - startx; /* where CURRENT char is */
    start_line = curlin;
    old_y = tvdlin;             /* original line */

    /*  3. Echo off. */

    old_ef = state.echof;
    state.echof = 0;    

    /*  4. Tidy down to PP or blank line or line beginning with white */

    for (lines = 1 ; lines < 50 && curlin <= lastLine ;
         ++lines )
      {
        lineFill((long) 1);     /* fix current line */
        chr = GCh(curchr);
        if (chr == '.' || chr == ' ' || chr == '\t' || IsEndLine(chr))
            break;
      }

    /*  5. Restore echo */

    state.echof = old_ef;

    /*  6. Return to current line, making sure it stays where it was
           on the screen (or shift by one if it gets moved),
           and repaint the rest of the screen.
    */

    curlin = start_line;
    curchr = GLine(curlin);
    tvdlin = old_y;

    if (state.echof)
      {
        tvxy(1,old_y);
        istrt = curlin;
        ityp =(int) minl((long)(GetRows()-tvdlin+1),
                         lastLine - curlin + 1);
        type_lines(istrt, ityp);
      }
    
    tvhdln();   /* home to display line */

    for ( ; ; )
      {
        bx = lastBX(curlin);    /* see if still on this line */
        cp = col_pos(bx,0);
        if (go_right <= cp)     /* on this line */
          {
            curchr = GLine(curlin) + go_right;
            break;
          }
        go_right -= cp; /* next line */
        if (!lineDownBeg((long) 1,0))
            break;
      }
    tvhdln();   /* home to display line */
    ChangeLoc(curlin,col_pos(curchr,0));
  }
        
// =====================>>> vTextEditor::lineFill <<<=======================
  int vTextEditor::lineFill(long count)
  {  // lineFill - fill lines to current margin

    char curline[40];           /* first part of current line */
    char* special[] =           /* special keywords */
      {                         /* that shouldn't cause a wrap */
        " ",                    /* the usual non-breakers */
        "\t",
        ".",
        ""
      };

    int oldef, i, key_found;
    BUFFPTR linbeg, old_cc;
    int retval;
    int chr;

    if (state.readOnly || lastLineBF() < 1 || state.wraplm < 1)
        return 0;                       /* no auto-wrap going on */

    ClearMarkRange();           /* no range now */
    wasColCmd = 0;

    retval = 1;
    oldef = state.echof;
    if (count > 1)
        state.echof = 0;
    if (state.wraplm <= 1 || curlin > lastLine)
        goto l900;              /* work only if wrap limit turned on */

    for (i = 1 ; i <= count ; ++i)
      {
        lineBeginning();                /* start at beginning of line */
        if (curlin >= lastLine)
            goto l900;

        /* don't fill leading space, cr, period,  tab, or latex keyword */

        getCurLine(curline,curlin);     /* current line */
        key_found = 0;
        if (*curline)
          {
            for (int iy = 0 ; *special[iy] ; ++iy)      /* search keyword list */
              {
                if (strstr(curline,special[iy]) == curline)
                  {
                    key_found = 1;
                    break;
                  }
              }
          }
        else
            key_found = 1;
        if (key_found)
          {
            lineDownBeg((long) 1,0);
            continue;           /* skip dot commands */
          }

        while (curlin < lastLine)
          {
            if (IsEndLine(GCh(curchr)))
              {
                if (tvx+leftmg-1 < state.wraplm)        /* combine lines! */
                  {
                    /* pt to first char of next line */
                    linbeg = GLine(curlin+1);
                    if (GCh(linbeg) == ' ' || IsEndLine(GCh(linbeg))
                      || GCh(linbeg) == '\t' || GCh(linbeg) == '.')
                      {
                        lineDownBeg((long) 1);
                        break;  /* no more combining */
                      }
                    if (! Fill1(1,32))
                        goto l990;
                    /* fall thru to test for wrap */
                  }
                else
                  {
                    lineDownBeg((long) 1);      /* no more combining on line */
                    break;
                  }
              }

            old_cc = curchr;
            wordRight((long) 1);
            if (tvx+leftmg-1 > state.wraplm)
              {
                if (tvx+leftmg-2 == state.wraplm && IsEndLine(GCh(curchr)) )
                  {
                    lineDownBeg((long) 1);
                    break;
                  }
                else if ((tvx+leftmg-2 == state.wraplm ||
                    tvx+leftmg-3 == state.wraplm) &&
                    (GCh(curchr-1) == ' ' || GCh(curchr-1) == '\t'))
                  {
                    if (!Fill1(-1,'\n'))
                        goto l990;
                  }
                else if (GCh(old_cc-1) == ' ' || 
                    GCh(old_cc-1) == '\t')
                  {
                    curchr = old_cc;
                    if (!Fill1(-1,'\n'))
                        goto l990;
                  }
                else if (GCh(old_cc-2) == ' ' || 
                    GCh(old_cc-2) == '\t')
                  {
                    curchr = old_cc-1;
                    if (!Fill1(-1,'\n'))
                        goto l990;
                  }
                else if (IsEndLine(GCh(curchr)) )
                  {
                    lineDownBeg((long) 1);
                    break;
                  }
                else if (GCh(curchr-1) == ' ' || 
                    GCh(curchr-1) == '\t')
                  {
                    if (!Fill1(-1,'\n'))
                        goto l990;
                  }
                else
                  {
                    for (wordRight((long) -1) ;  /* go back a word, then break */
                        (GCh(curchr-1) != ' ' &&  GCh(curchr-1) != '\t'
                        && !IsEndLine(GCh(curchr-1)) ) ;
                        wordRight((long) -1))
                      {
                        /* this line can't be filled - stop now */
                        if (curchr == GLine(curlin))
                            goto l990;
                      }
                    charInsert('\n');
                  }
                break;
              }
            
          } /* end of for (;;) */

      } /*# end of the for (count) */
l900:
    state.echof = oldef;
    if (oldef && count > 1)
        Verify();

    ChangeLoc(curlin,col_pos(curchr,0));
    return retval;

l990:                           /* failure return */
    retval = 0;
    goto l900;
  }

// #########################################################################
//
// word methods
//
// #########################################################################

// #########################################################################
//
//  Misc. methods
//
// #########################################################################

// =========================>>> vTextEditor::AddToRange <<<=================
  int vTextEditor::AddToRange(long cnt, int by_lines)
  {
    long change, oldlin;

    /* add to mark range varaiable, update current line, then go down */

    if (cnt < 0)                /* don't allow negative counts */
      {
        ErrorMsg("Negative count not allowed for AddToRange");
        return 0;
      }
    else if (cnt == 0)
      {
        ClearMarkRange();               /* no range now */
        return 1;
      }

    if (by_lines)
      {
        if (curlin + cnt > lastLine)    /* past end? */
            cnt = lastLine - curlin + 1;
        if (mark.beg_lin == 0)  /* no lines yet */
          {
            mark.beg_lin = curlin;
            mark.beg_chr = curchr;
            mark.beg_col = col_pos(curchr,0);
          }
        if (mark.end_lin == 0)
            mark.end_lin = curlin + cnt - 1;
        else
            mark.end_lin += cnt;
        mark.end_col = 1000;            /* BIG */
        mark.end_chr = 0;               /* end of line */

        /* paint the lines to highlight */
        if (tvdlin+cnt-1 <= GetRows())
          {
            tvxy(1,tvy);        /* fix it up */
            type_lines(curlin,(int)cnt);
          }

        if (curlin == lastLine && cnt > 0)      /* down from last line? */
          {
            return 1;
          }

        oldlin = curlin;                /* remember where we started from */
        curlin = maxl(minl(curlin+cnt,lastLine),(long)1); /* move down lines */
        curchr = GLine(curlin);         /* point to the current character */
        change = curlin - oldlin;       /* calculate how many lines changed */
      }
    else                                /* by character */
      {
        oldlin = curlin;                /* remember where we started from */
        if (mark.beg_lin == 0)          /* no lines yet */
          {
            mark.beg_lin = curlin;
            mark.beg_chr = curchr;
            mark.beg_col = col_pos(curchr,0);
          }


        charRight(cnt,0);                       /* go right cnt */

        mark.end_lin = curlin;
        mark.end_chr = curchr;
        mark.end_col = col_pos(curchr,0);

        /* paint the lines to highlight */

        if (curlin == oldlin)
          {
            tvxy(1,tvy);                /* fix it up */
            type_lines(curlin, 1);
            tvhdln();
          }
        else
            Verify();
            
        return 1;
      }

    update(change);     /* update the screen */

    return 1;           /* nothing to add, assume success */
  }

// ======================>>> vTextEditor::CopySelection <<<=================
  int vTextEditor::CopySelection(char* buff, int max)
  {
    long cnt;
    int ix;
    char *to;
    MARK_RANGE r_mark;

    to = buff;

    if (mark.beg_lin != 0)              /* we have some text to kill */
      {
        r_mark = mark;                  /* make copy to avoid recursion */

        /* now safe to kill off range */
        mark.beg_lin = mark.end_lin =
        mark.beg_col = mark.end_col = 0;
        mark.beg_chr = mark.end_chr = 0;

        /* clean up screen */
        if (curlin != r_mark.beg_lin || 
            r_mark.end_lin - r_mark.beg_lin > 0)
          {
            Verify();
          }
        else
          {
            tvxy(1,tvdlin);     /* fix it up */
            type_lines(curlin, 1);
            tvhdln();
          }

        /* copy the range */

        cnt = r_mark.end_lin - r_mark.beg_lin + 1;

        if (cnt == 1)                   /* all on current line */
          {
            if (r_mark.end_chr) /* find type range */
              {
                cnt = r_mark.end_chr - r_mark.beg_chr;
              }
            else
              {
                for (cnt = 0 ; GCh(r_mark.beg_chr + cnt) != 0 && cnt < 100 ;
                        ++cnt)
                    ;
              }
            for (ix = 0 ; ix < cnt && ix < 100 ; ++ix)  /* copy directly */
              {
                *(to+ix) = GCh(r_mark.beg_chr+ix);
                if (*(to+ix) == 0)
                    break;
              }
            *(to+ix) = 0;
          }
        else                    /* more than one line */
          {
            return 0;
          }
      }
    return 1;
  }

// ======================>>> vTextEditor::ClearMarkRange <<<================
  void vTextEditor::ClearMarkRange(void)
  {

    checkIfScrolled();                  // can check for scroll here

    /* clear the range variables */
    if (mark.beg_lin != 0)
      {
        mark.beg_lin = mark.end_lin = 
        mark.beg_col = mark.end_col = 0;
        mark.beg_chr = mark.end_chr = 0;
        Verify();
      }
    else                        /* just be sure we stay in phase */
      {
        mark.beg_lin = mark.end_lin =
        mark.beg_col = mark.end_col = 0;
        mark.beg_chr = mark.end_chr = 0;
      }
  }

// =====================>>> vTextEditor::RemoveMarkRange <<<================
  int vTextEditor::RemoveMarkRange(void)
  {
     /* delete the text in the marked range
        return
            0 : no text deleted
            1 : text deleted
    */

    static int inHere = 0;                      // don't let it be recursive
    int retval = 0;

    long cnt, orig_line;
    MARK_RANGE r_mark;
    BUFFPTR orig_chr;

    if (inHere)
        return 0;
    inHere = 1;
    if (mark.beg_lin != 0)              /* we have some text to kill */
      {
        r_mark = mark;                  /* make copy to avoid recursion */

        /* now safe to kill off range */
        mark.beg_lin = mark.end_lin = 
        mark.beg_col = mark.end_col = 0;
        mark.beg_chr = mark.end_chr = 0;

        /* kill off range */
        orig_line = curlin;
        orig_chr = curchr;
        curlin = r_mark.beg_lin;
        curchr = r_mark.beg_chr;
        cnt = r_mark.end_lin - r_mark.beg_lin + 1;

        if (curlin != orig_line)        /* refresh screen if moved */
            Verify();
        else
            tvhdln();                   /* just fix the cursor */

        if (!state.readOnly && cnt == 1)                        /* all on current line */
          {
            if (r_mark.end_chr)         /* find type range */
              {
                long dcnt = r_mark.end_chr - r_mark.beg_chr;
                charDelete(dcnt);
              }
            else                        /* ^M type range */
              {
                if (curchr == GLine(curlin))
                    lineDelete(cnt);    /* kill whole current line */
                else
                  {
                    lineDeleteToEnd();          /* past line beg, just kill rest */
                  }
              }
            retval = 1;
          }
        else if (!state.readOnly)       /* more than one line */
          {
            lineDeleteToEnd();          /* past line beg, just kill rest */
            lineDownBeg((long)1,0);     /* and the end of line */
            --cnt;              /* one less */
            lineDelete(cnt - 1);        /* kill off all but last line */
            if ( !r_mark.end_chr)       /* kill whole last line */
              {
                lineDelete((long)1);
              }
            else
              {
                curchr = r_mark.end_chr;        /* get last char */
                if (IsEndLine(GCh(curchr)))
                    lineDelete((long)1);
                else
                    lineDeleteFront();          /* kill last line */
              }
            charDelete((long)-1);               /* and the lead cr */
            retval = 1;
          }
      }
    ChangeLoc(curlin,col_pos(curchr,0));
    inHere = 0;
    return retval;
  }

// ===========================>>> BalMatch <<<==============================
  int vTextEditor::BalMatch(long val)
  {
    /* Find balance of )]} or [{( */

    int orig_line, dir, nest, old_ef, echo_off;
    long limit, ix;
    BUFFPTR orig_chr;
    char start_c, match;

    if (lastLineBF() < 1)
        return 0;
    ClearMarkRange();           /* no range now */
    wasColCmd = 0;

    old_ef = state.echof;
    echo_off = 0;               /* haven't turned off echo */

    limit = ((long) val) * 4000L;       /* limit for search */

    orig_chr = curchr;
    orig_line = curlin;
    start_c = GCh(curchr);              /* original char */
    switch (start_c)
      {
        case '(':
            match = ')';
            dir = 1;
            break;

        case '{':
            match = '}';
            dir = 1;
            break;

        case '[':
            match = ']';
            dir = 1;
            break;

        case ')':
            match = '(';
            dir = -1;
            break;

        case '}':
            match = '{';
            dir = -1;
            break;

        case ']':
            match = '[';
            dir = -1;
            break;

        default:
            return 0;           /* no op if not a paren thing */

      }

    for (ix = 1, nest = 0 ; ix < limit ; ++ix)
      {
        charRight((long)dir,1);         /* go right */
        if (!echo_off && (curlin != orig_line))
          {
            state.echof = 0;
            echo_off = 1;       /* disable echoing */
            StatusMessage("Scanning for matching paren");
          }
        if (GCh(curchr) == start_c)
            ++nest;             /* bump nest */
        else if (GCh(curchr) == match)
          {
            if (nest)
                --nest;                 /* undo nest */
            else
              {
                if (echo_off)           /* we've turned echo off */
                  {
                    if ((state.echof = old_ef)) /* update if it was on */
                        Verify();
                  }
                return 1;                       /* found the matching thing */
              }
          }
      }

    /* fall through ==> died */
    curchr = orig_chr;
    curlin = orig_line;
    state.echof = old_ef;
    newscr();
    ChangeLoc(curlin,col_pos(curchr,0));
    return 0;
  }

// #########################################################################
//
// Private helper methods
//
// #########################################################################

//========================>>> vTextEditor::col_pos <<<======================
  int vTextEditor::col_pos(BUFFPTR chr_pos, int do_shift)
  {  /* col_pos - find the x position of the character chr_pos on current line
                handles spacing for tabs, control characters etc */

    BUFFPTR i;
    int pos;

    pos = 1;
    for (i = GLine(curlin) + 1 ; i <= chr_pos ; ++i)
      {
        if (GCh(i-1) < ' ' && GCh(i-1) > 0)     /* cur pos depends on last chr */
          {
            if (GCh(i-1) == '\t' && state.tabspc > 0)   /* handle tabs */
              {
                for (++pos ; ((pos-1) % state.tabspc) != 0; ++pos)
                    ;
              }
            else                /* control characters (echoed as ^X) */
                pos += 2;       /* 2 spaces for other control character */
          }
        else                    /* normal character */
            ++pos;
      }

    while (do_shift)
      {
        if (pos < leftmg)       /* won't fit on screen */
          {
            leftmg -= 16;       /* shift left */
          }
        else if (pos >= GetCols() + leftmg)
          {
            leftmg += 16;
          }
        else
            break;
      }

    if (do_shift)
        return pos - leftmg + 1;
    else
        return pos;
  }

// ===========================>>> vTextEditor::mint <<<=====================
  int vTextEditor::mint(int v1, int v2)
  {
    return (v1 > v2 ? v2 : v1);
  }

// ============================>>> vTextEditor::maxt <<<====================
  int vTextEditor::maxt(int v1, int v2)
  {
    return (v1 > v2 ? v1 : v2);
  }

// ===========================>>> vTextEditor::minl <<<=====================
  long vTextEditor::minl(long v1,long v2)
  {
    return (v1 > v2 ? v2 : v1);
  }

// ===========================>>> vTextEditor::maxl <<<=====================
  long vTextEditor::maxl(long v1, long v2)
  {
    return (v1 > v2 ? v1 : v2);
  }

// ===========================>>> vTextEditor::clower <<<===================
  int vTextEditor::clower(int ch)
  {
    return ((ch >='A' && ch<='Z') ? ch + ' ' : ch);
  }

// ==========================>>> vTextEditor::cupper  <<<===================
  int vTextEditor::cupper(int ch)
  {
    return ((ch >= 'a' && ch <= 'z') ? ch - ' ' : ch);
  }

// ===================>>> vTextEditor::line_can_fit <<<=====================
  int vTextEditor::line_can_fit(long l)
  {
    /* if line can't fit onto screen width, we need to update deleted
        characters a bit differently.
    */
    BUFFPTR to;
    int len;
    char tmp;
    
    for (to = GLine(l) + 1, len = 0 ; !IsEndLine(GCh(to)) ; ++to)
      {
        if (GCh(to) < ' ')
            return 0;                   /* ctrl chars mess it up, so false */
        ++len;
      }
    return (len < GetCols());
  }

// =======================>>> vTextEditor::FindDispLine <<<=================
  void vTextEditor::FindDispLine(long& ibeg, int& cnt)
  {  /* FindDispLine - find the display line
        known: current line, calculate where it would go on the screen */

    if (curlin <= dsplin)
      {                                 /* it is in first part of the display */
        ibeg = 1;
        cnt = (int) minl((long)GetRows(),(long)(lastLine));
        tvdlin = (int)curlin;           /* update the display line */
      }
    else if (lastLine-curlin < GetRows()-dsplin)
      {  /* at bottom of display */
        ibeg = maxl((long)1,(long)(lastLine - GetRows() + 1));
        cnt = mint(GetRows(),(int) lastLine);
        tvdlin = minl(curlin,(long)(GetRows()-(lastLine-curlin+1)+1));
      }
    else                        /* normal case: in middle */
      {
        ibeg = maxl((long) 1,(long)(curlin-dsplin+1));
        cnt = minl((long)GetRows(),(long)(lastLine-(ibeg)+1));
        tvdlin = dsplin;
      }
 }

// ======================>>> vTextEditor::findX  <<<========================
  int vTextEditor::findX(void)
  {  /* findX - find the x position of the current character
                handles spacing for tabs, control characters etc */

    BUFFPTR i;
    int pos, need_newscr;

    if (curlin < 1 || GetCols() < 1 || lastLineBF() < 1)
        return 1;
    need_newscr = 0;
    pos = 1;

    for (i = GLine(curlin)+1 ; i <= curchr ; ++i)
      {
        if (GCh(i-1) < ' ' && GCh(i-1) > 0)     /* cur pos depends on last chr */
          {
            if (GCh(i-1) == '\t' && state.tabspc > 0)   /* handle tabs */
              {
                for (++pos ; ((pos-1) % state.tabspc) != 0; ++pos)
                    ;
              }
            else                /* control characters (echoed as ^X) */
                pos += 2;       /* 2 spaces for other control character */
          }
        else                    /* normal character */
            ++pos;
      }

    for (;;)
      {
        if (pos < leftmg)       /* won't fit on screen */
          {
            leftmg -= 16;       /* shift left */
            need_newscr = 1;
          }
        else if (pos >= GetCols()+leftmg)
          {
            leftmg += 16;
            need_newscr = 1;
          }
        else
            break;
      }

    if (need_newscr)
      {
        Verify();
      }

    return (pos-leftmg+1);
  }

// ===================>>> vTextEditor::getCurLine <<<=======================
  void vTextEditor::getCurLine(char* buff, long start)
  {
    int ix;
    BUFFPTR bi;

    buff[0] = 0;                /* empty line for sure */

    if (lastLineBF() < 1)
        return;

    bi = GLine(start);          /* first char of buffer */

    for (ix = 0 ; ix < 38 ; ++ix)       /* 38 chars max */
      {
        if (IsEndLine(GCh(bi+ix)))
            break;
        buff[ix] = GCh(bi+ix);  /* copy the char */
      }
    buff[ix] = 0;                       /* terminate the string */
  }

// ====================>>> vTextEditor::lastBX <<<==========================
  BUFFPTR vTextEditor::lastBX(long line)
  {
    /* return the buff index of the last char of the line */
    BUFFPTR bx;

    for (bx = GLine(line) ; !IsEndLine(GCh(bx)) ; ++bx)
        ;                       /* find last char in line */
    return bx;
  }

// ====================>>> vTextEditor::Fill1  <<<==========================
  int vTextEditor::Fill1(int dir, int val)
  {  /* change character dir to val */

    int oldwrp;

    if (state.readOnly)
        return 0;
    oldwrp = state.wraplm;
    state.wraplm = 0;
    if (! charDelete((long)dir))
        goto l900;
    if (! charInsert(val))
        goto l900;
    state.wraplm = oldwrp;
    return 1;
l900:
    state.wraplm = oldwrp;
    return 0;
  }

// ======================>>> vTextEditor::newscr <<<========================
  void vTextEditor::newscr(void)
  { /* newscr - retype entire screen, updating cursor position if necessary */

   long ibeg;
   int cnt;


    if (curlin < 1)
      {
        tvclr();
        tvxy(1,1);
        return;
      }
    if (lastLine < GetRows())   /* two kinds of screen rewrite */
        tvclr();                        /* clear the screen and home */
    tvxy(1,1);

    dsplin = tvdlin = ddline;   /* home to middle */
    FindDispLine(ibeg, cnt);    /* calculate where it will go */
    type_lines(ibeg,cnt);       /* type it out */
    tvhdln();                   /* home to display line */
  }

// =====================>>> vTextEditor::range  <<<=========================
  void vTextEditor::range(long cnt, long *lbeg, long *lend)
  { /* determine a legal line number range given cnt */

    if (cnt <= 0)
      {
        if ((*lbeg = curlin + cnt) < 0)
            *lbeg = 1;
        *lend = curlin;
        if (cnt < 0)
           *lend = (*lend)-1;
      }
    else
      {
        *lbeg = curlin;
        if ((*lend = curlin+cnt-1) > lastLine)
            *lend = lastLine;
      }
 }

// ====================>>> vTextEditor::setScrollBar <<<====================
  void vTextEditor::setScrollBar()
  {
    long shown;
    long top;
    long last = lastLine;

    if (last < 1)
        last = 1;

    if (lastLine <= (long)GetRows())
        shown = 100L;
    else
        shown = ((long) GetRows() * 100L) / last;

    if (shown < 1)
        shown = 1;

    long cur = (scroll_lin > 0) ? scroll_lin : curlin;  // handle scrolling

    if (cur >= last)
        top = 100L;
    else if (cur == 1)
        top = 0;
    else
        top = (cur * 100L) / last;

    if (top < 0)
        top = 0;

    SetVScroll((int) shown, (int) top);
  }

// ====================>>> vTextEditor::checkIfScrolled <<<=================
  void vTextEditor::checkIfScrolled()
  {
    // If we are scrolled, we need to restore screen

    if (scroll_lin > 0)
      {
        scroll_lin = -1;
        ShowTextCursor();
        setScrollBar();
        Verify();
      }
  }

// ====================>>> vTextEditor::scrollDown <<<======================
  void vTextEditor::scrollDown(long delta)
  { /* scroll screen without moving cursor either 1 line or screenful*/

    long change;

    if (!state.echof)
        return;

    if (lastLineBF() < 1 || delta == 0)
        return;

    if (!state.fixed_scroll && scroll_lin < 0)  /* might be on same screen */
      {
        // this just adjusts the screen without really scrolling
        if (delta == 1 && dsplin > 1)           /* scroll down */
          {
            if ((curlin+GetRows()-tvdlin) >= lastLine)
                return;                         // no where to go
            tvbotb(1);          /* make room */
            tvxy(1,GetRows());  /* home to last line */
            dsplin = tvdlin = tvdlin - 1;       /* change line */
            type_lines((long)(curlin+GetRows()-tvdlin),1);      /* fix up screen */
            tvhdln();   /* home to display line */
            setScrollBar();
            return;
          }
        else if (delta == -1 && dsplin < GetRows())
          {
            if ((curlin-tvdlin) <= 0 )
                return;
            tvtopb(1);          /* make blank lines at top */
            dsplin = tvdlin = tvdlin + 1;       /* change line */
            type_lines((long)(curlin-tvdlin+1),1);      /* fill in */
            tvhdln();   /* home to display line */
            setScrollBar();
            return;
          }
      }

    if (scroll_lin < 0)         /* initial setup */
      {
        if (curlin <= dsplin)   /* on first screen */
          {
            scroll_lin = 1;             /* assume 1st line */
          }
        else if (lastLine - curlin < GetRows() - dsplin)
          {                             /* at bottom of display */
            scroll_lin = maxl((long)1,(long)(lastLine - GetRows() + 1));
          }
        else                    /* normal case: in middle */
          {
            scroll_lin = maxl((long)1,(long)(curlin-dsplin+1));
          }
      }

    if (delta < 0 && scroll_lin == 1)   /* at top already */
        return;

    change = delta;

    if (change < 0)                     /* have to scroll screen down */
      {
        if (change == -1)
          {
            if (scroll_lin <= 1)
                return;
            scroll_lin -= 1;
            tvtopb(1);          /* make blank lines at top */
            type_lines(scroll_lin,1);   /* fill in */
          }
        else                    /* a screenful or so */
          {
            scroll_lin = maxl((long)1,(long)(scroll_lin + delta));
            newScrollScreen(scroll_lin);
          }
      }
    else if (change > 0)                /* have to scroll screen up */
      {
        if ((scroll_lin+GetRows()) > lastLine)
                return;                         // no where to go
        if (change == 1)
          {
            scroll_lin += 1;
            tvbotb(1);          /* make blank lines at top */
            type_lines((long)(scroll_lin+GetRows()-1),1);       /* fill in */
          }
        else                    /* a screenful or so */
          {
            scroll_lin = minl(lastLine, (long)(scroll_lin + delta));
            newScrollScreen(scroll_lin);
          }
      }

    // now, if the curlin is on the new scrolled screen, we need to
    // repaint it so the cursor and highlight area show


    if (curlin >= scroll_lin  && curlin < (scroll_lin + GetRows()))
      {
        scroll_lin = -1;
        ShowTextCursor();
        int xf = findX();
        tvxy(xf,tvdlin);
      }
    else
        HideTextCursor();

    setScrollBar();
    
  }

// =================>>> vTextEditor::newScrollScreen <<<====================
  void vTextEditor::newScrollScreen(long ibeg)
  { /* newScrollScreen - retype entire screen, 
        updating cursor position if necessary */

    tvclr();                    /* clear the screen and home */
    type_lines(ibeg,(int)minl(lastLine,(long)GetRows()));       /* type it out */
  }

// ===================>>> vTextEditor::SaveKillLine <<<=====================
  void vTextEditor::SaveKillLine(long lin)
  { /* SaveKillLine - save one line that will be killed */

    BUFFPTR from;
    int to;

    to=0;
    for (from = GLine(lin); !IsEndLine(GCh(from)) ; ++from)
      {
        unkbuf[to]= GCh(from);  /* put it in unkill buffer */
        to = mint(130,to+1);
      }
    unkbuf[to]=0;
  }

// =====================>>> vTextEditor::tvhdln <<<=========================
  void vTextEditor::tvhdln(void)
  { /* tvhdln - home to display line */

    int xf;

    if (curlin < 1)
        tvxy(1,1);
    else
      {
        if (mark.beg_lin > 0)
          {
            tvxy(1,tvdlin);
            type_lines(curlin, 1);
          }
        xf = findX();
        tvxy(xf,tvdlin);
      }
  }

// ========================>>> vTextEditor::tvtyln <<<======================
  void vTextEditor::tvtyln(long lineNum, BUFFPTR chrptr, int whole_line)
  { /* tvtyln - type a line on the screen without cr/lf */

    xoutcm = tvx;
    tvplin(lineNum,chrptr,whole_line,-1,-1);
  }

// =======================>>> vTextEditor::unkill <<<=======================
  int vTextEditor::unkill(void)
  { /* unkill the single last line killed */

    char chrval;
    int i;

    if (state.readOnly)
        return 0;
    lineOpen((long)1);          /* put the CR 1st - makes update pretty */
    for (i=0; unkbuf[i]; ++i)
      {
        chrval = unkbuf[i];
        if (! charInsert(chrval))       /* unkill slowly by using insert */
          {
            return 0;
          }
      }
    lineDownBeg((long)1);               /* back to where we were */
    return 1;
  }

// =======================>>> vTextEditor::update <<<=======================
  void vTextEditor::update(long change)
  { /* update - update the screen when line position has changed
                will not be used if any alterations have been made */

    if (change == 0)
      {
        tvhdln();
        return;
      }
    setScrollBar();
    if (state.fixed_scroll)
        updateScroll(change);   /* cursor stays on fixed line */
    else
        updateNoScroll(change); /* cursor roams around screen */
  }

// ===================>>> vTextEditor::updateScroll <<<=====================
  void vTextEditor::updateScroll(long change)
  { /* update - update the screen when line position has changed
                will not be used if any alterations have been made */

    long abschg;

    if (! state.echof)
        return;
    abschg =  change;

    if (change < 0)                     /* have to scroll screen down */
      {
        abschg = (-change);
        if (tvdlin-abschg < 1)
            Verify();
        else if (curlin < tvdlin)       /* won't fit exactly */
          {
            if (tvdlin >= dsplin && abschg != 1)
              {
                tvclr();                /* clear the screen */
                type_lines((long)1,GetRows());  /* type out a screen */
              }
            tvdlin = curlin;
          }
        else if (tvdlin - abschg >= dsplin)
            tvdlin -= abschg;
        else
          {
            if (tvdlin > dsplin)
              {                         /* moving up from below display line */
                abschg = dsplin-(tvdlin-abschg);
                tvdlin=dsplin;          /* update */
              }
            tvtopb((int)abschg);                /* make blank lines at top */
            type_lines((long)(curlin-tvdlin+1),(int)abschg);    /* fill in */
          }
      }
    else if (change > 0)                /* have to scroll screen up */
      {
        if (tvdlin+change > GetRows() && tvdlin < dsplin ||
                change >= GetRows())
            Verify();
        else if (tvdlin < dsplin || lastLine <= GetRows())
            if (tvdlin+change > dsplin && lastLine > GetRows())
                Verify();
            else
                tvdlin += change;
        else if (lastLine - curlin < GetRows() - tvdlin)        /* on bottom part */
          {
            if (tvdlin<=dsplin && abschg != 1)
              {
                tvclr();                /* rewrite whole screen */
                type_lines((long)(lastLine - GetRows() + 1), GetRows());
              }
            tvdlin = (int) minl((long)GetRows(), (long)(lastLine))
                         - (lastLine - curlin + 1) + 1;
          }
        else
          {
            tvbotb((int)abschg);                /* make room */
            tvxy(1,(int)(GetRows()-abschg+1));  /* home to right line */
            type_lines((long)(curlin+GetRows()-tvdlin-abschg+1),(int)abschg);  /* fix up screen */
            if (tvdlin < dsplin)
                tvdlin = dsplin;
          }
      }
    tvhdln();
  }

// ==================>>> vTextEditor::updateNoScroll <<<====================
  void vTextEditor::updateNoScroll(long change)
  { /* update - update the screen when line position has changed
                will not be used if any alterations have been made */

    long abschg;

    if (! state.echof)
        return;
    abschg =  change;

    if (change < 0)                     /* have to scroll screen down */
      {
        abschg = (-change);
        if (curlin + abschg < tvdlin && curlin < tvdlin) // won't fit exactly
          {
            dsplin = tvdlin = curlin;
          }
        else if (tvdlin - abschg >= 1)
          {
            dsplin = tvdlin -= (int)abschg;
          }
        else if (abschg == 1) /* simple case */  
          {
            tvtopb((int)abschg);                /* make blank lines at top */
            type_lines((long)(curlin-tvdlin+1),(int)abschg);    /* fill in */
            dsplin = tvdlin = 1;
          }
        else            /* scroll to above top line */
          {
            dsplin = tvdlin;
            Verify();
          }
      }
    else if (change > 0)                /* have to scroll screen up */
      {
        if (tvdlin + change <= GetRows())
          {
            dsplin = 
            tvdlin = tvdlin + change;
          }
        else if (change == 1)
          {
            tvbotb((int)abschg);                /* make room */
            tvxy(1,(int)(GetRows()-abschg+1));  /* home to right line */
            type_lines((long)(curlin+GetRows()-tvdlin-abschg+1),(int)abschg);
            /* fix up screen */
            dsplin = 
            tvdlin = GetRows();
          }
        else            /* scroll to above top line */
          {
            dsplin = tvdlin;
            Verify();
          }
      }
    tvhdln();
  }

// =====================>>> vTextEditor::wordRight  <<<=====================
  int vTextEditor::wordRight(long cnt)
  {  /* wordRight - move cursor over words */

    long lim, words, incr, lenmov;
    int rv;

    if (lastLineBF() < 1)
        return 0;
    ClearMarkRange();           /* no range now */
    wasColCmd = 0;

    rv = 1;
    lenmov = 0;
    if (cnt < 0)
      {
        incr = (-1);            /* change */
        lim = (-cnt);
      }
    else if (cnt == 0)
      {
        incr = -1;
        lim = 0;
      }
    else 
      {
        incr = 1; 
        lim = cnt;
      }

    for (words = 1; words <= lim; ++words)
      {
        if ((IsEndLine(GCh(curchr)) && incr > 0) ||
            (curchr == GLine(curlin) && incr < 0) )
          {
            if (curlin + incr > lastLine || curlin + incr < 1)
              {
                rv = 0;
                break;          /* at a buffer limit, so quit */
              }
            lineDownBeg((long)incr,0);  /* move up or down */
            lenmov += incr;
            if (cnt<0)
                lineEnd();
            continue;           /* move to next word */
          }

    /* ok, first, skip over word characters */
        while (IsWordChar(GCh(curchr)))
          {
            if (curchr == GLine(curlin) && incr < 0)
                goto l100;
            else
              {
                curchr += incr;
                lenmov += incr;
              }
          }

    /* now skip over remaining non word chars */
        while (! IsWordChar(GCh(curchr)))
           {
            if ((IsEndLine(GCh(curchr)) && incr > 0) ||
                (curchr == GLine(curlin) && incr < 0) )
                break;
            else
              {
                curchr += incr;
                lenmov += incr;
              }
          }
l100: ;
      }

    if (incr < 0)               /* move cursor to beginning of word */
      {
        while (IsWordChar(GCh(curchr-1)))
          {
            curchr += incr;
            lenmov += incr;
          }
      }
    tvhdln();
    oldlen = lenmov;
    ChangeLoc(curlin,col_pos(curchr,0));
    return rv;
  }

// ====================>>> vTextEditor::IsWordChar <<<======================
  int vTextEditor::IsWordChar(int chr)
  { /* IsWordChar - determine if a character is a "word" type character */

    if ((chr>='a' && chr <= 'z') || (chr >= 'A' && chr <= 'Z') ||
        (chr >= '0' && chr <= '9') || chr == '_' || chr == '\\')
        return 1;
    else
        return 0;
  }
