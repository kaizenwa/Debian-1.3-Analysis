//===============================================================
// vtexted.h - vTextEditor class definitions - based on vTextCanvas
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VTEXTED_H
#define VTEXTED_H

#include <v/vtextcnv.h>

    class vBaseWindow;

    // These values may be used a the id paramater of the
    // EditCommand method, which will then carry out the
    // specfied command. The val parameter is used to
    // pass a count when necessary, and can usually be
    // a positive or negative value. Thus edCharRight with
    // a positive val moves right; with a negative val, left.
    enum                        // editor commands
      {
        edVerify = 10000,       // force repaint of screen (no val)
        edFind,                 // invoke TextEd's find dialog (no val)
        edFindNext,             // find next occurrence of prev (no val)
        edBufferBottom,         // move to bottom of file (no val)
        edCharDelete,           // delete +/- val chars
        edCharFoldCase,         // swap case of +/- val letters
        edCharInsert,           // insert char val
        edCharRight,            // move +/- val chars right
        edLineBeginning,        // move to line beginning (no val)
        edLineDown,             // move down +/- val lines in column
        edLineDownBeg,          // move down +/- val lines
        edLineDelete,           // delete +/- val lines
        edLineDeleteFront,      // delete to beginning of line (no val)
        edLineDeleteToEnd,      // delete to end of line (no val)
        edLineEnd,              // move to end of line (no val)
        edLineGoto,             // move cursor to line val
        edLineOpen,             // open val new blank lines
        edWordRight,            // move cursor +/- val words right
        edBalMatch,             // find matching paren (if > 1, up to val lines away)
        edScrollDown,           // scroll val lines without changing cursor
        edNoOp
      };
    const int MAX_LINE = 300;   // Max line length we will handle

    typedef char* BUFFPTR;

    typedef struct MARK_RANGE
      {
        long beg_lin;           /* first line of marked range */
        long end_lin;           /* last line of marked range */
        int beg_col;            /* col where first line begins */
        int end_col;            /* col where last line ends */
        BUFFPTR beg_chr;                /* first chr */
        BUFFPTR end_chr;
      } MARK_RANGE;

    typedef struct edState
      {
        long changes,           // count of changes
             cmdCount;          // how many times to repeat command
        int
            counter,            // counter for + insert
            echof,              // whether or not to echo action
            findAtBeginning,    // leave find at beginning of pattern
            fixed_scroll,       // flag if using fixed scroll
            ins_mode,           // true if insert mode
            readOnly,           // true if read only
            tabspc,             // tab spacing
            wraplm;             // right limit
      } edState;

    class vTextEditor : public vTextCanvasPane          // The main way to deal with a file
      {
        public:           //---------------------------------------- public
            vTextEditor(vBaseWindow* parent);
            ~vTextEditor();     // Destructor

            virtual void resetBuff();                           // open the buffer
            virtual int addLine(char* line);                    // add a line to end of buffer
            virtual int getFirstLine(char* line, int maxChars); // first line in buffer
            virtual int getNextLine(char* line, int maxChars);  // next line in buffer, -1 = END
            virtual int getLine(char* line, int maxChars, long lineNum); // retrieve given line
            virtual void displayBuff(long lineNum = 1);          // finished with buffer
        
            // Editor command interpreter

            virtual int EditCommand(int id, long val);
            virtual int EditKeyIn(vKey key, unsigned int shift);

            // State Notification

            virtual void ChangeLoc(long line, int col) {}
            virtual void ChangeInsMode(int IsInsMode) {}
            virtual void StatusMessage(char *msg) {}
            virtual void ErrorMsg(char *str) {};
            edState GetEdState() { return state; }
            void SetEdState(edState setState) { state = setState; }
            long GetLines() { return lastLine; }
            int Changed() { return state.changes > 0; }
            void Verify(void);                  // repaint screen

        protected:        //--------------------------------------- protected

            virtual void initBuff(); // create buffers
            virtual BUFFPTR GLine(long lineNum);
            virtual int GCh(BUFFPTR charNum) { return *(charNum); }
            virtual BUFFPTR deleteCharBF(BUFFPTR charNum, long lineNum);
            virtual long deleteLinesBF(long start, long end);
            virtual int insertCharBF(int chr, BUFFPTR& curchr, long& curlin);
            virtual int lineLenBF(long lineNum);
            virtual long lastLineBF() { return _nextLine - 1; } // last line in buff

            void bufferBottom(void);            // move to bottom of file

            // Editor command interpreter

            virtual int defaultKeyIn(vKey key, unsigned int shift);

            // Scrolling

            virtual void HPage(int, int);
            virtual void VPage(int, int);

            virtual void HScroll(int);
            virtual void VScroll(int);

            // Events

            virtual void FontChanged(vFont& newFont);
            virtual void ResizeText(const int rows, const int cols);
            virtual void Redraw(int x, int y, int width , int height);
            virtual void TextMouseDown(int row, int col, int button);
            virtual void TextMouseUp(int row, int col, int button);
            virtual void TextMouseMove(int row, int col, int button);

            // Character oriented methods

            int charDelete(long cnt);           // delete next cnt chars
            int charFoldCase(long cnt); // swap case of letter
            int charInsert(int ival);           // forced insert
            int charRight(long cnt, int clear_mark = 1); // move char right
        

            // line oriented methods

            void lineAutoFill(void);            // automatic filling
            void lineBeginning();               // move to line beginning
            int lineDown(long cnt);             // move down cnt lines
            int lineDownBeg(long cnt, int notify = 1);          // move down cnt lines
            void lineDelete(long cnt);          // delete cnt lines
            int lineDeleteFront(void);          // delete to beginning of line
            int lineDeleteToEnd(void);          // delete to end of line
            void lineEnd();                     // move to end of line
            int lineFill(long count);           // fill count lines
            int lineGoto(long cnt);             // move cursor to line cnt
            int lineOpen(long cnt);             // open new blank line

            // other movement methods

            int wordRight(long cnt);
            void scrollDown(long delta);

            int AddToRange(long cnt, int by_lines);     // add to mark range
            int CopySelection(char* buff, int max);
            void ClearMarkRange(void);
            int RemoveMarkRange(void);
            int BalMatch(long val);             // find matching paren

            void newscr(void);
            int unkill(void);

            int Find(char* pat, int caseSensitive, int Down, int Wrap);
            int FindNext(int caseSensitive, int Down, int Wrap);

            virtual void paintLine(char* linout, int lineStart, 
                int hiStart, int hiLast, long lineNum);
        
            vBaseWindow* _parent;

	    edState state;                      // state stuff for outside
            static int findCaseSensitive;
            static char theFindPat[MAX_LINE + 2]; // one static find pattern

        private:          //--------------------------------------- private

            int IsEndLine(int chr) { return (chr == '\n' || chr == '\r'); }
            void tvhdln(void);
            void setScrollBar();
            void checkIfScrolled();
            void FindDispLine(long& ibeg, int& cnt);
            int CombineLines(void);
            int findX(void);
            int line_can_fit(long l);
            int Fill1(int dir, int val);
            void range(long cnt,long *lbeg, long *lend);
            void updateScroll(long change);
            void updateNoScroll(long change);
            void getCurLine(char* buff, long start);
            BUFFPTR lastBX(long line);
            void newScrollScreen(long ibeg);
            void SaveKillLine(long lin);
            void tvtyln(long lineNum,BUFFPTR chrptr, int whole_line);
            int IsWordChar(int chr);
            void update(long change);

            // Screen manip

            void tvxy(int ix, int iy);
            void tvplin(long lineNum, BUFFPTR chrptr, int whole_line, int hibeg, int hiend);
            void type_lines(long ibeg, int icnt);
            void tvbotb(int n);
            void tvclr(void);
            void tvelin(void);
            void tvescr(void);
            void tvtopb(int n);

            // Utils
            int col_pos(BUFFPTR chr_pos, int do_shift);
            int mint(int v1, int v2);
            int maxt(int v1, int v2);
            long minl(long v1,long v2);
            long maxl(long v1, long v2);
            int clower(int ch);
            int cupper(int ch);

            BUFFPTR appendToBuff(char* line);

            // Operating state - type of scrolling, etc.

            // variables -------------------------------------------
            int 
                ddline,         // default display line
                dsplin,         // the default display line
                last_col_out,   // last column output
                leftmg,         // left margin of display
                linptr,         // used to build output lines
                mouseCol,       // where mouse is
                mouseRow,
                oldcol,         // beginning column for dwncol
                oldlen,         // length for '=' command
                tvdlin,         // the "active" display line
                tvx,            // current x cursor position
                tvy,            // current y cursor position
                use_wild,       // use wild cards in match
                wasColCmd,      // if last command was a column
                xoutcm;         // used for 240 col virtual screen

            long
                b_lastln,       // last line for JUMP
                curlin,         // pointer to current line
                noteloc[10],    // note locations
                lastLine,       // last line in file
                scroll_lin;     // for scrolling commands

            BUFFPTR
                curchr,         // pointer to current character
                scroll_chr;     // for scroll commands

            MARK_RANGE mark;    // marked portion of this

            char unkbuf[132];   // one line of unkill

            BUFFPTR* _lines;

            long _nextLine;     // index into line array
            long _curLineBF;    // current line for getXLine
            long _maxLines;     // size of line array
            
      };
#endif
/* ************************************************************************ */
