                        INTERFACE SPECIFICATION FOR MGT

If you wish to add a new interface to mgt, these are the functions you need to 
come up with.

void open()
        Initialize interface.  Any *interface* specific initialization is done
        here.

void close()
        Leave the interface.

void refreshIO()
        update screen to look like memory image.  Somewhat curses specific.
        Probably will be a null function null(){} in most interfaces.

void plotPiece(pBoard b, int i, int j)
        Plots the piece contained in b at i,j on the real screen.
        Board is read by call boardGet(b, i, j) which returns
        one of P_NOTHING, P_BLACK, P_WHITE, P_DAME, P_BLACKTERR,
        or P_WHITETERR. 


void clearComment()
        Clears comment window

void initBoard()
        Draw a blank board of size 'boardsize'

void clearScreen()
        clears screen. 

int idle(nodep n) 
        Gets the next command.  Acts like an event driven loop.
        Waits for keystroke.  Returns a command casted to an int
        to indicate the command for doit.c to process.

void drawTree(nodep n)
        Show the variations that are available in the var list on 
        the right when starting at node n. (in Ascii version)
        Interface-local variables exists to handle scrolling.
        scrolling should be handled by the interface alone.

void highlightLast(int x, y, movenum, turn)
        Show last move number, current turn, whose move it is,
        and the prisoner count.
        turn==0 means Black just moved.
        If x and y are equal to PASSVAL then the move was a pass.
        Use these to get current player turn and prisoner count
        extern int prisoners[black=0,white=1] ;
        extern int curPlayer;

void readEnv(char **env)
        Check *env for environment string.  Increment it to point to 
        the first character not used.

void notifyMessage(char *s)
        Print single line message someplace.

void notifyClear()
        Clear notify message area

int queryStr(char *query, char *dst, int maxLen)
        Print query out.  Get at most maxLen chars into dst.
        Return length of the result.

void setCursor(int i, int j)
        Move cursor (pointer) to position i,j on go board

void plotMark(pBoard b,int i,int j) 
        Plot mark at position i,j on displayed board.
        DOES NOT modify *b.
        Mark is erased by call to plotPiece

void plotLetter(int i, int j, char c)
        Puts letter c on the screen at board position i,j

int getPoint()    
        Allow user to specify a board position (Cursor
        keys, mouse, etc) Used when scoring the game.
        Uses globals xcur and ycur to
        store the position.  The return value 
        can be C_QUIT to abort score.  C_SCORE to calculate
        the score.  C_REDRAW to redraw screen, or C_NOTHING
        to kill the group we're on right now.

void editComment(char *inp, char **out)
        Edit a comment.  inp contains the input comment to be
        edited.  THIS MUST BE free()'d.  out contains the return
        pointer.  It should be allocated to the appropriate size
        and the new edited comment should be placed in the
        allocated space.

int askYN(char *query, int defalt)
        Prompt user with query.  The query will NOT end in " (y/n)? " 
        If needed by the interface, this should be added here. 
        Returns 1 for yes, 0 for no.  defalt contains the default 
        response (1 for yes, 0 for no).

void notifyError(char *errormsg)
        Display error message.  Give user a chance to see it, and
        clear message.  

void displayInfo(char *s)
        Formats and displays string s as a list of game information.  Each
        informational item starts on its own line but may contain newlines.

Token getInfoToChange()
        Passes back an informational token to change, or t_EOF to escape
        from the info editor.  (Scrolling of the info region should be 
        handled internally here.)

void editInfo(char *input, char **output, property info_item)
	Edits the info item corresponding to info_item.  The current
        text is given in input which should be freed.  The replacement
        text should be put in *output which needs to be allocated by 
        this function.  (This works like the comment editor.)
