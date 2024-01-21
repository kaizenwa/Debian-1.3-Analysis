/* -*- c -*-
 *
 * Author:      James Brister <brister@vix.com> -- berkeley-unix --
 * Start Date:  Sun Dec 31 18:52:04 1995
 * Project:     INN (innfeed)
 * File:        tape.c
 * RCSId:       $Id: tape.c,v 1.15 1996/11/20 18:02:44 brister Exp $
 *
 * Copyright:   Copyright (c) 1996 by Internet Software Consortium
 *
 *              Permission to use, copy, modify, and distribute this
 *              software for any purpose with or without fee is hereby
 *              granted, provided that the above copyright notice and this
 *              permission notice appear in all copies.
 *
 *              THE SOFTWARE IS PROVIDED "AS IS" AND INTERNET SOFTWARE
 *              CONSORTIUM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 *              SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 *              MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL INTERNET
 *              SOFTWARE CONSORTIUM BE LIABLE FOR ANY SPECIAL, DIRECT,
 *              INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 *              WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 *              WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 *              TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
 *              USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Description: The implementation of the Tape class. Tapes are read-only
 *              or write-only files that are accessed sequentially. Their
 *              basic unit of i/o is an Article. Tapes work out of a single
 *              directory and manage all file names themselves.
 *
 *              Tapes will checkpoint themselves periodically so
 *              that when innfeed exits or crashes things can
 *              restart close to where they were last. The period
 *              checkpointing is handled entirely by the Tape class,
 *              but the checkpoint period needs to be set by some
 *              external user before the first tape is created.
 *
 */

 
#if ! defined (lint)
static const char *rcsid = "$Id: tape.c,v 1.15 1996/11/20 18:02:44 brister Exp $" ;
static void use_rcsid (const char *rid) {   /* Never called */
  use_rcsid (rcsid) ; use_rcsid (rid) ;
}
#endif

#include "config.h"

#if defined (DO_HAVE_UNISTD)
#include <unistd.h>
#endif

#include <stdlib.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <syslog.h>
#include <errno.h>

#if defined (DIR_DIRENT)
#include <dirent.h>
typedef struct dirent DIRENTRY ;
#endif

#if defined (DIR_DIRECT)
#include <sys/dir.h>
typedef struct direct DIRENTRY ;
#endif

#if defined (DO_NEED_TIME)
#include <time.h>
#endif
#include <sys/time.h>


#include "tape.h"
#include "article.h"
#include "endpoint.h"
#include "msgs.h"



  /* pathname of directory we store tape files in. */
static char *tapeDirectory ;

  /* the callback ID of the checkpoint timer callback. */
static TimeoutId checkPtId ;

  /* number of seconds between tape checkpoints. */
static u_int tapeCkPtPeriod ;

  /* global list of tapes so we can checkpoint them periodically */
static Tape *activeTapes ;

  /* Size of the activeTapes array */
static size_t activeTapeSize ;

  /* index of last element in activeTapes that's being used. */
static size_t activeTapeIdx ;



  /* a structure for temporary storage of articles. */
typedef struct q_e_s
{
    Article article ;
    struct q_e_s *next ;
} *QueueElem ;


  /* The Tape class type. */
struct tape_s
{
      /* the pathname of the file the administrator can drop in by hand. */
    char *handFilename ;

      /* the pathname of the file the Tape will read from */
    char *inputFilename ;

      /* the pathname of the file the Tape will write to. */
    char *outputFilename ;

      /* the pathname of the file used in locking */
    char *lockFilename ;

      /* the pathname of the file to store the checkpoint information. */
    char *checkpointFilename ;

      /* the peer we're doing this for. */
    char *peerName ;
    
    FILE *inFp ;                /* input FILE */
    FILE *outFp ;               /* output FILE */

      /* the tape holds a small output queue in memory to avoid thrashing. */
    QueueElem head ;            
    QueueElem tail ;
    u_int qLength ;             /* amount on the queue */

    bool outputHasStuff ;       /* true if there are articles to be had */
    bool outputFileExists ;     /* true if we've created the output file */
    
    long tellpos ;              /* for input file checkpointing. */
    bool changed ;              /* true if tape was read since last
                                   checkpoint or start. */

      /* true if articles that are output are NOT later input. */
    bool noRotate ;     
};

void addPointerFreedOnExit (char *pointerToFree) ;


static void checkpointTape (Tape tape) ;
static void removeTapeGlobally (Tape tape) ;
static void addTapeGlobally (Tape tape) ;
static void prepareFiles (Tape tape) ;
static void tapeCheckpointCallback (TimeoutId id, void *d) ;




  /* Create a new Tape object. There are three potential files involved in
     I/O. 'peerName' is what the admin may have dropped in by
     hand. 'peerName.input' is the file that was being used as input the last
     time things were run. 'peerName.output' is the file that was being used
     as output. The file 'peerName' is appended to 'peerName.input' (or
     renamed if 'peerName.input' doesn't exist). Then 'peerName.output' is
     appeneded (or renamed) to 'peerName.input' */

Tape newTape (const char *peerName, bool dontRotate)
{
  Tape nt = ALLOC (struct tape_s, 1) ;
  size_t pLen = strlen (peerName) ;
  size_t dLen = strlen (tapeDirectory) ;
  
  ASSERT (nt != NULL) ;

  if (endsIn (peerName,INPUT_TAIL))
    die ("Sorry, can't have a peer name ending in \"%s\"",INPUT_TAIL) ;

  if (endsIn (peerName,OUTPUT_TAIL))
    die ("Sorry, can't have a peer name ending in \"%s\"",OUTPUT_TAIL) ;

  if (endsIn (peerName,LOCK_TAIL))
    die ("Sorry, can't have a peer name ending in \"%s\"",LOCK_TAIL) ;

  if (endsIn (peerName,CKPT_TAIL))
    die ("Sorry, can't have a peer name ending in \"%s\"",CKPT_TAIL) ;

  nt->peerName = strdup (peerName) ;
  
  nt->handFilename = MALLOC (pLen + dLen + 2) ;
  ASSERT (nt->handFilename != NULL) ;
  sprintf (nt->handFilename,"%s/%s",tapeDirectory,peerName) ;

  nt->lockFilename = MALLOC (pLen + dLen + strlen(LOCK_TAIL) + 2) ;
  ASSERT (nt->lockFilename != NULL) ;
  sprintf (nt->lockFilename,"%s/%s%s",tapeDirectory,peerName,LOCK_TAIL) ;

  nt->inputFilename = MALLOC (pLen + dLen + strlen(INPUT_TAIL) + 2) ;
  ASSERT (nt->inputFilename != NULL) ;
  sprintf (nt->inputFilename,"%s/%s%s",tapeDirectory,peerName,INPUT_TAIL) ;

  nt->outputFilename = MALLOC (pLen + dLen + strlen(OUTPUT_TAIL) + 2) ;
  ASSERT (nt->outputFilename != NULL) ;
  sprintf (nt->outputFilename,"%s/%s%s",tapeDirectory,peerName,OUTPUT_TAIL) ;

  nt->checkpointFilename = MALLOC (pLen + dLen + strlen(CKPT_TAIL) + 2) ;
  ASSERT (nt->checkpointFilename != NULL) ;
  sprintf (nt->checkpointFilename,"%s/%s%s",tapeDirectory,peerName,CKPT_TAIL) ;

  nt->inFp = NULL ;
  nt->outFp = NULL ;
  
  nt->head = NULL ;
  nt->tail = NULL ;
  
  nt->tellpos = 0 ;
  nt->qLength = 0 ;

  nt->outputHasStuff = false ;
  nt->outputFileExists = false ; /* it won't */

  nt->changed = false ;
  
  if ( !lockFile (nt->lockFilename) )
    {
      syslog (LOG_ERR,NO_LOCK_TAPE,nt->lockFilename) ;
      
      FREE (nt->handFilename) ;
      FREE (nt->lockFilename) ;
      FREE (nt->inputFilename) ;
      FREE (nt->outputFilename) ;
      FREE (nt->checkpointFilename) ;
      FREE (nt) ;

      return NULL ;
    }
  
  nt->noRotate = false ;        /* for first time prepare */

  prepareFiles (nt) ;

  nt->noRotate = dontRotate ;

  addTapeGlobally (nt) ;

  if (checkPtId == 0 && tapeCkPtPeriod > 0)     /* only done once. */
    checkPtId = prepareSleep (tapeCheckpointCallback,tapeCkPtPeriod,NULL);

  return nt ;
}


void gPrintTapeInfo (FILE *fp, u_int indentAmt)
{
  char indent [INDENT_BUFFER_SIZE] ;
  u_int i ;
  
  for (i = 0 ; i < MIN(INDENT_BUFFER_SIZE - 1,indentAmt) ; i++)
    indent [i] = ' ' ;
  indent [i] = '\0' ;

  fprintf (fp,"%sGlobal Tape List : (count %d) {\n",
           indent,activeTapeIdx) ;

  for (i = 0 ; i < activeTapeIdx ; i++)
    printTapeInfo (activeTapes [i],fp,indentAmt + INDENT_INCR) ;
  fprintf (fp,"%s}\n",indent) ;
}

void printTapeInfo (Tape tape, FILE *fp, u_int indentAmt)
{
  char indent [INDENT_BUFFER_SIZE] ;
  u_int i ;
  QueueElem qe ;
  
  for (i = 0 ; i < MIN(INDENT_BUFFER_SIZE - 1,indentAmt) ; i++)
    indent [i] = ' ' ;
  indent [i] = '\0' ;

  fprintf (fp,"%sTape : %p {\n",indent,tape) ;

  if (tape == NULL)
    {
      fprintf (fp,"%s}\n",indent) ;
      return ;
    }
  
  fprintf (fp,"%s    master-file : %s\n", indent, tape->handFilename) ;
  fprintf (fp,"%s    input-file : %s\n", indent, tape->inputFilename) ;
  fprintf (fp,"%s    output-file : %s\n",indent, tape->outputFilename) ;
  fprintf (fp,"%s    lock-file : %s\n",indent, tape->lockFilename) ;
  fprintf (fp,"%s    checkpoint-file : %s\n",indent,tape->checkpointFilename);
  fprintf (fp,"%s    peerName : %s\n",indent,tape->peerName) ;
  fprintf (fp,"%s    input-FILE : %p\n",indent, tape->inFp) ;
  fprintf (fp,"%s    output-FILE : %p\n",indent, tape->outFp) ;

  fprintf (fp,"%s    in-memory article queue (length  %d) {\n",indent,
           tape->qLength) ;
  for (qe = tape->head ; qe != NULL ; qe = qe->next)
    {
#if 0
      printArticleInfo (qe->article,fp,indentAmt + INDENT_INCR) ;
#else
      fprintf (fp,"%s    %p\n",indent,qe->article) ;
#endif
    }

  fprintf (fp,"%s    }\n",indent) ;

  fprintf (fp,"%s    output-has-stuff : %s\n",indent,
           boolToString (tape->outputHasStuff)) ;
  fprintf (fp,"%s    output-file-exists : %s\n",indent,
           boolToString (tape->outputFileExists)) ;
  fprintf (fp,"%s    tell-position : %ld\n",indent,(long) tape->tellpos) ;
  fprintf (fp,"%s    input-FILE-changed : %s\n",indent,
           boolToString (tape->changed)) ;

  fprintf (fp,"%s    no-rotate : %s\n",indent, boolToString (tape->noRotate));
  
  fprintf (fp,"%s}\n",indent) ;
  
  
}




  /* delete the tape. Spools the in-memory articles to disk. */
void delTape (Tape tape)
{
  if (tape == NULL)
    return ;

    /* flush out articles not yet gone to disk. */
  if (tape->head != NULL)
    {
      tapeOpen (tape) ;         /* will flush articles to disk */
      tapeClose (tape) ;
    }

  if (tape->inFp)
    {
      checkpointTape (tape) ;
      (void) fclose (tape->inFp) ;
    }

  unlockFile (tape->lockFilename) ;

  freeCharP (tape->handFilename) ;
  freeCharP (tape->inputFilename) ;
  freeCharP (tape->outputFilename) ;
  freeCharP (tape->lockFilename) ;
  freeCharP (tape->checkpointFilename) ;
  freeCharP (tape->peerName) ;
             
  removeTapeGlobally (tape) ;
  
  FREE (tape) ;
}


  /* Request the tape to hang onto the article information. If the output
     side of the tape has been opened explicitly, then the article infor is
     dumped straight to disk. Otherwise it's stored on a small queue. When
     the queue gets too big it will be flushed to disk. */
void tapeTakeArticle (Tape tape, Article article)
{
  QueueElem elem ;

  ASSERT (tape != NULL) ;
  
  tape->outputHasStuff = true ;
  
  if (tape->outFp != NULL)      /* output size opened explicitly */
    {
      tape->outputFileExists = true ;
      
      fprintf (tape->outFp,"%s %s\n",
               artFileName (article), artMsgId (article)) ;

      fflush (tape->outFp) ;
      delArticle (article) ;
    }
  else                          /* stick on queue */
    {
      elem = ALLOC (struct q_e_s, 1) ;
      ASSERT (elem != NULL) ;

      elem->article = article ;
      elem->next = NULL ;
      if (tape->tail != NULL)
        tape->tail->next = elem ;
      else
        tape->head = elem ;
      tape->tail = elem ;
      tape->qLength++ ;

      if (tape->qLength > TAPE_HIGHWATER)
        {
          dprintf (2,"Gone over tape HIGHWATER (%d). Flushing to disk %s\n",
                   TAPE_HIGHWATER, tape->outputFilename) ;
          tapeOpen (tape) ;
          tapeClose (tape) ;
        }
    }
}


  /* Pick an article off a tape and return it. NULL is returned if there
     are no more articles. */
Article getArticle (Tape tape)
{
  char line [2048] ;            /* ick. 1024 for filename + 1024 for msgid */
  char *p, *q ;
  char *msgid, *filename ;
  Article art = NULL ;
  int c ;

  ASSERT (tape != NULL) ;
  
  if (tape->inFp == NULL)
    prepareFiles (tape) ;       /* will flush queue too. */

  while (tape->inFp != NULL && art == NULL)
    {
      tape->changed = true ;
      
      if (fgets (line,sizeof (line), tape->inFp) == NULL)
        {
          if (ferror (tape->inFp))
            syslog (LOG_ERR,TAPE_INPUT_ERROR,tape->inputFilename) ;
          else if ( !feof (tape->inFp) )
            syslog (LOG_ERR,FGETS_FAILED,tape->inputFilename) ;
          
          if (fclose (tape->inFp) != 0)
            syslog (LOG_ERR,FCLOSE_FAILED, tape->inputFilename);

          dprintf (1,"No more articles on tape %s\n",tape->inputFilename) ;

          (void) unlink (tape->inputFilename) ;
          (void) unlink (tape->checkpointFilename) ;

          tape->inFp = NULL ;
          prepareFiles (tape) ; /* rotate files to try next. */
        }
      else
        {
          msgid = filename = NULL ;
          
          for (p = line ; *p && isspace (*p) ; p++) /* eat whitespace */
              /* nada */ ;

          if (*p != '\0')
            {
              q = strchr (p,' ') ;

              if (q != NULL)
                {
                  filename = p ;
                  *q = '\0' ;
      
                  for (q++ ; *q && isspace (*q) ; q++) /* eat more white */
                      /* nada */ ;

                  if (*q != '\0')
                    {
                      if (((p = strchr (q, ' ')) != NULL) ||
                          ((p = strchr (q, '\n')) != NULL))
                        *p = '\0' ;

                      if (p != NULL)
                        msgid = q ;
                      else
                        filename = NULL ; /* no trailing newline or blank */
                    }
                  else
                    filename = NULL ; /* line had one field and some blanks */
                }
              else
                filename = NULL ; /* line only had one field */
            }

          if (filename != NULL && msgid != NULL)
            art = newArticle (filename, msgid) ;

            /* art may be NULL here if the file is no longer valid. */
        }
    }

  
    /* now we either have an article or there is no more on disk */
  
  if (tape->inFp != NULL && ((c = fgetc (tape->inFp)) != EOF))
    ungetc (c,tape->inFp) ;
  else if (tape->inFp != NULL)
    {
        /* last article read was the end of the tape. */
      if (fclose (tape->inFp) != 0)
        syslog (LOG_ERR,FCLOSE_FAILED,tape->inputFilename) ;
      
      tape->inFp = NULL ;

        /* toss out the old input file and prepare the new one */
      (void) unlink (tape->inputFilename) ;
      (void) unlink (tape->checkpointFilename) ;

      prepareFiles (tape) ;
    }
  
  if (art == NULL)
    dprintf (2,"%s All out of articles in the backlog\n",
             tape->peerName) ;
  else
    dprintf (2,"%s Peeled article %s from backlog\n",tape->peerName,
             artMsgId (art)) ;
      
  return art ;
}


  /* (re)open a tape for output. */
bool tapeOpen (Tape tape)
{
  QueueElem elem ;

  ASSERT (tape != NULL) ;

  if (tape->outFp == NULL)
    {
      if ((tape->outFp = fopen (tape->outputFilename,"a")) == NULL)
        {
          syslog (LOG_ERR,TAPE_OPEN_FAILED, "a", tape->outputFilename) ;
          return false ;
        }
    }

    /* flush out queue to disk. */
  elem = tape->head ;
  while (elem != NULL)
    {
      tape->head = tape->head->next ;
      fprintf (tape->outFp,"%s %s\n",artFileName (elem->article),
               artMsgId (elem->article)) ;
      tape->outputFileExists = true ;
      delArticle (elem->article) ;
      
      FREE (elem) ;
      elem = tape->head ;
    }

  tape->tail = NULL ;
  tape->qLength = 0;
      
  fflush (tape->outFp) ;

  return true ;
}


  /* close the output side of the tape. */
void tapeClose (Tape tape)
{
  struct stat st;
  
  ASSERT (tape != NULL) ;
  
  if (tape->outFp != NULL && fclose (tape->outFp) != 0)
    syslog (LOG_ERR,FCLOSE_FAILED, tape->outputFilename) ;

  if (stat(tape->outputFilename, &st) == 0 && st.st_size == 0)
    {
      dprintf (1,"removing empty output tape: %s\n",tape->outputFilename) ;
      (void) unlink (tape->outputFilename) ;
    }
  
  tape->outFp = NULL ;
}



  /* return true if there are articles on the tape somewhere avialable for
     processing. In the NOROTATE case we only look at the input side. */
bool tapeHasArticles (Tape tape)
{
  bool rval = false ;

  ASSERT (tape != NULL) ;
  
  if (tape->inFp != NULL)
    rval = true ;
  
  if ((tape->outputFileExists || tape->head != NULL) && !tape->noRotate)
    rval = true ;
  
  return rval ;
}


  /****************************************************/
  /**                  CLASS FUNCTIONS               **/
  /****************************************************/


  /* set the number of seconds between checkpoints of the input side of a
     tape. */
void tapeSetCheckpointPeriod (u_int ckPtPeriod)
{
  tapeCkPtPeriod = ckPtPeriod ;
}


  /* Cause all the Tapes to checkpoint themselves. */
void checkPointTapes (void)
{
  u_int i ;

  for (i = 0 ; i < activeTapeIdx ; i++)
    checkpointTape (activeTapes [i]) ;
}


  /* Set the pathname of the directory for storing tapes in. can only set
     it once, and before any tapes are created.  */
void setTapeDirectory (const char *newDir)
{
  ASSERT (tapeDirectory == NULL) ;

  tapeDirectory = strdup (newDir) ;

  addPointerFreedOnExit (tapeDirectory) ;
}


  /* Get the pathname of the directory tapes are stored in. */
const char *getTapeDirectory ()
{
  if (tapeDirectory == NULL)
    {
      tapeDirectory = strdup (TAPE_DIRECTORY) ;
      addPointerFreedOnExit (tapeDirectory) ;
    }

  return tapeDirectory ;
}









  /**********************************************************************/
  /*                          PRIVATE FUNCTIONS                         */
  /**********************************************************************/


  /* Add a new tape to the class-level list of active tapes.  */
static void addTapeGlobally (Tape tape)
{
  ASSERT (tape != NULL) ;
  
  if (activeTapeSize == activeTapeIdx)
    {
      u_int i ;
      
      activeTapeSize += 10 ;
      if (activeTapes != NULL)
        activeTapes = REALLOC (activeTapes, Tape, activeTapeSize) ;
      else
        activeTapes = ALLOC (Tape, activeTapeSize) ;

      ASSERT (activeTapes != NULL) ;

      for (i = activeTapeIdx ; i < activeTapeSize ; i++)
        activeTapes [i] = NULL ;
    }
  activeTapes [activeTapeIdx++] = tape ;
}


  /* Remove a tape for the class-level list of active tapes. */
static void removeTapeGlobally (Tape tape)
{
  u_int i ;

  if (tape == NULL)
    return ;
  
  ASSERT (activeTapeIdx > 0) ;
  
  for (i = 0 ; i < activeTapeIdx ; i++)
    if (activeTapes [i] == tape)
      break ;

  ASSERT (i < activeTapeIdx) ;

  for ( ; i < (activeTapeIdx - 1) ; i++)
    activeTapes [i] = activeTapes [i + 1] ;

  activeTapes [--activeTapeIdx] = NULL ;

  if (activeTapeIdx == 0)
    {
      FREE (activeTapes) ;
      activeTapes = NULL ;
    }
}


  /* Have a tape checkpoint itself so that next process can pick up where
     this one left off. */
static void checkpointTape (Tape tape)
{
  FILE *tout ;
  
  if (tape->inFp == NULL)       /* no input file being read. */
    return ;

  if (!tape->changed)   /* haven't read since last checkpoint */
    {
      dprintf (1,"Not checkpointing unchanged tape: %s\n",
               tape->checkpointFilename) ;
      return ;
    }
  
  if ((tape->tellpos = ftell (tape->inFp)) < 0)
    {
      syslog (LOG_ERR,FTELL_FAILED,tape->inputFilename) ;
      (void) unlink (tape->checkpointFilename) ;

      return ;
    }
  
  if ((tout = fopen (tape->checkpointFilename,"w")) == NULL)
    syslog (LOG_ERR,CHECKPOINT_OPEN,tape->checkpointFilename) ;
  else
    {
      fprintf (tout,"%ld\n",tape->tellpos) ;

      if (fclose (tout) != 0)
        syslog (LOG_ERR, FCLOSE_FAILED, tape->checkpointFilename) ;
    }
  
  tape->changed = false ;
}



/* Prepare the tape file(s) for input. For a given Tape there are
 * three possible input files: PEER.input PEER and
 * PEER.output. PEER.input and PEER.output are private to
 * innfeed. PEER is where a sysadmin can drop a file that (s)he
 * wants to inject into the process.  There is a also a checkpojint
 * file that relates to the .input file.
 *
 * prepareFiles will them in a manner much like the following shell
 * commands:
 *
 * if [ ! -f PEER.input ]; then
 * 	if [ -f PEER ]; then mv PEER PEER.input; fi
 * 	elif [ -f PEER.output ]; then mv PEER.output PEER; fi
 * 	rm PEER.checkpoint
 * fi
 *
 * At this point PEER.input is opened for reading if it exists.
 *
 * The checkpoint file is left as-is unless the PEER.input file
 * happens to be newer that the checkpoint file.
 */
static void prepareFiles (Tape tape)
{
  bool inpExists ;
  bool outExists ;
  bool newExists ;
  bool ckptExists ;

    /* flush any in memory articles to disk */
  if (tape->head != NULL)
    {
      tapeOpen (tape) ;
      tapeClose (tape) ;
    }

  inpExists = fileExistsP (tape->inputFilename) ;
  outExists = fileExistsP (tape->outputFilename) ;
  newExists = fileExistsP (tape->handFilename) ;
  ckptExists = fileExistsP (tape->checkpointFilename) ;


    /* we blast the checkpoint file if the input file doesn't exist, or if
       the input file is newer */
  if (ckptExists)
    {
      if (!inpExists || isOlder (tape->checkpointFilename,tape->inputFilename))
        {
          if (unlink (tape->checkpointFilename) != 0)
            syslog (LOG_ERR, UNLINK_FAILED, tape->checkpointFilename) ;
          
          ckptExists = false ;
        }
    }
  
    /* move the hand-dropped file to the input file if needed. */
  if (newExists && !inpExists)
    {
      if (rename (tape->handFilename,tape->inputFilename) != 0)
        syslog (LOG_ERR,RENAME_FAILED,tape->handFilename, tape->inputFilename);
      else
        inpExists = true ;
    }
  
    /* now move the output file to the input file, if needed and only if in
       not in NOROTATE mode. */
  if (outExists && !inpExists && !tape->noRotate)
    {
      if (rename (tape->outputFilename,tape->inputFilename) != 0)
        syslog (LOG_ERR,RENAME_FAILED,
                tape->outputFilename,tape->inputFilename);
      else
        inpExists = true ;
      
      tape->outputFileExists = false ;
    }

    /* get the checkpoint value if possible */
  tape->tellpos = 0 ;
  if (ckptExists)
    {
      long tellVal ;
      int rval ;
      FILE *ckptFile ;

      if ((ckptFile = fopen (tape->checkpointFilename,"r")) == NULL)
        syslog (LOG_ERR,FOPEN_FAILURE,tape->checkpointFilename) ;
      else
        {
          char cbuff [50] ;
          size_t len = 0 ;

          rval = 1 ;

          if (fgets (cbuff,sizeof (cbuff) - 1,ckptFile) == NULL)
            rval = 0 ;
          else
            {
              len = strlen (cbuff) ;
              if (cbuff [len - 1] == '\n')
                cbuff [--len] = '\0' ;
            }

          if (rval && (strspn (cbuff,"0123456789") != len))
            rval = 0 ;
          
          if (rval)
            rval = sscanf (cbuff,"%ld",&tellVal) ;
          
          if (fclose (ckptFile) != 0)
            syslog (LOG_ERR,FCLOSE_FAILED,tape->checkpointFilename) ;

          if (rval == 1)
            tape->tellpos = tellVal ;
          else
            {
              syslog (LOG_ERR,BAD_CHECKPOINT,tape->checkpointFilename) ;
              
              if (unlink (tape->checkpointFilename) != 0)
                syslog (LOG_ERR, UNLINK_FAILED, tape->checkpointFilename) ;
            }
        }
    }
  

    /* now open up the input file and seek to the proper position. */
  if (inpExists)
    {
      long flength ;
      
      if ((tape->inFp = fopen (tape->inputFilename,"r")) == NULL)
        syslog (LOG_ERR,FOPEN_FAILURE,tape->inputFilename) ;

        /* make sure we're not trying to seek past the end. */
      if ((flength = fileLength (fileno (tape->inFp))) < tape->tellpos)
        {
          syslog (LOG_ERR,FILE_SHORT,tape->inputFilename,
                  flength, tape->tellpos);
          tape->tellpos = 0 ;

          if (unlink (tape->checkpointFilename) != 0)
            syslog (LOG_ERR, UNLINK_FAILED, tape->checkpointFilename) ;
        }

      if (fseek (tape->inFp,tape->tellpos,SEEK_SET) != 0)
        syslog (LOG_ERR,FSEEK_FAILED,tape->inputFilename,tape->tellpos);
    }
}



  /* The timer callback function that will checkpoint all the active tapes. */
static void tapeCheckpointCallback (TimeoutId id, void *d)
{
  (void) d ;                    /* keep lint happy */

  ASSERT (id == checkPtId) ;
  ASSERT (tapeCkPtPeriod > 0) ;
  
  dprintf (1,"Checkpointing tapes\n") ;
  
  checkPointTapes () ;

  checkPtId = prepareSleep (tapeCheckpointCallback,tapeCkPtPeriod,NULL) ;
}
