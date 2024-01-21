/* -*- c -*-
 *
 * Author:      James Brister <brister@vix.com> -- berkeley-unix --
 * Start Date:  Wed Dec 27 09:55:07 1995
 * Project:     INN (innfeed)
 * File:        tape.h
 * RCSId:       $Id: tape.h,v 1.7 1996/04/13 02:51:33 brister Exp $
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
 * Description: The public interface to the Tape class.
 *
 *              The Tape class simulates a mag tape. It only reads or writes
 *              Articles. A tape is either in an Input or Output state. When
 *              an Article is given to a Tape it will store the Article in
 *              memory until it reaches a highwater mark at which point it
 *              dumps all it's articles to disk.
 *
 *              Input tapes generate article objects on request if the
 *              underlying tape file has info in it. The Tapes take care of
 *              cleaning up used-up files as needed.
 *
 */

#if ! defined ( tape_h__ )
#define tape_h__

#include <stdio.h>

#include "misc.h"


  /* If dontRotate is true, then any articles that get written to the tape
     will never be read back in again. This is for the batch-mode-only case
     where articles written to tape were done so 'cause the remote
     temporarily rejected them. */
Tape newTape (const char *peerName, bool dontRotate) ;

void gPrintTapeInfo (FILE *fp, u_int inedntAmt) ;
void printTapeInfo (Tape tape, FILE *fp, u_int indentAmt) ;

  /* deletes the tape objects. If it has any articles cached then it dumps
     them to the disk. */
void delTape (Tape tape) ;

  /* give an article to the Tape for storage */
void tapeTakeArticle (Tape tape, Article article) ;

  /* get a new article from an Input tape. */
Article getArticle (Tape tape) ;

  /* Opnes the output side of the tape for writing.  Returns false if the
     underlying fopen() failed, Any articles the Tape has in memory will be
     written to it and any new articles it gets will go straight to the
     disk. */
bool tapeOpen (Tape tape) ;

  /* close the Tape's output side. */
void tapeClose (Tape tape) ;

  /* return true if article info on disk or in memory */
bool tapeHasArticles (Tape tape) ;



  /**************************************************/
  /*               CLASS LEVEL FUNCTIONS            */
  /**************************************************/

  /* get all the active input tapes to checkpoint their current positions */
void checkPointTapes (void) ;

  /* set the number of seconds between checkpoints of the tape files. */
void tapeSetCheckpointPeriod (u_int ckPtPeriod) ;

  /* set the directory to store tapes in. */
void setTapeDirectory (const char *newDir) ;

  /* get the name of the directory tapes are being stored in. */
const char *getTapeDirectory (void) ;


#endif /* tape_h__ */
