/*

Copyright 1990 by Cray Research, Inc.

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of Cray Research, Inc. not be used in
advertising or publicity pertaining to distribution of the software without
specific, written prior permission.  Cray Research, Inc. makes no
representations about the suitability of this software for any purpose.  It
is provided "as is" without express or implied warranty.

*/

/*
 *      mailhandler.h
 *
 */

#ifndef _MAILHANDLER_H
#define _MAILHANDLER_H
#define MAXMAIL 5000
#define IR_LISTSIZE 250
#define TRUE    1
#define FALSE   0

#define NO_SORT         0
#define SUBJECT_SORT    1
#define FROM_SORT       2
#define SortFrom 1
#define SortCc 2
#define SortSubject 3
#define SortReturnPath 4
#define SortReplyTo 5
#define SortInReplyTo 6
#define SortReferences 7
#define SortKeywords 8
#define SortComments 9
#define SortDate 10
#define SortMessageId 11
#define SortSender 12
#define SortPosition 13
#define SortSize 14
#define SortStatus 15
#define SortFace 16

#define FIRSTWORD(word, str) (!strncmp(word, str, strlen(word)))


#define MMOLD           0x01
#define MMREAD          0x02
#define MMSAVED		0x04
#define MMDELETE        0x08

#define ISMMDEL(m)      ( m->status & MMDELETE )
#define ISMMOLD(m)      ( m->status & MMOLD )
#define ISMMREAD(m)     ( m->status & MMREAD )
#define ISMMSAVED(m)	( m->status & MMSAVED )
#define MMSETREAD(m)    m->status |= MMREAD;
#define MMSETOLD(m)     m->status |= MMOLD;
#define MMSETDEL(m)     m->status |= MMDELETE;
#define MMSETSAVED(m)	m->status |= MMSAVED;
#define MMCLREAD(m)	m->status &= (char)((char)MMREAD ^ (char)0xff);
#define MMCLOLD(m)	m->status &= (char)((char)MMOLD ^ (char)0xff);
#define MMCLDEL(m)	m->status &= (char)((char)MMDELETE ^ (char)0xff);
#define MMCLSAVED(m)	m->status &= (char)((char)MMSAVED ^ (char)0xff);
#define MMUNDEL(m)      m->status &= (char)((char)MMDELETE ^ (char)0xff);

#define ISWHITE(c)      (c == ' ' || c == '\t')

#define VALIDMSG(n)	((n)>=0 && (n)<LetterCount)

#define CEM_ATTACH    1
#define PMAIL_ATTACH  2
#define MIME_ATTACH   3

struct HEADLIST {
        long            hdrIndex,
                        bodyIndex,
                        subjectIndex,
                        fromIndex,
                        returnPathIndex,
                        byteSize;
        int             hdrCount,
                        bodyCount,
			hdr_left,
			hdr_len;
	char		*To,
			*From,
			*From_username,
			*From_UID,
			*From_DAY,
			*From_MONTH,
			*From_DATE,
			*From_TIME,
			*HFrom,
			*Cc,
			*ContentType,
			*Subject,
			*ReturnPath,
			*ReplyTo,
			*InReplyTo,
			*References,
			*Keywords,
			*Comments,
			*Encrypted,
			*Date,
			*MessageId,
			*Sender,
			*ContentTransferEncoding,
			*XMailer,
			*Attach,
			*XFace;
        unsigned char   status,
			modified;
	int		From_me;
	int		ContentLength;
        int		attachment;
	int		display_position;
        int		attachmentType;
        int		attachmentSeen;
	struct HEADLIST *next, *prev;
};

struct lockent {
	int fd;
	char *fname;
};

extern char *MailBoxPath;

#endif
