/**
 * plyBcktAsync - loads a sound file into a bucket and plays it asynchronously
 *
 * usage: plyBcktAsync file
 *
 * $NCDId: @(#)plyBcktAsync.c,v 1.1 1994/06/14 17:59:41 greg Exp $
 */

#include <stdio.h>
#include <audio/audiolib.h>
#include <audio/soundlib.h>

static void
fatalError(message, arg)
char           *message,
               *arg;
{
    fprintf(stderr, message, arg);
    fprintf(stderr, "\n");
    exit(1);
}

static void
doneCB(aud, handler, ev, data)
AuServer       *aud;
AuEvent        *ev;
AuEventHandlerRec *handler;
AuPointer       data;
{
    AuBool         *done = (int *) data;

    *done = AuTrue;
}

/* this function must return periodically to service audio events */
static void
doSomeWork()
{
    printf("doing some work\n");
}

int
main(argc, argv)
int             argc;
char          **argv;
{
    AuServer       *aud;
    AuBucketID      bucket;
    AuDeviceID      device = AuNone;
    AuBool          done = AuFalse;
    char           *file = argv[1];

    if (argc < 2)
	fatalError("usage: plyBcktAsync file");

    if (!(aud = AuOpenServer(NULL, 0, NULL, 0, NULL, NULL)))
	fatalError("Can't open audio server");

    if (!(bucket = AuSoundCreateBucketFromFile(aud, file, AuAccessAllMasks,
					       NULL, NULL)))
	fatalError("Can't create bucket");

    AuSoundPlayFromBucket(aud, bucket, AuNone, AuFixedPointFromSum(1, 0),
		      doneCB, (AuPointer) &done, 1, NULL, NULL, NULL, NULL);

    while (!done)
    {
	AuHandleEvents(aud);
	doSomeWork();
    }

    AuDestroyBucket(aud, bucket, NULL);
    AuCloseServer(aud);
    return 0;
}
