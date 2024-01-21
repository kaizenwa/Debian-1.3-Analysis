#ifndef __SIO__
#define __SIO__

#include "atari.h"

extern char sio_status[256];

int SIO_Mount (int diskno, char *filename);
void SIO_Dismount (int diskno);
void SIO_DisableDrive (int diskno);
void SIO (void);

void SIO_SEROUT (unsigned char byte, int cmd);
int SIO_SERIN (void);

#endif
