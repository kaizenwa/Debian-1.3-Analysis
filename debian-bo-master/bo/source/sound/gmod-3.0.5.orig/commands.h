// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

/* This file is part of the GMOD package */

#ifndef __commands_h
#define __commands_h

#define CMD_ARPEG		0x00
#define CMD_SLIDEUP		0x01
#define CMD_SLIDEDOWN		0x02
#define CMD_SLIDETO		0x03
#define CMD_VIBRATO		0x04
#define CMD_PORTANDVOL		0x05
#define CMD_VIBRAANDVOL		0x06
#define CMD_TREMOLO		0x07
#define CMD_SETOFFSET		0x09
#define CMD_VOLSLIDE		0x0a
#define CMD_JUMP		0x0b
#define CMD_VOLUME		0x0c
#define CMD_BREAK		0x0d
#define CMD_EXTENDED		0x0e
#define CMD_SPEED		0x0f
#define CMD_NOP			0xfe

/* "extended" commands */
#define CMD_FINEPORTUP		0xe1
#define CMD_FINEPORTDOWN	0xe2
#define CMD_GLISSANDO		0xe3
#define CMD_VIBRA_WAVE		0xe4
#define CMD_FINETUNE            0xe5
#define CMD_PATTERN_LOOP	0xe6
#define CMD_TREMOLO_WAVE	0xe7
#define CMD_SET_PAN		0xe8
#define CMD_RETRIGGER		0xe9
#define CMD_FINEVOLUP		0xea
#define CMD_FINEVOLDOWN		0xeb
#define CMD_CUT_NOTE		0xec
#define CMD_DELAY_NOTE		0xed
#define CMD_DELAY_PAT		0xee

/* more commands */
#define CMD_SET_TICKS           0x10
#define CMD_SET_BPM             0x11
#define CMD_TREMOR		0x12
#define CMD_ARPEG2		0x13
#define CMD_INFO		0x14
#define CMD_RETRIGVOL		0x15
#define CMD_GLOBAL_VOL		0x16
#define CMD_SETOFFSET_1024	0x17
#define CMD_SETOFFSET_FINE	0x18
#define CMD_KEYOFF		0x19
#define CMD_VIBRASPEED          0x20
#define CMD_XFINEPORTUP         0x21
#define CMD_XFINEPORTDOWN       0x22
#define CMD_PANSLIDE            0x23
#define CMD_GLOBALVOL_SLIDE     0x24
#define CMD_SETENV_POS          0x25

#endif
