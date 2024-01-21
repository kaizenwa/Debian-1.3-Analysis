
/***************************************************************************\
*                               SWI numbers                                 *
\***************************************************************************/

#define SWI_WriteC                 0x0
#define SWI_Write0                 0x2
#define SWI_ReadC                  0x4
#define SWI_CLI                    0x5
#define SWI_GetEnv                 0x10
#define SWI_Exit                   0x11
#define SWI_EnterOS                0x16

#define SWI_GetErrno               0x60
#define SWI_Clock                  0x61
#define SWI_Time                   0x63
#define SWI_Remove                 0x64
#define SWI_Rename                 0x65
#define SWI_Open                   0x66

#define SWI_Close                  0x68
#define SWI_Write                  0x69
#define SWI_Read                   0x6a
#define SWI_Seek                   0x6b
#define SWI_Flen                   0x6c

#define SWI_IsTTY                  0x6e
#define SWI_TmpNam                 0x6f
#define SWI_InstallHandler         0x70
#define SWI_GenerateError          0x71
