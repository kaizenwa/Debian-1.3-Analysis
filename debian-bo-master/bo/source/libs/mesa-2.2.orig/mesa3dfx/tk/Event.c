#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <conio.h>

#include "gltk.h"

#include "fx/fxmesa.h"

/******************************************************************************/

void (*ExposeFunc)(int, int) = 0;
void (*ReshapeFunc)(int, int) = 0;
void (*DisplayFunc)(void) = 0;
GLenum (*KeyDownFunc)(int, GLenum) = 0;
GLenum (*MouseDownFunc)(int, int, GLenum) = 0;
GLenum (*MouseUpFunc)(int, int, GLenum) = 0;
GLenum (*MouseMoveFunc)(int, int, GLenum) = 0;
void (*IdleFunc)(void) = 0;

static int width,height;
static fxMesaContext fc=NULL;

/******************************************************************************/

void tkExec(void)
{
	static int firsttime=1;
	int key;
	int ch;
	GLenum mask=0;

    if(ReshapeFunc && firsttime) {
    	(*ReshapeFunc)(640,480);
		firsttime=0;
		ch=0;
	}

    while (ch!=27) {
		  if(kbhit()) {
		 	ch = getch();

			if(KeyDownFunc) {
				switch(ch) {
					case ' ':
						key = TK_SPACE;
						break;
					case 27:
						key = TK_ESCAPE;
						break;
					case '1':
						key = TK_1;
						break;
					case '2':
						key = TK_2;
						break;
					case '3':
						key = TK_3;
						break;
					case '4':
						key = TK_4;
						break;
					case '5':
						key = TK_5;
						break;
					case '6':
						key = TK_6;
						break;
					case '7':
						key = TK_7;
						break;
					case '8':
						key = TK_8;
						break;
					case '9':
						key = TK_9;
						break;
					case '0':
						key = TK_0;
						break;
					case 'a':
						key = TK_a;
						break;
					case 'b':
						key = TK_b;
						break;
					case 'c':
						key = TK_c;
						break;
					case 'd':
						key = TK_d;
						break;
					case 'e':
						key = TK_e;
						break;
					case 'f':
						key = TK_f;
						break;
					case 'g':
						key = TK_g;
						break;
					case 'h':
						key = TK_h;
						break;
					case 'i':
						key = TK_i;
						break;
					case 'j':
						key = TK_j;
						break;
					case 'k':
						key = TK_k;
						break;
					case 'l':
						key = TK_l;
						break;
					case 'm':
						key = TK_m;
						break;
					case 'n':
						key = TK_n;
						break;
					case 'o':
						key = TK_o;
						break;
					case 'p':
						key = TK_p;
						break;
					case 'q':
						key = TK_q;
						break;
					case 'r':
						key = TK_r;
						break;
					case 's':
						key = TK_s;
						break;
					case 't':
						key = TK_t;
						break;
					case 'u':
						key = TK_u;
						break;
					case 'v':
						key = TK_v;
						break;
					case 'w':
						key = TK_w;
						break;
					case 'x':
						key = TK_x;
						break;
					case 'y':
						key = TK_y;
						break;
					case 'z':
						key = TK_z;
						break;
					case 'A':
						key = TK_A;
						break;
					case 'B':
						key = TK_B;
						break;
					case 'C':
						key = TK_C;
						break;
					case 'D':
						key = TK_D;
						break;
					case 'E':
						key = TK_E;
						break;
					case 'F':
						key = TK_F;
						break;
					case 'G':
						key = TK_G;
						break;
					case 'H':
						key = TK_H;
						break;
					case 'I':
						key = TK_I;
						break;
					case 'J':
						key = TK_J;
						break;
					case 'K':
						key = TK_K;
						break;
					case 'L':
						key = TK_L;
						break;
					case 'M':
						key = TK_M;
						break;
					case 'N':
						key = TK_N;
						break;
					case 'O':
						key = TK_O;
						break;
					case 'P':
						key = TK_P;
						break;
					case 'Q':
						key = TK_Q;
						break;
					case 'R':
						key = TK_R;
						break;
					case 'S':
						key = TK_S;
						break;
					case 'T':
						key = TK_T;
						break;
					case 'U':
						key = TK_U;
						break;
					case 'V':
						key = TK_V;
						break;
					case 'W':
						key = TK_W;
						break;
					case 'X':
						key = TK_X;
						break;
					case 'Y':
						key = TK_Y;
						break;
					case 'Z':
						key = TK_Z;
						break;
					case 0:
					case 0xe0:
						ch = getch();
						switch( ch ) {
							case 'P':
								key = TK_UP;
								break;
							case 'M':
								key = TK_LEFT;
								break;
							case 'K':
								key = TK_RIGHT;
								break;
							case 'H':
								key = TK_DOWN;
								break;
						}
						break;
					/*default:
						printf("\n====%x %c %d\n",ch,ch,ch);
						break;*/
					}
					(*KeyDownFunc)(key,mask);
					key = 0;
				}
			}

			if (IdleFunc)
				(*IdleFunc)();

			if(DisplayFunc)
				(*DisplayFunc)();
        }

}



/******************************************************************************/

void tkExposeFunc(void (*Func)(int, int))
{

    ExposeFunc = Func;
}

/******************************************************************************/

void tkReshapeFunc(void (*Func)(int, int))
{

    ReshapeFunc = Func;
}

/******************************************************************************/

void tkDisplayFunc(void (*Func)(void))
{

    DisplayFunc = Func;
}

/******************************************************************************/

void tkKeyDownFunc(GLenum (*Func)(int, GLenum))
{

    KeyDownFunc = Func;
}

/******************************************************************************/

void tkMouseDownFunc(GLenum (*Func)(int, int, GLenum))
{

    MouseDownFunc = Func;
}

/******************************************************************************/

void tkMouseUpFunc(GLenum (*Func)(int, int, GLenum))
{

    MouseUpFunc = Func;
}

/******************************************************************************/

void tkMouseMoveFunc(GLenum (*Func)(int, int, GLenum))
{

    MouseMoveFunc = Func;
}

/******************************************************************************/

void tkIdleFunc(void (*Func)(void))
{

    IdleFunc = Func;
}

/******************************************************************************/

void tkInitDisplayMode(GLenum type)
{
}

void tkInitPosition(int x, int y, int w, int h)
{
	width=w;
	height=h;
}

void tkSwapBuffers(void)
{
	fxMesaSwapBuffers();
}

void tkQuit(void)
{
	fxMesaDestroyContext(fc);
	exit(0);
}

GLenum tkInitWindow(char *title)
{
#define NUM_RES 3

	static GrScreenResolution_t res[NUM_RES]={GR_RESOLUTION_512x384,GR_RESOLUTION_640x480,GR_RESOLUTION_800x600};
	static int xres[NUM_RES]={512,640,800};
	static int yres[NUM_RES]={384,480,600};
	int i;

	for(i=0;i<NUM_RES;i++)
		if((width==xres[i]) && (height==yres[i])) {
			fc=fxMesaCreateContext(res[i],GR_REFRESH_75Hz);
			fxMesaMakeCurrent(fc);

			return GL_TRUE;
		}

	return GL_FALSE;				
}

void tkSetOneColor(int index, float r, float g, float b)
{
}

void tkSetGreyRamp(void)
{
}

void tkSetRGBMap( int Size, float *Values )
{
}

GLint tkGetColorMapSize(void)
{
	return 256;
}
