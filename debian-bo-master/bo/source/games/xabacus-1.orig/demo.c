#include <stdio.h>
#include <string.h>
#include "main.h"

#define drawText(x,y,str) XDrawString(display,demobase,gc[FRAME],x,y,str,strlen(str));


/* This array represents 4 lines of text that are to be displayed in the
 * demo window; the module drawDemoWindow() will be passed a pointer to this
 * array */
char displaytext[4][64];

char *introtext[]={
	"Place this window below the abacus, then click & leave",
	"the pointer in the abacus window to begin the demo.",
	"During the demo, use the Space-bar to step.",
	"Type `q' to quit the demo.",
};

char *querytext[]={
	"Type:",
	"  `c' to continue to next lesson",
	/*"                               ",*/
	"  `r' to repeat previous lesson",
	"  `q' to quit the demo",
};

int font_height;

drawIntro()
{
int i;

#ifdef DEBUG
	(void)fprintf(stderr," DEBUG drawIntro():\n");
#endif

	font_height = font_info->max_bounds.ascent + font_info->max_bounds.descent;

	for(i=0; i<4; i++){
		drawText(1,font_height+(font_height*i),introtext[i]); 
		XFlush(display);
	}

}/*drawIntro*/

/* drawQuery & drawDemoWindow do look identical. Why, I hear you asking, didn't
 * he just pass a pointer to the text that needs to be displayed ?
 * Good question.
 */
drawQuery()
{
int i;

	XClearWindow(display,demobase);

    for(i=0; i<4; i++){
		drawText(1,font_height+(font_height*i),querytext[i]);
		XFlush(display);
	}

}/* draw_Query */

drawDemoWindow()
{
int i;

	XClearWindow(display,demobase);

	for(i=0; i<4; i++){
        drawText(1,font_height+(font_height*i),displaytext[i]);
		XFlush(display);
	}

}/*drawDemoWindow*/

