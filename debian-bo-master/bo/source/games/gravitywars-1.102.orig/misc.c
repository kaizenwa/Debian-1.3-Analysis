/* GravityWars 1.1,  (C) Sami Niemi -95 */

/* Wait Any Key */
void waitanykey() {

  while(keyboard_update()) 
    vga_waitretrace();
 
  while(!keyboard_update()) 
    vga_waitretrace();

}

/*------------------------------------------------------------------ doPanic */
/* It also panics if you push a mouse button, in case svgalib crashes.. */

void doPanic() {
  printf("------------------------------\n"
	 "- PANICING!!!!!!! WHOAAAAA!! -\n"
	 "------------------------------\n");
  keyboard_close();
  mouse_close();
  exit(1);
}


