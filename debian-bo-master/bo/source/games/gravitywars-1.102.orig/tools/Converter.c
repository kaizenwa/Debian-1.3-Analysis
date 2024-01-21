/*
    LevelEditorData -> Level.gfx & Level.obj Converter  (C) Sami Niemi 1995
*/

#include<stdio.h>
 
FILE  *fileptr;

char filename[32];

char buf20x45[20*45];

char buf40x45[40*45];


#define TRUE 1
#define FALSE 0

void loadit() {
  
  char wrdy[4];
  
  if ( ( fileptr=fopen(filename,"r") )!=NULL) {
    
    do {
      wrdy[0]=wrdy[1];	 
      wrdy[1]=wrdy[2];
      wrdy[2]=wrdy[3];
      wrdy[3]=fgetc(fileptr);  
    } while((wrdy[0]!='S') ||
	    (wrdy[1]!='M') ||
	    (wrdy[2]!='A') || 
	    (wrdy[3]!='P'));
    
    fread(buf20x45,20*45,1,fileptr);
    
    fclose(fileptr);
  }
  else {
    printf("ERROR!!!!!!!!! CAN'T LOAD THE LEVEL!\n");
    exit(1);
  }
};


/*
char chk_S(int xx, int yy) {
   
  static int x,y;
  static unsigned char ch;
  static char bool;

  xx--; 
  yy--;

  bool=FALSE;
  for(y=yy; y<=(yy+2); y++) {
    for(x=xx; x<=(xx+2); x++) {
      ch=buf40x45[x+y*40];
      if (BACKGROUND) {
	bool=TRUE;
	goto out;
      }
    }
  }
 out:
  return bool;
}
	
char chk_M(int xx, int yy) {

  static int x,y;
  static unsigned char ch;
  static char bool;

  xx-=2;
  yy-=2;

  bool=FALSE;
  for(y=yy; y<=(4+yy); y++) {
    for(x=xx; x<=(4+xx); x++) {
      ch=buf40x45[x+y*40];
      if (BACKGROUND) {
        bool=TRUE;
        goto out;
      }
    }
  }
 out:
  return bool;
}
*/


void procit() {

  unsigned char ch,obj;

  int x,y;

  for(y=0; y<=44; y++) {
    memcpy(&buf40x45[y*40],&buf20x45[y*20],20);
  }

  for(y=0; y<=44; y++) { 
    for(x=0; x<=19; x++) {
      
      ch=buf40x45[x+y*40];
      
      switch(ch) {

      case 19:      /* Start Field */
        obj='s';
        break;

      case 28:      /* Fuel Field */
        obj='f';
        break;
 

      case 36:      /* Background */
      case 37:
      case 38:
	obj='.';
	break;

      case 41:      /* Exit */
	obj='x';
	break;	

      case 51:      /* Air Key */
      	obj='&';
	break;

      case 149:     /* Water Fuel */
	obj='%';
	break;

      case 209:
	obj='F';  /* Extra Fuel */
	break;
	
      case 166:
      case 175:
      case 184:
	obj='L';  /* Extra Life */
	break;

      case 162:
      case 163:
      case 164:
      case 165:
	obj='1';  /* Bonus 10 pts */
	break;

      case 170:   /* Water Key */
        obj='?';
	break;
	
      case 171:     
      case 172:
      case 173:
	obj='5';
	break;

      case 174:    
	obj='6';
	break;

      case 180:
      case 181:
	obj='2';
	break;

      case 182:
	obj='T';  /* Extra Time */
	break;

      case 183:
      case 189:
      case 190:
	obj='4';
	break;
	
      case 191:
      case 192:
      case 193:
	obj='3';
	break;
	
      case 168:     /* Destroyable Graphics (Ex. The Red Doors) */
      case 176:
      case 177:
      case 178:
      case 186:
	obj='@';
	break;
	
      case 194:     /* Water */
      case 195:
      case 196:
      case 201:
      case 202:
      case 203:
      case 204:
      case 205:
      case 210:
      case 211:
      case 212:
      case 213:
      case 214:
	obj='w';
	break;
	
      case 198:     /* Top Water */
      case 199:
      case 200:
	obj='v';
	break;

      case 179:     /* Water Bonus */  
      case 206:
	obj='(';
	break;

      case 188:
	obj=')';
	break;

      case 197:
	obj='[';
	break;

      case 215:
	obj=']';
	break;

      case 123:     /* The Fan, you have to edit the wind yourself! */
      case 124:
      case 125:     /* Wind: QWE */
      case 132:     /*       A D */
      case 133:     /*       ZXC */
      case 134:
      case 141:
      case 142:
      case 143:
      case 150:
      case 151:
      case 152:
	obj='S';
	break;
 

      default:      /* Default */
	obj='*';
	break;
      };
    
      
      /* Air above the water */
      if ( (y<45) && (((ch=buf40x45[x+y*40+40])>=198) && (ch<=200) ) )
	obj='a';
      
      buf40x45[20+x+y*40]=obj;
    }
  }
}


void saveit() {
  
  int y;

  char name[64];
  char author[64];

  strcpy(filename+strlen(filename),".obj");

  if ( ( fileptr=fopen(filename,"w") )!=NULL) {

    for(y=0; y<=44; y++) {
      fwrite(&buf40x45[20+y*40],20,1,fileptr);
      fprintf(fileptr,"--%d\n",y);
    }
    fprintf(fileptr,
	    "||||||||||||||||||||\n"
	    "00000000001111111111\n"
	    "01234567890123456789\n\n");
   
    fclose(fileptr);
  }
  else {
    printf("ERROR!!!!!!!!! CAN'T SAVE THE LEVEL OBJECT DATA!\n");
    exit(1);
  }

  strcpy(filename+strlen(filename)-4,".gfx");

  if ( ( fileptr=fopen(filename,"w") )!=NULL) {

    for(y=0; y<=44; y++) {
      fwrite(&buf40x45[y*40],20,1,fileptr);
    }
    fclose(fileptr);
  }
  else {
    printf("ERROR!!!!!!!!! CAN'T SAVE THE LEVEL GFX DATA!\n");
    exit(1);
  }

}

main(int number, char *arg[]){

  strcpy(filename,arg[1]);
  printf("\n"
	 "GravityWars Level Converter v0.01, Sami Niemi -95\n\n"
	 "Converting '%s'\n\n",filename,filename);

  loadit();
  procit(); 
  saveit();
}



