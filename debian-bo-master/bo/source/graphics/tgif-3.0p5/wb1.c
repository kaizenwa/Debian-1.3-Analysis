/*
 * Author:      Renato Santana, <renato@nce.ufrj.br> in January, 1996.
 *
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/wb1.c,v 3.0 1996/05/06 16:12:54 william Exp $
 */

#ifdef _TGIF_WB

#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netdb.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "const.h"
#include "types.h"

#include "choice.e"
#include "cmd.e"
#include "color.e"
#include "drawing.e"
#include "file.e"
#include "msg.e"
#include "obj.e"
#include "page.e"
#include "select.e"
#include "setup.e"
#include "tcp.e"
#ifndef _NO_EXTERN
#include "wb1.e"
#endif
#include "wb2.e"
#include "wb3.e"

#define CUR_VERSION 31

struct ObjRec *obj_ptr_aux=NULL;

int Cmd_Flag_Arrived=(-1);
int sock=(-1);
int ClientServer=0;
int NumAck=(-1);
int NumArrObj=(-1);

/* prototypes definitions */
void WhiteBoard ARGS_DECL((void));
void CheckConForRecord ARGS_DECL((int,struct SubCmdRec*,struct SelRec*,int,struct SelRec*));
int  CheckConForUndo  ARGS_DECL((struct CmdRec*));
void CheckConForRedo  ARGS_DECL((void));
void SendCmdMov       ARGS_DECL((void));
int CheckClientServer ARGS_DECL((void));
int CountObj          ARGS_DECL((struct SelRec*));
void ClearClient ARGS_DECL((void));
void CloseCon ARGS_DECL((void));
void CmdReceived ARGS_DECL((char*));
void CmdNewRec  ARGS_DECL((int,char*));
void CmdDelRec  ARGS_DECL((char*));
void CmdMovRec  ARGS_DECL((int,int,int,char*));
void CmdRepRec  ARGS_DECL((int,char*));
void CmdMTORec  ARGS_DECL((int,char*));
void CmdOTMRec  ARGS_DECL((int,char*));
void CmdMovRec  ARGS_DECL((int,int,int,char*));
int CmdRepAObj  ARGS_DECL((struct ObjRec*,struct ObjRec*));
void CmdStrRec  ARGS_DECL((int,char*));
void CmdUndoRec ARGS_DECL((void));
void CmdRedoRec ARGS_DECL((void));
void StretchAllSelObjects ARGS_DECL((int));
int FindObjRec ARGS_DECL((struct ObjRec*,struct ObjRec**));
int ReadObjFromBuff ARGS_DECL((char*,struct ObjRec**));
void ReadObjAttrsFromBuff ARGS_DECL((int,char*,struct ObjRec**));
void AddObjFromBuffer ARGS_DECL((struct ObjRec*,struct ObjRec*,struct ObjRec*));
void RecordObjToBuffer ARGS_DECL((struct ObjRec*,char*));
void SendCmd ARGS_DECL((int,struct SubCmdRec*,struct SelRec*,int,struct SelRec*));
void WaitForAck ARGS_DECL((int,struct SubCmdRec*,struct SelRec*,struct SelRec*,int));
void SendAlreadyDone ARGS_DECL((void));
void ErrorOnConsist ARGS_DECL((void));
void NackFromClient ARGS_DECL((void));

/* prototypes defined */
 
void WhiteBoard()
{
   int sockstr , length=0, port, rc;
   char host_port[50], b[100], host[50], *pport;
   struct sockaddr_in server;
   struct hostent *hp, *gethostbyname();
   u_long aux_addr;

   if (ClientServer != 0)      /* there is a already a connection */
   {
      sprintf(b, "There is a connection already opened (socket #%d).\nDo you want to close it and start a new one? [ync]",sock );
      switch (MsgBox(b, TOOL_NAME, YNC_MB))
      {
             case MB_ID_YES: strcpy(b, "24####");
                             write(sock, b, sizeof(b));
                             close(sock);
                             ClientServer=0;
                             sprintf(b, "The connection has been closed!");
                             sprintf(b, "%s \nSave your work and start a NEW connection",b);
                             MsgBox(b, TOOL_NAME, INFO_MB);
       }
       return;
   }

   host_port[0]='\0';
   Dialog ("Please, enter host: (host:port)","(  <ESC>: Cancel )",host_port);

   if ( (host_port[0] == '\0') && (ClientServer != 0)) {
      return;
   }
   XSync(mainDisplay, False);
		
   pport = ParseStr(host_port, (int)':', host, sizeof(host_port));
   port = atoi(pport);

   if (port == 0) {
      MsgBox("Invalid port number", TOOL_NAME, INFO_MB);
      return;
   }

   if (sock >= 0) close(sock);
   sock = 0;
   ClientServer = 0;    /* 0 -> no connection  1-> Client  2-> Server  */

   rc = TcpDoConnect(host, port, &sock);

   if (rc == TG_REMOTE_STATUS_OK) {
      Msg("Host connected");
      ClientServer = 1;    /*  I'm client  */
      NewProc();   /*clearing all canvas.Client will receive it from server*/
      Msg (" I'M CLIENT ");
      fileVersion = CUR_VERSION;
      /*SendAlreadyDone();   this is only for server*/
      return;
   } else {
      Msg("Error connecting host");
   }

#ifdef NOT_DEFINED
   sock = socket(AF_INET, SOCK_STREAM, 0);
   if (sock < 0) {
      MsgBox("Error opening the Socket.", TOOL_NAME, INFO_MB);
      return;
   }
   server.sin_family = AF_INET;
   if (*host >= '0' && *host <= '9') {
      aux_addr = inet_addr(host);
      hp = gethostbyaddr((char *)&aux_addr,4,AF_INET);
   } else {
      hp = gethostbyname(host);
   }
   if (hp == 0) {
      Msg ("Host Unknown");
      close(sock);
      return;
   }
   memcpy( (char *)hp->h_addr, (char *)&server.sin_addr, hp->h_length);
   server.sin_port=htons(port);

   if (connect(sock, (struct sockaddr *)&server, sizeof(server)) < 0) {
      Msg("Error connecting host");
   } else {
      Msg("Host connected");
      ClientServer = 1;    /*  I'm client  */
      NewProc();   /*clearing all canvas.Client will receive it from server*/
      Msg (" I'M CLIENT ");
      fileVersion = CUR_VERSION;
      /*SendAlreadyDone();   this is only for server*/
      return;
   }
#endif /* NOT_DEFINED */

   Msg ("Host not connected ");
   close(sock);
   sockstr = socket(AF_INET, SOCK_STREAM, 0);
   if (sockstr < 0)
   {
      Dialog ("Error opening the Socket.","(  <ESC>: Cancel )",b);
      return;
   }
 
   server.sin_family = AF_INET;
   server.sin_port=htons(port);
   server.sin_addr.s_addr = INADDR_ANY;

   if (bind(sockstr, (struct sockaddr *)&server, sizeof(server)) < 0)
   {
      Dialog (" Bind Error ","(  <ESC>: Cancel )",b);
      close(sockstr);
      return;
   }

   if (getsockname(sockstr, (struct sockaddr *)&server,&length)< 0)
   {
      Dialog ("Error on getsockname","(  <ESC>: Cancel )",b);
      close(sockstr);
      return;
   }

   sprintf(b, "Socket port #%d ready,\nokay to go to the listening mode? [ync](y)",ntohs(server.sin_port) );
   switch (MsgBox(b, TOOL_NAME, YNC_MB)) 
   {
          case MB_ID_YES: Msg("Waiting for connection"); break;
          case MB_ID_NO: return;
   	  case MB_ID_CANCEL: return;
   }
   XSync(mainDisplay, False);

   listen(sockstr, 1);
   sock = accept(sockstr, (struct sockaddr *)0, (int *)0);	
   if (sock == -1)
   {
      Dialog ("Error on accepting socket","(  <ESC>: Cancel )",b);
      close(sock);
      return;
   }

   ClientServer = 2;   /* I'm server */
   Msg ("connection OK: I'M SERVER ");
   fileVersion = CUR_VERSION;
   SendAlreadyDone();
}


void CheckConForRecord(CmdType, SubCmdPtr, TopSel, NumObjs, TopSelBefore)
   int            CmdType, NumObjs;
   struct SelRec        * TopSel, *TopSelBefore;
   struct SubCmdRec     * SubCmdPtr;
{
   char b[80];
 
   if (ClientServer == 2)    /*  connection is on and I'm server*/
   {
      if (Cmd_Flag_Arrived > 0)  /*   command came from client   */
      {
         NumAck++;
         if (NumAck == NumArrObj)
         {
            NumAck = 0;
            strcpy(b, "20####");
            if (write(sock, b, sizeof(b)) < 0) /* sending ACK */
            {
               sprintf(b, "There is a problem writing to the socket");
               sprintf(b, "%s #%d.\nStart again the connection!", b, sock);
               MsgBox(b, TOOL_NAME, INFO_MB);
               return;
            }
            Cmd_Flag_Arrived = -1;  /* should send command to other clients*/
         }
      }
      else
      {
	 SendCmd (CmdType, SubCmdPtr, TopSel, NumObjs, TopSelBefore);
         Cmd_Flag_Arrived = -1; 
      }
   }
   else if (ClientServer == 1)    /*  connection is on and I'm Client*/
   {
      if (Cmd_Flag_Arrived > 0)  /*   command came from server */
      {
          Cmd_Flag_Arrived = -1;  /* should not send it back*/
      }
      else
      {
          SendCmd (CmdType, SubCmdPtr, TopSel, NumObjs, TopSelBefore);
          Cmd_Flag_Arrived = -1;  /*  should not send it back*/
      }
   }
 
}


int CheckConForUndo(cmd_ptr)
struct    CmdRec        * cmd_ptr;
{
   char      b[80];
   int       NumObjs, status;
   struct    SubCmdRec     * SubCmdAux = NULL;
   struct    CmdRec        * cmd_aux;

   status = TRUE;
   strcpy(b, "Undo not Implemented yet!");
 
   if (ClientServer == 1)          /* I'm client  */
   {

      if ( (cmd_ptr->type  == CMD_STRETCH) || 
           (cmd_ptr->type  == CMD_ONE_TO_MANY) || 
           (cmd_ptr->type  == CMD_MANY_TO_ONE) || 
           (cmd_ptr->type  == CMD_GOTO_PAGE) )
         {
            Msg (b); 
            return(FALSE);
         }

      strcpy(b, "30####");
 
      if (write(sock, b, sizeof(b)) < 0)   /*  sending UNDO to the server */
      {
         sprintf(b, "There is a problem writing to the socket #%d.",sock);
         sprintf(b, "%s\nStart again the connection!",b);
         MsgBox(b, TOOL_NAME, INFO_MB);
         status = FALSE;
      }
   }
   else  /* I'm server and should send (to the client) command to be undone */
   {
      if (cmd_ptr == NULL) return(FALSE);
      switch (cmd_ptr->type)
      {
         case CMD_COMPOSITE: 
              for (cmd_aux=cmd_ptr->last; cmd_aux!=NULL; cmd_aux=cmd_aux->prev)
                  CheckConForUndo(cmd_aux);
              break;
         case CMD_NEW: SendCmd(CMD_DELETE, NULL, NULL,0, 
                               cmd_ptr->top_after); break;
         case CMD_DELETE: SendCmd(CMD_NEW, NULL, cmd_ptr->top_before,
                                  CountObj(cmd_ptr->top_before),NULL); break;
         case CMD_MOVE: SendCmdMov(); break;
         case CMD_STRETCH: Msg (b); status = FALSE; break;
         case CMD_REPLACE: SendCmd(CMD_REPLACE, NULL, cmd_ptr->top_before,
                           CountObj(cmd_ptr->top_before), topSel); break;
/* this above only works when the server starts the undo, if the
   server has another object selected and client undo's changes to another
   object, it DOES NOT work !!  
   Why it doesn't work as below */
/*       case CMD_REPLACE: SendCmd(CMD_REPLACE, NULL, cmd_ptr->top_before, 
                  CountObj(cmd_ptr->top_before), cmd_ptr->top_after); break;*/ 
         case CMD_ONE_TO_MANY: Msg (b); status = FALSE; break;
         case CMD_MANY_TO_ONE: Msg (b); status = FALSE; break;
         case CMD_GOTO_PAGE: Msg (b); status = FALSE; break; 
      }
   }
   return(status);

}

void CheckConForRedo()
{
   char      b[80];
   int       NumObjs;
 
   if (ClientServer == 1)
   {
      strcpy(b, "30####");
 
      if (write(sock, b, sizeof(b)) < 0)   /*  sending UNDO to the server */
      {
         sprintf(b, "3There is a problem writing to the socket #%d.",sock);
         sprintf(b, "%s\nStart again the connection!",b);
         MsgBox(b, TOOL_NAME, INFO_MB);
         return;
      }
   }
   else   /* I'm server and should send command to be undone to the client */
   {
      if (curCmd == NULL) return;
      switch (curCmd->type)
      {
         case CMD_COMPOSITE: break;
         case CMD_NEW:    SendCmd(CMD_DELETE, NULL, NULL,0,
                                  curCmd->top_after); break;
         case CMD_DELETE: SendCmd(CMD_NEW, NULL, curCmd->top_before,
                                  CountObj(curCmd->top_before),NULL); break;
         case CMD_MOVE: SendCmdMov(); break;
/*
         case CMD_STRETCH: UndoOrRedoReplaceCmd (CmdPtr, TRUE); break;
         case CMD_REPLACE: UndoOrRedoReplaceCmd (CmdPtr, HighLight); break;
         case CMD_ONE_TO_MANY: UndoOrRedoOneToManyCmd (CmdPtr); break;
         case CMD_MANY_TO_ONE: UndoOrRedoOneToManyCmd (CmdPtr); break;
         case CMD_GOTO_PAGE: UndoOrRedoGotoPageCmd (CmdPtr); break;
*/
      }
   }
 
}

void SendCmdMov()
{

   struct MoveSubCmdRec         * move_cmd;
   struct SubCmdRec             * sub_cmd;


   move_cmd = (struct MoveSubCmdRec *) malloc (sizeof(struct MoveSubCmdRec));
   if (move_cmd == NULL) FailAllocMessage();
   memset(move_cmd, 0, sizeof(struct MoveSubCmdRec));
   sub_cmd = (struct SubCmdRec *) malloc (sizeof(struct SubCmdRec));
   if (sub_cmd == NULL) FailAllocMessage();
   memset(sub_cmd, 0, sizeof(struct SubCmdRec));
   sub_cmd->detail.mv = move_cmd;

   sub_cmd->detail.mv->dx = -(curCmd->dx);
   sub_cmd->detail.mv->dy = -(curCmd->dy);
   SendCmd(CMD_MOVE, sub_cmd, topSel, CountObj(topSel) , NULL);

}


int CountObj(SelPtr)
struct SelRec        * SelPtr;
{
   int i;

   for (i=0 ; SelPtr != NULL ; SelPtr = SelPtr->next)
       i++;
   return(i);

}


int CheckClientServer()
{
   char  buff[20000]; 

   if (ClientServer == 0)  /*   no connection  */ 
   {
      return(FALSE);
   }

   fcntl(sock,F_SETFL,O_NONBLOCK);
	
   memset(buff, 0, sizeof(buff));
   Cmd_Flag_Arrived = -1;
   if (read(sock, buff, sizeof(buff) ) > 0)
   {
      if (strlen(buff) > 0)
      {
         CmdReceived(buff);
         return(TRUE);
      }
      else
      {
         return(FALSE);
      }
   }
   return(FALSE);
}


void CmdReceived(buff)
   char                 * buff;
{
   int    CmdTypeRec,num_obj,dx,dy;
   char   aux[10000], aux2[10000];
   register struct ObjRec       * obj_ptr, * obj_ptr_aux, * obj_ptr_aux2;
   register struct SelRec       * topSelRec;
   register struct SelRec       * botSelRec;

   buff = ParseStr (buff,(int) '#', aux, strlen(buff));
   CmdTypeRec = atoi (aux); 
   Cmd_Flag_Arrived = CmdTypeRec;
   buff = ParseStr (buff,(int) '#', aux, strlen(buff));
   num_obj = atoi (aux);

   NumAck = 0;
   NumArrObj = num_obj;
   if (CmdTypeRec == CMD_MOVE)
   {
      buff = ParseStr (buff,(int) '#', aux, strlen(buff));
      dx = atoi (aux);
      buff = ParseStr (buff,(int) '#', aux, strlen(buff));
      dy = atoi (aux);
   }
   else
   {
      buff = buff + 2;
   }
	
   switch (CmdTypeRec)
   {
      case CMD_COMPOSITE:     break;
      case CMD_NEW:           CmdNewRec(num_obj, buff); break;
      case CMD_DELETE:        CmdDelRec(buff); break;
      case CMD_MOVE:          CmdMovRec(num_obj,dx,dy,buff);  break;
      case CMD_STRETCH:       CmdStrRec(num_obj, buff); break;
      case CMD_ONE_TO_MANY:   CmdOTMRec(num_obj, buff); break;
      case CMD_MANY_TO_ONE:   CmdMTORec(num_obj, buff); break;
      case CMD_REPLACE:       CmdRepRec(num_obj, buff); break; 
      case CMD_GOTO_PAGE:     CmdRepRec(num_obj, buff); break;
      case 20:                break;                   /* ACK  Server->client*/
      case 21:                ErrorOnConsist(); break; /* NACK Server->Client*/
      case 22:                NackFromClient(); break; /* NACK Client->Server*/
      case 23:                ClearClient(); break;    /* Server -> Client   */
      case 24:                CloseCon();              /* both close_con requ.*/
      case 30:                CmdUndoRec(); break;     /* UNDO Client->Server*/
      /*case 31:              RedoCmd(); break;        REDO Client->Server*/
   }
}


void CmdNewRec(num_obj, buff)
   int                     num_obj;
   char                  * buff;
{
   struct ObjRec       * obj_ptr;
   char                           aux[20000];
   int                            i;

   for (i=1 ; i <= num_obj ; i++)
   {
       Cmd_Flag_Arrived = 1;
       buff = ParseStr (buff,(int) '#', aux, strlen(buff));
       ReadObjFromBuff(aux, &obj_ptr);
       numRedrawBBox = 0;
       obj_ptr->tmp_parent = NULL;
       DrawObj (drawWindow, obj_ptr);
       AddObj (NULL, topObj, obj_ptr);
       RecordNewObjCmd ();
       SetFileModified (TRUE);
   }
}


void CmdDelRec(buff)   /*  delete arrived objects */
   char                  * buff;
{
        struct ObjRec       * obj_ptr;
        struct ObjRec       * obj_ptr_aux;
        struct SelRec       * topSelBefRec;
	char                           aux[10000];

        topSelBefRec = (struct SelRec *) malloc (sizeof(struct SelRec));
        if (topSelBefRec == NULL) FailAllocMessage();
        memset(topSelBefRec, 0, sizeof(struct SelRec));

	buff += 1;
	for (; (strlen(buff)) > 2 ;)
	{
           buff = ParseStr (buff,(int) '#', aux, strlen(buff));
           ReadObjFromBuff(aux, &obj_ptr);
           /*FindObjRec(obj_ptr,&obj_ptr_aux);*/
 	   if((FindObjRec(obj_ptr,&obj_ptr_aux)) == FALSE) return;
	   topSelBefRec->next = NULL; 
           topSelBefRec->obj = obj_ptr_aux;
	   topSelBefRec->prev = NULL;
           PrepareToRecord (CMD_DELETE, topSelBefRec, topSelBefRec, 1);
           DelObj(obj_ptr_aux);
	   Cmd_Flag_Arrived = 2;
           RecordCmd (CMD_DELETE, NULL, NULL, NULL, 0);
           RedrawAnArea (botObj, obj_ptr->obbox.ltx-GRID_ABS_SIZE(1),
                                 obj_ptr->obbox.lty-GRID_ABS_SIZE(1), 
                                 obj_ptr->obbox.rbx+GRID_ABS_SIZE(1),
                                 obj_ptr->obbox.rby+GRID_ABS_SIZE(1));
	}

}

void CmdMovRec(num_obj,dx,dy,buff)
   int                     num_obj, dx, dy;
   char                  * buff;
{

   int                            i;
   char                           aux[10000];
   struct ObjRec       * obj_ptr;
   struct ObjRec       * obj_ptr_aux;
   struct SelRec       * topSelRec;
   struct SelRec       * botSelRec;
   struct MoveSubCmdRec * move_cmd;
   struct SubCmdRec     * sub_cmd;

   move_cmd=(struct MoveSubCmdRec *)malloc(sizeof(struct MoveSubCmdRec));
   if (move_cmd == NULL) FailAllocMessage();
   sub_cmd = (struct SubCmdRec *)malloc(sizeof(struct SubCmdRec));
   if (sub_cmd == NULL) FailAllocMessage();
   sub_cmd->detail.mv = move_cmd;
   move_cmd->dx = dx;
   move_cmd->dy = dy;


   for (i=1 ; i <= num_obj; i++)
   {
      buff = ParseStr (buff,(int) '#', aux, strlen(buff));
      ReadObjFromBuff(aux, &obj_ptr);
      if (obj_ptr != NULL)
      {
         obj_ptr->obbox.ltx -= dx; obj_ptr->obbox.lty -= dy;
         obj_ptr->obbox.rbx -= dx; obj_ptr->obbox.rby -= dy;
         obj_ptr->x -= dx; obj_ptr->y -= dy;
      }
      /*FindObjRec(obj_ptr,&obj_ptr_aux); */
      if( (FindObjRec(obj_ptr,&obj_ptr_aux)) == FALSE) 
      {
         return;
      }
      topSelRec = (struct SelRec *) malloc (sizeof(struct SelRec));
      if (topSelRec == NULL) FailAllocMessage();
      memset(topSelRec, 0, sizeof(struct SelRec));
      topSelRec->obj = obj_ptr_aux;
      topSelRec->next = NULL;
      topSelRec->prev = NULL;
      botSelRec = topSelRec;

      MoveObj(obj_ptr_aux, dx, dy);
      PrepareToRecord (CMD_MOVE, NULL, NULL, 0);
      Cmd_Flag_Arrived = 3;
      RecordCmd (CMD_MOVE, sub_cmd, topSelRec, botSelRec, 1); /* numobj ta' 1 */
      RedrawAreas (botObj, obj_ptr->obbox.ltx-GRID_ABS_SIZE(3), 
                           obj_ptr->obbox.lty-GRID_ABS_SIZE(3), 
			   obj_ptr->obbox.rbx+GRID_ABS_SIZE(3),
                           obj_ptr->obbox.rby+GRID_ABS_SIZE(3),
                           obj_ptr->obbox.ltx+dx-GRID_ABS_SIZE(3),
                           obj_ptr->obbox.lty+dy-GRID_ABS_SIZE(3),
                           obj_ptr->obbox.rbx+dx+GRID_ABS_SIZE(3), 
                           obj_ptr->obbox.rby+dy+GRID_ABS_SIZE(3));
   }
}

void CmdRepRec (num_obj, buff) 
   int                     num_obj;
   char                  * buff;
{
   struct ObjRec       * obj_ptr_from;
   struct ObjRec       * obj_ptr_to;
   char 		          buff_to[10000], buff_from[10000];
   char                         * s, aux[10000];
   int                            i;


   buff_to[0] = '\0'; buff_from[0] = '\0';
   s = buff;
   for (i=1; i <= num_obj ; i++)
   {
      s = FindChar ((int)'#', s);
   }
   strcpy(aux, s);
   s = aux;

   for (i=1; i <= num_obj ; i++)
   {

      buff = ParseStr (buff,(int) '#', buff_to, strlen(buff));
      ReadObjFromBuff(buff_to, &obj_ptr_to);
      s = ParseStr (s,(int) '#', buff_from, strlen(s));
      ReadObjFromBuff(buff_from, &obj_ptr_from);


      if( (CmdRepAObj (obj_ptr_from, obj_ptr_to)) == FALSE) return;

   }
}

int CmdRepAObj (obj_ptr_from, obj_ptr_to)
   struct ObjRec       * obj_ptr_from;
   struct ObjRec       * obj_ptr_to;
{
   struct ObjRec       * obj_ptr_aux;
   struct SelRec                * sel_ptr_rec, * sel_ptr_bef;

   if( (FindObjRec(obj_ptr_from,&obj_ptr_aux)) == FALSE) return(FALSE);

   StartCompositeCmd ();
   sel_ptr_bef = (struct SelRec *) malloc (sizeof(struct SelRec));
   if (sel_ptr_bef == NULL) FailAllocMessage();
   memset(sel_ptr_bef, 0, sizeof(struct SelRec));
   sel_ptr_bef->next = sel_ptr_bef->prev = NULL;
   sel_ptr_bef->obj = obj_ptr_aux;
   PrepareToRecord (CMD_REPLACE, sel_ptr_bef, sel_ptr_bef, 1);
   free(sel_ptr_bef);
   obj_ptr_from->next = obj_ptr_aux->next;
   obj_ptr_from->prev = obj_ptr_aux->prev;
   CopyObj(obj_ptr_to, obj_ptr_aux);   /* from this -> to this*/

   sel_ptr_rec = (struct SelRec *) malloc (sizeof(struct SelRec));
   if (sel_ptr_rec == NULL) FailAllocMessage();
   memset(sel_ptr_rec, 0, sizeof(struct SelRec));
   sel_ptr_rec->next = sel_ptr_rec->prev = NULL;

   obj_ptr_aux->next = obj_ptr_from->next;
   obj_ptr_aux->prev = obj_ptr_from->prev;

   sel_ptr_rec->obj = obj_ptr_aux;

   Cmd_Flag_Arrived = 4;
   RecordCmd (CMD_REPLACE, NULL, sel_ptr_rec, sel_ptr_rec, 1);
   free(sel_ptr_rec);
   EndCompositeCmd ();

   if (obj_ptr_aux->type == 0)   /* this still to be solved! */
       MoveObj(obj_ptr_aux, 0, 0);
   RedrawAreas(botObj, obj_ptr_from->obbox.ltx-GRID_ABS_SIZE(1),
                       obj_ptr_from->obbox.lty-GRID_ABS_SIZE(1),
                       obj_ptr_from->obbox.rbx+GRID_ABS_SIZE(1),
                       obj_ptr_from->obbox.rby+GRID_ABS_SIZE(1),
                       obj_ptr_to->obbox.ltx-GRID_ABS_SIZE(1),
                       obj_ptr_to->obbox.lty-GRID_ABS_SIZE(1),
                       obj_ptr_to->obbox.rbx+GRID_ABS_SIZE(1),
                       obj_ptr_to->obbox.rby+GRID_ABS_SIZE(1));

}

void CmdStrRec (num_obj, buff)
   int                     num_obj;
   char                  * buff;
{
   register struct ObjRec       * obj_ptr_aux;
   register struct ObjRec       * obj_ptr_from;
   register struct ObjRec       * obj_ptr_to;
   register struct SelRec       * topSelRec;
   register struct SelRec       * botSelRec;
   struct AttrRec       	* attr_ptr;

   int   i;
   char  aux1[10000], aux2[10000];


   for (i=1 ; i <= num_obj ; i++)
   {
	buff = ParseStr (buff,(int) '#', aux1, strlen(buff));
	buff = ParseStr (buff,(int) '#', aux2, strlen(buff));
	strcat (aux1, "#");
	strcat (aux2, "#");
	strcat (aux1, aux2);
	CmdRepRec (num_obj,aux1);  /* this is not wright !! */
   }

}

/********  objects should be streched and not replaced...
           but, for now, it's working until I finish it...
 
   ReadObjFromBuff(buff_to, &obj_ptr_to);
   ReadObjFromBuff(buff_from, &obj_ptr_from);
   FindObjRec(obj_ptr_from,&obj_ptr_aux);
   saved_ltx = obj_ptr_aux->obbox.ltx; saved_lty = obj_ptr_aux->obbox.lty;
   saved_rbx = obj_ptr_aux->obbox.rbx; saved_rby = obj_ptr_aux->obbox.rby;
   topSelRec = (struct SelRec *) malloc (sizeof(struct SelRec));
   if (topSelRec == NULL) FailAllocMessage();
   memset(topSelRec, 0, sizeof(struct SelRec));
   topSelRec->obj = obj_ptr_aux;
   topSelRec->next = NULL;
   topSelRec->prev = NULL;
   botSelRec = topSelRec;
   PrepareToRecord (CMD_STRETCH, topSelRec, botSelRec,1);*/ /*numObjSelected);*/
/*   attr_ptr=obj_ptr_aux->fattr;
*/


/*   int  ltx, lty, rbx, rby, saved_ltx, saved_lty, saved_rbx, saved_rby;
   int  poly_stretched;
 
   saved_ltx = selLtX; saved_lty = selLtY;
   saved_rbx = selRbX; saved_rby = selRbY;


      PrepareToRecord (CMD_STRETCH, topSel, botSel, numObjSelected);
      StretchAllSelObjects (Corner);

void StretchAllSelObjects (Corner)
   int  Corner;
{
   struct SelRec        * sel_ptr;
 
   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
      if (!sel_ptr->obj->locked)
         StretchObj (sel_ptr->obj, Corner, FALSE);

{
   struct AttrRec       * attr_ptr=ObjPtr->fattr;
 
   if (AutoCenterAttr)
   {
      for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
         if (attr_ptr->shown)
            CenterObjInOBBox (attr_ptr->obj, ObjPtr->obbox, NULL);
         else
            StretchObj (attr_ptr->obj, Corner, InsideIcon);
   }
   else
      for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
         StretchObj (attr_ptr->obj, Corner, InsideIcon);
}




   if (numObjLocked != 0) Msg ("Locked objects are not stretched.");
}

      RecordCmd (CMD_STRETCH, NULL, topSel, botSel, numObjSelected);
      UpdSelBBox ();
      RedrawAreas (botObj, saved_ltx-GRID_ABS_SIZE(1),
            saved_lty-GRID_ABS_SIZE(1),
            saved_rbx+GRID_ABS_SIZE(1), saved_rby+GRID_ABS_SIZE(1),
            selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));

    here is the end of strech command - not implemented yet    ***********/


void CmdMTORec(num_obj, buff)
int                     num_obj;
char                  * buff;
{
   int                 i,numObjSelectedSaved;
   char                aux[10000];
   struct ObjRec       * obj_ptr;
   struct SelRec       * topSelAux, * topSelSaved;

   Cmd_Flag_Arrived = 6;
   topSelSaved = (struct SelRec *) malloc (sizeof(struct SelRec));
   if (topSelSaved == NULL) FailAllocMessage();
   memset(topSelSaved, 0, sizeof(struct SelRec));
   topSelSaved = topSel; /* saving topSel */
   topSel = NULL;
   numObjSelectedSaved = numObjSelected;
   numObjSelected = num_obj;

   /* *buff++; */
   buff = ParseStr (buff,(int) '#', aux, strlen(buff));

   for (i=0 ; i < num_obj ; i++)
   {
      buff = ParseStr (buff,(int) '#', aux, strlen(buff));
      ReadObjFromBuff(aux, &obj_ptr);
      if((FindObjRec(obj_ptr,&obj_ptr_aux)) == FALSE) return;
      topSelAux = (struct SelRec *) malloc (sizeof(struct SelRec));
      if (topSelAux  == NULL) FailAllocMessage();
      memset(topSelAux, 0, sizeof(struct SelRec));
      topSelAux->obj = obj_ptr_aux;
      topSelAux->prev = NULL;
      topSelAux->next = NULL;
       
      if (topSel == NULL)
      {
         topSel = topSelAux;
         botSel = topSel;
      }
      else
      {
         topSel->prev = topSelAux;
         topSelAux->next = topSel;
         topSel = topSelAux;
      }
   }


   GroupSelObj();
/*   topSel = topSelSaved;
   numObjSelected = numObjSelectedSaved;
*/

/*   RedrawAnArea (botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
                         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
*/

}

static struct ObjRec *tmpTopObj, *tmpBotObj;

static
void PushTmpObj(ObjPtr)
   struct ObjRec *ObjPtr;
{
   ObjPtr->next = tmpTopObj;
   ObjPtr->prev = NULL;

   if (tmpBotObj == NULL) {
      tmpBotObj = ObjPtr;
   } else {
      tmpTopObj->prev = ObjPtr;
   }
   tmpTopObj = ObjPtr;
}

void CmdOTMRec(num_obj, buff)
int                     num_obj;
char                  * buff;

{
/*   GroupSelObj() no select.c  */ 
/* o problema e' a variavel  topSel que nao pode ser alterada! */
/* talvez seja melhor duplicar pro wb3.c  */
  int                 i;
  struct ObjRec       * obj_ptr;
  struct ObjRec       * obj_ptr_aux;
  struct SelRec       * topSelBefRec, * botSelBefRec, * topSelAux, * sel_ptr;
  char                aux[10000];

  topSelBefRec = (struct SelRec *) malloc (sizeof(struct SelRec));
  if (topSelBefRec == NULL) FailAllocMessage();
  memset(topSelBefRec, 0, sizeof(struct SelRec));
 
  topSelBefRec->next = NULL;
  topSelBefRec->prev = NULL;

  Cmd_Flag_Arrived = 6;

/* criando o TopSelBefRec para poder dar group nele! */
/* os objetos estao do bottom ao top! */

  *buff++;
  for (i=0 ; i < num_obj ; i++)
  {
      buff = ParseStr (buff,(int) '#', aux, strlen(buff));
      ReadObjFromBuff(aux, &obj_ptr);
      if((FindObjRec(obj_ptr,&obj_ptr_aux)) == FALSE) return;
      topSelAux = (struct SelRec *) malloc (sizeof(struct SelRec));
      if (topSelAux  == NULL) FailAllocMessage();
      memset(topSelAux, 0, sizeof(struct SelRec));
      topSelAux->obj = obj_ptr_aux;
      topSelAux->prev = NULL;

      if (topSelBefRec->obj == NULL) 
      {
         topSelBefRec->obj = obj_ptr_aux;
         botSelBefRec = topSelBefRec;
      }
      else
      {
         topSelBefRec->prev = topSelAux;
         topSelAux->next = topSelBefRec;
         topSelBefRec = topSelAux;
      }
   }

   tmpTopObj = tmpBotObj = NULL;

   PrepareToRecord (CMD_REPLACE, topSelBefRec, botSelBefRec, num_obj);
   
   for (sel_ptr = botSelBefRec; sel_ptr != NULL; sel_ptr = sel_ptr->prev)
   {
      UnlinkObj (sel_ptr->obj);
      PushTmpObj (sel_ptr->obj);
   }

   CreateGroupObj (tmpTopObj, tmpBotObj);

   /* RemoveAllSel (); */
/*   while (topSelBefRec != NULL)
   {
      sel_ptr = topSelBefRec->next;
      free(topSelBefRec);
      topSelBefRec = sel_ptr;
   }
   botSelBefRec = NULL;
*/

/* topSel = botSel = (struct SelRec *) malloc (sizeof(struct SelRec));
   if (topSel == NULL) FailAllocMessage();
   memset(topSel, 0, sizeof(struct SelRec));
   topSel->obj = topObj;
   topSel->next = topSel->prev = NULL;
   UpdSelBBox ();
*/

   RecordCmd (CMD_MANY_TO_ONE, NULL, topSelBefRec, botSelBefRec, 1);


/* dar free nao top e bot selbefrec  */

/*   RedrawAnArea (botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
                 selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
*/

}

void CmdUndoRec()
{
   int saved_curChoice;

   saved_curChoice = curChoice;
   UndoCmd();
   Cmd_Flag_Arrived = -1;
   SetCurChoice (saved_curChoice);

}

void CmdRedoRec()
{
   int saved_curChoice;
 
   saved_curChoice = curChoice;
   UndoCmd();
   Cmd_Flag_Arrived = -1;
   SetCurChoice (saved_curChoice);
 
}


int FindObjRec(obj_ptr,obj_ptr_aux)
   struct ObjRec         * obj_ptr;
   struct ObjRec         * * obj_ptr_aux;
{
	int       found = 0;
	register struct ObjRec       * obj_aux;

	if (obj_ptr == NULL)
        {
             (*obj_ptr_aux) = NULL;
             ErrorOnConsist();
             return(FALSE);
        }

        for (obj_aux = topObj; obj_aux != NULL; obj_aux = obj_aux->next)
        {
            if (obj_ptr->type == obj_aux->type)
            {
                if ((obj_ptr->obbox.ltx == obj_aux->obbox.ltx) &&
                    (obj_ptr->obbox.lty == obj_aux->obbox.lty))
		{
		   if ((obj_ptr->obbox.rbx == obj_aux->obbox.rbx) &&
                       (obj_ptr->obbox.rby == obj_aux->obbox.rby))
		   {
                        found=1;
			break;
		   }
                }
            }
        }

	if (found == 1)
	{
		(*obj_ptr_aux) = obj_aux; 
	}
	else
	{
		(*obj_ptr_aux) = NULL;
                ErrorOnConsist();
                return(FALSE);
	}
        return(TRUE);

}

int ReadObjFromBuff (buff, ObjPtr)
   char                 * buff;
   struct ObjRec        * * ObjPtr;
{

   char     obj_name[20], * buffattr, buffaux[10000];
   int      allocated;

   strcpy(buffaux, buff);
   ParseStr (buff,(int) '(', obj_name, strlen(buff));
   buffattr = FindChar ((int)'[', buffaux);
   buffattr++; 

   while (buffattr[0] == '\t')  /* a waste for text */
   {
      strcpy(buffaux, buffattr);
      buffattr = FindChar ((int)'[', buffaux);
      buffattr++;
   }

   if (strcmp (obj_name, "poly") == 0)
   {
      ReadPoly (buff, ObjPtr);
      if (*ObjPtr == NULL) return (FALSE);
      ReadObjAttrsFromBuff (INVALID, buffattr, ObjPtr);
      /*if (RetractedArrowAttr(*ObjPtr) ||
         AutoRetractedArrowAttr(*ObjPtr, TRUE)) { */
         /* fake the undoingOrRedoing so that no */
         /*          actual auto-adjusting is done */
         /*undoingOrRedoing = TRUE;
         AdjObjSplineVs(*ObjPtr);
         undoingOrRedoing = FALSE; 
         } */

      AdjObjBBox (*ObjPtr);
      if (allocated) free(buff);
         return (TRUE);
   }
   if (strcmp (obj_name, "box") == 0)
   {
      ReadBoxObj (buff, ObjPtr);
      /* if (*ObjPtr == NULL) return (FALSE);  */
      ReadObjAttrsFromBuff (START_HAVING_ATTRS-1, buffattr, ObjPtr);
      AdjObjBBox (*ObjPtr);
      if (allocated) free(buff);
      return (TRUE);
   }
   else if (strcmp (obj_name, "oval") == 0)
   {
      ReadOvalObj (buff, ObjPtr);
      /* if (*ObjPtr == NULL) return (FALSE);  */
      ReadObjAttrsFromBuff (START_HAVING_ATTRS-1, buffattr, ObjPtr);
      AdjObjBBox (*ObjPtr); 
      if (allocated) free(buff);
      return (TRUE);
   }
   else if (strcmp (obj_name, "text") == 0)
   {
      ReadText (buff, ObjPtr);
      /*if (*ObjPtr == NULL) return (FALSE);*/
      if (allocated) free(buff);
      return (TRUE);
   }
   else if (strcmp (obj_name, "polygon") == 0)
   {
      ReadPolygon (buff,  ObjPtr);
      if (*ObjPtr == NULL) return (FALSE);
      ReadObjAttrsFromBuff (START_HAVING_ATTRS-1, buffattr, ObjPtr);
      AdjObjBBox (*ObjPtr);
      if (allocated) free(buff);
      return (TRUE);
   }
   else if (strcmp (obj_name, "arc") == 0)
   {
      ReadArcObj (buff, ObjPtr);
      /* if (*ObjPtr == NULL) return (FALSE); */
      ReadObjAttrsFromBuff (START_HAVING_ATTRS-1, buffattr, ObjPtr);
      AdjObjBBox (*ObjPtr);
      if (allocated) free(buff);
      return (TRUE);
   }
   else if (strcmp (obj_name, "rcbox") == 0)
   {
      ReadRCBoxObj (buff, ObjPtr);
      /*if (*ObjPtr == NULL) return (FALSE); */
      ReadObjAttrsFromBuff (START_HAVING_ATTRS-1, buffattr, ObjPtr);
      AdjObjBBox (*ObjPtr);
      if (allocated) free(buff);
      return (TRUE);
   }
   else if (strcmp (obj_name, "group") == 0)
   {
      ReadGroup (buff, OBJ_GROUP, ObjPtr);
      /*if (*ObjPtr == NULL) return (FALSE); */
      ReadObjAttrsFromBuff (INVALID, buffattr, ObjPtr);
      AdjObjBBox (*ObjPtr);
      if (allocated) free(buff);
      return (TRUE);
   }

}

void ReadObjAttrsFromBuff (MinFileVersion, buff, ObjPtr)
   int                  MinFileVersion;
   char                 * buff;
   struct ObjRec        * * ObjPtr;
{
   struct AttrRec       * top_attr = NULL, * bot_attr = NULL, * attr_ptr;
 
   if (fileVersion <= MinFileVersion) return;
 
   while (ReadAttrFromBuff (buff, &attr_ptr))
   {
      attr_ptr->owner = *ObjPtr;
      attr_ptr->prev = NULL;
      attr_ptr->next = top_attr;
      if (top_attr == NULL)
         bot_attr = attr_ptr;
      else
         top_attr->prev = attr_ptr;
      top_attr = attr_ptr;
   }
   if (bot_attr != NULL) bot_attr->next = NULL;
   (*ObjPtr)->fattr = top_attr;
   (*ObjPtr)->lattr = bot_attr;
}
 

void AddObjFromBuffer (PrevPtr, NextPtr, ObjPtr)
   struct ObjRec        * PrevPtr, * NextPtr, * ObjPtr;
{
   ObjPtr->prev = PrevPtr;
   ObjPtr->next = NextPtr;
 
   if (PrevPtr == NULL)
      curPage->top = topObj = ObjPtr;
   else
      PrevPtr->next = ObjPtr;
 
   if (NextPtr == NULL)
      curPage->bot = botObj = ObjPtr;
   else
      NextPtr->prev = ObjPtr;
}


void RecordObjToBuffer(obj_ptr,buff)
   struct ObjRec        * obj_ptr;
   char                 * buff;
{

      switch (obj_ptr->type)
      {
         case OBJ_POLY: RecordPolyToBuffer (obj_ptr, buff); break;
         case OBJ_BOX: RecordBoxToBuffer (obj_ptr,buff); break;
         case OBJ_OVAL: RecordOvalToBuffer (obj_ptr, buff); break;
         case OBJ_TEXT: RecordTextToBuffer (obj_ptr, buff); break;
         case OBJ_POLYGON: RecordPolygonToBuffer (obj_ptr, buff); break; 
         case OBJ_ARC: RecordArcToBuffer (obj_ptr, buff); break;
         case OBJ_RCBOX: RecordRCBoxToBuffer (obj_ptr, buff); break;
/*         case OBJ_XBM: SaveXBmObj (FP, obj_ptr); break;
           case OBJ_XPM: SaveXPmObj (FP, obj_ptr); break; */
         case OBJ_GROUP: RecordGroupToBuffer (obj_ptr, buff, 0); break;
         /* level was set to 0 like saving file (file.c)   */
/*         case OBJ_SYM: SaveCompObj (FP, obj_ptr, Level); break;
           case OBJ_ICON: SaveIconObj (FP, obj_ptr, Level); break;  */
      }

}

void SendCmd(CmdType,SubCmdPtr, TopSel, NumObjs, TopSelBefore)
int                  CmdType, NumObjs;
struct SelRec        * TopSel, * TopSelBefore;
struct SubCmdRec     * SubCmdPtr;
{
   int i, numobj, CmdTypeRec;
   char buff[20000], aux[10];
   struct SelRec        * SelAux, * BotSel_aux;
   struct ObjRec        * obj_ptr;

   memset(buff, 0, sizeof(buff) );
 
/*   CmdType#NumObjs#CMD_MOVE->dx#CMD_MOVE->dy#TopSel_Obj#...objs */
/*   CmdType#NumObjs#CMD_MOVE->dx#CMD_MOVE->dy#BotSel_Obj#...until top_obj */
 
   sprintf (buff, "%d#", CmdType);

   if ( (CmdType == CMD_DELETE) || (CmdType == CMD_MANY_TO_ONE) )
   {
      for (numobj=0,SelAux=TopSelBefore ; SelAux != NULL ; 
                                           ++numobj,SelAux = SelAux->next);
      sprintf (buff, "%s%d#",buff, numobj);
   }
   else
   {
      sprintf (buff, "%s%d#",buff, NumObjs);
   }
 
   if (CmdType == CMD_MOVE)
   {
      sprintf (buff, "%s%d#%d#",buff,SubCmdPtr->detail.mv->dx,
                                     SubCmdPtr->detail.mv->dy);
   }
   else
   {
      sprintf (buff, "%s##" , buff);
   }
 
   if (TopSel == NULL)
   {
      sprintf (buff, "%s#", buff);
   }
   else
   {
      if (TopSel->obj == NULL)
      {
         close(sock);
         exit(1);
      }
      else
      {
         SelAux = TopSel;
         for (i=1; i <= NumObjs ; i++)   /* geting the bottom obj */
         {
             obj_ptr = SelAux->obj;
             SelAux = SelAux->next;
         }
         for (i=1; i <= NumObjs ; i++)
         {
             /*RecordObjToBuffer(SelAux->obj,buff); */
             RecordObjToBuffer(obj_ptr,buff);
             sprintf (buff, "%s#" , buff);
             /* SelAux = SelAux->next; */
             obj_ptr = obj_ptr->prev;
         }
      }
   }

   if (TopSelBefore != NULL)
   {
      numobj = 0; 
      for (SelAux = TopSelBefore; SelAux != NULL ; SelAux = SelAux->next)
      {
          numobj = numobj + 1;
          /*obj_ptr = SelAux->obj;*/
          BotSel_aux = SelAux;
      }
      for (i=1 ;  i <= numobj ; i++)
      {
          RecordObjToBuffer(BotSel_aux->obj,buff);
          /*RecordObjToBuffer(obj_ptr,buff); */
          sprintf (buff, "%s#" , buff);
          /*obj_ptr = obj_ptr->prev;*/
          BotSel_aux = BotSel_aux->prev;
      }
   } 
   else
   {
      sprintf (buff, "%s#" , buff);
   } 
 
   if (write(sock, buff, sizeof(buff)) < 0)
   {
      sprintf(buff, "There is a problem writing to the socket #%d.\nStart again the connection!", sock);
      MsgBox(buff, TOOL_NAME, INFO_MB);
      return;
   }

   /* CheckClientServer(); */
   if (ClientServer == 1)  /* connection is on and I'm Client!   */
   {
      WaitForAck(CmdType, SubCmdPtr, TopSel, botSel, NumObjs);
   }

}

void WaitForAck(CmdType, SubCmdPtr, TopSel, BotSel, NumObjs)
   int                  CmdType, NumObjs;
   struct SelRec        * TopSel, * BotSel;
   struct SubCmdRec     * SubCmdPtr;
{
   int CmdTypeRec = 0, saved_curChoice;
   char buff[20000], aux[10];

/* finishing the recording of the last command of client and undoing it 
   until the ack arrives. In case of a nack it only kills the last command,
   otherwise (ack arrives) it REDOs the last command and kills it.
   Clients don't keep list of commands only a temporary last one*/

   if (curCmd == NULL)
      ClearRedoRecords (firstCmd);
   else if (curCmd != lastCmd)
      ClearRedoRecords (curCmd);
 
   if (++historyCount == historyDepth && !composingCommand)
   {
      struct CmdRec     * new_first_cmd = firstCmd->next;
 
      new_first_cmd->prev = NULL;
      firstCmd->next = NULL;
      DeleteARedoRecord (firstCmd, (-1.0), (-1.0));
      historyCount--;
      firstCmd = new_first_cmd;
   }
 
   curCmd = (struct CmdRec *) malloc (sizeof(struct CmdRec));
   if (curCmd == NULL) FailAllocMessage();
   memset(curCmd, 0, sizeof(struct CmdRec));
   curCmd->top_before = topSelBeforeInCmd;
   curCmd->bot_before = botSelBeforeInCmd;
   curCmd->pos_before = stackingPosition;
   curCmd->count_before = stackingCount;
   curCmd->type = CmdType;
   curCmd->undone = FALSE;
   curCmd->include_tgif_obj = recordCmdIncludeTgifObj;
 
   if (TopSel != NULL)
   {
      CopySel (TopSel, NumObjs, &(curCmd->top_after), &(curCmd->bot_after));
      PrepareStacking (TopSel, BotSel, NumObjs);
      curCmd->pos_after = stackingPosition;
      curCmd->count_after = stackingCount;
   }
   else
   {
      curCmd->top_after = curCmd->bot_after = NULL;
      curCmd->pos_after = NULL;
      curCmd->count_after = 0;
   }
   InsertCmd (lastCmd, NULL, curCmd);
 
   switch (CmdType)
   {
      case CMD_NEW: break;
      case CMD_DELETE: break;
      case CMD_MOVE:
         curCmd->dx = SubCmdPtr->detail.mv->dx;
         curCmd->dy = SubCmdPtr->detail.mv->dy;
         break;
      case CMD_STRETCH: FreeAfterSel (curCmd); break;
      case CMD_REPLACE: FreeAfterSel (curCmd); break;
      case CMD_ONE_TO_MANY: break;
      case CMD_MANY_TO_ONE: break;
      case CMD_GOTO_PAGE: curCmd->count_after = NumObjs; break;
   }
   curCmd = lastCmd;
   saved_curChoice = curChoice;

   /* UndoCmd (); */
   TieLooseEnds ();
   SetCurChoice (NOTHING);
   if (curCmd == NULL)
   {
      Msg ("No commands to Undo!");
      return;
   }
   undoingOrRedoing = TRUE;
   UndoACmd (curCmd, TRUE);
   curCmd = curCmd->prev;
   undoingOrRedoing = FALSE;


/* now I have the last command of the client recorded and UNDONE */


/* I'll wait for and ack(20) or nack(21) 
   While other commands arrive from  the server I keep executing them */

  Msg("Waiting for ACK/NACK from server");

  CmdTypeRec = -1;
  while ( (CmdTypeRec != 20) && (CmdTypeRec != 21) ) 
  {
      memset(buff, 0, sizeof(buff));
      if ( (read(sock, buff, sizeof(buff)) > 0)  &&  (strlen(buff) > 0) )
      {
         ParseStr (buff,(int) '#', aux, strlen(buff));
         CmdTypeRec = atoi (aux);
         Cmd_Flag_Arrived = CmdTypeRec;
         if ((CmdTypeRec != 20) && (CmdTypeRec != 21))
         {
            CmdReceived(buff);
            Msg("Still waiting for ACK/NACK");
	 }
      }
  }
  if (CmdTypeRec == 20)   /* an ack has arrived, redoing the command */
  {
     Msg("An ACK has arrived from server");
     /* RedoCmd (); */
     TieLooseEnds ();
     SetCurChoice (NOTHING);
     if (firstCmd==NULL || (curCmd!=NULL && curCmd->next==NULL))
     {
        Msg ("No commands to Redo!");
        return;
     }
 
     if (curCmd == NULL)
        curCmd = firstCmd;
     else
        curCmd = curCmd->next;
 
     undoingOrRedoing = TRUE;
     RedoACmd (curCmd, TRUE);
     undoingOrRedoing = FALSE;

  }
  else   /* an ACK has arrived */
  {
     Msg(" Last command ignored! ");
  }
  Cmd_Flag_Arrived = -1;
  SetCurChoice (saved_curChoice);

  /*   should kill here last command */ 
}

void SendAlreadyDone()
{
   struct SelRec        * TopSelAux;
   struct ObjRec        * obj_ptr, * topOld;
   int                    status, count;

   if (ClientServer == 1)  /* I'm client */   /* never used-only for server */
   {
      topOld = topObj;
      sleep(2);
      while((status = CheckClientServer()) == TRUE) continue;
      for (obj_ptr = curPage->bot; obj_ptr != topOld; obj_ptr = obj_ptr->prev)
      {
          TopSelAux->obj = obj_ptr;
          TopSelAux->next = NULL;
          TopSelAux->prev  = NULL;
          /*SendCmd (1, NULL, TopSelAux, 1, NULL);*/
          CheckConForRecord(1, NULL, TopSelAux, 1, NULL);
      }
   }
   else      /* I'm server */
   {
      count = 0;
      SelAllObj(FALSE);
      if (topSel == NULL) 
      {
         return;
      }
      for (obj_ptr = topSel->obj; obj_ptr != NULL; obj_ptr = obj_ptr->next)
          count++;

      CheckConForRecord(1, NULL, topSel, count, NULL);
/*
      for (obj_ptr = curPage->bot; obj_ptr != NULL; obj_ptr = obj_ptr->prev)
      {
          Cmd_Flag_Arrived = -1; 
          TopSelAux->obj = obj_ptr;
          TopSelAux->next = NULL;
          TopSelAux->prev  = NULL;
          CheckConForRecord(1, NULL, TopSelAux, 1, NULL);
      }
*/
   }

}

void ErrorOnConsist()
{
   char      msg[MAXSTRING], b[80];

   strcpy(b, "21####");

   Cmd_Flag_Arrived = 21;

   if (ClientServer == 2) /* I'm server and received an inconsistent command */
   {
      if (write(sock, b, sizeof(b)) < 0)   /* sending NACK */
      {
         sprintf(b, "There is a problem writing to the socket #%d.\nStart again the connection!", sock);
         MsgBox(b, TOOL_NAME, INFO_MB);
         return;
      }
   }
   else     /* I'm client */
   {
      sprintf(b,"There is an inconsistency between you and the server!");
      sprintf(b,"%s\n Recovering from the server!",b);
      MsgBox(b, TOOL_NAME, INFO_MB);
      NewProc();

      strcpy(b, "22####");
      if (write(sock, b, sizeof(b)) < 0)   /* sending NACK-inconsist. */
      {
         sprintf(b, "There is a problem writing to the socket #%d.\nStart again the connection!", sock);
         MsgBox(b, TOOL_NAME, INFO_MB);
         return;
      }
/*      sprintf(b,"There is an inconsistency between you and the server!");
      sprintf(b,"%s\n Recovering from the server!",b);
      MsgBox(b, TOOL_NAME, INFO_MB);
      NewProc();  */
   }
}

void NackFromClient()
{
   char b[80];

   sprintf(b,"There is an inconsistency between you and a client!");
   sprintf(b,"%s\nRecovering from your status",b);
   MsgBox(b, TOOL_NAME, INFO_MB);

   Cmd_Flag_Arrived = -1;
   SendAlreadyDone();
   ClearAndRedrawDrawWindow();

}

void ClearClient()
{
   char b[80];
 
   Cmd_Flag_Arrived = 23;
   sprintf(b,"Recovering from Server status !");
   MsgBox(b, TOOL_NAME, INFO_MB);

   NewProc();
}

void CloseCon()
{
   char b[80];
 
   if (ClientServer == 2)    /*  connection is on and I'm server*/
   {
      sprintf(b," Client requested to close the connection");
      sprintf(b,"%s \n Closing the connection ", b);
   }
   else
   {
      sprintf(b," Server requested to close the connection");
      sprintf(b,"%s \n Closing the connection ", b);
   }
   MsgBox(b, TOOL_NAME, INFO_MB);
   close(sock);
   ClientServer = 0;

}

#endif     /* _TGIF_WB */
