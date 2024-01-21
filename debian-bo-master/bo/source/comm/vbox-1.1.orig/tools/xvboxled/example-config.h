/*---------------------------------------------------------------------------*/
/* Config file for xvboxled                                                  */
/*---------------------------------------------------------------------------*/
/* Edit these to customize the builtin defaults:

   The path where the messages are stored (you will probably want to 
   edit this one):                                                           */

#define INCOMING_DIR "/var/spool/vbox/joachim/incoming"

/* The command to execute if the xvboxled window is clicked (usually, this
   will be a call to vbox to play the received messages):                   */

#define VBOX_COMMAND  "xterm -T VBOX -e vbox &"

/* The time interval (in seconds) between two checks for new messages:      */

#define CHECK_PERIOD 5

/* the default colors                                                       */

#define FG_COLOR "red"
#define BG_COLOR "grey20"
#define LO_COLOR "#800510"

/* thats it folks                                                           */
