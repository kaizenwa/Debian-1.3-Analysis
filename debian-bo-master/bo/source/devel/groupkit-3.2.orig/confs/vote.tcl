###
### voting booth application
### 

gk_initConf $argv
wm title . "Voting Booth"
gk_defaultMenu .menubar
pack .menubar -side top -fill x

### environment used to hold questionid plus info on questions

gk_newenv vote
vote nextid 1

### help text and menus

set help_title "About Voting Booth"
set help_text  { 
    {normal}  {Lets you vote on, like, you know, stuff!}
}

.menubar itemcommand 2 add command \
        -label "$help_title" \
        -command "gk_topicWindow .helpWindow \
                -height 8 \
                -width 20 \
                -title \"$help_title\" \
                -text \"$help_text\""

.menubar itemcommand 0 insert 1 command -label "New Question" -command NewQuestion
.menubar itemcommand 0 insert 2 separator


### main interface showing list of questions

label .lbl -text "Active Questions" -anchor c
pack .lbl -side top -padx 5 -pady 5 -fill x
listbox .questionlist -width 40 -height 5 -yscrollcommand ".questionscroll set" -borderwidth 1 -relief sunken
scrollbar .questionscroll -orient vertical -command ".questionlist yview"
pack .questionlist -side left
pack .questionscroll -side left -fill y


### pop up a dialog allowing the user to create a new question

proc NewQuestion {} {
   if {[winfo exists .question]} {return}
   toplevel .question
   wm title .question "New Question"
   message .question.msg -text "Enter your question in the space below and then press the \"OK\" button." -aspect 500
   text .question.txt -relief sunken -borderwidth 2 -width 50 -height 5 
   button .question.ok -text "OK" -command "proceedWithQuestion"
   button .question.cancel -text "Cancel" -command "destroy .question"
   pack .question.msg -side top -padx 5 -pady 5
   pack .question.txt -side top -padx 5 -pady 5
   pack .question.cancel -side right -padx 5 -pady 5
   pack .question.ok -side right -padx 5 -pady 5
   focus .question.txt
}


### invoked when the user finishes posing question; 
### cleanup and call CreateQuestion

proc proceedWithQuestion {} {
  set text [.question.txt get 0.0 end]
  destroy .question
  set questionid [users local.usernum]x[vote nextid]
  vote nextid [expr [vote nextid]+1]
  CreateQuestion $questionid $text
}


### here the asker creates the question:
###   1. create the window to hold responses from remote users
###   2. add the question to the main question list
###   3. request a response from all remote users

proc CreateQuestion {questionid text} {
  set w .owner$questionid
  toplevel $w
  wm title $w "Vote Results"
  wm protocol $w WM_DELETE_WINDOW "closeMyQuestion $questionid"
  message $w.question -text $text -aspect 500
  pack $w.question -side top

  frame $w.yes
  label $w.yes.title -text "Yes:"
  label $w.yes.value -text 0
  pack $w.yes.title -side left 
  pack $w.yes.value -side left

  frame $w.no
  label $w.no.title -text "No:"
  label $w.no.value -text 0
  pack $w.no.title -side left
  pack $w.no.value -side left

  frame $w.undec
  label $w.undec.title -text "Pending:"
  label $w.undec.value -text [llength [users keys remote]]
  pack $w.undec.title -side left
  pack $w.undec.value -side left

  pack $w.yes -side top
  pack $w.no -side top
  pack $w.undec -side top

  addToList [users local.usernum] $questionid $text 

  gk_toOthers requestResponse [users local.usernum] $questionid $text
}


### a question asker has requested a response; pop up a window to ask
### our user, and add the question to the question list

proc requestResponse {asker questionid text} {

# teensy hack; make sure we have remote users's name before proceeding
  if {[users remote.$asker.username]==""} {
    after 200 requestResponse $asker $questionid \"$text\"
    return
  }

  addToList $asker $questionid $text
  set w .answer$questionid
  toplevel $w
  wm title $w "Your Ballot"
  label $w.asker -text "Question from [users remote.$asker.username]:"
  pack $w.asker -side top -padx 5 -pady 5
  message $w.question -text $text -aspect 500
  pack $w.question -side top
  frame $w.buttons
  button $w.buttons.yes -text "Yes" -command "enterVote $asker $questionid yes"
  button $w.buttons.no -text "No" -command "enterVote $asker $questionid no"
  pack $w.buttons.yes -side left -padx 10
  pack $w.buttons.no -side left -padx 10
  pack $w.buttons -side top -fill x -padx 5 -pady 5
}


### pass a yes or no vote for a question on to the question's asker

proc enterVote {asker questionid response} {
  gk_toUserNum $asker voteResponse $questionid $response
  destroy .answer$questionid
}


### receive a response to a question; adjust the yes/no/pending counts

proc voteResponse {questionid response} {
  set w .owner$questionid
  if {[winfo exists $w]==0} {error "got a vote response for invalid window"}
  if {$response=="yes"} {
    set val [lindex [$w.yes.value configure -text] 4]
    incr val
    $w.yes.value configure -text $val
  } else {
    set val [lindex [$w.no.value configure -text] 4]
    incr val
    $w.no.value configure -text $val
  }
  set val [lindex [$w.undec.value configure -text] 4]
  incr val -1
  $w.undec.value configure -text $val
}


### called when the asker closes the response window; close the question

proc closeMyQuestion {questionid} {
  gk_toOthers closeQuestion $questionid
  rmFromList $questionid
  destroy .owner$questionid
}


### close a given question (both answer boxes and from the question list)

proc closeQuestion questionid {
  if {[winfo exists .answer$questionid]} {
    destroy .answer$questionid
  }
  rmFromList $questionid
}


### add a question to the question list (and store in the vote environment)

proc addToList {asker questionid text} {
  .questionlist insert end $text
  vote votes.$questionid.asker $asker
  vote votes.$questionid.text $text
}


### remove a question from the question list (and the vote environment)

proc rmFromList questionid {
  set text [vote votes.$questionid.text]
  for {set i 0} {$i<[.questionlist size]} {incr i} {
    if {[.questionlist get $i]==$text} {
      .questionlist delete $i
      break
    }
  }
  vote delete votes.$questionid
}


### when a new user joins, send them all questions

gk_bind updateEntrant "updateEntrant %U"

proc updateEntrant usernum {
  foreach i [vote keys votes] {
    gk_toUserNum $usernum requestResponse [vote votes.$i.asker] $i [vote votes.$i.text]
  }
}


### when a user leaves, remove questions asked by them

gk_bind userDeleted "userDeleted %U"

proc userDeleted usernum {
  foreach i [vote keys votes] {
    if {[vote votes.$i.asker]==$usernum} {
      closeQuestion $i
    }
  }
# note: we should decrement the count for any votes we're holding;
#   only problem is we don't know if the leaving guy voted already or not!
}


### when a new user arrives, add 1 to pending count in our questions

gk_bind newUserArrived userArrived

proc userArrived {} {
  foreach i [vote keys votes] {
    if {[vote votes.$i.asker]==[users local.usernum]} {
      set w .owner$i
      set val [lindex [$w.undec.value configure -text] 4]
      incr val
      $w.undec.value configure -text $val
    }
  }
}
