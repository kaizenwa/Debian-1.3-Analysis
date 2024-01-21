;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Big5-picture-mode: provides the same functions as picture-mode, but
;;;;; working with Big5 encoding and provides extra functions:
;;;;; Return and Backspace, if used in left or down directions, do the
;;;;; expected things.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This is a hack from picture.el: functions, variables, etc... are
;;;;; prefixed with big5-... but unless notified, they aren't modified.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Don't expect it to be as comfortable as « normal » mode... It's
;;;;; really picture-mode, you know...

;;; big5-picture.el --- "Picture mode" (and more) for Big5
;;; Editing using quarter-plane screen model.

(defun big5-move-to-column-force (column)
  "Move to column COLUMN in current line.
Differs from `move-to-column' in that it creates or modifies whitespace
if necessary to attain exactly the specified column."
  (or (natnump column) (setq column 0))
  (move-to-column column)
  (let ((col (current-column)))
    (if (< col column)
	(indent-to column)
      (if (and (/= col column)
	       (= (preceding-char) ?\t))
	  (let (indent-tabs-mode)
	    (delete-char -1)
            (indent-to col)
            (move-to-column column))))
    ;; This call will go away when Emacs gets real horizontal autoscrolling
    (hscroll-point-visible)))

;; Picture Movement Commands

(defun big5-picture-beginning-of-line (&optional arg)
  "Position point at the beginning of the line.
With ARG not nil, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error."
  (interactive "P")
  (if arg (forward-line (1- (prefix-numeric-value arg))))
  (beginning-of-line)
  ;; This call will go away when Emacs gets real horizontal autoscrolling
  (hscroll-point-visible))

(defun big5-picture-end-of-line (&optional arg)
  "Position point after last non-blank character on current line.
With ARG not nil, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error."
  (interactive "P")
  (if arg (forward-line (1- (prefix-numeric-value arg))))
  (beginning-of-line)
  (skip-chars-backward " \t" (prog1 (point) (end-of-line)))
  ;; This call will go away when Emacs gets real horizontal autoscrolling
  (hscroll-point-visible))

(defun big5-picture-forward-column (arg)
  "Move cursor right, making whitespace if necessary.
With argument, move that many columns."
  (interactive "p")
  (let ((target-column (+ (current-column) arg)))
    (big5-move-to-column-force target-column)
    ;; Picture mode isn't really suited to multi-column characters,
    ;; but we might as well let the user move across them.
    (and (< arg 0)
	 (> (current-column) target-column)
	 (forward-char -1))))

(defun big5-picture-backward-column (arg)
  "Move cursor left, making whitespace if necessary.
With argument, move that many columns."
  (interactive "p")
  (big5-picture-forward-column (- arg)))

(defun big5-picture-move-down (arg)
  "Move vertically down, making whitespace if necessary.
With argument, move that many lines."
  (interactive "p")
  (let ((col (current-column)))
    (big5-picture-newline arg)
    (big5-move-to-column-force col)))

(defconst big5-picture-vertical-step 0
  "Amount to move vertically after text character in Picture mode.")

(defconst big5-picture-horizontal-step 1
  "Amount to move horizontally after text character in Picture mode.")

(defvar big5-normal-line-column 0 "0: normal, 1: line, 2: column")

(defun big5-picture-go-starting-line ()
  (interactive)
  (let ((current-col (current-column)))
    (goto-line starting-line)
    (big5-move-to-column-force (- current-col 2))))

(defun big5-picture-go-starting-column ()
  (interactive)
  (big5-picture-move-up -1)
  (big5-move-to-column-force starting-column))

(defun big5-recover-newline ()
  (interactive)
  (cond ((= big5-normal-line-column 0) nil)
	((= big5-normal-line-column 1)   
	 (big5-picture-substitute 'big5-picture-go-starting-column 'newline))
	((= big5-normal-line-column 2)
	 (big5-picture-substitute 'big5-picture-go-starting-line 'newline)))
  (setq big5-normal-line-column 0))
	  
(defun big5-picture-move-up (arg)
  "Move vertically up, making whitespace if necessary.
With argument, move that many lines."
  (interactive "p")
  (big5-picture-move-down (- arg)))

(defun big5-picture-movement-right ()
  "Move right after self-inserting character in Picture mode."
  (interactive)
  (setq starting-column (current-column))
  (big5-recover-newline)
  (big5-picture-substitute 'newline 'big5-picture-go-starting-column)
  (big5-picture-set-motion 0 2 "right"))
  
(defun big5-picture-movement-left ()
  "Move left after self-inserting character in Picture mode."
  (interactive)
  (setq starting-column (current-column))
  (big5-recover-newline)
  (big5-picture-substitute 'newline 'big5-picture-go-starting-column)
  (big5-picture-set-motion 0 -2 "left"))

(defun big5-picture-movement-up ()
  "Move up after self-inserting character in Picture mode."
  (interactive)
  (big5-recover-newline)
  (big5-picture-set-motion -1 0 "up"))

(defun big5-picture-movement-down ()
  "Move down after self-inserting character in Picture mode."
  (interactive)
  (setq starting-line (count-lines (point-min) (point)))
  (big5-recover-newline)
  (big5-picture-substitute 'newline 'big5-picture-go-starting-line)
  (big5-picture-set-motion 1 0 "down"))	

(defun big5-picture-movement-nw ()
  "Move up and left after self-inserting character in Picture mode."
  (interactive)
  (big5-recover-newline)
  (big5-picture-set-motion -1 -2 "nw"))

(defun big5-picture-movement-ne ()
  "Move up and right after self-inserting character in Picture mode."
  (interactive)
  (big5-recover-newline)
  (big5-picture-set-motion -1 1 "ne"))

(defun big5-picture-movement-sw ()
  "Move down and left after self-inserting character in Picture mode."
  (interactive)
  (big5-recover-newline)
  (big5-picture-set-motion 1 -2 "sw"))

(defun big5-picture-movement-se ()
  "Move down and right after self-inserting character in Picture mode."
  (interactive)
  (big5-recover-newline)
  (big5-picture-set-motion 1 1 "se"))

(defun big5-picture-set-motion (vert horiz mode)
  "Set VERTICAL and HORIZONTAL increments for movement in Picture mode.
The mode line is updated to reflect the current direction."
  (setq big5-picture-vertical-step vert
	big5-picture-horizontal-step horiz)
  (setq mode-name
	(format "Big5:%s" mode))
  (force-mode-line-update)
  (message ""))

(defun big5-picture-move ()
  "Move in direction of `picture-vertical-step' and `picture-horizontal-step'."
  (big5-picture-move-down big5-picture-vertical-step)
  (big5-picture-forward-column big5-picture-horizontal-step))

(defun big5-picture-motion (arg)
  "Move point in direction of current picture motion in Picture mode.
With ARG do it that many times.  Useful for delineating rectangles in
conjunction with diagonal picture motion.
Do \\[command-apropos]  picture-movement  to see commands which control 
motion."
  (interactive "p")
  (big5-picture-move-down (* arg big5-picture-vertical-step))
  (big5-picture-forward-column (* arg big5-picture-horizontal-step)))

(defun big5-picture-motion-reverse (arg)
  "Move point in direction opposite of current picture motion in Picture mode.
With ARG do it that many times.  Useful for delineating rectangles in
conjunction with diagonal picture motion.
Do \\[command-apropos] `big5-picture-movement' to see commands which control 
motion."
  (interactive "p")
  (big5-picture-motion (- arg)))

;; Picture insertion and deletion.

(setq first-byte-big5 nil starting-line 0 starting-column 0)

(defun big5-picture-self-insert (arg)
  "Insert this character in place of character previously at the cursor.
The cursor then moves in the direction you previously specified
with the commands `big5-picture-movement-right', `big5-picture-movement-up', 
etc. Do \\[command-apropos] `big5-picture-movement' to see those commands."
  (interactive "p")
  (if (= -1 arg)				; Comes from big5-insert-*
      (progn
	(if (and (not (eq 10 (char-after (point))))	; Line-feed
		 (not (eobp)))
	    (delete-char 2))
	(forward-char -2)
	(big5-picture-move))
    (while (> arg 0)
      (setq arg (1- arg))
      (big5-move-to-column-force (1+ (current-column)))
      (delete-char -1)
      (insert last-command-event)	; Always a character in this case.
      (if (>= last-command-event 160)
	  (if first-byte-big5
	      (progn
		(forward-char -2)
		(big5-picture-move)
		(setq first-byte-big5 nil))
	    (setq first-byte-big5 t))
	(progn
	  (if first-byte-big5
	      (progn
		(forward-char -2)
		(big5-picture-move)
		(setq first-byte-big5 nil))
	    (progn
	      (forward-char -1)
	      (big5-picture-move))))))))
  
(defun big5-picture-clear-column (arg)
  "Clear out ARG columns after point without moving."
  (interactive "p")
  (let* ((opoint (point))
	 (original-col (current-column))
	 (target-col (+ original-col arg)))
    (big5-move-to-column-force target-col)
    (delete-region opoint (point))
    (save-excursion
     (indent-to (max target-col original-col)))))

(defun big5-picture-backward-clear-column (arg)
  "Clear out ARG columns before point, moving back over them."
  (interactive "p")
  (big5-picture-clear-column (- arg)))

(defun big5-picture-clear-line (arg)
  "Clear out rest of line; if at end of line, advance to next line.
Cleared-out line text goes into the kill ring, as do newlines that are
advanced over.  With argument, clear out (and save in kill ring) that
many lines."
  (interactive "P")
  (if arg
      (progn
       (setq arg (prefix-numeric-value arg))
       (kill-line arg)
       (newline (if (> arg 0) arg (- arg))))
    (if (looking-at "[ \t]*$")
	(kill-ring-save (point) (progn (forward-line 1) (point)))
      (kill-region (point) (progn (end-of-line) (point))))))

(defun big5-picture-newline (arg)
  "Move to the beginning of the following line.
With argument, moves that many lines (up, if negative argument);
always moves to the beginning of a line."
  (interactive "p")
  (if (< arg 0)
      (forward-line arg)
    (while (> arg 0)
      (end-of-line)
      (if (eobp) (newline) (forward-char 1))
      (setq arg (1- arg))))
  ;; This call will go away when Emacs gets real horizontal autoscrolling
  (hscroll-point-visible))

(defun big5-picture-open-line (arg)
  "Insert an empty line after the current line.
With positive argument insert that many lines."
  (interactive "p")
  (save-excursion
   (end-of-line)
   (open-line arg))
  ;; This call will go away when Emacs gets real horizontal autoscrolling
  (hscroll-point-visible))

(defun big5-picture-duplicate-line ()
  "Insert a duplicate of the current line, below it."
  (interactive)
  (save-excursion
   (let ((contents
	  (buffer-substring
	   (progn (beginning-of-line) (point))
	   (progn (big5-picture-newline 1) (point)))))
     (forward-line -1)
     (insert contents))))

;; Like replace-match, but overwrites.
(defun big5-picture-replace-match (newtext fixedcase literal)
  (let (ocolumn change pos)
    (goto-char (setq pos (match-end 0)))
    (setq ocolumn (current-column))
    ;; Make the replacement and undo it, to see how it changes the length.
    (let ((buffer-undo-list nil)
	  list1)
      (replace-match newtext fixedcase literal)
      (setq change (- (current-column) ocolumn))
      (setq list1 buffer-undo-list)
      (while list1
	(setq list1 (primitive-undo 1 list1))))
    (goto-char pos)
    (if (> change 0)
	(delete-region (point)
		       (progn
			 (big5-move-to-column-force (+ change 
						       (current-column)))
			 (point))))
    (replace-match newtext fixedcase literal)
    (if (< change 0)
	(insert-char ?\ (- change)))))

;; Picture Tabs

(defvar big5-picture-tab-chars "!-~"
  "*A character set which controls behavior of commands
\\[big5-picture-set-tab-stops] and \\[big5-picture-tab-search].  It is NOT a
regular expression, any regexp special characters will be quoted.
It defines a set of \"interesting characters\" to look for when setting
\(or searching for) tab stops, initially \"!-~\" (all printing characters).
For example, suppose that you are editing a table which is formatted thus:
| foo		| bar + baz | 23  *
| bubbles	| and + etc | 97  *
and that `big5-picture-tab-chars' is \"|+*\".  Then invoking
\\[big5-picture-set-tab-stops] on either of the previous lines would result
in the following tab stops
		:     :     :     :
Another example - \"A-Za-z0-9\" would produce the tab stops
  :		  :	:     :

Note that if you want the character `-' to be in the set, it must be
included in a range or else appear in a context where it cannot be
taken for indicating a range (e.g. \"-A-Z\" declares the set to be the
letters `A' through `Z' and the character `-').  If you want the
character `\\' in the set it must be preceded by itself: \"\\\\\".

The command \\[big5-picture-tab-search] is defined to move beneath (or to) a
character belonging to this set independent of the tab stops list.")

(defun big5-picture-set-tab-stops (&optional arg)
  "Set value of `tab-stop-list' according to context of this line.
This controls the behavior of \\[big5-picture-tab].  A tab stop is set at
every column occupied by an \"interesting character\" that is preceded
by whitespace.  Interesting characters are defined by the variable
`big5-picture-tab-chars', see its documentation for an example of usage.
With ARG, just (re)set `tab-stop-list' to its default value.  The tab
stops computed are displayed in the minibuffer with `:' at each stop."
  (interactive "P")
  (save-excursion
    (let (tabs)
      (if arg
	  (setq tabs (default-value 'tab-stop-list))
	(let ((regexp (concat "[ \t]+[" (regexp-quote big5-picture-tab-chars) 
			      "]")))
	  (beginning-of-line)
	  (let ((bol (point)))
	    (end-of-line)
	    (while (re-search-backward regexp bol t)
	      (skip-chars-forward " \t")
	      (setq tabs (cons (current-column) tabs)))
	    (if (null tabs)
		(error "No characters in set %s on this line."
		       (regexp-quote big5-picture-tab-chars))))))
      (setq tab-stop-list tabs)
      (let ((blurb (make-string (1+ (nth (1- (length tabs)) tabs)) ?\ )))
	(while tabs
	  (aset blurb (car tabs) ?:)
	  (setq tabs (cdr tabs)))
	(message blurb)))))

(defun big5-picture-tab-search (&optional arg)
  "Move to column beneath next interesting char in previous line.
With ARG move to column occupied by next interesting character in this
line.  The character must be preceded by whitespace.
\"interesting characters\" are defined by variable `big5-picture-tab-chars'.
If no such character is found, move to beginning of line."
  (interactive "P")
  (let ((target (current-column)))
    (save-excursion
      (if (and (not arg)
	       (progn
		 (beginning-of-line)
		 (skip-chars-backward
		  (concat "^" (regexp-quote big5-picture-tab-chars))
		  (point-min))
		 (not (bobp))))
	  (move-to-column target))
      (if (re-search-forward
	   (concat "[ \t]+[" (regexp-quote big5-picture-tab-chars) "]")
	   (save-excursion (end-of-line) (point))
	   'move)
	  (setq target (1- (current-column)))
	(setq target nil)))
    (if target
	(big5-move-to-column-force target)
      (beginning-of-line))))

(defun big5-picture-tab (&optional arg)
  "Tab transparently (just move point) to next tab stop.
With prefix arg, overwrite the traversed text with spaces.  The tab stop
list can be changed by \\[big5-picture-set-tab-stops] and \\[edit-tab-stops].
See also documentation for variable `big5-picture-tab-chars'."
  (interactive "P")
  (let* ((opoint (point)))
    (move-to-tab-stop)
    (if arg
	(let (indent-tabs-mode
	      (column (current-column)))
	  (delete-region opoint (point))
	  (indent-to column)))))

;; Picture Rectangles

(defconst big5-picture-killed-rectangle nil
  "Rectangle killed or copied by \\[big5-picture-clear-rectangle] in Picture 
mode. The contents can be retrieved by \\[big5-picture-yank-rectangle]")

(defun big5-picture-clear-rectangle (start end &optional killp)
  "Clear and save rectangle delineated by point and mark.
The rectangle is saved for yanking by \\[big5-picture-yank-rectangle] 
and replaced with whitespace.  The previously saved rectangle, if any, is 
lost.  
With prefix argument, the rectangle is actually killed, shifting remaining 
text."
  (interactive "r\nP")
  (setq big5-picture-killed-rectangle (big5-picture-snarf-rectangle 
				       start end killp)))

(defun big5-picture-clear-rectangle-to-register (start end register 
						       &optional killp)
  "Clear rectangle delineated by point and mark into REGISTER.
The rectangle is saved in REGISTER and replaced with whitespace.  With
prefix argument, the rectangle is actually killed, shifting remaining text."
  (interactive "r\ncRectangle to register: \nP")
  (set-register register (big5-picture-snarf-rectangle start end killp)))

(defun big5-picture-snarf-rectangle (start end &optional killp)
  (let ((column (current-column))
	(indent-tabs-mode nil))
    (prog1 (save-excursion
             (if killp
                 (delete-extract-rectangle start end)
               (prog1 (extract-rectangle start end)
                      (clear-rectangle start end))))
	   (big5-move-to-column-force column))))

(defun big5-picture-yank-rectangle (&optional insertp)
  "Overlay rectangle saved by \\[big5-picture-clear-rectangle]
The rectangle is positioned with upper left corner at point, overwriting
existing text.  With prefix argument, the rectangle is inserted instead,
shifting existing text.  Leaves mark at one corner of rectangle and
point at the other (diagonally opposed) corner."
  (interactive "P")
  (if (not (consp big5-picture-killed-rectangle))
      (error "No rectangle saved.")
    (big5-picture-insert-rectangle big5-picture-killed-rectangle insertp)))

(defun big5-picture-yank-at-click (click arg)
  "Insert the last killed rectangle at the position clicked on.
Also move point to one end of the text thus inserted (normally the end).
Prefix arguments are interpreted as with \\[yank].
If `mouse-yank-at-point' is non-nil, insert at point
regardless of where you click."
  (interactive "e\nP")
  (or mouse-yank-at-point (mouse-set-point click))
  (big5-picture-yank-rectangle arg))

(defun big5-picture-yank-rectangle-from-register (register &optional insertp)
  "Overlay rectangle saved in REGISTER.
The rectangle is positioned with upper left corner at point, overwriting
existing text.  With prefix argument, the rectangle is
inserted instead, shifting existing text.  Leaves mark at one corner
of rectangle and point at the other (diagonally opposed) corner."
  (interactive "cRectangle from register: \nP")
  (let ((rectangle (get-register register)))
    (if (not (consp rectangle))
	(error "Register %c does not contain a rectangle." register)
      (big5-picture-insert-rectangle rectangle insertp))))

(defun big5-picture-insert-rectangle (rectangle &optional insertp)
  "Overlay RECTANGLE with upper left corner at point.
Optional argument INSERTP, if non-nil causes RECTANGLE to be inserted.
Leaves the region surrounding the rectangle."
  (let ((indent-tabs-mode nil))
    (if (not insertp)
	(save-excursion
	  (delete-rectangle (point)
			    (progn
			      (big5-picture-forward-column (length 
							    (car rectangle)))
			      (big5-picture-move-down (1- (length rectangle)))
			      (point)))))
    (push-mark)
    (insert-rectangle rectangle)))

;; Picture Keymap, entry and exit points.

(defconst big5-picture-mode-map nil)

(defun big5-picture-substitute (oldfun newfun)
  (substitute-key-definition oldfun newfun big5-picture-mode-map global-map))

(if (not big5-picture-mode-map)
    (progn
      (setq big5-picture-mode-map (list 'keymap (make-vector 256 nil)))
      (big5-picture-substitute 'self-insert-command 'big5-picture-self-insert)
      (big5-picture-substitute 'completion-separator-self-insert-command
			  'big5-picture-self-insert)
      (big5-picture-substitute 'completion-separator-self-insert-autofilling
			  'big5-picture-self-insert)
      (big5-picture-substitute 'forward-char 'big5-picture-forward-column)
      (big5-picture-substitute 'backward-char 'big5-picture-backward-column)
      (big5-picture-substitute 'delete-char 'big5-picture-clear-column)
      ;; There are two possibilities for what is normally on DEL.
      (big5-picture-substitute 'backward-delete-char-untabify 
				'big5-picture-backward-clear-column)
      (big5-picture-substitute 'delete-backward-char 
				'big5-picture-backward-clear-column)
      (big5-picture-substitute 'kill-line 'big5-picture-clear-line)
      (big5-picture-substitute 'open-line 'big5-picture-open-line)
;      (big5-picture-substitute 'newline 'big5-picture-newline)
      (big5-picture-substitute 'newline 'big5-picture-go-starting-line)
      (big5-picture-substitute 'newline-and-indent 
				'big5-picture-duplicate-line)
      (big5-picture-substitute 'next-line 'big5-picture-move-down)
      (big5-picture-substitute 'previous-line 'big5-picture-move-up)
      (big5-picture-substitute 'beginning-of-line 
				'big5-picture-beginning-of-line)
      (big5-picture-substitute 'end-of-line 'big5-picture-end-of-line)
      (define-key big5-picture-mode-map "\C-c\C-d" 'delete-char)
      (define-key big5-picture-mode-map "\e\t" 'big5-picture-toggle-tab-state)
      (define-key big5-picture-mode-map "\t" 'big5-picture-tab)
      (define-key big5-picture-mode-map "\e\t" 'big5-picture-tab-search)
      (define-key big5-picture-mode-map "\C-c\t" 'big5-picture-set-tab-stops)
      (define-key big5-picture-mode-map "\C-c\C-k" 
	'big5-picture-clear-rectangle)
      (define-key big5-picture-mode-map "\C-c\C-w" 
	'big5-picture-clear-rectangle-to-register)
      (define-key big5-picture-mode-map "\C-c\C-y" 
	'big5-picture-yank-rectangle)
      (define-key big5-picture-mode-map "\C-c\C-x" 
	'big5-picture-yank-rectangle-from-register)
      (define-key big5-picture-mode-map "\C-c\C-c" 'big5-picture-mode-exit)
      (define-key big5-picture-mode-map "\C-c\C-f" 'big5-picture-motion)
      (define-key big5-picture-mode-map "\C-c\C-b" 
	'big5-picture-motion-reverse)
      (define-key big5-picture-mode-map "\C-c<" 'big5-picture-movement-left)
      (define-key big5-picture-mode-map "\C-c>" 'big5-picture-movement-right)
      (define-key big5-picture-mode-map "\C-c^" 'big5-picture-movement-up)
      (define-key big5-picture-mode-map "\C-c." 'big5-picture-movement-down)
      (define-key big5-picture-mode-map "\C-c`" 'big5-picture-movement-nw)
      (define-key big5-picture-mode-map "\C-c'" 'big5-picture-movement-ne)
      (define-key big5-picture-mode-map "\C-c/" 'big5-picture-movement-sw)
      (define-key big5-picture-mode-map "\C-c\\" 'big5-picture-movement-se)
      ))

(defvar big5-picture-mode-hook nil
  "If non-nil, its value is called on entry to Picture mode.
Picture mode is invoked by the command \\[big5-picture-mode].")

(defvar big5-picture-mode-old-local-map)
(defvar big5-picture-mode-old-mode-name)
(defvar big5-picture-mode-old-major-mode)
(defvar big5-picture-mode-old-truncate-lines)

(defun big5-picture-mode ()
  "Switch to Picture mode, in which a quarter-plane screen model is used.
Printing characters replace instead of inserting themselves with motion
afterwards settable by these commands:
  C-c <	  Move left after insertion.
  C-c >	  Move right after insertion.
  C-c ^	  Move up after insertion.
  C-c .	  Move down after insertion.
  C-c `	  Move northwest (nw) after insertion.
  C-c '	  Move northeast (ne) after insertion.
  C-c /	  Move southwest (sw) after insertion.
  C-c \\   Move southeast (se) after insertion.
The current direction is displayed in the mode line.  The initial
direction is right.  Whitespace is inserted and tabs are changed to
spaces when required by movement.  You can move around in the buffer
with these commands:
  \\[big5-picture-move-down]   Move vertically to SAME column in previous line.
  \\[big5-picture-move-up]     Move vertically to SAME column in next line.
  \\[big5-picture-end-of-line] Move to column following last non-whitespace character.
  \\[big5-picture-forward-column]	  Move right inserting spaces if required.
  \\[big5-picture-backward-column]	  Move left changing tabs to spaces if required.
  C-c C-f Move in direction of current picture motion.
  C-c C-b Move in opposite direction of current picture motion.
  Return  Move to beginning of next line.
You can edit tabular text with these commands:
  M-Tab	  Move to column beneath (or at) next interesting character.
	    `Indents' relative to a previous line.
  Tab	  Move to next stop in tab stop list.
  C-c Tab Set tab stops according to context of this line.
	    With ARG resets tab stops to default (global) value.
	    See also documentation of variable	big5-picture-tab-chars
	    which defines \"interesting character\".  You can manually
	    change the tab stop list with command \\[edit-tab-stops].
You can manipulate text with these commands:
  C-d	  Clear (replace) ARG columns after point without moving.
  C-c C-d Delete char at point - the command normally assigned to C-d.
  \\[big5-picture-backward-clear-column]  Clear (replace) ARG columns before point, moving back over them.
  \\[big5-picture-clear-line]	  Clear ARG lines, advancing over them.	 The cleared
	    text is saved in the kill ring.
  \\[big5-picture-open-line]	  Open blank line(s) beneath current line.
You can manipulate rectangles with these commands:
  C-c C-k Clear (or kill) a rectangle and save it.
  C-c C-w Like C-c C-k except rectangle is saved in named register.
  C-c C-y Overlay (or insert) currently saved rectangle at point.
  C-c C-x Like C-c C-y except rectangle is taken from named register.
  \\[copy-rectangle-to-register]   Copies a rectangle to a register.
  \\[advertised-undo]   Can undo effects of rectangle overlay commands
	    commands if invoked soon enough.
You can return to the previous mode with:
  C-c C-c Which also strips trailing whitespace from every line.
	    Stripping is suppressed by supplying an argument.

Entry to this mode calls the value of  big5-picture-mode-hook  if non-nil.

Note that Picture mode commands will work outside of Picture mode, but
they are not defaultly assigned to keys."
  (interactive)
  (if (eq major-mode 'big5-picture-mode)
      (error "You are already editing a Big5 picture.")
    (make-local-variable 'big5-picture-mode-old-local-map)
    (setq big5-picture-mode-old-local-map (current-local-map))
    (use-local-map big5-picture-mode-map)
    (make-local-variable 'big5-picture-mode-old-mode-name)
    (setq big5-picture-mode-old-mode-name mode-name)
    (make-local-variable 'big5-picture-mode-old-major-mode)
    (setq big5-picture-mode-old-major-mode major-mode)
    (setq major-mode 'big5-picture-mode)
    (make-local-variable 'big5-picture-killed-rectangle)
    (setq big5-picture-killed-rectangle nil)
    (make-local-variable 'tab-stop-list)
    (setq tab-stop-list (default-value 'tab-stop-list))
    (make-local-variable 'big5-picture-tab-chars)
    (setq big5-picture-tab-chars (default-value 'big5-picture-tab-chars))
    (make-local-variable 'big5-picture-vertical-step)
    (make-local-variable 'big5-picture-horizontal-step)
    (make-local-variable 'big5-picture-mode-old-truncate-lines)
    (setq big5-picture-mode-old-truncate-lines truncate-lines)
    (setq truncate-lines t)
    (make-local-variable 'starting-line)
    (setq starting-line (count-lines (point-min) (point)))
    (make-local-variable 'starting-column)
    (setq starting-column (current-column))
    (make-local-variable 'first-byte-big5)

    ;;; Default: down
    (setq big5-normal-line-column 2)
    (big5-picture-set-motion 1 0 "down")

    ;; edit-big5-picture-hook is what we used to run, big5-picture-mode-hook is in doc.
    (run-hooks 'edit-big5-picture-hook 'big5-picture-mode-hook)
    (big5-mode 1)
    (message "Type %s in this buffer to return it to %s mode."
	     (substitute-command-keys "\\[big5-picture-mode-exit]")
	     big5-picture-mode-old-mode-name)))

(defalias 'edit-big5-picture 'big5-picture-mode)

(defun big5-picture-mode-exit (&optional nostrip)
  "Undo big5-picture-mode and return to previous major mode.
With no argument strips whitespace from end of every line in Picture buffer
  otherwise just return to previous mode."
  (interactive "P")
  (if (not (eq major-mode 'big5-picture-mode))
      (error "You aren't editing a Picture.")
    (if (not nostrip) (big5-picture-clean))
    (setq mode-name big5-picture-mode-old-mode-name)
    (use-local-map big5-picture-mode-old-local-map)
    (setq major-mode big5-picture-mode-old-major-mode)
    (kill-local-variable 'tab-stop-list)
    (setq truncate-lines big5-picture-mode-old-truncate-lines)
    (force-mode-line-update)))

(defun big5-picture-clean ()
  "Eliminate whitespace at ends of lines."
  (save-excursion
   (goto-char (point-min))
   (while (re-search-forward "[ \t][ \t]*$" nil t)
     (delete-region (match-beginning 0) (point)))))

