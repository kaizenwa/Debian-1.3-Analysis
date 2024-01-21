;;; m-big5.el: A Chinese mode for Emacs with the Big5 encoding
;;; Georges KO (kott@ccms.ntu.edu.tw)

;;; $Log$

;;; - the cursor movement code comes from cemacs.el.
;;; - the big5-insert-*'s mechanism has been taken from iso-insert.


;;; Cursor movement (cemacs.el)

;; From its author:
;; >NOTE: The code below will not work very will with Big5 in mixed
;; >Chinese/ASCII files.  The problem is that, in Big5, the second byte
;; >of a Chinese character can be an ASCII byte, so it is harder to
;; >distinguish Chinese from ASCII.  Can someone suggest a way to get
;; >around this problem?
;;
;; Me:
;; Big5's encoding looks like:
;;      First byte: >= 160 (xA0)
;;      Second byte: 64 (x40)  <= x <= 126 (x7e) and 
;;                   128 (x80) <= x <=254 (xfe)
;; Recent Eten's encoding uses first byte less than 160, to encode things
;; such as keyboard's keys, BoPoMoFo's combinations, 1 to 100's, accented
;; characters, icons, etc.
;; This package ignores them (for the moment).

;; TODO:
;; - Write a function to recognize if two bytes form a Big5 character.

;; replacement for forward-char
(defun cemacs-forward-char (&optional arg)
  "cemacs replacement for forward-char\n
Move point right ARG characters.
On reaching end of buffer, stop and signal error.
ARG defaults to 1.\n
arguments: (&optional n)\n"
  (interactive "p")
  (while (> arg 0)
    (setq arg (- arg 1))
    (cond
      ((= (point) (point-max))
       (message "End of buffer")
       (setq arg 0))
      ((>= (char-after (point)) 160)
       (forward-char 2))
      (t (forward-char 1)))
))

;; replacement for backward-char
(defun cemacs-backward-char (&optional arg)
  "cemacs replacement for backward-char\n
Move point left ARG characters.
On reaching end of buffer, stop and signal error.
ARG defaults to 1.\n
arguments: (&optional n)\n"
  (interactive "p")
  (while (> arg 0)
    (setq arg (- arg 1))
    (cond
      ((= (point) (point-min))
       (message "Beginning of buffer")
       (setq arg 0))
      ((>= (char-after (- (point) 2)) 160)
       (backward-char 2))
      (t (backward-char 1)))
))

;; replacement for delete-char
(defun cemacs-delete-char (&optional arg)
  "cemacs replacement for delete-char\n
Delete the following ARG characters.
On reaching end of buffer, stop and signal error.
ARG defaults to 1.\n
arguments: (&optional n)\n"
  (interactive "p")
  (while (> arg 0)
    (setq arg (- arg 1))
    (cond
      ((= (point) (point-max))
       (message "End of buffer")
       (setq arg 0))
      ((>= (char-after (point)) 160)
       (delete-char 2))
      (t (delete-char 1)))
))

;; replacement for backward-delete-char and delete-backward-char
(defun cemacs-backward-delete-char (&optional arg)
  "cemacs replacement for backward-delete-char and delete-backward-char\n
Delete ARG characters backward, changing tabs into spaces.
On reaching end of buffer, stop and signal error.
ARG defaults to 1.\n
arguments: (&optional n)\n"
  (interactive "p")
  (while (> arg 0)
    (setq arg (- arg 1))
    (cond
      ((= (point) (point-min))
       (message "Beginning of buffer")
       (setq arg 0))
      ((>= (char-after (- (point) 2)) 160)
       (backward-delete-char 2))
      (t (backward-delete-char 1)))
))

;; replacement for backward-delete-char-untabify
(defun cemacs-backward-delete-char-untabify (&optional arg)
  "cemacs replacement for backward-delete-char-untabify\n
Delete ARG characters backward, changing tabs into spaces.
On reaching end of buffer, stop and signal error.
ARG defaults to 1.\n
arguments: (&optional n)\n"
  (interactive "p")
  (while (> arg 0)
    (setq arg (- arg 1))
    (cond
      ((= (point) (point-min))
       (message "Beginning of buffer")
       (setq arg 0))
      ((>= (char-after (- (point) 2)) 160)
       (backward-delete-char-untabify 2))
      (t (backward-delete-char-untabify 1)))
))

;;; Insertion of Big5's special characters
;; TODO: drawing facilities...
;;       better keybindings (especially for graphical stuff)
(defvar big5-map nil "Keymap for Big5 character insertion.")

(if big5-map nil
   (setq big5-map (make-keymap))
   (define-key big5-map "p" 'big5-picture-mode)
   (define-key big5-map "(" 'big5-insert-open-parenthesis)
   (define-key big5-map ")" 'big5-insert-close-parenthesis)
   (define-key big5-map "[" 'big5-insert-open-bracket)
   (define-key big5-map "]" 'big5-insert-close-bracket)
   (define-key big5-map "{" 'big5-insert-open-curly-bracket)
   (define-key big5-map "}" 'big5-insert-close-curly-bracket)
   (define-key big5-map "<" 'big5-insert-open-quote)
   (define-key big5-map ">" 'big5-insert-close-quote)
   (define-key big5-map "`" 'big5-insert-open-simple-quote)
   (define-key big5-map "'" 'big5-insert-close-simple-quote)
   (define-key big5-map "\"" 'big5-insert-double-quote)
   (define-key big5-map "." 'big5-insert-period)
   (define-key big5-map "-" 'big5-insert-dash)
   (define-key big5-map "," 'big5-insert-comma)
   (define-key big5-map ";" 'big5-insert-semicolon)
   (define-key big5-map ":" 'big5-insert-colon)
   (define-key big5-map "?" 'big5-insert-question)
   (define-key big5-map "!" 'big5-insert-exclamation)
   (define-key big5-map "*" 'big5-insert-star)
   (define-key big5-map "#" 'big5-insert-number)
   (define-key big5-map "&" 'big5-insert-ampersand)
   (define-key big5-map "a" 'big5-insert-arrow)
   (define-key big5-map "/" 'big5-insert-slash)
   (define-key big5-map "\\" 'big5-insert-backslash)
   (define-key big5-map "|" 'big5-insert-bar)
   (define-key big5-map "_" 'big5-insert-underline)
   (define-key big5-map "c" 'big5-insert-circle-shape)
   (define-key big5-map "t" 'big5-insert-triangle-shape)
   (define-key big5-map "s" 'big5-insert-square-shape)
   (define-key big5-map "r"   (make-sparse-keymap))
   (define-key big5-map "rh" 'big5-insert-rectangle-height)
   (define-key big5-map "rw" 'big5-insert-rectangle-width)
   (define-key big5-map "1" 'big5-insert-one)
   (define-key big5-map "2" 'big5-insert-two)
   (define-key big5-map "3" 'big5-insert-three)
   (define-key big5-map "4" 'big5-insert-four)
   (define-key big5-map "5" 'big5-insert-five)
   (define-key big5-map "6" 'big5-insert-six)
   (define-key big5-map "7" 'big5-insert-seven)
   (define-key big5-map "8" 'big5-insert-eight)
   (define-key big5-map "9" 'big5-insert-nine)
   (define-key big5-map "0" 'big5-insert-zero)
   (define-key big5-map "f"   (make-sparse-keymap))
   (define-key big5-map "fr"  'big5-insert-round-frame)
   (define-key big5-map "fl"  'big5-insert-line-frame)
   (define-key big5-map "fs"  (make-sparse-keymap))
   (define-key big5-map "fs1" (make-sparse-keymap))
   (define-key big5-map "fs11" 'big5-insert-sharp-frame-11)
   (define-key big5-map "fs12" 'big5-insert-sharp-frame-12)
   (define-key big5-map "fs2" (make-sparse-keymap))
   (define-key big5-map "fs21" 'big5-insert-sharp-frame-21)
   (define-key big5-map "fs22" 'big5-insert-sharp-frame-22)
   (define-key big5-map "+" 'big5-insert-plus)
   (define-key big5-map "-" 'big5-insert-minus)
   (define-key big5-map "x" 'big5-insert-multiply)
   (define-key big5-map "^" 'big5-insert-square-root)
   (define-key big5-map "=" 'big5-insert-egal)
   (define-key big5-map "m" (make-sparse-keymap))
   (define-key big5-map "m<" 'big5-insert-inferior)
   (define-key big5-map "m>" 'big5-insert-superior)
   (define-key big5-map "mm" 'big5-insert-other-maths)
   (define-key big5-map "y" 'big5-insert-symbol)
   (define-key big5-map "~" 'big5-insert-tild)
   (define-key big5-map "$" 'big5-insert-currency)
   (define-key big5-map "@" 'big5-insert-at)
   (define-key big5-map "o" 'big5-insert-degree)
   (define-key big5-map "u" 'big5-insert-unit)
   (define-key big5-map " " 'big5-insert-repeat)
   (define-key big5-map "i" 'big5-insert-intonation)
)

;; The main insertion function
;; TODO: improve user-interface

(defun big5-insert-choice (lvector)
  "This function lets you choose which characters to insert. lvector is
a list of vectors, where each vector contains a set of characters to insert."
  (let* ((l (length lvector))
	 (i 1)
	 (prompt "")
	 (answer 0))
    (if (= l 1)
	(insert (setq big5-last-inserted1
		      (elt (nth 0 lvector) 0))
		(setq big5-last-inserted2
		      (elt (nth 0 lvector) 1)))
      (progn
	(while (<= i l)
	  (setq prompt (concat prompt (format "%d:%c%c  "
					      i
					      (elt (nth (1- i) lvector) 0)
					      (elt (nth (1- i) lvector) 1))))
	  (setq i (1+ i)))
	(setq prompt (concat prompt ">"))
	(while (= answer 0)
	  (setq answer (string-to-number (read-no-blanks-input prompt)))
	  (if (not (equal nil answer))
	      (if (> answer l)
		  (setq answer 0)
		(progn
		  (insert (setq big5-last-inserted1 
				(elt (nth (1- answer) lvector) 0))
			  (setq big5-last-inserted2 
				(elt (nth (1- answer) lvector) 1)))
		  (setq answer 1)))))))
    (if (eq major-mode 'big5-picture-mode)
	(big5-picture-self-insert -1)	; big5-picture-mode specific...
      )
    )
  )

(defun big5-insert-repeat (times)
  (interactive "p")
  (let ((i 1))
    (while (<= i times)
      (insert big5-last-inserted1 big5-last-inserted2)
      (setq i (1+ i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: 1, 2, 3, etc... should have Chinese 1, 2. 3...

;;; Parenthesis
(defun big5-insert-open-parenthesis  ()
  (interactive "*")
  (big5-insert-choice '([161 125] [161 163] [161 93] [161 101]
			[161 95] [161 103])))
(defun big5-insert-close-parenthesis ()
  (interactive "*")
  (big5-insert-choice '([161 126] [161 164] [161 94] [161 102]
			[161 96] [161 104])))

;;; Brackets
(defun big5-insert-open-bracket  ()
  (interactive "*")
  (big5-insert-choice '([161 105] [161 107] [198 228])))
(defun big5-insert-close-bracket  ()
  (interactive "*")
  (big5-insert-choice '([161 106] [161 108] [198 229] )))

;;; Curly brackets
(defun big5-insert-open-curly-bracket  ()
  (interactive "*")
  (big5-insert-choice '([161 161] [161 97] [161 99])))
(defun big5-insert-close-curly-bracket  ()
  (interactive "*")
  (big5-insert-choice '([161 162] [161 98] [161 100])))

;;; Quotes
(defun big5-insert-open-quote ()
  (interactive "*")
  (big5-insert-choice '([161 113] [161 109] [161 117] [161 121]
			[161 115] [161 111] [161 119] [161 123])))
(defun big5-insert-close-quote ()
  (interactive "*")
  (big5-insert-choice '([161 114] [161 110] [161 118] [161 122]
			[161 116] [161 112] [161 120] [161 124])))

;;; Simple quotes
(defun big5-insert-open-simple-quote ()
  (interactive "*")
  (big5-insert-choice '([161 171] [161 165])))
(defun big5-insert-close-simple-quote ()
  (interactive "*")
  (big5-insert-choice '([161 172] [161 166])))

;;; Double quotes
(defun big5-insert-double-quote ()
  (interactive "*")
  (big5-insert-choice '([161 167] [161 169] [161 168] [161 170])))

;;; Punctuation
(defun big5-insert-period () (interactive "*")
  (big5-insert-choice '([161 67] [161 69] [161 80] [161 68] [161 79]
			[161 75] [161 76]
			[161 198] [161 199] [161 200] [161 201])))
(defun big5-insert-dash () (interactive "*")
  (big5-insert-choice '([161 86]  [161 88])))
(defun big5-insert-comma () (interactive "*")
  (big5-insert-choice '([161 65] [161 77] [161 66] [161 78])))
(defun big5-insert-semicolon () (interactive "*")
  (big5-insert-choice '([161 70] [161 81])))
(defun big5-insert-colon () (interactive "*")
  (big5-insert-choice '([161 71] [161 74] [161 82])))
(defun big5-insert-question () (interactive "*")
  (big5-insert-choice '([161 72] [161 83])))
(defun big5-insert-exclamation () (interactive "*")
  (big5-insert-choice '([161 73] [161 84])))
(defun big5-insert-star () (interactive "*")
  (big5-insert-choice '([161 175] [161 206] [161 176] [161 184] [161 185])))
(defun big5-insert-number () (interactive "*")
  (big5-insert-choice '([161 173] [161 204])))
(defun big5-insert-ampersand () (interactive "*")
  (big5-insert-choice '([161 174] [161 205])))

;;; Arrows
(defun big5-insert-arrow () (interactive "*") ; like keypad !
  (big5-insert-choice '([161 250] [161 245] [161 251]
			[161 246] [164 197] [161 247]
			[161 248] [161 244] [161 249])))
(defun big5-insert-slash () (interactive "*")
  (big5-insert-choice '( [161 210] [161 254] [162 172] [162 65])))
(defun big5-insert-backslash () (interactive "*")
  (big5-insert-choice '([162 64] [162 173] [162 66])))

;;; Others
(defun big5-insert-bar () (interactive "*")
  (big5-insert-choice '([161 91] [161 85] [161 87] [161 89] 
			[161 253] [161 252])))
(defun big5-insert-underline () (interactive "*")
  (big5-insert-choice '( [161 90] [161 196] [161 197] [161 194] [161 195] 
			 [161 92] [161 203] [161 202]
			 [161 200] [161 201] [161 198] [161 199])))
(defun big5-insert-circle-shape () (interactive "*")
  (big5-insert-choice '([161 179] [161 183] [161 243] [161 242] [161 180]
			[161 192])))
(defun big5-insert-triangle-shape () (interactive "*")
  (big5-insert-choice '([161 181] [161 182] [161 190] [161 191]
			[162 168] [162 169] [162 170] [162 171])))
(defun big5-insert-square-shape () (interactive "*")
  (big5-insert-choice '([161 186] [161 187] [161 188] [161 189])))
(defun big5-insert-rectangle-height () (interactive "*")
  (big5-insert-choice '([162 98] [162 99] [162 100] [162 101] [162 102]
			[162 103] [162 104] [162 105])))
(defun big5-insert-rectangle-width () (interactive "*")
  (big5-insert-choice '([162 106] [162 107] [162 108] [162 109] [162 110]
			[162 111] [162 112])))
(defun big5-insert-one () (interactive "*")
  (big5-insert-choice '([162 176] [198 161] [198 171] [198 181] [162 185])))
(defun big5-insert-two () (interactive "*")
  (big5-insert-choice '([162 177] [198 162] [198 172] [198 182] [162 186])))
(defun big5-insert-three () (interactive "*")
  (big5-insert-choice '([162 178] [198 163] [198 173] [198 183] [162 187])))
(defun big5-insert-four () (interactive "*")
  (big5-insert-choice '([162 179] [198 164] [198 174] [198 184] [162 188])))
(defun big5-insert-five () (interactive "*")
  (big5-insert-choice '([162 180] [198 165] [198 175] [198 185] [162 189])))
(defun big5-insert-six () (interactive "*")
  (big5-insert-choice '([162 181] [198 166] [198 176] [198 186] [162 190])))
(defun big5-insert-seven () (interactive "*")
  (big5-insert-choice '([162 182] [198 167] [198 177] [198 187] [162 191])))
(defun big5-insert-eight () (interactive "*")
  (big5-insert-choice '([162 183] [198 168] [198 178] [198 188] [162 192])))
(defun big5-insert-nine () (interactive "*")
  (big5-insert-choice '([162 183] [198 169] [198 179] [198 189] [162 193])))
(defun big5-insert-zero () (interactive "*")
  (big5-insert-choice '([162 175] [198 170] [198 180] [198 190] [162 194])))

;;; Drawing graphics
(defun big5-insert-sharp-frame-11 () (interactive "*")
  (big5-insert-choice '([162 124] [162 114] [162 125]
			[162 117] [162 113] [162 116]
			[162 122] [162 115] [162 123])))
(defun big5-insert-sharp-frame-22 () (interactive "*")
  (big5-insert-choice '([249 227] [249 228] [249 229]
			[249 224] [249 225] [249 226]
			[249 221] [249 222] [249 223])))
(defun big5-insert-sharp-frame-21 () (interactive "*")
  (big5-insert-choice '([249 236] [249 237] [249 238]
			[249 233] [249 234] [249 235]
			[249 230] [249 231] [249 232])))
(defun big5-insert-sharp-frame-12 () (interactive "*")
  (big5-insert-choice '([249 245] [249 246] [249 247]
			[249 242] [249 243] [249 244]
			[249 239] [249 240] [249 241])))
(defun big5-insert-round-frame () (interactive "*")
  (big5-insert-choice '([162 162] [162 163] [162 126] [162 161]
			[249 250] [249 251] [249 252] [249 253])))
(defun big5-insert-line-frame () (interactive "*")
  (big5-insert-choice '([162 118] [162 119] [249 249] 
			[162 120] [162 121] [249 248])))

;;; Mathematics
(defun big5-insert-plus () (interactive "*")
  (big5-insert-choice '([161 222] [161 207] [161 211])))
(defun big5-insert-minus () (interactive "*")
  (big5-insert-choice '([161 223] [161 208] [161 211])))
(defun big5-insert-multiply () (interactive "*")
  (big5-insert-choice '([161 209])))
(defun big5-insert-square-root () (interactive "*")
  (big5-insert-choice '([161 212])))
(defun big5-insert-equal () (interactive "*")
  (big5-insert-choice '([161 226] [161 215] [161 218] [161 220] [161 221])))
(defun big5-insert-inferior () (interactive "*")
  (big5-insert-choice '([161 224] [161 213] [161 216] )))
(defun big5-insert-superior () (interactive "*")
  (big5-insert-choice '([161 225] [161 214] [161 217] )))
(defun big5-insert-other-maths () (interactive "*")
  (big5-insert-choice '([161 228] [161 229] [161 230] [161 231] [161 232]
			[161 233] [161 234] [161 235] [161 219] 
			[161 236] [161 237])))
(defun big5-insert-symbol () (interactive "*")
  (big5-insert-choice '([161 238] [161 239] [161 240] [161 241])))
(defun big5-insert-tild () (interactive "*")
  (big5-insert-choice '([161 227])))
(defun big5-insert-currency () (interactive "*")
  (big5-insert-choice '([162 76] [162 67] [162 70] [162 71]
			[162 68] [162 69])))
(defun big5-insert-percent () (interactive "*")
  (big5-insert-choice '([162 77] [162 72])))
(defun big5-insert-at () (interactive "*")
  (big5-insert-choice '([162 78] [162 73])))
(defun big5-insert-degree () (interactive "*")
  (big5-insert-choice '([162 88] [162 74] [162 75])))
(defun big5-insert-unit () (interactive "*")
  (big5-insert-choice '([162 80] [162 81] [162 82] [162 83] [162 84]
			[162 85] [162 86] [162 87])))
(defun big5-insert-intonation () '(interactive "*")
  (big5-insert-choice '([163 187] [163 189] [163 190] [163 191])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-input-mode
  (car (current-input-mode)) (car (cdr (current-input-mode))) 0)
(local-set-key [27 91 ?D] 'cemacs-backward-char)

(substitute-key-definition 'forward-char 'cemacs-forward-char big5-map)
(substitute-key-definition 'backward-char 'cemacs-backward-char big5-map)
(substitute-key-definition 'delete-char 'cemacs-delete-char big5-map)
(substitute-key-definition 'delete-backward-char
  'cemacs-backward-delete-char big5-map)
(substitute-key-definition 'backward-delete-char
  'cemacs-backward-delete-char big5-map)
(substitute-key-definition 'backward-delete-char-untabify
  'cemacs-backward-delete-char-untabify big5-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "m-big5-picture.el")