;; Display tables...
;;; ISOMS

(setq isoms-display-table (make-display-table))
(aset isoms-display-table 161 [173]) ; ¡ (inverted !)
(if isoms850 (aset isoms-display-table 162 [189]) ; ¢ (cents)
  (aset isoms-display-table 162 [155]))
(aset isoms-display-table 163 [156]) ; £ (pounds)
(if isoms850
    (aset isoms-display-table 164 [207])  ; ¤ (a circle with 4 dots): space
  (aset isoms-display-table 164 [32]))  ;
(aset isoms-display-table 165 [157]) ; ¥ (Yen)
(aset isoms-display-table 166 [124]) ; ¦ (vertical bar)
(if isoms850 (aset isoms-display-table 167 [245]) ; § (paragraph) PC-850
  (aset isoms-display-table 167 [32]))
(if isoms850 (aset isoms-display-table 168 [249]) ; ¨ (trema) PC-850
  (aset isoms-display-table 168 [34])) ; "
(if isoms850 (aset isoms-display-table 169 [184]) ; © (copyright) PC-850
  (aset isoms-display-table 169 [40 99 41])) ; (c)
(aset isoms-display-table 170 [166]) ; ª (superscript a)
(aset isoms-display-table 171 [174]) ; « (opening French guillemets)
(aset isoms-display-table 172 [170]) ; ¬ (logical not)
(if isoms850 (aset isoms-display-table 173 [240]) ; ­ (small dash) PC-850
  (aset isoms-display-table 173 [45]))	; -
(if isoms850 (aset isoms-display-table 174 [184]) ; ® (Registered) PC-850
  (aset isoms-display-table 174 [40 114 41])) ; (r)
(if isoms850 (aset isoms-display-table 175 [238]) ; ¯ (overline) PC-850
  (aset isoms-display-table 175 [45]))	; -
(aset isoms-display-table 176 [248]) ; ° (degree)
(aset isoms-display-table 177 [241]) ; ± (+ over -)
(aset isoms-display-table 178 [253]) ; ² (2 superscript)
(if isoms850 (aset isoms-display-table 179 [252]) ; ³ (3 superscript) PC-850
  (aset isoms-display-table 179 [94 51])) ; ^3
(if isoms850 (aset isoms-display-table 180 [239]) ; ´ (acute accent) PC-850
  (aset isoms-display-table 180 [39]))	; '
(aset isoms-display-table 181 [230])	; µ (Greek mu)
(if isoms850 (aset isoms-display-table 182 [244]) ; ¶ (flipped double P) PC-850
  (aset isoms-display-table 182 [32]))
(aset isoms-display-table 183 [250])	; · (multiplication dot)
(if isoms850 (aset isoms-display-table 184 [247]) ; ¸ (cedilla) PC-850
  (aset isoms-display-table 184 [44]))	; ,
(if isoms850 (aset isoms-display-table 185 [251]) ; ¹ (1 superscript) PC-850
  (aset isoms-display-table 185 [94 49])) ; ^1
(aset isoms-display-table 186 [167])	; º (o superscript)
(aset isoms-display-table 187 [175])	; » (closing French guillemets)
(aset isoms-display-table 188 [172])	; ¼ (1/4)
(aset isoms-display-table 189 [171])	; ½ (1/2)
(if isoms850 (aset isoms-display-table 190 [243]) ; ¾ (3/4) PC-850
  (aset isoms-display-table 190 [51 47 52])) ; 3/4
(aset isoms-display-table 191 [168])	; ¿ (inverted ?)
(if isoms850 (aset isoms-display-table 192 [183]) ; À
  (if isoms850-one2one (aset isoms-display-table [65])
    (if prefix-accent (aset isoms-display-table 192 [96 65]) ; `A
      (aset isoms-display-table 192 [65 96])))) ; A`
(if isoms850 (aset isoms-display-table 193 [181]) ; Á
  (if isoms850-one2one (aset isoms-display-table [65])
    (if prefix-accent (aset isoms-display-table 193 [39 65]) ; 'A
      (aset isoms-display-table 193 [65 39])))) ; A'
(if isoms850 (aset isoms-display-table 194 [182]) ; Â
  (if isoms850-one2one (aset isoms-display-table [65])
    (if prefix-accent (aset isoms-display-table 194 [94 65]) ; ^A
      (aset isoms-display-table 194 [65 94])))) ; A^
(if isoms850 (aset isoms-display-table 195 [199]) ; Ã
  (if isoms850-one2one (aset isoms-display-table [65])
    (if prefix-accent (aset isoms-display-table 195 [126 65]) ; ~A
      (aset isoms-display-table 195 [65 126])))) ; A~
(aset isoms-display-table 196 [142]) ; Ä (A trema)
(aset isoms-display-table 197 [143]) ; Å (A circle)
(aset isoms-display-table 198 [146]) ; Æ (AE with lig.)
(aset isoms-display-table 199 [128]) ; Ç (C cedilla)
(if isoms850 (aset isoms-display-table 200 [212]) ; È
  (if isoms850-one2one (aset isoms-display-table [69])
    (if prefix-accent (aset isoms-display-table 200 [96 69]) ; `E
      (aset isoms-display-table 200 [69 96])))) ; E`
(aset isoms-display-table 201 [144]) ; É
(if isoms850 (aset isoms-display-table 202 [210]) ; Ê
  (if isoms850-one2one (aset isoms-display-table [69])
    (if prefix-accent (aset isoms-display-table 202 [94 69]) ; ^E
      (aset isoms-display-table 202 [69 94])))) ; E^
(if isoms850 (aset isoms-display-table 203 [211]) ; Ë
  (if isoms850-one2one (aset isoms-display-table [69])
    (if prefix-accent (aset isoms-display-table 203 [34 69]) ; "E
      (aset isoms-display-table 203 [69 34])))) ; E^
(if isoms850 (aset isoms-display-table 204 [222]) ; Ì
  (if isoms850-one2one (aset isoms-display-table [73])
    (if prefix-accent (aset isoms-display-table 204 [96 73]) ; `I
      (aset isoms-display-table 204 [73 96])))) ; I'
(if isoms850 (aset isoms-display-table 205 [214]) ; Í
  (if isoms850-one2one (aset isoms-display-table [73])
    (if prefix-accent (aset isoms-display-table 205 [39 73]) ; 'I
      (aset isoms-display-table 205 [73 39])))) ; I'
(if isoms850 (aset isoms-display-table 206 [215]) ; Î
  (if isoms850-one2one (aset isoms-display-table [73])
    (if prefix-accent (aset isoms-display-table 206 [94 73]) ; ^I
      (aset isoms-display-table 206 [73 94])))) ; I^
(if isoms850 (aset isoms-display-table 207 [216]) ; Ï
  (if isoms850-one2one (aset isoms-display-table [73])
    (if prefix-accent (aset isoms-display-table 207 [34 73]) ; "I
      (aset isoms-display-table 207 [73 34])))) ; I"
(if isoms850 (aset isoms-display-table 208 [209]) ; Ğ
  (if isoms850-one2one (aset isoms-display-table [68])
    (if prefix-accent (aset isoms-display-table 208 [45 68]) ; -D
      (aset isoms-display-table 208 [68 45])))) ; D-
(aset isoms-display-table 209 [165]) ; Ñ (N tilded)
(if isoms850 (aset isoms-display-table 210 [227]) ; Ò
  (if isoms850-one2one (aset isoms-display-table [79])
    (if prefix-accent (aset isoms-display-table 210 [96 79]) ; `O
      (aset isoms-display-table 210 [79 96])))) ; O`
(if isoms850 (aset isoms-display-table 211 [224]) ; Ó
  (if isoms850-one2one (aset isoms-display-table [79])
    (if prefix-accent (aset isoms-display-table 211 [39 79]) ; 'O
      (aset isoms-display-table 211 [79 39])))) ; O'
(if isoms850 (aset isoms-display-table 212 [226]) ; Ô
  (if isoms850-one2one (aset isoms-display-table [79])
    (if prefix-accent (aset isoms-display-table 212 [94 79]) ; ^O
      (aset isoms-display-table 212 [79 94])))) ; O^
(if isoms850 (aset isoms-display-table 213 [229]) ; Õ
  (if isoms850-one2one (aset isoms-display-table [79])
    (if prefix-accent (aset isoms-display-table 213 [126 79]) ; ~O
      (aset isoms-display-table 213 [79 126])))) ; O~
(aset isoms-display-table 214 [153]) ; Ö (O trema)
(if isoms850 (aset isoms-display-table 215 [158]) ; × (times)
  (aset isoms-display-table 215 [120]))	; x
(if isoms850 (aset isoms-display-table 216 [157]) ; Ø (O bar)
  (if isoms850-one2one (aset isoms-display-table [79])
    (if prefix-accent (aset isoms-display-table 216 [47 79]) ; /O
      (aset isoms-display-table 216 [79 47])))) ; O/
(if isoms850 (aset isoms-display-table 217 [235]) ; Ù
  (if isoms850-one2one (aset isoms-display-table [85])
    (if prefix-accent (aset isoms-display-table 217 [96 85]) ; `U
      (aset isoms-display-table 217 [85 96])))) ; U`
(if isoms850 (aset isoms-display-table 218 [233]) ; Ú
  (if isoms850-one2one (aset isoms-display-table [85])
    (if prefix-accent (aset isoms-display-table 218 [39 85]) ; 'U
      (aset isoms-display-table 218 [85 39])))) ; U'
(if isoms850 (aset isoms-display-table 219 [234]) ; Û
  (if isoms850-one2one (aset isoms-display-table [85])
    (if prefix-accent (aset isoms-display-table 219 [94 85]) ; ^U
      (aset isoms-display-table 219 [85 94])))) ; U^
(aset isoms-display-table 220 [154]) ; Ü (U")
(if isoms850 (aset isoms-display-table 221 [237]) ; İ
  (if isoms850-one2one (aset isoms-display-table [89])
    (if prefix-accent (aset isoms-display-table 221 [39 89]) ; 'Y
      (aset isoms-display-table 221 [89 39])))) ; Y'
(if isoms850 (aset isoms-display-table 222 [232]) ; Ş (don't know the name...)
  (aset isoms-display-table 222 [112])) ; p
(aset isoms-display-table 223 [225]) ; ß (estzet)
(aset isoms-display-table 224 [133]) ; à (a`)
(aset isoms-display-table 225 [160]) ; á (a')
(aset isoms-display-table 226 [131]) ; â (a^)
(if isoms850 (aset isoms-display-table 227 [198]) ; ã
  (if isoms850-one2one (aset isoms-display-table [97])
    (if prefix-accent (aset isoms-display-table 227 [126 97]) ; ~a
      (aset isoms-display-table 227 [97 126])))) ; a~
(aset isoms-display-table 228 [132]) ; ä (a")
(aset isoms-display-table 229 [134]) ; å (a circle)
(aset isoms-display-table 230 [145]) ; æ (ae lig.)
(aset isoms-display-table 231 [135]) ; ç (c cedilla)
(aset isoms-display-table 232 [138]) ; è (`e)
(aset isoms-display-table 233 [130]) ; é (e')
(aset isoms-display-table 234 [136]) ; ê (e^)
(aset isoms-display-table 235 [137]) ; ë (e")
(aset isoms-display-table 236 [141]) ; ì (`i)
(aset isoms-display-table 237 [161]) ; í (i')
(aset isoms-display-table 238 [140]) ; î (i^)
(aset isoms-display-table 239 [139]) ; ï (i")
(if isoms850 (aset isoms-display-table 240 [208]) ; ğ (don't know the name...)
  (if isoms850-one2one (aset isoms-display-table [100])
    (if prefix-accent (aset isoms-display-table 240 [47 100]) ; /d
      (aset isoms-display-table 240 [100 47])))) ; d/
(aset isoms-display-table 241 [164]) ; ñ (n~)
(aset isoms-display-table 242 [149]) ; ò (`o)
(aset isoms-display-table 243 [162]) ; ó ('o)
(aset isoms-display-table 244 [147]) ; ô (^o)
(if isoms850 (aset isoms-display-table 245 [228]) ; õ
  (if isoms850-one2one (aset isoms-display-table [111])
    (if prefix-accent (aset isoms-display-table 245 [126 111]) ; ~o
    (aset isoms-display-table 245 [111 126])))) ; o~
(aset isoms-display-table 246 [148]) ; ö ("o)
(aset isoms-display-table 247 [246]) ; ÷ (divide)
(if isoms850 (aset isoms-display-table 248 [155]) ; ø
  (if isoms850-one2one (aset isoms-display-table [111])
    (if prefix-accent (aset isoms-display-table 248 [47 111]) ; /o
      (aset isoms-display-table 248 [111 47])))) ; o/
(aset isoms-display-table 249 [151]) ; ù (`u)
(aset isoms-display-table 250 [163]) ; ú (u')
(aset isoms-display-table 251 [150]) ; û (u^
(aset isoms-display-table 252 [129]) ; ü (u")
(if isoms850 (aset isoms-display-table 253 [236]) ; ı
  (if isoms850-one2one (aset isoms-display-table [121])
    (if prefix-accent (aset isoms-display-table 253 [39 121]) ; 'y
      (aset isoms-display-table 253 [121 39])))) ; 'y
(if isoms850 (aset isoms-display-table 254 [231]) ; ş (don't know the name...)
  (aset isoms-display-table 254 [80])) ; P
(aset isoms-display-table 255 [152]) ; ÿ (y")