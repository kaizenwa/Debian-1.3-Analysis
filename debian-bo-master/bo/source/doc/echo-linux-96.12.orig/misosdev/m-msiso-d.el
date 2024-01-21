;;; Display tables...
;;; MSISO
(setq msiso-display-table (make-display-table))
(aset msiso-display-table 12 [? ])
(aset msiso-display-table 13 [? ])
(aset msiso-display-table 26 [? ])
(aset msiso-display-table 128 [199])	; � ,C
(aset msiso-display-table 129 [252])	; � "u
(aset msiso-display-table 130 [233])	; � 'e
(aset msiso-display-table 131 [226])	; � ^a
(aset msiso-display-table 132 [228])	; � "a
(aset msiso-display-table 133 [224])	; � `a
(aset msiso-display-table 134 [229])	; � a circle
(aset msiso-display-table 135 [231])	; � ,c
(aset msiso-display-table 136 [234])	; � ^e
(aset msiso-display-table 137 [235])	; � "e
(aset msiso-display-table 138 [232])	; � `e
(aset msiso-display-table 139 [239])	; � "i
(aset msiso-display-table 140 [238])	; � ^i
(aset msiso-display-table 141 [236])	; � `i
(aset msiso-display-table 142 [196])	; � "A
(aset msiso-display-table 143 [197])	; � A circle
(aset msiso-display-table 144 [201])	; � 'E
(aset msiso-display-table 145 [230])	; � ae lig.
(aset msiso-display-table 146 [198])	; � AE lig.
(aset msiso-display-table 147 [244])	; � ^o
(aset msiso-display-table 148 [246])	; � "o
(aset msiso-display-table 149 [242])	; � `o
(aset msiso-display-table 150 [251])	; � ^u
(aset msiso-display-table 151 [249])	; � `u
(aset msiso-display-table 152 [255])	; � "y
(aset msiso-display-table 153 [214])	; � "O
(aset msiso-display-table 154 [220])	; � "U
(if ms850iso (aset msiso-display-table 155 [248]) ; � o bar
  (aset msiso-display-table 155 [162]))	; � cents
(aset msiso-display-table 156 [163])	; � pounds
(if ms850iso (aset msiso-display-table 157 [216]) ; � O bar
  (aset msiso-display-table 157 [165]))	; � yen
(if ms850iso (aset msiso-display-table 158 [215]) ; � times
  (aset msiso-display-table 158 [80 116])) ; Pt (Pesetas)
(aset msiso-display-table 159 [102])	; f round
(aset msiso-display-table 160 [225])	; � 'a
(aset msiso-display-table 161 [237])	; � 'i
(aset msiso-display-table 162 [243])	; � 'o
(aset msiso-display-table 163 [250])	; � 'u
(aset msiso-display-table 164 [?�])	; � ~n
(aset msiso-display-table 165 [?�])	; � ~N
(aset msiso-display-table 166 [?�])	; � a superscript
(aset msiso-display-table 167 [?�])	; � o superscript
(aset msiso-display-table 168 [?�])	; � ? inverted
(if ms850iso (aset msiso-display-table 169 [?�]) ; � Registered
  (aset msiso-display-table 169 [?-]))	; Flipped Logical Not
(aset msiso-display-table 170 [?�])	; � logical not
(aset msiso-display-table 171 [?�])	; � 1/2
(aset msiso-display-table 172 [?�])	; � 1/4
(aset msiso-display-table 173 [?�])	; � ! inverted
(aset msiso-display-table 174 [?�])	; � opening French guillemets
(aset msiso-display-table 175 [?�])	; � closing French guillemets
(aset msiso-display-table 176 [? ])	; block
(aset msiso-display-table 177 [? ])	; block
(aset msiso-display-table 178 [? ])	; block
(aset msiso-display-table 179 [?|])	; Vertical bar
(aset msiso-display-table 180 [?+])	; -|
(if ms850iso (aset msiso-display-table 181 [?�]) ; � 'A
  (aset msiso-display-table 181 [?+]))	; =|
(if ms850iso (aset msiso-display-table 182 [?�]) ; � ^A
  (aset msiso-display-table 182 [?+]))	; -||
(if ms850iso (aset msiso-display-table 183 [?�]) ; � `A
  (aset msiso-display-table 183 [?\\]))	; upper right
(if ms850iso (aset msiso-display-table 184 [?�]) ; � (c)
  (aset msiso-display-table 184 [?\\]))	; upper right
(aset msiso-display-table 185 [?+])	; =||
(aset msiso-display-table 186 [?|])	; ||
(aset msiso-display-table 187 [?\\])	; upper right
(aset msiso-display-table 188 [?/])	; down right
(if ms850iso (aset msiso-display-table 189 [?�]) ; � cents
  (aset msiso-display-table 189 [?/]))	; down right
(if ms850iso (aset msiso-display-table 190 [?�]) ; � yen
  (aset msiso-display-table 190 [?/]))	; down right
(aset msiso-display-table 191 [?\\])	; upper right
(aset msiso-display-table 192 [?\\])	; down left
(aset msiso-display-table 193 [?+])	; _|_
(aset msiso-display-table 194 [?+])	; �|�
(aset msiso-display-table 195 [?+])	; |-
(aset msiso-display-table 196 [?-])	; -
(aset msiso-display-table 197 [?+])	; +
(if ms850iso (aset msiso-display-table 198 [?�]) ; � ~a
  (aset msiso-display-table 198 [?+]))	; |=
(if ms850iso (aset msiso-display-table 199 [?�]) ; � ~A
  (aset msiso-display-table 199 [?+]))	; ||-
(aset msiso-display-table 200 [?\\])	; down right
(aset msiso-display-table 201 [?//])	; upper left
(aset msiso-display-table 202 [?+])	; =||= (up)
(aset msiso-display-table 203 [?+])	; =||= (down)
(aset msiso-display-table 204 [?+])	; ||=
(aset msiso-display-table 205 [?=])	; =
(aset msiso-display-table 206 [?+])	; + (double)
(aset msiso-display-table 207 [?+])	; =|= (up)
(if ms850iso (aset msiso-display-table 208 [?�]) ; � d round bar
  (aset msiso-display-table 208 [?+]))	; -||- (up)
(if ms850iso (aset msiso-display-table 209 [?�]) ; � -D
  (aset msiso-display-table 209 [?+]))	; =|= (up)
(if ms850iso (aset msiso-display-table 210 [?�]) ; � ^E
  (aset msiso-display-table 210 [?+]))	; -||- (down)
(if ms850iso (aset msiso-display-table 211 [?�]) ; � "E
  (aset msiso-display-table 211 [?\\]))	; down left
(if ms850iso (aset msiso-display-table 212 [?�]) ; �
  (aset msiso-display-table 212 [?\\]))	; down left
(if ms850iso (aset msiso-display-table 213 [?i]) ; i dotless (?)
  (aset msiso-display-table 213 [?/]))	; upper left
(if ms850iso (aset msiso-display-table 214 [?�]) ; � 'I
  (aset msiso-display-table 214 [?+]))	; upper left
(if ms850iso (aset msiso-display-table 215 [?�]) ; � ^I
  (aset msiso-display-table 215 [?+]))	; -||-
(if ms850iso (aset msiso-display-table 216 [?�]) ; � "I
  (aset msiso-display-table 216 [?+]))	; =|=-
(aset msiso-display-table 217 [?/])	; down right
(aset msiso-display-table 218 [?/])	; upper left
(aset msiso-display-table 219 [? ])	; block
(aset msiso-display-table 220 [? ])	; block
(if ms850iso (aset msiso-display-table 221 [?�]) ; � Vertical bar
  (aset msiso-display-table 221 [? ]))	; block
(if ms850iso (aset msiso-display-table 222 [?� ])	; block
  (aset msiso-display-table 222 [? ]))	; block
(aset msiso-display-table 223 [? ])	; block
(if ms850iso (aset msiso-display-table 224 [?�]) ; � 'O
  (if ms850iso-one2one (aset msiso-display-table 224 [?a]) ; alpha
    (aset msiso-display-table 224 [?{?a?l?p?h?a?}])))
(aset msiso-display-table 225 [?�])	; � etzet
(if ms850iso (aset msiso-display-table 226 [?�]) ; � ^O
  (if ms850iso-one2one (aset msiso-display-table 226 [?g]) ; gamma
    (aset msiso-display-table 226 [?{?g?a?m?m?a?}])))
(if ms850iso (aset msiso-display-table 227 [?�]) ; � `O
  (if ms850iso-one2one (aset msiso-display-table 227 [?p]) ; pi
    (aset msiso-display-table 227 [?{?p?i?}])))
(if ms850iso (aset msiso-display-table 228 [?�]) ; � ~o
  (if ms850iso-one2one (aset msiso-display-table 228 [?S]) ; Sygma
    (aset msiso-display-table 228 [?{?S?y?g?m?a?}])))
(if ms850iso (aset msiso-display-table 229 [?�]) ; � ~O
  (if ms850iso-one2one (aset msiso-display-table 229 [?s]) ; sygma
    (aset msiso-display-table 229 [?{?s?y?g?m?a?}])))
(aset msiso-display-table 230 [?�])	; � mu
(if ms850iso (aset msiso-display-table 231 [?�]) ; � p
  (if ms850iso-one2one (aset msiso-display-table 231 [?t]) ; tau
    (aset msiso-display-table 231 [?{?t?a?u?}])))
(if ms850iso (aset msiso-display-table 232 [?�]) ; � p
  (if ms850iso-one2one (aset msiso-display-table 232 [?P]) ; Phi
    (aset msiso-display-table 232 [?{?P?h?i?}])))
(if ms850iso (aset msiso-display-table 233 [?�]) ; � 'U
  (if ms850iso-one2one (aset msiso-display-table 233 [?T]) ; Tau
    (aset msiso-display-table 233 [?{?T?a?u?}])))
(if ms850iso (aset msiso-display-table 234 [?�]) ; � ^U
  (if ms850iso-one2one (aset msiso-display-table 234 [?O]) ; Omega
    (aset msiso-display-table 234 [?{?O?m?e?g?a?}])))
(if ms850iso (aset msiso-display-table 235 [?�]) ; � `U
  (if ms850iso-one2one (aset msiso-display-table 235 [?d]) ; delta
    (aset msiso-display-table 235 [?{?d?e?l?t?a?}])))
(if ms850iso (aset msiso-display-table 236 [?�]) ; �
  (if ms850iso-one2one (aset msiso-display-table 236 [?i]) ; 
    (aset msiso-display-table 236 [?{?i?n?f?i?n?i?t?e?}])))
(if ms850iso (aset msiso-display-table 237 [?�]) ; � 'Y
  (if ms850iso-one2one (aset msiso-display-table 237 [?p]) ; phi
    (aset msiso-display-table 237 [?{?p?h?i?}])))
(if ms850iso (aset msiso-display-table 238 [?�]) ; � superscript bar
  (if ms850iso-one2one (aset msiso-display-table 238 [?e]) ; epsilon/belong to
    (aset msiso-display-table 238 [?{?b?e?l?o?n?g?}])))
(if ms850iso (aset msiso-display-table 239 [?�]) ; � '
  (if ms850iso-one2one (aset msiso-display-table 239 [?n]) ; intersection...
    (aset msiso-display-table 239 [?{?i?n?t?e?r?}])))
(if ms850iso (aset msiso-display-table 240 [?�]) ; � dash
  (if ms850iso-one2one (aset msiso-display-table 240 [?=]) ; equivalent to...
    (aset msiso-display-table 240 [?{?e?q?u?i?v?a?l?e?n?t?}])))
(aset msiso-display-table 241 [?�])	; +-
(if ms850iso (aset msiso-display-table 242 [?_]) ; = on baseline
  (if ms850iso-one2one (aset msiso-display-table 242 [?>]) ; >= argh...
    (aset msiso-display-table 242 [?>?=])))
(if ms850iso (aset msiso-display-table 243 [?�]) ; � 3/4
  (if ms850iso-one2one (aset msiso-display-table 243 [?<]) ; <= argh...
    (aset msiso-display-table 243 [?<?=])))
(if ms850iso (aset msiso-display-table 244 [?�]) ; � flipped P
  (aset msiso-display-table 244 [?|]))	; supposed to be upper part of f
(if ms850iso (aset msiso-display-table 245 [?�]) ; � paragraph
  (aset msiso-display-table 245 [?|]))	; supposed to be bottom part of f
(aset msiso-display-table 246 [?�])	; �
(if ms850iso (aset msiso-display-table 247 [?�]) ; � cedilla
  (aset msiso-display-table 247 [?=]))	; about...
(aset msiso-display-table 248 [?�])	; � degree
(if ms850iso (aset msiso-display-table 249 [?�]) ; � trema
  (aset msiso-display-table 249 [?�]))	; � dot
(aset msiso-display-table 250 [?�])	; � dot
(if ms850iso (aset msiso-display-table 251 [?�]) ; � 1 superscript
  (if ms850iso-one2one (aset msiso-display-table 251 [?s]) ; square root (!)
    (aset msiso-display-table 251 [?{?s?q?r?t?}])))
(if ms850iso (aset msiso-display-table 252 [?�]) ; � 3 superscript
  (if ms850iso-one2one (aset msiso-display-table 252 [?n]) ; power n (!!)
    (aset msiso-display-table 252 [?^?n])))
(aset msiso-display-table 253 [?�])	; � 2 superscript
(aset msiso-display-table 254 [? ])	; block
(aset msiso-display-table 255 [160])	; NSB (?)


