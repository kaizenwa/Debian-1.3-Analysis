(game-module "ww2-42"
  (title "WWII Jan 42")
  (blurb "Full WWII, January 1942.")
  (base-module "ww2-adv")
  (variants
   (see-all true)
   )
  (instructions (
   "In this game you play the national leader of your country during WWII."
   ))
  )

;;; Define basic terrain.

(include "earth-1deg")

(set synthesis-methods '(name-units-randomly))

(include "ww2-sides")

;;; France is out of the picture now.

(side 2 (active false))

;;; Define the people.

(area (people-sides
  "360X"
  "360X"
  "360X"
  "136X9a138h77X"
  "136X9a139h76X"
  "134X12a139hX2h25X18d27b2X"
  "22X4b106X2e12a145h22X18dX27bX"
  "b20X6b103X3e6a2X6a147h20X10dXdX5d28b"
  "27b102X4e5a3X6a148h20X5dXd2X8d28b"
  "27b101X4e6a4X6a147h18X20d27b"
  "27b100X5e5a4X7a143h22X6dX4dX8d27b"
  "25b15X6b80X6e5a4X7a134h4X4h24X11dX6dX4dX22b"
  "25b16X7b78X6e6a4X6a128h2X5h4X3h25X25d3bX16b"
  "25b17X8b45Xa30X6e6a10Xe125h5X4h33X11d3X12d18b"
  "25b18X8b4Xb38X3a29X7e5a13X124h5X3h35X7d3X3d9X5d15b"
  "26b17X10b2X3b18Xa18X2a19X2b9X3e3X5a5XeX9e108h16X5h41X4d16XdXd13b"
  "28b16X15b56X4b12XeX5a5XeX9e105h18X7h25Xd13X2d20Xd2X12b"
  "34b8Xb2X16b56X2b12X2e2X3a5X16e98h21X7h37X4d2X2d16XdXd11b"
  "36b8X18b52X2bX4b10X4e8X16e98h2Xh17X7h35X4d25X2dX3bX5b"
  "41b3X21b49Xa2b2X4b9Xe6X3eX16e100h19X6h4XhXh27X2d31X9b"
  "41b3X23b47X3a3X3b6X32e103hXh12X5h31XdXd33Xb2X7b"
  "43b2X23b46X3a2X5b4X34e103hXh12X3h15Xd11XdXdXd38Xb2X5b"
  "44bX22b48Xa4X5b2X35e74h2a27hXh13Xh22XdXdXd48X4b"
  "66b2Xb50X3b5X36e70h6a10g16h2X2h11Xh76X3b"
  "36b3X22b6X4b52XeX38e68h8a17g9h2X2h89X2b"
  "4X31d5X22bXb3X5b50X41e64h12a19g8h2Xh91X"
  "2XdX31d2Xd4X20b5X6b50X11e3a27e63h12a19g7h3Xh9Xh81X"
  "3X38d3X19b3Xc58X3e8a3f20eX2e3X12h3X47h12a19g7h4Xh90X"
  "3X37d2X2d3X9bd9b60X2e6a6fX15e4X3e2X10h5X47h12agX17g6h13Xg82X"
  "4X36d2X2d2X10b2d2b2X2b63Xe7a6f2X14e4Xe2X11h5X39h4i15a2i18g6h5X2g4X2g83X"
  "4X37dX3dX4b3X5d4Xb57X8j7a4X3f2X13e10X9h4X33hX2h8i13a3i19g4h6X5g86X"
  "5X37dX3d2b2X8d62X11ja5Xc3X3f3X10e12X8h5X29h4X10i11a5i19g10X3g87X"
  "5X37dX3d3X10d62X2a9j10X4f2X8e2a3X4a5X8h5X28h3X11i11a4i19g101X"
  "6X51d64X2a8j7X2f4X4fXf4e2X20a3h4X27h18i7a5i10g2X7g11X2g88X"
  "6X50d65X2a7j2Xj6X2f5Xf3Xf2e5X20a2h4X26h31i8g2X2g2X3g12X2g88X"
  "8X48d47Xa17X2a8j16Xf4X3e2X23a5X18h2X5h31i8g7X3g11X2g88X"
  "8X48d49Xa17X7j17Xf4X2e2XfX10a3ca3b5a5X7a12h2X4h31i9gXg5X3g9X3g88X"
  "10X46d69Xb8X9c2X2f7Xe5XaXaX2a2X4c5b17a10h4X2h13iX18i13g3X3g7X5g87X"
  "11X45d76X11c13X2eXf8X2c9b16a6h2b7X17iX14i10g6X3g2X10g87X"
  "12X44d70X17c5Xb16X2b2X11b16a5h5b6XiX7iX20i11g6Xg5X2gX2g90X"
  "15X39d71X19c25X12b17a11b21X19i5g9X2gX2g92X"
  "14Xd2X37d15Xb47Xj7X19c5f5X3f12X4b3a6b16a12b22X18i5g9X2g94X"
  "18XdX33d71X21c4f5X5f5X2b3X3b2a2X2a5b15a15b20X15i2gi5g8Xg95X"
  "18X2a2Xa31d71X8c8X4c16f10b2a5X2a4b15a17b18X14iX7g45Xa58X"
  "19X2a2X9a8d3X2d7X2d63Xj7X7c10X3c16f10ba8X2ab2X14a20b16X13i9g103X"
  "21X2a2X8a7d13X2d64XjXj3Xj4c13X3c3f11X2f7bXbXa9X2a3X13a22b13X13iX9g7Xg52Xd41X"
  "21X3a2X10a3d15X2d67X3j3c15X3c2f12Xfb2X5b2X2a10Xa4X11a27bX4b4X21i103X"
  "24X2aX11a2d14X3d66X3j2c17X2c2f17X5b2X2a10X2a7X7a33bX21ig7Xg95X"
  "25Xa2X2aX8ad15X2dXb63X3j2c19X2cf18X5b2Xa11XaXa3Xa3X6a33b22i73Xd29X"
  "26X2a2X10a20Xb61X2j3c40X6b2X2a10X3a2X3a10X31b18igigX2g2Xg16Xg80X"
  "27X2a2X9a22Xb59X2jc43X5b3X3a9XaX7a9X31b14igX2g3X2g100X"
  "33X8a14X5a5Xb57Xjc43X6b3X3a14X5a9X22bX7b3ic10iXb5Xg79X2d19X"
  "34X8a17X6a58X2c43X7b3X3a17Xa10X2bX15b4X7b3a5ci2Xi92Xd17X"
  "35X8a6X4a10X3a58X2c43X7b4X2a16Xa14X14b5X6b4a5c97Xd15X"
  "29Xa5X9a5X4a14X3a54X2c44X7b4Xa15X2a14X13b7X5b5a3c3X2i94X2d13X"
  "37X9a3X3ab10X2b2X6aX2dXa47X2c29X2c12X9b3X2a12X2a16X11b10X4b5a3c3X2i10Xd82X3d12X"
  "40X12ab72X6c23X10c5X11b3X2a11Xa17X10b11X4b6a3c13X3d96X"
  "43X9ab27Xb36Xa8X10c13X3c2X13c14b4X7aX4a18X9b12X2bXb6a4c12X2d96X"
  "49X9a67X42c15b3X7b2a22X6b18Xb6a4c11X2d22Xg73X"
  "51X9a21Xc37Xa5X42c17b2X6b25X6b18X6a5c11X2d95X"
  "53X7a23Xb42X3b18c9b12c17bX3b29X5b18X5a6c13X2d20Xg72X"
  "57X4a14Xa6Xa44X20c10b11c18b33X5b13Xa4X2aXa7c12X2dXd91X"
  "58X3a10XaXa10Xb43X3a16c10b12c18b6X2b3Xb20X5b18X2a3X6c16Xd90X"
  "59X3a7X4aX5a2XaXa46X3c2b7c4b2c9b12c20b2X5b20Xb4X3b14Xa4Xa5X4c11XdXdX2d90X"
  "61Xa4Xa2X4aX10a47X2c3b7c4bc9b13c26b25X3b2Xb16Xa6Xc12X2d5Xd12Xa76X"
  "62X4aX19a47X3ba6c4b2c8b14c25b27Xb2X2b12Xa3X2a17Xd4X4d88X"
  "65Xa2X18ab47X2b2a6c4bc8b15c24b30X2b17X2b21X5d25Xa61X"
  "70X16a3b47X3a6c4bc7b15c24b31X2b18X2b15Xa6X2d32Xa54X"
  "70X17a3b2c45X2a7cb5X5b16c23b48Xa2X3b12X4a94X"
  "71X17a2b2cX3a44Xc12X2b2X13c5X21b48X3aX3b11Xa2X2a93X"
  "71X17a2b7a61X10c8X19b50X3aX3b9X2a2Xa94X"
  "71X27a61X8c10X18b52X2a2X2b8X2a3Xa93X"
  "70X29a60X8c2a10X15b54X3aXb5X4a11Xa48Xa37X"
  "70X30a60X2j5c3a10X13b54XaX3a7Xa7Xa2X5a36Xa48X"
  "58XaXa9X31aXa53Xj3X3j4c3a12X10b58X4a5X2a6Xa2Xa5Xa5X2aXa74X"
  "70X36a54X3j4c2a12X2abX7b59X5a5Xa4X2a2X5a3Xa3X3a54Xa20X"
  "71X37a48Xj4X2j4c2a12X2ab2X5b61X5aXa2X6a3X3a10X2a2X5a6Xa2Xa58X"
  "71X41a51X3c4a11X2ab2X6b62X4a8X2a3XaXa6X2a3X3a4X3a65X"
  "71X43a5Xa44X2cX3a11X2a2bX6b63X3a13XaX2a13X3a3X3a6Xa56X"
  "72X46a47X5a11X3a8b16Xb48X2a13Xa3Xa14Xa4X3a2X4a3Xa51X"
  "73X45a49X3a11XaXa8b68Xa3X2a31Xb3a7XaXa21Xa27X"
  "74X45a48X4a10X2a2X8b68X7a20Xa9X4b30Xa27X"
  "75X44a19Xb28X4a10X2a3X8b73X2aXaXa3Xa2X2a4Xa14X4b7Xa48X"
  "16Xa59X43a49X4a9X3abX9b80Xa3Xa23X2b10Xa18Xa25X"
  "77X42a50X3a9X4a4bX6b109Xb11Xa42X"
  "78X41a51X2a7XbX4a5bX6b56Xb32XbXb9Xb61X"
  "79X39a24Xa27X2a7X11bX6b3Xc4Xc81X6b5X2b60X"
  "81X37a52X2a7X12bX6a7X2c80X6b6X2b43XaX2a12X"
  "82X36a52X3a6X9b3a2b5a7X2c76X2bX7b6X3b58X"
  "27Xa55X36a33Xb18X2a7X7b5a2b4a6X4c75X11b6X3b21Xa35X"
  "85X34a52X3a6X9b4ab4a5X5c73X15b4X3b22Xa34X"
  "9Xa78X31a53X2a7X10b5a7X5c73X17b2X5b22Xa32X"
  "89X2aX27a53X2a7X11b3a8X5c72X25b55X"
  "90X2a2X26a53X19b3a9X5c70X28b22Xa30X"
  "90X2a2X26a54X18b3a9X5c8Xb60X31b15X2c34X"
  "Xa89X3aX25a55X18b3a8X5c7Xb59X34b17Xa32X"
  "91X2aX25a56X18b4a8X4c67X35b49X"
  "91X3aX22a59X18b3a9X3c67X37b47X"
  "91X3aX20a62X17b3a10X2c67X38b46X"
  "92X2a2X19a63X16b2a12Xc68X38b45X"
  "92X2aX19a64X17ba81X39b44X"
  "54Xa38X2aX19a64X17b82X39b43X"
  "93X22a65X16b82X40b42X"
  "94X21a67X14b84X38b14Xa27X"
  "94X20a68X14b84X38b42X"
  "94X20a70X12b85X38b41X"
  "94X19a71X11b87X9b9X19b41X"
  "87Xa7X14aX3a72X10b87X9b11X16b42X"
  "95X15a76X2b95X3b16XbX14b42X"
  "95X16a196X11b23Xb18X"
  "95X16a197X10b24Xb17X"
  "95X16a198X9b25X2b15X"
  "95X12a205XbX2b27X4b13X"
  "96X12a235X4b13X"
  "96X10a207Xb31X2b13X"
  "97X10a208X4b24X2bXb13X"
  "97XaX4aX3a209X4b22X4b14X"
  "99X8a210X3b21X4b15X"
  "99X8a211X2b20X4b16X"
  "99XaX5a234X4b16X"
  "98XaX6a235X2b17X"
  "100X7a234Xb18X"
  "99X8a253X"
  "99XaX6a253X"
  "100X6a254X"
  "100XaX5a8XbXb242X"
  "102X5a253X"
  "105Xa2Xa251X"
  "104Xa2X4a249X"
  "106X4a30Xb219X"
  "360X"
  "360X"
  "360X"
  "360X"
  "360X"
  "360X"
  "360X"
  "360X"
  "360X"
  "360X"
))

;;; Define the cities.

;; (should have more British bases - for instance at Bermuda and in West Indies)

;(include "u-e1-1938")
(base 152 78 0 (n "Kano"))
(base 95 124 0 (n "Reykjavik"))
(town 110 29 0 (n "Mar del Plata"))
(town 110 32 0 (n "Montevideo"))
(town 108 32 0 (n "Buenos Aires"))
(town 96 32 0 (n "Santiago"))
(town 97 33 0 (n "Mendoza"))
(town 94 33 0 (n "Valparaiso"))
(town 113 36 0 (n "Porto Alegre"))
(town 97 39 0 (n "Cordoba"))
(town 113 40 0 (n "Curitiba"))
(town 105 40 0 (n "Asuncion"))
(town 115 42 0 (n "Sao Paulo"))
(town 91 42 0 (n "Antofagasta"))
(town 117 43 0 (n "Rio de Janeiro"))
(town 116 46 0 (n "Belo Horizonte"))
(town 108 49 0 (n "Goiania"))
(town 89 49 0 (n "La Paz"))
(town 117 53 0 (n "Salvador"))
(town 79 53 0 (n "Lima"))
(base 183 54 0 (n "Elizabethville"))
(base 167 57 0 (n "Luanda"))
(town 118 57 0 (n "Recife"))
(town 266 58 6 (n "Surabaya"))
(town 260 59 6 (n "Jakarta"))
(base 272 60 6 (n "Ujung Pendang"))
(base 166 60 0 (n "Matadi"))
(base 167 61 0 (n "Kinshasa"))
(town 113 61 0 (n "Fortaleza"))
(base 266 62 6 (n "Banjarmasin"))
(base 91 62 0 (n "Manaus"))
(base 78 62 0 (n "Iquitos"))
(town 71 63 0 (n "Guayaquil"))
(base 253 64 0 (n "Palembang"))
(town 102 64 0 (n "Belem"))
(town 71 65 0 (n "Quito"))
(base 168 66 0 (n "Mboudaka"))
(base 246 69 0 (n "Medan"))
(town 71 69 0 (n "Cali"))
(town 73 70 0 (n "Bogota"))
(town 71 72 0 (n "Medellin"))
(base 143 73 0 (n "Kumasi"))
(base 73 73 0 (n "Cucuta"))
(base 66 74 3 (n "Panama Canal"))
(base 131 75 0 (n "Conakry"))
(town 78 76 0 (n "Caracas"))
(town 72 76 0 (n "Maracaibo"))
(town 69 76 0 (n "Baranquilla"))
(base 57 78 0 (n "Managua"))
(base 243 79 0 (n "Bangkok"))
(base 53 79 0 (n "San Salvador"))
(base 55 80 0 (n "Tegucigalpa"))
(base 71 84 0 (n "Santo Domingo"))
(base 68 84 0 (n "Port-au-Prince"))
(town 43 85 0 (n "Veracruz"))
(town 40 85 0 (n "Mexico City"))
(town 64 86 0 (n "Santiago de Cuba"))
(town 36 86 0 (n "Guadalajara"))
(base 178 87 0 (n "Mecca"))
(town 56 88 0 (n "La Habana"))
(town 37 88 0 (n "San Luis Potosi"))
(town 36 91 0 (n "Monterrey"))
(base 119 94 0 (n "Las Palmas"))
(town 29 94 0 (n "Chihuahua"))
(town 206 96 0 (n "Multan"))
(town 126 97 2 (n "Marrakech"))
(town 185 98 0 (n "Esfahan"))
(town 183 101 0 (n "Tehran"))
(town 191 102 0 (n "Mashhad"))
(base 166 103 0 (n "Adana"))
(base 177 104 0 (n "Tabriz"))
(town 158 104 0 (n "Izmir"))
(town 154 104 4 (n "Athens"))
(town 125 104 0 (n "Cordoba"))
(town 122 104 0 (n "Lisbon"))
(town 174 105 0 (n "Jerevan"))
(town 171 105 0 (n "Erzurum"))
(town 162 105 0 (n "Ankara"))
(town 153 106 4 (n "Thessaloniki"))
(town 198 107 0 (n "Tashkent"))
(city 157 107 0 (n "Istanbul"))
(town 121 107 0 (n "Porto"))
(town 152 108 4 (n "Sofia"))
(town 154 110 4 (n "Bucharest"))
(town 148 110 4 (n "Belgrade"))
(town 143 111 4 (n "Zagreb"))
(base 232 113 0 (n "Ulaan Bataar"))
(town 145 113 4 (n "Budapest"))
(town 142 114 4 (n "Vienna"))
(town 139 116 4 (n "Prague"))
(town 128 116 4 (n "Brussels"))
(town 145 118 4 (n "Warsaw"))
(city 128 118 4 (n "Rotterdam"))
(town 116 119 0 (n "Dublin"))
(town 148 120 4 (n "Vilnius"))
(city 134 121 4 (n "Copenhagen"))
(town 146 122 4 (n "Riga"))
(town 137 125 0 (n "Stockholm"))

(infantry 121 120 1)
(infantry 121 119 1)
(infantry 121 118 1)
(infantry 122 118 1)
(infantry 121 117 1)
(infantry 122 117 1)
(infantry 123 117 1)
(infantry 165 96 1)
(infantry 225 89 1)
(infantry 214 85 1)
(infantry 219 81 1)
(infantry 317 33 1)
(armor 122 119 1)
(armor 165 95 1)
(bomber 119 121 1 (in "Newcastle"))
(bomber 119 121 1 (in "Newcastle"))
(bomber 120 117 1 (in "Bristol"))
(interceptor 122 120 1 (in "Hull"))
(interceptor 122 120 1 (in "Hull"))
(interceptor 120 119 1 (in "Manchester"))
(interceptor 120 119 1 (in "Manchester"))
(interceptor 123 118 1 (in "London"))
(interceptor 123 118 1 (in "London"))
(interceptor 120 117 1 (in "Bristol"))
(interceptor 124 117 1 (in "Dover"))
(interceptor 124 117 1 (in "Dover"))
(fleet 117 125 1)
(fleet 116 122 1)
(fleet 121 121 1)
(fleet 119 119 1)
(fleet 119 117 1)
(base 146 72 1 (n "Accra"))
(base 187 79 1 (n "Aden"))
(base 170 90 1 (n "Aswan"))
(base 168 92 1 (n "Asyut"))
(base 179 98 1 (n "Baghdad"))
(base 192 50 1 (n "Blantyre"))
(base 303 48 1 (n "Cairns"))
(base 227 72 1 (n "Colombo"))
(base 192 58 1 (n "Dar-es-Salaam"))
(base 287 53 1 (n "Darwin"))
(base 124 117 1 (n "Dover"))
(base 183 64 1 (n "Entebbe"))
(base 125 102 1 (n "Gibraltar"))
(base 319 22 1 (n "Hobart"))
(base 170 98 1 (n "Jerusalem"))
(base 202 100 1 (n "Kabul"))
(base 174 81 1 (n "Khartoum"))
(base 64 84 1 (n "Kingston"))
(base 249 69 1 (n "Kuala Lumpur"))
(base 150 72 1 (n "Lagos"))
(base 222 79 1 (n "Madras"))
(base 148 100 1 (n "Malta"))
(base 235 88 1 (n "Mandalay"))
(base 191 61 1 (n "Mombasa"))
(base 187 64 1 (n "Nairobi"))
(base 192 32 1 (n "Port Elizabeth"))
(base 155 70 1 (n "Port Harcourt"))
(base 302 56 1 (n "Port Moresby"))
(base 167 96 1 (n "Port Said"))
(base 237 83 1 (n "Rangoon"))
(base 190 47 1 (n "Salisbury"))
(base 168 95 1 (n "Suez"))
(base 212 87 1 (n "Surat"))
(town 306 31 1 (n "Adelaide"))
(town 210 89 1 (n "Ahmadabad"))
(town 164 97 1 (n "Alexandria"))
(town 218 91 1 (n "Allahabad"))
(town 342 29 1 (n "Auckland"))
(town 220 79 1 (n "Bangalore"))
(town 115 121 1 (n "Belfast"))
(town 215 89 1 (n "Bhopal"))
(town 316 38 1 (n "Brisbane"))
(town 120 117 1 (n "Bristol"))
(town 166 95 1 (n "Cairo"))
(town 9 117 1 (n "Calgary"))
(town 316 30 1 (n "Canberra"))
(town 185 32 1 (n "Cape Town"))
(town 344 22 1 (n "Christchurch"))
(town 220 77 1 (n "Coimbatore"))
(town 228 89 1 (n "Dacca"))
(town 213 94 1 (n "Delhi"))
(town 195 35 1 (n "Durban"))
(town 9 119 1 (n "Edmonton"))
(town 253 88 1 (n "Hong Kong"))
(town 122 120 1 (n "Hull"))
(town 219 83 1 (n "Hyderabad"))
(town 212 92 1 (n "Jaipur"))
(town 191 39 1 (n "Johannesburg"))
(town 216 92 1 (n "Kanpur"))
(town 203 91 1 (n "Karachi"))
(town 208 97 1 (n "Lahore"))
(town 314 28 1 (n "Melbourne"))
(town 53 111 1 (n "Montreal"))
(town 218 87 1 (n "Nagpur"))
(town 119 121 1 (n "Newcastle"))
(town 222 91 1 (n "Patna"))
(town 282 33 1 (n "Perth"))
(town 216 83 1 (n "Poona"))
(town 191 36 1 (n "Pretoria"))
(town 199 97 1 (n "Qandahar"))
(town 252 67 1 (n "Singapore"))
(town 72 113 1 (n "St. John's"))
(town 317 32 1 (n "Sydney"))
(town 49 110 1 (n "Toronto"))
(town 2 115 1 (n "Vancouver"))
(town 27 115 1 (n "Winnipeg"))
(city 214 84 1 (n "Bombay"))
(city 227 88 1 (n "Calcutta"))
(city 117 121 1 (n "Glasgow"))
(city 120 119 1 (n "Manchester"))
(capital 123 118 1 (n "London"))

(base 142 71 2 (n "Abidjan"))
(base 169 102 2 (n "Aleppo"))
(base 206 47 2 (n "Antananarivo"))
(base 135 78 2 (n "Bamako"))
(base 249 81 6 (n "Da-nang"))
(base 125 80 2 (n "Dakar"))
(base 158 70 2 (n "Douala"))
(base 244 87 6 (n "Ha-noi"))
(base 204 50 2 (n "Majanga"))
(base 248 77 6 (n "Phnom-Penh"))
(base 252 76 6 (n "Sai-gon"))
(base 142 102 2 (n "Tunis"))
(town 135 102 2 (n "Algiers"))
(town 125 99 2 (n "Casablanca"))
(town 127 99 2 (n "Fez"))
(town 132 101 2 (n "Oran"))
(town 129 109 2 (n "Toulouse"))
(town 134 110 2 (n "Nice"))
(town 124 113 2 (n "Nantes"))
(town 132 109 2 (n "Marseilles"))
(town 131 111 2 (n "Lyon"))
(town 125 115 2 (n "Le Havre"))
(town 127 110 2 (n "Bordeaux"))
(capital 128 114 2 (n "Paris"))

(infantry 56 108 3)
(infantry 41 105 3)
(infantry 10 103 3)
(infantry 54 101 3)
(infantry 263 81 3)
(fleet 3 113 3)
(fleet 58 108 3)
(fleet 7 104 3)
(fleet 54 104 3)
(fleet 14 99 3)
(base 268 76 3 (n "Cebu"))
(base 65 75 3 (n "Panama City"))
(town 25 101 3 (n "Albuquerque"))
(town 48 99 3 (n "Atlanta"))
(town 46 99 3 (n "Birmingham"))
(town 50 108 3 (n "Buffalo"))
(town 45 105 3 (n "Cincinnati"))
(town 48 106 3 (n "Cleveland"))
(town 27 97 3 (n "El Paso"))
(town 341 87 3 (n "Honolulu"))
(town 43 105 3 (n "Indianapolis"))
(town 53 96 3 (n "Jacksonville"))
(town 35 105 3 (n "Kansas City"))
(town 263 80 3 (n "Manila"))
(town 56 91 3 (n "Miami"))
(town 39 109 3 (n "Milwaukee"))
(town 34 110 3 (n "Minneapolis-St. Paul"))
(town 45 102 3 (n "Nashville"))
(town 43 95 3 (n "New Orleans"))
(town 54 102 3 (n "Norfolk"))
(town 34 101 3 (n "Oklahoma City"))
(town 33 107 3 (n "Omaha"))
(town 20 99 3 (n "Phoenix"))
(town 50 106 3 (n "Pittsburgh"))
(town 3 111 3 (n "Portland"))
(town 52 108 3 (n "Rochester"))
(town 9 104 3 (n "Sacramento"))
(town 18 106 3 (n "Salt Lake City"))
(town 36 95 3 (n "San Antonio"))
(town 17 98 3 (n "San Diego"))
(town 74 84 3 (n "San Juan"))
(town 39 104 3 (n "St. Louis"))
(town 54 93 3 (n "Tampa"))
(town 23 97 3 (n "Tucson"))
(town 36 102 3 (n "Tulsa"))
(town 33 103 3 (n "Wichita"))
(city 57 108 3 (n "Boston"))
(city 41 107 3 (n "Chicago"))
(city 37 98 3 (n "Dallas/Ft Worth"))
(city 25 105 3 (n "Denver"))
(city 45 108 3 (n "Detroit"))
(city 39 95 3 (n "Houston"))
(city 14 100 3 (n "Los Angeles"))
(city 56 106 3 (n "New York"))
(city 54 105 3 (n "Philadelphia"))
(city 8 103 3 (n "San Francisco"))
(city 4 113 3 (n "Seattle"))
(capital 53 104 3 (n "Washington DC"))

(infantry 148 126 4)
(infantry 149 124 4)
(infantry 150 124 4)
(infantry 151 124 4)
(infantry 153 124 4)
(infantry 153 123 4)
(infantry 156 122 4)
(infantry 157 122 4)
(infantry 158 122 4)
(infantry 158 121 4)
(infantry 158 120 4)
(infantry 159 119 4)
(infantry 161 118 4)
(infantry 161 117 4)
(infantry 128 115 4)
(infantry 162 115 4)
(infantry 127 114 4)
(infantry 162 114 4)
(infantry 125 113 4)
(infantry 162 113 4)
(infantry 163 113 4)
(infantry 127 111 4)
(armor 152 124 4)
(armor 160 119 4)
(armor 162 116 4)
(air-force 132 118 4)
(air-force 129 116 4)
(air-force 126 114 4)
(air-force 129 114 4)
(air-force 129 113 4)
(air-force 126 112 4)
(bomber 142 119 4)
(bomber 138 117 4)
(bomber 128 113 4)
(interceptor 131 119 4)
(interceptor 129 118 4)
(interceptor 131 118 4)
(interceptor 127 115 4)
(interceptor 134 114 4)
(convoy 132 124 4)
(convoy 135 120 4)
(convoy 142 120 4)
(convoy 124 115 4)
(sub-fleet 131 133 4)
(sub-fleet 125 127 4)
(sub-fleet 130 121 4)
(sub-fleet 130 120 4)
(sub-fleet 131 120 4)
(sub-fleet 127 118 4)
(sub-fleet 124 112 4)
(sub-fleet 126 110 4)
(base 126 127 4 (n "Bergen"))
(base 132 132 4 (n "Narvik"))
(town 131 117 4 (n "Dortmund"))
(town 132 117 4 (n "Essen"))
(town 134 116 4 (n "Frankfurt"))
(town 143 120 4 (n "Konigsberg"))
(town 137 114 4 (n "Munich"))
(town 131 125 4 (n "Oslo"))
(town 135 114 4 (n "Stuttgart"))
(city 141 120 4 (n "Danzig"))
(city 132 119 4 (n "Hamburg"))
(capital 137 118 4 (n "Berlin"))

(infantry 138 111 5)
(infantry 143 107 5)
(infantry 146 102 5)
(infantry 158 97 5)
(convoy 137 109 5)
(convoy 143 106 5)
(convoy 145 103 5)
(convoy 150 97 5)
(fleet 147 105 5)
(base 184 74 5 (n "Adis Adeba"))
(base 180 81 5 (n "Asmera"))
(base 194 68 5 (n "Mogadishu"))
(town 154 97 5 (n "Benghazi"))
(town 136 110 5 (n "Genoa"))
(town 145 102 5 (n "Palermo"))
(town 134 111 5 (n "Torino"))
(town 151 96 5 (n "Tripoli"))
(city 136 111 5 (n "Milan"))
(capital 142 107 5 (n "Rome"))

(infantry 251 111 6)
(infantry 251 107 6)
(infantry 247 106 6)
(infantry 247 103 6)
(infantry 270 102 6)
(infantry 271 102 6)
(infantry 242 101 6)
(infantry 244 100 6)
(infantry 247 100 6)
(infantry 249 97 6)
(infantry 249 94 6)
(infantry 253 94 6)
(infantry 252 89 6)
(convoy 272 103 6)
(convoy 271 100 6)
(convoy 262 99 6)
(convoy 269 99 6)
(fleet 267 100 6)
(fleet 270 100 6)
(fleet 272 100 6)
(fleet 268 99 6)
(cv-fleet 264 100 6)
(cv-fleet 263 98 6)
(sub-fleet 270 107 6)
(sub-fleet 263 100 6)
(sub-fleet 265 99 6)
(town 270 106 6 (n "Aomori"))
(town 253 109 6 (n "Changchun"))
(town 258 107 6 (n "Changjin"))
(town 251 105 6 (n "Dairen"))
(town 263 99 6 (n "Fukuoka"))
(town 253 111 6 (n "Haerbin"))
(town 266 100 6 (n "Hiroshima"))
(town 264 97 6 (n "Kagoshima"))
(town 268 102 6 (n "Kanazawa"))
(town 258 101 6 (n "Kwangjin"))
(town 264 92 6 (n "Naha"))
(town 269 103 6 (n "Niigata"))
(town 260 101 6 (n "Pusan"))
(town 255 105 6 (n "Pyongyang"))
(town 249 113 6 (n "Qiqihaer"))
(town 269 109 6 (n "Sapporo"))
(town 271 104 6 (n "Sendai"))
(town 257 103 6 (n "Seoul"))
(town 252 107 6 (n "Shengyang"))
(town 259 88 6 (n "Kaohsiun"))
(town 259 90 6 (n "T'aipei"))
(city 269 101 6 (n "Nagoya"))
(city 268 100 6 (n "Osaka"))
(capital 271 101 6 (n "Tokyo"))

(infantry 149 126 7)
(infantry 150 126 7)
(infantry 151 125 7)
(infantry 152 125 7)
(infantry 154 124 7)
(infantry 154 123 7)
(infantry 155 123 7)
(infantry 156 123 7)
(infantry 157 123 7)
(infantry 158 123 7)
(infantry 159 122 7)
(infantry 160 121 7)
(infantry 159 120 7)
(infantry 160 120 7)
(infantry 161 119 7)
(infantry 162 118 7)
(infantry 238 118 7)
(infantry 162 117 7)
(infantry 252 117 7)
(infantry 163 116 7)
(infantry 163 115 7)
(infantry 164 114 7)
(infantry 164 113 7)
(infantry 260 113 7)
(infantry 165 112 7)
(infantry 259 110 7)
(base 204 109 7 (n "Alma-Ata"))
(base 175 112 7 (n "Astrachan'"))
(base 252 116 7 (n "Blagoveshchenk"))
(base 237 118 7 (n "Chita"))
(base 203 108 7 (n "Frunze"))
(base 228 118 7 (n "Irkutsk"))
(base 180 124 7 (n "Niznij Tagil"))
(base 282 119 7 (n "P. Kamchatkij"))
(base 232 117 7 (n "Ulan Ude"))
(base 207 115 7 (n "Ust' Kamenogorsk"))
(base 259 109 7 (n "Vladivostok"))
(town 179 106 7 (n "Baku"))
(town 205 119 7 (n "Barnaul"))
(town 157 119 7 (n "Br'ansk"))
(town 261 114 7 (n "Chabarovsk"))
(town 184 120 7 (n "Chelabinsk"))
(town 161 114 7 (n "Dnepropetrovsk"))
(town 199 104 7 (n "Dusanbe"))
(town 165 122 7 (n "Gorky"))
(town 160 123 7 (n "Jaroslavi"))
(town 198 115 7 (n "Karaganda"))
(town 208 120 7 (n "Kemerovo"))
(town 161 116 7 (n "Kharkov"))
(town 261 116 7 (n "Komsomol'sk"))
(town 213 123 7 (n "Krasnojarsk"))
(town 173 119 7 (n "Kujbyshev"))
(town 188 120 7 (n "Kurgan"))
(town 149 125 7 (n "Leningrad"))
(town 182 119 7 (n "Magnitogorsk"))
(town 150 119 7 (n "Minsk"))
(town 210 119 7 (n "Novokuzneck"))
(town 157 112 7 (n "Odessa"))
(town 196 120 7 (n "Omsk"))
(town 179 117 7 (n "Orenburg"))
(town 192 120 7 (n "Petropavlovsk"))
(town 182 122 7 (n "Sverdlovsk"))
(town 187 122 7 (n "T'umen'"))
(town 173 107 7 (n "Tbilisi"))
(town 206 122 7 (n "Tomsk"))
(town 170 114 7 (n "Stalingrad"))
(city 163 114 7 (n "Doneck"))
(city 155 116 7 (n "Kiev"))
(city 204 121 7 (n "Novosibirsk"))
(capital 159 121 7 (n "Moscow"))

(infantry 237 105 8)
(infantry 240 104 8)
(infantry 239 103 8)
(infantry 241 102 8)
(infantry 241 99 8)
(infantry 242 99 8)
(infantry 245 98 8)
(infantry 247 98 8)
(infantry 247 97 8)
(infantry 245 95 8)
(infantry 247 95 8)
(infantry 249 93 8)
(infantry 252 93 8)
(infantry 239 92 8)
(infantry 255 92 8)
(infantry 240 91 8)
(infantry 255 91 8)
(infantry 249 90 8)
(town 244 106 8 (n "Baoding"))
(town 246 105 8 (n "Beijing"))
(town 248 94 8 (n "Changsha"))
(town 239 96 8 (n "Chengdu"))
(town 241 95 8 (n "Chonqing"))
(town 256 92 8 (n "Fuzhou"))
(town 250 89 8 (n "Guangzhou"))
(town 250 85 8 (n "Haikou"))
(town 250 98 8 (n "Huainan"))
(town 239 91 8 (n "Kunming"))
(town 251 94 8 (n "Nanchang"))
(town 252 98 8 (n "Nanjing"))
(town 256 95 8 (n "Ningbao"))
(town 255 97 8 (n "Shanghai"))
(town 243 103 8 (n "Taiyuan"))
(town 248 104 8 (n "Tianjin"))
(town 249 96 8 (n "Wuhan"))
(town 241 100 8 (n "Xi'an"))
(town 233 102 8 (n "Xining"))
(town 250 100 8 (n "Xuzhou"))
(town 245 101 8 (n "Zhengzhou"))

(town 131 107 9 (n "Barcelona"))
(town 125 109 9 (n "Bilbao"))
(town 129 105 9 (n "Valencia"))
(town 128 107 9 (n "Zaragoza"))
(city 126 106 9 (n "Madrid"))

;;; Modify for territorial changes from 1938.

;; British gains in East Africa.

(unit "Adis Adeba" (s uk))
(unit "Asmera" (s uk))
(unit "Mogadishu" (s uk))

;; German gains in Europe.

(unit "Copenhagen" (s de))
(unit "Rotterdam" (s de))
(unit "Warsaw" (s de))
(unit "Brussels" (s de))
(unit "Prague" (s de))
(unit "Vienna" (s de))
(unit "Budapest" (s de))
(unit "Zagreb" (s de))
(unit "Bucharest" (s de))
(unit "Thessaloniki" (s de))
(unit "Athens" (s de))
(unit "Bordeaux" (s de))
(unit "Le Havre" (s de))
(unit "Lyon" (s de))
(unit "Marseilles" (s de))
(unit "Nantes" (s de))
(unit "Nice" (s de))
(unit "Toulouse" (s de))
(unit "Paris" (s de))
(unit "Kiev" (s de))
(unit "Minsk" (s de))
(unit "Odessa" (s de))
(unit "Riga" (s de))
(unit "Vilnius" (s de))

;; Japanese gains in China.

(unit "Baoding" (s jp))
(unit "Beijing" (s jp))
(unit "Tianjin" (s jp))

;; (add minor countries also?)

;; (set up agreements applying to particular dates)

(scorekeeper (do last-side-wins))

(set initial-date "Jan 1942")

;;; Set up January 1942 alliances.
