#objdump: -dr
#as: -mips3
#name: mips16

# Test the mips16 instruction set.

.*: +file format .*mips.*

Disassembly of section .text:

0+000000 <data1>:
   0:	00 00 00 00 	nop

0+000004 <insns1>:
   4:	3b 40       	ld	\$v0,0\(\$v1\)
   6:	f0 00 3b 41 	ld	\$v0,1\(\$v1\)
   a:	f0 00 3b 42 	ld	\$v0,2\(\$v1\)
   e:	f0 00 3b 43 	ld	\$v0,3\(\$v1\)
  12:	f0 00 3b 44 	ld	\$v0,4\(\$v1\)
  16:	3b 41       	ld	\$v0,8\(\$v1\)
  18:	3b 42       	ld	\$v0,16\(\$v1\)
  1a:	3b 44       	ld	\$v0,32\(\$v1\)
  1c:	3b 48       	ld	\$v0,64\(\$v1\)
  1e:	3b 50       	ld	\$v0,128\(\$v1\)
  20:	f1 00 3b 40 	ld	\$v0,256\(\$v1\)
  24:	f2 00 3b 40 	ld	\$v0,512\(\$v1\)
  28:	f4 00 3b 40 	ld	\$v0,1024\(\$v1\)
  2c:	f0 01 3b 40 	ld	\$v0,2048\(\$v1\)
  30:	f7 ff 3b 5f 	ld	\$v0,-1\(\$v1\)
  34:	f7 ff 3b 5e 	ld	\$v0,-2\(\$v1\)
  38:	f7 ff 3b 5d 	ld	\$v0,-3\(\$v1\)
  3c:	f7 ff 3b 5c 	ld	\$v0,-4\(\$v1\)
  40:	f7 ff 3b 58 	ld	\$v0,-8\(\$v1\)
  44:	f7 ff 3b 50 	ld	\$v0,-16\(\$v1\)
  48:	f7 ff 3b 40 	ld	\$v0,-32\(\$v1\)
  4c:	f7 df 3b 40 	ld	\$v0,-64\(\$v1\)
  50:	f7 9f 3b 40 	ld	\$v0,-128\(\$v1\)
  54:	f7 1f 3b 40 	ld	\$v0,-256\(\$v1\)
  58:	f6 1f 3b 40 	ld	\$v0,-512\(\$v1\)
  5c:	f4 1f 3b 40 	ld	\$v0,-1024\(\$v1\)
  60:	f0 1f 3b 40 	ld	\$v0,-2048\(\$v1\)
  64:	f7 bf fc 40 	ld	\$v0,0 <data1>
  68:	f6 a0 fc 54 	ld	\$v0,71c <data2>
  6c:	f0 01 fc 40 	ld	\$v0,868 <bar>
  70:	f0 c1 fc 40 	ld	\$v0,930 <quux>
  74:	f8 40       	ld	\$v0,0\(\$sp\)
  76:	f0 00 f8 41 	ld	\$v0,1\(\$sp\)
  7a:	f0 00 f8 42 	ld	\$v0,2\(\$sp\)
  7e:	f0 00 f8 43 	ld	\$v0,3\(\$sp\)
  82:	f0 00 f8 44 	ld	\$v0,4\(\$sp\)
  86:	f8 41       	ld	\$v0,8\(\$sp\)
  88:	f8 42       	ld	\$v0,16\(\$sp\)
  8a:	f8 44       	ld	\$v0,32\(\$sp\)
  8c:	f8 48       	ld	\$v0,64\(\$sp\)
  8e:	f8 50       	ld	\$v0,128\(\$sp\)
  90:	f1 00 f8 40 	ld	\$v0,256\(\$sp\)
  94:	f2 00 f8 40 	ld	\$v0,512\(\$sp\)
  98:	f4 00 f8 40 	ld	\$v0,1024\(\$sp\)
  9c:	f0 01 f8 40 	ld	\$v0,2048\(\$sp\)
  a0:	f7 ff f8 5f 	ld	\$v0,-1\(\$sp\)
  a4:	f7 ff f8 5e 	ld	\$v0,-2\(\$sp\)
  a8:	f7 ff f8 5d 	ld	\$v0,-3\(\$sp\)
  ac:	f7 ff f8 5c 	ld	\$v0,-4\(\$sp\)
  b0:	f7 ff f8 58 	ld	\$v0,-8\(\$sp\)
  b4:	f7 ff f8 50 	ld	\$v0,-16\(\$sp\)
  b8:	f7 ff f8 40 	ld	\$v0,-32\(\$sp\)
  bc:	f7 df f8 40 	ld	\$v0,-64\(\$sp\)
  c0:	f7 9f f8 40 	ld	\$v0,-128\(\$sp\)
  c4:	f7 1f f8 40 	ld	\$v0,-256\(\$sp\)
  c8:	f6 1f f8 40 	ld	\$v0,-512\(\$sp\)
  cc:	f4 1f f8 40 	ld	\$v0,-1024\(\$sp\)
  d0:	f0 1f f8 40 	ld	\$v0,-2048\(\$sp\)
  d4:	bb 40       	lwu	\$v0,0\(\$v1\)
  d6:	f0 00 bb 41 	lwu	\$v0,1\(\$v1\)
  da:	f0 00 bb 42 	lwu	\$v0,2\(\$v1\)
  de:	f0 00 bb 43 	lwu	\$v0,3\(\$v1\)
  e2:	bb 41       	lwu	\$v0,4\(\$v1\)
  e4:	bb 42       	lwu	\$v0,8\(\$v1\)
  e6:	bb 44       	lwu	\$v0,16\(\$v1\)
  e8:	bb 48       	lwu	\$v0,32\(\$v1\)
  ea:	bb 50       	lwu	\$v0,64\(\$v1\)
  ec:	f0 80 bb 40 	lwu	\$v0,128\(\$v1\)
  f0:	f1 00 bb 40 	lwu	\$v0,256\(\$v1\)
  f4:	f2 00 bb 40 	lwu	\$v0,512\(\$v1\)
  f8:	f4 00 bb 40 	lwu	\$v0,1024\(\$v1\)
  fc:	f0 01 bb 40 	lwu	\$v0,2048\(\$v1\)
 100:	f7 ff bb 5f 	lwu	\$v0,-1\(\$v1\)
 104:	f7 ff bb 5e 	lwu	\$v0,-2\(\$v1\)
 108:	f7 ff bb 5d 	lwu	\$v0,-3\(\$v1\)
 10c:	f7 ff bb 5c 	lwu	\$v0,-4\(\$v1\)
 110:	f7 ff bb 58 	lwu	\$v0,-8\(\$v1\)
 114:	f7 ff bb 50 	lwu	\$v0,-16\(\$v1\)
 118:	f7 ff bb 40 	lwu	\$v0,-32\(\$v1\)
 11c:	f7 df bb 40 	lwu	\$v0,-64\(\$v1\)
 120:	f7 9f bb 40 	lwu	\$v0,-128\(\$v1\)
 124:	f7 1f bb 40 	lwu	\$v0,-256\(\$v1\)
 128:	f6 1f bb 40 	lwu	\$v0,-512\(\$v1\)
 12c:	f4 1f bb 40 	lwu	\$v0,-1024\(\$v1\)
 130:	f0 1f bb 40 	lwu	\$v0,-2048\(\$v1\)
 134:	9b 40       	lw	\$v0,0\(\$v1\)
 136:	f0 00 9b 41 	lw	\$v0,1\(\$v1\)
 13a:	f0 00 9b 42 	lw	\$v0,2\(\$v1\)
 13e:	f0 00 9b 43 	lw	\$v0,3\(\$v1\)
 142:	9b 41       	lw	\$v0,4\(\$v1\)
 144:	9b 42       	lw	\$v0,8\(\$v1\)
 146:	9b 44       	lw	\$v0,16\(\$v1\)
 148:	9b 48       	lw	\$v0,32\(\$v1\)
 14a:	9b 50       	lw	\$v0,64\(\$v1\)
 14c:	f0 80 9b 40 	lw	\$v0,128\(\$v1\)
 150:	f1 00 9b 40 	lw	\$v0,256\(\$v1\)
 154:	f2 00 9b 40 	lw	\$v0,512\(\$v1\)
 158:	f4 00 9b 40 	lw	\$v0,1024\(\$v1\)
 15c:	f0 01 9b 40 	lw	\$v0,2048\(\$v1\)
 160:	f7 ff 9b 5f 	lw	\$v0,-1\(\$v1\)
 164:	f7 ff 9b 5e 	lw	\$v0,-2\(\$v1\)
 168:	f7 ff 9b 5d 	lw	\$v0,-3\(\$v1\)
 16c:	f7 ff 9b 5c 	lw	\$v0,-4\(\$v1\)
 170:	f7 ff 9b 58 	lw	\$v0,-8\(\$v1\)
 174:	f7 ff 9b 50 	lw	\$v0,-16\(\$v1\)
 178:	f7 ff 9b 40 	lw	\$v0,-32\(\$v1\)
 17c:	f7 df 9b 40 	lw	\$v0,-64\(\$v1\)
 180:	f7 9f 9b 40 	lw	\$v0,-128\(\$v1\)
 184:	f7 1f 9b 40 	lw	\$v0,-256\(\$v1\)
 188:	f6 1f 9b 40 	lw	\$v0,-512\(\$v1\)
 18c:	f4 1f 9b 40 	lw	\$v0,-1024\(\$v1\)
 190:	f0 1f 9b 40 	lw	\$v0,-2048\(\$v1\)
 194:	f6 7f b2 0c 	lw	\$v0,0 <data1>
 198:	f5 80 b2 04 	lw	\$v0,71c <data2>
 19c:	f6 c0 b2 0c 	lw	\$v0,868 <bar>
 1a0:	f7 80 b2 10 	lw	\$v0,930 <quux>
 1a4:	92 00       	lw	\$v0,0\(\$sp\)
 1a6:	f0 00 92 01 	lw	\$v0,1\(\$sp\)
 1aa:	f0 00 92 02 	lw	\$v0,2\(\$sp\)
 1ae:	f0 00 92 03 	lw	\$v0,3\(\$sp\)
 1b2:	92 01       	lw	\$v0,4\(\$sp\)
 1b4:	92 02       	lw	\$v0,8\(\$sp\)
 1b6:	92 04       	lw	\$v0,16\(\$sp\)
 1b8:	92 08       	lw	\$v0,32\(\$sp\)
 1ba:	92 10       	lw	\$v0,64\(\$sp\)
 1bc:	92 20       	lw	\$v0,128\(\$sp\)
 1be:	92 40       	lw	\$v0,256\(\$sp\)
 1c0:	92 80       	lw	\$v0,512\(\$sp\)
 1c2:	f4 00 92 00 	lw	\$v0,1024\(\$sp\)
 1c6:	f0 01 92 00 	lw	\$v0,2048\(\$sp\)
 1ca:	f7 ff 92 1f 	lw	\$v0,-1\(\$sp\)
 1ce:	f7 ff 92 1e 	lw	\$v0,-2\(\$sp\)
 1d2:	f7 ff 92 1d 	lw	\$v0,-3\(\$sp\)
 1d6:	f7 ff 92 1c 	lw	\$v0,-4\(\$sp\)
 1da:	f7 ff 92 18 	lw	\$v0,-8\(\$sp\)
 1de:	f7 ff 92 10 	lw	\$v0,-16\(\$sp\)
 1e2:	f7 ff 92 00 	lw	\$v0,-32\(\$sp\)
 1e6:	f7 df 92 00 	lw	\$v0,-64\(\$sp\)
 1ea:	f7 9f 92 00 	lw	\$v0,-128\(\$sp\)
 1ee:	f7 1f 92 00 	lw	\$v0,-256\(\$sp\)
 1f2:	f6 1f 92 00 	lw	\$v0,-512\(\$sp\)
 1f6:	f4 1f 92 00 	lw	\$v0,-1024\(\$sp\)
 1fa:	f0 1f 92 00 	lw	\$v0,-2048\(\$sp\)
 1fe:	8b 40       	lh	\$v0,0\(\$v1\)
 200:	f0 00 8b 41 	lh	\$v0,1\(\$v1\)
 204:	8b 41       	lh	\$v0,2\(\$v1\)
 206:	f0 00 8b 43 	lh	\$v0,3\(\$v1\)
 20a:	8b 42       	lh	\$v0,4\(\$v1\)
 20c:	8b 44       	lh	\$v0,8\(\$v1\)
 20e:	8b 48       	lh	\$v0,16\(\$v1\)
 210:	8b 50       	lh	\$v0,32\(\$v1\)
 212:	f0 40 8b 40 	lh	\$v0,64\(\$v1\)
 216:	f0 80 8b 40 	lh	\$v0,128\(\$v1\)
 21a:	f1 00 8b 40 	lh	\$v0,256\(\$v1\)
 21e:	f2 00 8b 40 	lh	\$v0,512\(\$v1\)
 222:	f4 00 8b 40 	lh	\$v0,1024\(\$v1\)
 226:	f0 01 8b 40 	lh	\$v0,2048\(\$v1\)
 22a:	f7 ff 8b 5f 	lh	\$v0,-1\(\$v1\)
 22e:	f7 ff 8b 5e 	lh	\$v0,-2\(\$v1\)
 232:	f7 ff 8b 5d 	lh	\$v0,-3\(\$v1\)
 236:	f7 ff 8b 5c 	lh	\$v0,-4\(\$v1\)
 23a:	f7 ff 8b 58 	lh	\$v0,-8\(\$v1\)
 23e:	f7 ff 8b 50 	lh	\$v0,-16\(\$v1\)
 242:	f7 ff 8b 40 	lh	\$v0,-32\(\$v1\)
 246:	f7 df 8b 40 	lh	\$v0,-64\(\$v1\)
 24a:	f7 9f 8b 40 	lh	\$v0,-128\(\$v1\)
 24e:	f7 1f 8b 40 	lh	\$v0,-256\(\$v1\)
 252:	f6 1f 8b 40 	lh	\$v0,-512\(\$v1\)
 256:	f4 1f 8b 40 	lh	\$v0,-1024\(\$v1\)
 25a:	f0 1f 8b 40 	lh	\$v0,-2048\(\$v1\)
 25e:	ab 40       	lhu	\$v0,0\(\$v1\)
 260:	f0 00 ab 41 	lhu	\$v0,1\(\$v1\)
 264:	ab 41       	lhu	\$v0,2\(\$v1\)
 266:	f0 00 ab 43 	lhu	\$v0,3\(\$v1\)
 26a:	ab 42       	lhu	\$v0,4\(\$v1\)
 26c:	ab 44       	lhu	\$v0,8\(\$v1\)
 26e:	ab 48       	lhu	\$v0,16\(\$v1\)
 270:	ab 50       	lhu	\$v0,32\(\$v1\)
 272:	f0 40 ab 40 	lhu	\$v0,64\(\$v1\)
 276:	f0 80 ab 40 	lhu	\$v0,128\(\$v1\)
 27a:	f1 00 ab 40 	lhu	\$v0,256\(\$v1\)
 27e:	f2 00 ab 40 	lhu	\$v0,512\(\$v1\)
 282:	f4 00 ab 40 	lhu	\$v0,1024\(\$v1\)
 286:	f0 01 ab 40 	lhu	\$v0,2048\(\$v1\)
 28a:	f7 ff ab 5f 	lhu	\$v0,-1\(\$v1\)
 28e:	f7 ff ab 5e 	lhu	\$v0,-2\(\$v1\)
 292:	f7 ff ab 5d 	lhu	\$v0,-3\(\$v1\)
 296:	f7 ff ab 5c 	lhu	\$v0,-4\(\$v1\)
 29a:	f7 ff ab 58 	lhu	\$v0,-8\(\$v1\)
 29e:	f7 ff ab 50 	lhu	\$v0,-16\(\$v1\)
 2a2:	f7 ff ab 40 	lhu	\$v0,-32\(\$v1\)
 2a6:	f7 df ab 40 	lhu	\$v0,-64\(\$v1\)
 2aa:	f7 9f ab 40 	lhu	\$v0,-128\(\$v1\)
 2ae:	f7 1f ab 40 	lhu	\$v0,-256\(\$v1\)
 2b2:	f6 1f ab 40 	lhu	\$v0,-512\(\$v1\)
 2b6:	f4 1f ab 40 	lhu	\$v0,-1024\(\$v1\)
 2ba:	f0 1f ab 40 	lhu	\$v0,-2048\(\$v1\)
 2be:	83 40       	lb	\$v0,0\(\$v1\)
 2c0:	83 41       	lb	\$v0,1\(\$v1\)
 2c2:	83 42       	lb	\$v0,2\(\$v1\)
 2c4:	83 43       	lb	\$v0,3\(\$v1\)
 2c6:	83 44       	lb	\$v0,4\(\$v1\)
 2c8:	83 48       	lb	\$v0,8\(\$v1\)
 2ca:	83 50       	lb	\$v0,16\(\$v1\)
 2cc:	f0 20 83 40 	lb	\$v0,32\(\$v1\)
 2d0:	f0 40 83 40 	lb	\$v0,64\(\$v1\)
 2d4:	f0 80 83 40 	lb	\$v0,128\(\$v1\)
 2d8:	f1 00 83 40 	lb	\$v0,256\(\$v1\)
 2dc:	f2 00 83 40 	lb	\$v0,512\(\$v1\)
 2e0:	f4 00 83 40 	lb	\$v0,1024\(\$v1\)
 2e4:	f0 01 83 40 	lb	\$v0,2048\(\$v1\)
 2e8:	f7 ff 83 5f 	lb	\$v0,-1\(\$v1\)
 2ec:	f7 ff 83 5e 	lb	\$v0,-2\(\$v1\)
 2f0:	f7 ff 83 5d 	lb	\$v0,-3\(\$v1\)
 2f4:	f7 ff 83 5c 	lb	\$v0,-4\(\$v1\)
 2f8:	f7 ff 83 58 	lb	\$v0,-8\(\$v1\)
 2fc:	f7 ff 83 50 	lb	\$v0,-16\(\$v1\)
 300:	f7 ff 83 40 	lb	\$v0,-32\(\$v1\)
 304:	f7 df 83 40 	lb	\$v0,-64\(\$v1\)
 308:	f7 9f 83 40 	lb	\$v0,-128\(\$v1\)
 30c:	f7 1f 83 40 	lb	\$v0,-256\(\$v1\)
 310:	f6 1f 83 40 	lb	\$v0,-512\(\$v1\)
 314:	f4 1f 83 40 	lb	\$v0,-1024\(\$v1\)
 318:	f0 1f 83 40 	lb	\$v0,-2048\(\$v1\)
 31c:	a3 40       	lbu	\$v0,0\(\$v1\)
 31e:	a3 41       	lbu	\$v0,1\(\$v1\)
 320:	a3 42       	lbu	\$v0,2\(\$v1\)
 322:	a3 43       	lbu	\$v0,3\(\$v1\)
 324:	a3 44       	lbu	\$v0,4\(\$v1\)
 326:	a3 48       	lbu	\$v0,8\(\$v1\)
 328:	a3 50       	lbu	\$v0,16\(\$v1\)
 32a:	f0 20 a3 40 	lbu	\$v0,32\(\$v1\)
 32e:	f0 40 a3 40 	lbu	\$v0,64\(\$v1\)
 332:	f0 80 a3 40 	lbu	\$v0,128\(\$v1\)
 336:	f1 00 a3 40 	lbu	\$v0,256\(\$v1\)
 33a:	f2 00 a3 40 	lbu	\$v0,512\(\$v1\)
 33e:	f4 00 a3 40 	lbu	\$v0,1024\(\$v1\)
 342:	f0 01 a3 40 	lbu	\$v0,2048\(\$v1\)
 346:	f7 ff a3 5f 	lbu	\$v0,-1\(\$v1\)
 34a:	f7 ff a3 5e 	lbu	\$v0,-2\(\$v1\)
 34e:	f7 ff a3 5d 	lbu	\$v0,-3\(\$v1\)
 352:	f7 ff a3 5c 	lbu	\$v0,-4\(\$v1\)
 356:	f7 ff a3 58 	lbu	\$v0,-8\(\$v1\)
 35a:	f7 ff a3 50 	lbu	\$v0,-16\(\$v1\)
 35e:	f7 ff a3 40 	lbu	\$v0,-32\(\$v1\)
 362:	f7 df a3 40 	lbu	\$v0,-64\(\$v1\)
 366:	f7 9f a3 40 	lbu	\$v0,-128\(\$v1\)
 36a:	f7 1f a3 40 	lbu	\$v0,-256\(\$v1\)
 36e:	f6 1f a3 40 	lbu	\$v0,-512\(\$v1\)
 372:	f4 1f a3 40 	lbu	\$v0,-1024\(\$v1\)
 376:	f0 1f a3 40 	lbu	\$v0,-2048\(\$v1\)
 37a:	7b 40       	sd	\$v0,0\(\$v1\)
 37c:	f0 00 7b 41 	sd	\$v0,1\(\$v1\)
 380:	f0 00 7b 42 	sd	\$v0,2\(\$v1\)
 384:	f0 00 7b 43 	sd	\$v0,3\(\$v1\)
 388:	f0 00 7b 44 	sd	\$v0,4\(\$v1\)
 38c:	7b 41       	sd	\$v0,8\(\$v1\)
 38e:	7b 42       	sd	\$v0,16\(\$v1\)
 390:	7b 44       	sd	\$v0,32\(\$v1\)
 392:	7b 48       	sd	\$v0,64\(\$v1\)
 394:	7b 50       	sd	\$v0,128\(\$v1\)
 396:	f1 00 7b 40 	sd	\$v0,256\(\$v1\)
 39a:	f2 00 7b 40 	sd	\$v0,512\(\$v1\)
 39e:	f4 00 7b 40 	sd	\$v0,1024\(\$v1\)
 3a2:	f0 01 7b 40 	sd	\$v0,2048\(\$v1\)
 3a6:	f7 ff 7b 5f 	sd	\$v0,-1\(\$v1\)
 3aa:	f7 ff 7b 5e 	sd	\$v0,-2\(\$v1\)
 3ae:	f7 ff 7b 5d 	sd	\$v0,-3\(\$v1\)
 3b2:	f7 ff 7b 5c 	sd	\$v0,-4\(\$v1\)
 3b6:	f7 ff 7b 58 	sd	\$v0,-8\(\$v1\)
 3ba:	f7 ff 7b 50 	sd	\$v0,-16\(\$v1\)
 3be:	f7 ff 7b 40 	sd	\$v0,-32\(\$v1\)
 3c2:	f7 df 7b 40 	sd	\$v0,-64\(\$v1\)
 3c6:	f7 9f 7b 40 	sd	\$v0,-128\(\$v1\)
 3ca:	f7 1f 7b 40 	sd	\$v0,-256\(\$v1\)
 3ce:	f6 1f 7b 40 	sd	\$v0,-512\(\$v1\)
 3d2:	f4 1f 7b 40 	sd	\$v0,-1024\(\$v1\)
 3d6:	f0 1f 7b 40 	sd	\$v0,-2048\(\$v1\)
 3da:	f9 40       	sd	\$v0,0\(\$sp\)
 3dc:	f0 00 f9 41 	sd	\$v0,1\(\$sp\)
 3e0:	f0 00 f9 42 	sd	\$v0,2\(\$sp\)
 3e4:	f0 00 f9 43 	sd	\$v0,3\(\$sp\)
 3e8:	f0 00 f9 44 	sd	\$v0,4\(\$sp\)
 3ec:	f9 41       	sd	\$v0,8\(\$sp\)
 3ee:	f9 42       	sd	\$v0,16\(\$sp\)
 3f0:	f9 44       	sd	\$v0,32\(\$sp\)
 3f2:	f9 48       	sd	\$v0,64\(\$sp\)
 3f4:	f9 50       	sd	\$v0,128\(\$sp\)
 3f6:	f1 00 f9 40 	sd	\$v0,256\(\$sp\)
 3fa:	f2 00 f9 40 	sd	\$v0,512\(\$sp\)
 3fe:	f4 00 f9 40 	sd	\$v0,1024\(\$sp\)
 402:	f0 01 f9 40 	sd	\$v0,2048\(\$sp\)
 406:	f7 ff f9 5f 	sd	\$v0,-1\(\$sp\)
 40a:	f7 ff f9 5e 	sd	\$v0,-2\(\$sp\)
 40e:	f7 ff f9 5d 	sd	\$v0,-3\(\$sp\)
 412:	f7 ff f9 5c 	sd	\$v0,-4\(\$sp\)
 416:	f7 ff f9 58 	sd	\$v0,-8\(\$sp\)
 41a:	f7 ff f9 50 	sd	\$v0,-16\(\$sp\)
 41e:	f7 ff f9 40 	sd	\$v0,-32\(\$sp\)
 422:	f7 df f9 40 	sd	\$v0,-64\(\$sp\)
 426:	f7 9f f9 40 	sd	\$v0,-128\(\$sp\)
 42a:	f7 1f f9 40 	sd	\$v0,-256\(\$sp\)
 42e:	f6 1f f9 40 	sd	\$v0,-512\(\$sp\)
 432:	f4 1f f9 40 	sd	\$v0,-1024\(\$sp\)
 436:	f0 1f f9 40 	sd	\$v0,-2048\(\$sp\)
 43a:	fa 00       	sd	\$ra,0\(\$sp\)
 43c:	f0 00 fa 01 	sd	\$ra,1\(\$sp\)
 440:	f0 00 fa 02 	sd	\$ra,2\(\$sp\)
 444:	f0 00 fa 03 	sd	\$ra,3\(\$sp\)
 448:	f0 00 fa 04 	sd	\$ra,4\(\$sp\)
 44c:	fa 01       	sd	\$ra,8\(\$sp\)
 44e:	fa 02       	sd	\$ra,16\(\$sp\)
 450:	fa 04       	sd	\$ra,32\(\$sp\)
 452:	fa 08       	sd	\$ra,64\(\$sp\)
 454:	fa 10       	sd	\$ra,128\(\$sp\)
 456:	fa 20       	sd	\$ra,256\(\$sp\)
 458:	fa 40       	sd	\$ra,512\(\$sp\)
 45a:	fa 80       	sd	\$ra,1024\(\$sp\)
 45c:	f0 01 fa 00 	sd	\$ra,2048\(\$sp\)
 460:	f7 ff fa 1f 	sd	\$ra,-1\(\$sp\)
 464:	f7 ff fa 1e 	sd	\$ra,-2\(\$sp\)
 468:	f7 ff fa 1d 	sd	\$ra,-3\(\$sp\)
 46c:	f7 ff fa 1c 	sd	\$ra,-4\(\$sp\)
 470:	f7 ff fa 18 	sd	\$ra,-8\(\$sp\)
 474:	f7 ff fa 10 	sd	\$ra,-16\(\$sp\)
 478:	f7 ff fa 00 	sd	\$ra,-32\(\$sp\)
 47c:	f7 df fa 00 	sd	\$ra,-64\(\$sp\)
 480:	f7 9f fa 00 	sd	\$ra,-128\(\$sp\)
 484:	f7 1f fa 00 	sd	\$ra,-256\(\$sp\)
 488:	f6 1f fa 00 	sd	\$ra,-512\(\$sp\)
 48c:	f4 1f fa 00 	sd	\$ra,-1024\(\$sp\)
 490:	f0 1f fa 00 	sd	\$ra,-2048\(\$sp\)
 494:	db 40       	sw	\$v0,0\(\$v1\)
 496:	f0 00 db 41 	sw	\$v0,1\(\$v1\)
 49a:	f0 00 db 42 	sw	\$v0,2\(\$v1\)
 49e:	f0 00 db 43 	sw	\$v0,3\(\$v1\)
 4a2:	db 41       	sw	\$v0,4\(\$v1\)
 4a4:	db 42       	sw	\$v0,8\(\$v1\)
 4a6:	db 44       	sw	\$v0,16\(\$v1\)
 4a8:	db 48       	sw	\$v0,32\(\$v1\)
 4aa:	db 50       	sw	\$v0,64\(\$v1\)
 4ac:	f0 80 db 40 	sw	\$v0,128\(\$v1\)
 4b0:	f1 00 db 40 	sw	\$v0,256\(\$v1\)
 4b4:	f2 00 db 40 	sw	\$v0,512\(\$v1\)
 4b8:	f4 00 db 40 	sw	\$v0,1024\(\$v1\)
 4bc:	f0 01 db 40 	sw	\$v0,2048\(\$v1\)
 4c0:	f7 ff db 5f 	sw	\$v0,-1\(\$v1\)
 4c4:	f7 ff db 5e 	sw	\$v0,-2\(\$v1\)
 4c8:	f7 ff db 5d 	sw	\$v0,-3\(\$v1\)
 4cc:	f7 ff db 5c 	sw	\$v0,-4\(\$v1\)
 4d0:	f7 ff db 58 	sw	\$v0,-8\(\$v1\)
 4d4:	f7 ff db 50 	sw	\$v0,-16\(\$v1\)
 4d8:	f7 ff db 40 	sw	\$v0,-32\(\$v1\)
 4dc:	f7 df db 40 	sw	\$v0,-64\(\$v1\)
 4e0:	f7 9f db 40 	sw	\$v0,-128\(\$v1\)
 4e4:	f7 1f db 40 	sw	\$v0,-256\(\$v1\)
 4e8:	f6 1f db 40 	sw	\$v0,-512\(\$v1\)
 4ec:	f4 1f db 40 	sw	\$v0,-1024\(\$v1\)
 4f0:	f0 1f db 40 	sw	\$v0,-2048\(\$v1\)
 4f4:	d2 00       	sw	\$v0,0\(\$sp\)
 4f6:	f0 00 d2 01 	sw	\$v0,1\(\$sp\)
 4fa:	f0 00 d2 02 	sw	\$v0,2\(\$sp\)
 4fe:	f0 00 d2 03 	sw	\$v0,3\(\$sp\)
 502:	d2 01       	sw	\$v0,4\(\$sp\)
 504:	d2 02       	sw	\$v0,8\(\$sp\)
 506:	d2 04       	sw	\$v0,16\(\$sp\)
 508:	d2 08       	sw	\$v0,32\(\$sp\)
 50a:	d2 10       	sw	\$v0,64\(\$sp\)
 50c:	d2 20       	sw	\$v0,128\(\$sp\)
 50e:	d2 40       	sw	\$v0,256\(\$sp\)
 510:	d2 80       	sw	\$v0,512\(\$sp\)
 512:	f4 00 d2 00 	sw	\$v0,1024\(\$sp\)
 516:	f0 01 d2 00 	sw	\$v0,2048\(\$sp\)
 51a:	f7 ff d2 1f 	sw	\$v0,-1\(\$sp\)
 51e:	f7 ff d2 1e 	sw	\$v0,-2\(\$sp\)
 522:	f7 ff d2 1d 	sw	\$v0,-3\(\$sp\)
 526:	f7 ff d2 1c 	sw	\$v0,-4\(\$sp\)
 52a:	f7 ff d2 18 	sw	\$v0,-8\(\$sp\)
 52e:	f7 ff d2 10 	sw	\$v0,-16\(\$sp\)
 532:	f7 ff d2 00 	sw	\$v0,-32\(\$sp\)
 536:	f7 df d2 00 	sw	\$v0,-64\(\$sp\)
 53a:	f7 9f d2 00 	sw	\$v0,-128\(\$sp\)
 53e:	f7 1f d2 00 	sw	\$v0,-256\(\$sp\)
 542:	f6 1f d2 00 	sw	\$v0,-512\(\$sp\)
 546:	f4 1f d2 00 	sw	\$v0,-1024\(\$sp\)
 54a:	f0 1f d2 00 	sw	\$v0,-2048\(\$sp\)
 54e:	62 00       	sw	\$ra,0\(\$sp\)
 550:	f0 00 62 01 	sw	\$ra,1\(\$sp\)
 554:	f0 00 62 02 	sw	\$ra,2\(\$sp\)
 558:	f0 00 62 03 	sw	\$ra,3\(\$sp\)
 55c:	62 01       	sw	\$ra,4\(\$sp\)
 55e:	62 02       	sw	\$ra,8\(\$sp\)
 560:	62 04       	sw	\$ra,16\(\$sp\)
 562:	62 08       	sw	\$ra,32\(\$sp\)
 564:	62 10       	sw	\$ra,64\(\$sp\)
 566:	62 20       	sw	\$ra,128\(\$sp\)
 568:	62 40       	sw	\$ra,256\(\$sp\)
 56a:	62 80       	sw	\$ra,512\(\$sp\)
 56c:	f4 00 62 00 	sw	\$ra,1024\(\$sp\)
 570:	f0 01 62 00 	sw	\$ra,2048\(\$sp\)
 574:	f7 ff 62 1f 	sw	\$ra,-1\(\$sp\)
 578:	f7 ff 62 1e 	sw	\$ra,-2\(\$sp\)
 57c:	f7 ff 62 1d 	sw	\$ra,-3\(\$sp\)
 580:	f7 ff 62 1c 	sw	\$ra,-4\(\$sp\)
 584:	f7 ff 62 18 	sw	\$ra,-8\(\$sp\)
 588:	f7 ff 62 10 	sw	\$ra,-16\(\$sp\)
 58c:	f7 ff 62 00 	sw	\$ra,-32\(\$sp\)
 590:	f7 df 62 00 	sw	\$ra,-64\(\$sp\)
 594:	f7 9f 62 00 	sw	\$ra,-128\(\$sp\)
 598:	f7 1f 62 00 	sw	\$ra,-256\(\$sp\)
 59c:	f6 1f 62 00 	sw	\$ra,-512\(\$sp\)
 5a0:	f4 1f 62 00 	sw	\$ra,-1024\(\$sp\)
 5a4:	f0 1f 62 00 	sw	\$ra,-2048\(\$sp\)
 5a8:	cb 40       	sh	\$v0,0\(\$v1\)
 5aa:	f0 00 cb 41 	sh	\$v0,1\(\$v1\)
 5ae:	cb 41       	sh	\$v0,2\(\$v1\)
 5b0:	f0 00 cb 43 	sh	\$v0,3\(\$v1\)
 5b4:	cb 42       	sh	\$v0,4\(\$v1\)
 5b6:	cb 44       	sh	\$v0,8\(\$v1\)
 5b8:	cb 48       	sh	\$v0,16\(\$v1\)
 5ba:	cb 50       	sh	\$v0,32\(\$v1\)
 5bc:	f0 40 cb 40 	sh	\$v0,64\(\$v1\)
 5c0:	f0 80 cb 40 	sh	\$v0,128\(\$v1\)
 5c4:	f1 00 cb 40 	sh	\$v0,256\(\$v1\)
 5c8:	f2 00 cb 40 	sh	\$v0,512\(\$v1\)
 5cc:	f4 00 cb 40 	sh	\$v0,1024\(\$v1\)
 5d0:	f0 01 cb 40 	sh	\$v0,2048\(\$v1\)
 5d4:	f7 ff cb 5f 	sh	\$v0,-1\(\$v1\)
 5d8:	f7 ff cb 5e 	sh	\$v0,-2\(\$v1\)
 5dc:	f7 ff cb 5d 	sh	\$v0,-3\(\$v1\)
 5e0:	f7 ff cb 5c 	sh	\$v0,-4\(\$v1\)
 5e4:	f7 ff cb 58 	sh	\$v0,-8\(\$v1\)
 5e8:	f7 ff cb 50 	sh	\$v0,-16\(\$v1\)
 5ec:	f7 ff cb 40 	sh	\$v0,-32\(\$v1\)
 5f0:	f7 df cb 40 	sh	\$v0,-64\(\$v1\)
 5f4:	f7 9f cb 40 	sh	\$v0,-128\(\$v1\)
 5f8:	f7 1f cb 40 	sh	\$v0,-256\(\$v1\)
 5fc:	f6 1f cb 40 	sh	\$v0,-512\(\$v1\)
 600:	f4 1f cb 40 	sh	\$v0,-1024\(\$v1\)
 604:	f0 1f cb 40 	sh	\$v0,-2048\(\$v1\)
 608:	c3 40       	sb	\$v0,0\(\$v1\)
 60a:	c3 41       	sb	\$v0,1\(\$v1\)
 60c:	c3 42       	sb	\$v0,2\(\$v1\)
 60e:	c3 43       	sb	\$v0,3\(\$v1\)
 610:	c3 44       	sb	\$v0,4\(\$v1\)
 612:	c3 48       	sb	\$v0,8\(\$v1\)
 614:	c3 50       	sb	\$v0,16\(\$v1\)
 616:	f0 20 c3 40 	sb	\$v0,32\(\$v1\)
 61a:	f0 40 c3 40 	sb	\$v0,64\(\$v1\)
 61e:	f0 80 c3 40 	sb	\$v0,128\(\$v1\)
 622:	f1 00 c3 40 	sb	\$v0,256\(\$v1\)
 626:	f2 00 c3 40 	sb	\$v0,512\(\$v1\)
 62a:	f4 00 c3 40 	sb	\$v0,1024\(\$v1\)
 62e:	f0 01 c3 40 	sb	\$v0,2048\(\$v1\)
 632:	f7 ff c3 5f 	sb	\$v0,-1\(\$v1\)
 636:	f7 ff c3 5e 	sb	\$v0,-2\(\$v1\)
 63a:	f7 ff c3 5d 	sb	\$v0,-3\(\$v1\)
 63e:	f7 ff c3 5c 	sb	\$v0,-4\(\$v1\)
 642:	f7 ff c3 58 	sb	\$v0,-8\(\$v1\)
 646:	f7 ff c3 50 	sb	\$v0,-16\(\$v1\)
 64a:	f7 ff c3 40 	sb	\$v0,-32\(\$v1\)
 64e:	f7 df c3 40 	sb	\$v0,-64\(\$v1\)
 652:	f7 9f c3 40 	sb	\$v0,-128\(\$v1\)
 656:	f7 1f c3 40 	sb	\$v0,-256\(\$v1\)
 65a:	f6 1f c3 40 	sb	\$v0,-512\(\$v1\)
 65e:	f4 1f c3 40 	sb	\$v0,-1024\(\$v1\)
 662:	f0 1f c3 40 	sb	\$v0,-2048\(\$v1\)
 666:	6a 00       	li	\$v0,0
 668:	6a 01       	li	\$v0,1
 66a:	f1 00 6a 00 	li	\$v0,256
 66e:	67 5e       	move	\$v0,\$s8
 670:	65 92       	move	\$s4,\$v0
 672:	43 50       	daddiu	\$v0,\$v1,0
 674:	43 51       	daddiu	\$v0,\$v1,1
 676:	43 5f       	daddiu	\$v0,\$v1,-1
 678:	f0 10 43 50 	daddiu	\$v0,\$v1,16
 67c:	f7 ff 43 50 	daddiu	\$v0,\$v1,-16
 680:	e3 88       	daddu	\$v0,\$v1,\$a0
 682:	fd 40       	daddiu	\$v0,0
 684:	fd 41       	daddiu	\$v0,1
 686:	fd 5f       	daddiu	\$v0,-1
 688:	f0 20 fd 40 	daddiu	\$v0,32
 68c:	f7 ff fd 40 	daddiu	\$v0,-32
 690:	f0 80 fd 40 	daddiu	\$v0,128
 694:	f7 9f fd 40 	daddiu	\$v0,-128
 698:	f1 7f fe 48 	dla	\$v0,0 <data1>
 69c:	f0 80 fe 40 	dla	\$v0,71c <data2>
 6a0:	f1 c0 fe 48 	dla	\$v0,868 <bar>
 6a4:	f2 80 fe 4c 	dla	\$v0,930 <quux>
 6a8:	fb 00       	daddiu	\$sp,0
 6aa:	f0 00 fb 01 	daddiu	\$sp,1
 6ae:	f7 ff fb 1f 	daddiu	\$sp,-1
 6b2:	fb 20       	daddiu	\$sp,256
 6b4:	fb e0       	daddiu	\$sp,-256
 6b6:	ff 40       	daddiu	\$v0,\$sp,0
 6b8:	f0 00 ff 41 	daddiu	\$v0,\$sp,1
 6bc:	f7 ff ff 5f 	daddiu	\$v0,\$sp,-1
 6c0:	ff 48       	daddiu	\$v0,\$sp,32
 6c2:	f7 ff ff 40 	daddiu	\$v0,\$sp,-32
 6c6:	f0 80 ff 40 	daddiu	\$v0,\$sp,128
 6ca:	f7 9f ff 40 	daddiu	\$v0,\$sp,-128
 6ce:	43 40       	addiu	\$v0,\$v1,0
 6d0:	43 41       	addiu	\$v0,\$v1,1
 6d2:	43 4f       	addiu	\$v0,\$v1,-1
 6d4:	f0 10 43 40 	addiu	\$v0,\$v1,16
 6d8:	f7 ff 43 40 	addiu	\$v0,\$v1,-16
 6dc:	e3 89       	addu	\$v0,\$v1,\$a0
 6de:	4a 00       	addiu	\$v0,0
 6e0:	4a 01       	addiu	\$v0,1
 6e2:	4a ff       	addiu	\$v0,-1
 6e4:	4a 20       	addiu	\$v0,32
 6e6:	4a e0       	addiu	\$v0,-32
 6e8:	f0 80 4a 00 	addiu	\$v0,128
 6ec:	4a 80       	addiu	\$v0,-128
 6ee:	f1 1f 0a 10 	la	\$v0,0 <data1>
 6f2:	0a 0b       	la	\$v0,71c <data2>
 6f4:	0a 5d       	la	\$v0,868 <bar>
 6f6:	0a 8f       	la	\$v0,930 <quux>
 6f8:	63 00       	addiu	\$sp,0
 6fa:	f0 00 63 01 	addiu	\$sp,1
 6fe:	f7 ff 63 1f 	addiu	\$sp,-1
 702:	63 20       	addiu	\$sp,256
 704:	63 e0       	addiu	\$sp,-256
 706:	02 00       	addiu	\$v0,\$sp,0
 708:	f0 00 02 01 	addiu	\$v0,\$sp,1
 70c:	f7 ff 02 1f 	addiu	\$v0,\$sp,-1
 710:	02 08       	addiu	\$v0,\$sp,32
 712:	f7 ff 02 00 	addiu	\$v0,\$sp,-32
 716:	02 20       	addiu	\$v0,\$sp,128
 718:	f7 9f 02 00 	addiu	\$v0,\$sp,-128

0+00071c <data2>:
 71c:	00 00 00 00 	nop

0+000720 <insns2>:
 720:	e3 8a       	dsubu	\$v0,\$v1,\$a0
 722:	e3 8b       	subu	\$v0,\$v1,\$a0
 724:	ea 6b       	neg	\$v0,\$v1
 726:	ea 6c       	and	\$v0,\$v1
 728:	ea 6d       	or	\$v0,\$v1
 72a:	ea 6e       	xor	\$v0,\$v1
 72c:	ea 6f       	not	\$v0,\$v1
 72e:	52 00       	slti	\$v0,0
 730:	52 01       	slti	\$v0,1
 732:	f7 ff 52 1f 	slti	\$v0,-1
 736:	52 ff       	slti	\$v0,255
 738:	f1 00 52 00 	slti	\$v0,256
 73c:	ea 62       	slt	\$v0,\$v1
 73e:	5a 00       	sltiu	\$v0,0
 740:	5a 01       	sltiu	\$v0,1
 742:	f7 ff 5a 1f 	sltiu	\$v0,-1
 746:	5a ff       	sltiu	\$v0,255
 748:	f1 00 5a 00 	sltiu	\$v0,256
 74c:	ea 63       	sltu	\$v0,\$v1
 74e:	72 00       	cmpi	\$v0,0
 750:	72 01       	cmpi	\$v0,1
 752:	72 ff       	cmpi	\$v0,255
 754:	f1 00 72 00 	cmpi	\$v0,256
 758:	ea 6a       	cmp	\$v0,\$v1
 75a:	f0 00 32 61 	dsll	\$v0,\$v1,0
 75e:	32 65       	dsll	\$v0,\$v1,1
 760:	32 61       	dsll	\$v0,\$v1,8
 762:	f2 40 32 61 	dsll	\$v0,\$v1,9
 766:	f7 e0 32 61 	dsll	\$v0,\$v1,63
 76a:	eb 54       	dsllv	\$v0,\$v1
 76c:	f0 00 e8 48 	dsrl	\$v0,0
 770:	e9 48       	dsrl	\$v0,1
 772:	e8 48       	dsrl	\$v0,0
 774:	f2 40 e8 48 	dsrl	\$v0,9
 778:	f7 e0 e8 48 	dsrl	\$v0,63
 77c:	eb 56       	dsrlv	\$v0,\$v1
 77e:	f0 00 e8 53 	dsra	\$v0,0
 782:	e9 53       	dsra	\$v0,1
 784:	e8 53       	dsra	\$v0,0
 786:	f2 40 e8 53 	dsra	\$v0,9
 78a:	f7 e0 e8 53 	dsra	\$v0,63
 78e:	eb 57       	dsrav	\$v0,\$v1
 790:	ea 12       	mflo	\$v0
 792:	eb 10       	mfhi	\$v1
 794:	f0 00 32 60 	sll	\$v0,\$v1,0
 798:	32 64       	sll	\$v0,\$v1,1
 79a:	32 60       	sll	\$v0,\$v1,8
 79c:	f2 40 32 60 	sll	\$v0,\$v1,9
 7a0:	f7 c0 32 60 	sll	\$v0,\$v1,31
 7a4:	eb 44       	sllv	\$v0,\$v1
 7a6:	f0 00 32 62 	srl	\$v0,\$v1,0
 7aa:	32 66       	srl	\$v0,\$v1,1
 7ac:	32 62       	srl	\$v0,\$v1,8
 7ae:	f2 40 32 62 	srl	\$v0,\$v1,9
 7b2:	f7 c0 32 62 	srl	\$v0,\$v1,31
 7b6:	eb 46       	srlv	\$v0,\$v1
 7b8:	f0 00 32 63 	sra	\$v0,\$v1,0
 7bc:	32 67       	sra	\$v0,\$v1,1
 7be:	32 63       	sra	\$v0,\$v1,8
 7c0:	f2 40 32 63 	sra	\$v0,\$v1,9
 7c4:	f7 c0 32 63 	sra	\$v0,\$v1,31
 7c8:	eb 47       	srav	\$v0,\$v1
 7ca:	ea 7c       	dmult	\$v0,\$v1
 7cc:	ea 7d       	dmultu	\$v0,\$v1
 7ce:	ea 7e       	ddiv	\$zero,\$v0,\$v1
 7d0:	2b 01       	bnez	\$v1,7d4 <insns2\+b4>
 7d2:	e8 e5       	break	7
 7d4:	ea 12       	mflo	\$v0
 7d6:	65 00       	nop
 7d8:	65 00       	nop
 7da:	ea 7f       	ddivu	\$zero,\$v0,\$v1
 7dc:	2b 01       	bnez	\$v1,7e0 <insns2\+c0>
 7de:	e8 e5       	break	7
 7e0:	ea 12       	mflo	\$v0
 7e2:	65 00       	nop
 7e4:	65 00       	nop
 7e6:	ea 78       	mult	\$v0,\$v1
 7e8:	ea 79       	multu	\$v0,\$v1
 7ea:	ea 7a       	div	\$zero,\$v0,\$v1
 7ec:	2b 01       	bnez	\$v1,7f0 <insns2\+d0>
 7ee:	e8 e5       	break	7
 7f0:	ea 12       	mflo	\$v0
 7f2:	65 00       	nop
 7f4:	65 00       	nop
 7f6:	ea 7b       	divu	\$zero,\$v0,\$v1
 7f8:	2b 01       	bnez	\$v1,7fc <insns2\+dc>
 7fa:	e8 e5       	break	7
 7fc:	ea 12       	mflo	\$v0
 7fe:	ea 00       	jr	\$v0
 800:	65 00       	nop
 802:	e8 20       	jr	\$ra
 804:	65 00       	nop
 806:	ea 40       	jalr	\$v0
 808:	65 00       	nop
 80a:	f3 ff 22 1b 	beqz	\$v0,4 <insns1>
 80e:	22 88       	beqz	\$v0,720 <insns2>
 810:	22 2b       	beqz	\$v0,868 <bar>
 812:	f0 80 22 0d 	beqz	\$v0,930 <quux>
 816:	f3 ff 2a 15 	bnez	\$v0,4 <insns1>
 81a:	2a 82       	bnez	\$v0,720 <insns2>
 81c:	2a 25       	bnez	\$v0,868 <bar>
 81e:	f0 80 2a 07 	bnez	\$v0,930 <quux>
 822:	f3 ff 60 0f 	bteqz	4 <insns1>
 826:	f7 7f 60 1b 	bteqz	720 <insns2>
 82a:	60 1e       	bteqz	868 <bar>
 82c:	f0 80 60 00 	bteqz	930 <quux>
 830:	f3 ff 61 08 	btnez	4 <insns1>
 834:	f7 7f 61 14 	btnez	720 <insns2>
 838:	61 17       	btnez	868 <bar>
 83a:	61 7a       	btnez	930 <quux>
 83c:	f3 ff 10 02 	b	4 <insns1>
 840:	17 6f       	b	720 <insns2>
 842:	10 12       	b	868 <bar>
 844:	10 75       	b	930 <quux>
 846:	e8 05       	break	0
 848:	e8 25       	break	1
 84a:	ef e5       	break	63
 84c:	18 00 00 00 	jal	0 <data1>
			84c: R_MIPS16_26	extern
 850:	65 00       	nop
 852:	e8 09       	entry	
 854:	e9 09       	entry	a0
 856:	eb 49       	entry	a0-a2,s0
 858:	e8 a9       	entry	s0-s1,ra
 85a:	e8 29       	entry	ra
 85c:	ef 09       	exit	
 85e:	ef 49       	exit	s0
 860:	ef a9       	exit	s0-s1,ra
 862:	ef 29       	exit	ra
 864:	00 00       	addiu	\$s0,\$sp,0
	...

0+000868 <bar>:
	...
