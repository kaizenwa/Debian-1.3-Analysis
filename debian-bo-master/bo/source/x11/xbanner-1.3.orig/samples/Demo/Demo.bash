#!/bin/sh

echo "This is the XBanner v1.3 demonstration script."
echo ""

if [ -x ../../xbanner -a -x ../../freetemp ]; then
  echo -n ""
else
  echo "Please make sure that the xbanner and freetemp binaries have been"
  echo "successfuly compiled. (Usually 'make' in the top directory is enough.)"
  exit
fi

echo "Please make sure that the top 2/3 of your screen is not obscured by any"
echo "application / utility windows. Then press ENTER. Closing all other apps"
echo "including FVWM's buttons/iconbox is recommended."
echo ""
echo "Notice that these demo configurations were designed for 1280x1024. I did"
echo "check them and they work fine in 1024x768 but some things aren't in the"
echo "right place."

echo ""
echo "Press ENTER to continue"
read

echo "The first effect is  3D-Shadow  and is shown here with a Fan style"
echo "background, made of a color gradient of Red,Green,Blue,Red and a"
echo "repeating period of 3 times."
../../freetemp && ../../xbanner -file ./3D-Shadow
echo "Press ENTER to continue"
read

echo "Next is the  Backlight  effect which is shown with a TopDown style"
echo "background. The text is underlined and you can see that the effect"
echo "is rendered on the underline as well."
../../freetemp && ../../xbanner -file ./Backlight
echo "Press ENTER to continue"
read

echo "The  Backlight  effect can be used in other ways, and here is a nice"
echo "example. The color gradient definition for the background used in"
echo "this sample is darkblue,darkblue,black,darkgreen - which makes the"
echo "entire top part seem darkblue, and only start fading into black later."
../../freetemp && ../../xbanner -file ./Backlight2
echo "Press ENTER to continue"
read

echo "The  Coin  effect given the letters of the text a 'rim', which can be"
echo "made to look like the rim of a lot of coins. With a background style"
echo "of LeftDiagonal and ShadowColor/HiColor selected properly this is a"
echo "neat sample. The Penguin pixmap by Larry Ewing adds a nice touch."
../../freetemp && ../../xbanner -file ./Coin
echo "Press ENTER to continue"
read

echo "The  Fade  effect makes the letters seem to have a thickness which"
echo "changes colors. Selected properly one can achieve nice results. The"
echo "glinting on the letters every few seconds is a new feature of V1.3"
../../freetemp && ../../xbanner -file ./Fade
echo "Press ENTER to continue"
read

echo "This is the  FatText  effect. I really didn't know what to call it."
../../freetemp && ../../xbanner -file ./FatText
echo "Press ENTER to continue"
read

echo "This is another idea how to use the  FatText  effect. Here, a"
echo "background style of Fan using the colors of the rainbow decorate"
echo "it repeated twice."
../../freetemp && ../../xbanner -file ./FatText2
echo "Press ENTER to continue"
read

echo "This is the  FgGrad  effect with a LeftRight background style and"
echo "an underline under the text."
../../freetemp && ../../xbanner -file ./FgGrad
echo "Press ENTER to continue"
read

echo "The  FgPlasma  effect can take a long time. Especially combined with"
echo "background style of Plasma. I hope the results are satisfying enough."
../../freetemp && ../../xbanner -file ./FgPlasma
echo "Press ENTER to continue"
read

echo "The  FunnyOutline  effect. Combined with a LeftSplit background style."
../../freetemp && ../../xbanner -file ./FunnyOutline
echo "Press ENTER to continue"
read

echo "The  PopArt  effect with a RightSplit background style."
../../freetemp && ../../xbanner -file ./PopArt
echo "Press ENTER to continue"
read

echo "The  Shadowed-Outline  effect with a LeftRight background."
../../freetemp && ../../xbanner -file ./Shadowed-Outline
echo "Press ENTER to continue"
read

echo "The  Shake  effect with the backgroung style BgPix - tiled pixmap."
../../freetemp && ../../xbanner -file ./Shake
echo "Press ENTER to continue"
read

echo "The  StandOut  effect, using the BgFill feature to fill the background"
echo "instead of an external program. Glints are not done on the DownLeft"
echo "corners to be consistent with the lighting angle."
../../freetemp && ../../xbanner -file ./StandOut
echo "Press ENTER to continue"
read

echo "The  Thick  Effect, using a Fan background style, with a repeat period"
echo "of 20, a color gradient of white->black->white and a bar-size of 1 pixel."
../../freetemp && ../../xbanner -file ./Thick
echo "Press ENTER to continue"
read

echo "The  StandOut2  effect, seen here with Glinting on, using 2 separate"
echo "resource files where the second one uses the CenteredOnY placement"
echo "type to place the second line of text. This effect is similar to the"
echo "StandOut effect only the effect is rendered into the painted area of"
echo "the letters instead of around them."
../../freetemp && ../../xbanner -file ./StandOut2_1 && ../../xbanner -file ./StandOut2_2
echo "Press ENTER to continue"
read

echo "The demo will now try to run several more effects, using color-cycling."
echo "Color-cycling is not supported on TrueColor screens. So if your screen"
echo "is currently set to 32K colors or more (i.e. > 8 bits per pixel), then"
echo "please answer NO to the following question. If unsure, try yes. The"
echo "demo will stop by itself if your screen cannot handle it."
echo ""
echo -n "Want to try the color-cycling part of the demo? [Yes] : "
read d

../../freetemp

if [ -z "$d" -o "$d" = "yes" -o "$d" = "Yes" -o "$d" = "YES" -o "$d" = "y" -o "$d" = "Y" ]; then
  echo "The  FgPlasma  effect in white and magenta, with the LeftSplit background"
  echo "style, and the colors of the FgPlasma cycling slowly."
  ../../xbanner -file ./Cyc_FgPlasma || exit
  echo "Press ENTER to continue"
  read

  echo "The  Fade  effect with a TopDown background style, repeated 8 times,"
  echo "and color cycling is done on the Fade effect as well as the background."
  ../../freetemp && ../../xbanner -file ./Cyc_Fade_TopDown
  echo "Press ENTER to continue"
  read

  echo "The simple  Shadow  effect gets a new touch with the option to cycle the"
  echo "foreground color along a color gradient. The LeftSplit background also"
  echo "looks nice when you color-cycle it."
  ../../freetemp && ../../xbanner -file ./Cyc_FGC_LeftSplit
  echo "Press ENTER to continue"
  read

  echo "A nice  FgGrad  effect with a cycling LeftRight background underneath."
  ../../freetemp && ../../xbanner -file ./Cyc_LeftRight
  echo "Press ENTER to continue"
  read

  echo "Here's another idea what to do with  FatText  effect. Here I also cycle"
  echo "the FatText's color gradient."
  ../../freetemp && ../../xbanner -file ./Cyc_FatText
  echo "Press ENTER to continue"
  read

  echo "A Plasma background style color-cycled with a simple Shadowed-Outline"
  echo "effect as the main text render."
  ../../freetemp && ../../xbanner -file ./Cyc_Plasma
  echo "Press ENTER to continue"
  read

  echo "Ripples background style color-cycled with FgPlasma also cycled"
  echo "as the text rendering effect."
  ../../freetemp && ../../xbanner -file ./Cyc_Ripples
  echo "Press ENTER to continue"
  read

  echo "The  Fan  background style can produce things like spot-lights."
  ../../freetemp && ../../xbanner -file ./Cyc_Fan
  echo "Press ENTER to continue"
  read

  echo "That's it for now. If you have any ideas, suggestions or bug reports"
  echo "Please feel free to email me at:"
  echo ""
  echo "Amit Margalit <amitm@doronx.iso.dec.com>"
  echo ""
  echo ""
  echo "Press ENTER to stop the color cycling."
  read
  ../../freetemp

fi

# EOF #