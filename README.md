2.5d Engine / Dungeon crawler (clone of notch's Prelude of the Chambered (https://www.youtube.com/watch?v=rhN35bGvM8c)) in Common Lisp

This is an attempt to redo the above mentioned game in Common Lisp. To get things running you will need
- sdl2kit
- png-read
- static-vectors

Best is, pull and compile them via quicklisp.
Note that you will have to get some texture (like 64x64 or 32x32 in PNG format) and edit the call 

    (setf *image* (load-png #p"~/Dropbox/texture.png")) 

in the function basic-test.

For now following things are working:
- rendering of floor and ceiling
- texture mapping of floor and ceiling with the same texture
- shading
 
The intention was to experiment with writing a game and beeing able to change things more interactively than notch himself is doing in the video. Once you have the programm running you can e.g. :
- change the width and height -> resulting in a better or worse looking quality which has of course high impact on the performance
- load different textures
- change the behaviour of rendering or post processing (shading in general)
- swith of post processing completely
All these things and more you can do on the fly while the program is running. You could achieve this with using Emacs + Slime. 
The code uses a resolution of 160x120 pixels (global parameters width and height) and scales up to whatever resolution you want (global parameters window-with and window-height).

The code is tested under Ubuntu 14.04, 64bit + SBCL in a VM (~25 fps), and Mac OSX Yosemit,64bit  +  CCL (~35 fps) and ECL (<10 fps). 

Please note, everything is done on a per pixel basis with the cpu, the gpu is only used for scaling and blitting the resulting  picture to the screen. 
