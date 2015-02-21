# cl-crawler
2.5d Engine / Dungeon crawler (clone of notch's Prelude of the Chambered (https://www.youtube.com/watch?v=rhN35bGvM8c)) in Common Lisp

This is an attempt to redo the above mentioned game in Common Lisp. To get things running you will need
- sdl2kit
- png-read
- static-vectors
Best is, pull and compile them via quicklisp.

For now follwing things are working:
- rendering of floor and ceiling
- texture mapping of floor and ceiling
- shading
 
The intention was to experiment with writing a game and beeing able to change things more interactively than notch himself is doing in the video.

I tested the code under Ubuntu 14.04, 64bit + SBCL in a VM (some thing like 25 fps), and Mac OSX CCL (35 fps) and ECL (<10 fps).  

