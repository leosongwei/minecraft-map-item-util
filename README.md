minecraft-map-item-util
=======================

A script generation minecraft map item file.

<img src="testoutput_wRGB.png" />

This software is released in public domain, without any warranty, use at your own risks.

How to
------

1. `cmake CMakeCache.txt`
2. edit and execute `main.lisp` with SBCL
3. `$ cat testmap.dat | gzip - > map_20002.dat`
4. In the game: `/give player minecraft:filled_map 1 20002`
