#!/bin/sh
convert -extract 20x33+130+0 stencil.gif E.png
convert -extract 20x33+130+0 stencil.gif E.eps
echo "E" > E.txt
convert -extract 20x33+195+59 stencil.gif T.png
convert -extract 20x33+195+59 stencil.gif T.eps
echo "T" > T.txt
