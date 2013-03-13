#!/bin/sh
makeinfo --html --output=html g100.texi
texi2dvi g100.texi
texi2pdf g100.texi
rm g100.aux g100.cp g100.fn g100.ky g100.log g100.pg g100.toc g100.tp g100.vr
