#!/bin/bash
# 1. take a rectangle screenshot
# 2. save to clipboard
# 3. post notification

scrot -s '/tmp/%F_%T_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f' & xmessage -timeout 2 "Draw a box to screenshot!"
