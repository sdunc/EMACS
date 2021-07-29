xsetroot -solid "#004c3d" &
xclock -digital -update 1 -fg black -bg "white" -padding 2 -fn "-adobe-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*" -geometry 1920x20+0+0 &
setxkbmap -option caps:escape &
xrdb ~/.Xresources &
xmodmap ~/.Xmodmap &
exec openbsd-cwm
