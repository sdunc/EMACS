xsetroot -solid "#004c3d" &
xclock -geometry -0+0 &
exec /usr/lib/notification-daemon/notification-daemon &
xrdb ~/.Xresources &
xmodmap ~/.Xmodmap &
exec openbsd-cwm
