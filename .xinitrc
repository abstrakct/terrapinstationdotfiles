#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# exec gnome-session
#exec startkde
# exec startxfce4
# ...or the Window Manager of your choice

exec xcompmgr -cC -t-5 -l-6 -r5 &
feh --bg-fill ~/Pictures/Wallpapers/wallpaper-162180.jpg
xsetroot -cursor_name left_ptr
setxkbmap -layout no
setxkbmap -option terminate:ctrl_alt_bksp
xset +fp /usr/share/fonts/local
xset fp rehash
android-receiver &

(sleep 2s && trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 2 --height 12 --transparent true --tint 0x000000) &

#ck-launch-session xmonad
#dbus-launch xmonad
xmonad

#exec awesome
#exec subtle
#urxvt -e screen
#exec wmfs
#exec dwm
