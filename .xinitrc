#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# exec gnome-session
#exec startkde
# exec startxfce4
# ...or the Window Manager of your choice

#exec xcompmgr -cC -t-5 -l-6 -r5 &
#exec xcompmgr -cCfF -t-5 -l-5 -r4.2 -o.55 -D5 &
#exec xcompmgr -cC -t-5 -l-5 -r4.2 -o.55 &
#exec xcompmgr -cC -o.55 &

#feh --bg-center ~/pictures/wallpapers/bruce.jpg
#feh --bg-fill ~/pictures/norway-beauty.jpg
feh --bg-fill ~/pictures/Wallpapers/wallpaper-13395.jpg
#feh --bg-fill ~/pictures/Wallpapers/alley-stairs-dark.jpg

xsetroot -cursor_name left_ptr

# sane keyboard settings for norwegian keyboard
setxkbmap -layout no
setxkbmap -option terminate:ctrl_alt_bksp

if [ -f $HOME/.config/xmodmap ]; then
    /usr/bin/xmodmap $HOME/.config/xmodmap
fi

#xmodmap -e 'keycode 35 = 0x7e'
# swap Caps Lock and CTRL!
#xmodmap /home/rolf/.config/swap-ctrl-caps.kmap

xset +fp /usr/share/fonts/local
xset fp rehash

android-receiver &

(sleep 2s && trayer --edge top --align right --SetDockType true --expand true --width 5 --height 12 --transparent true --tint 0x000000) &

#source /etc/X11/xinit/xinitrc.d/30-dbus
#exec ck-launch-session xmonad
xmonad

#ck-launch-session xmonad
#dbus-launch xmonad
#exec awesome
#exec subtle
#urxvt -e screen
#exec wmfs
#exec dwm
