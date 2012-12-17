#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# exec gnome-session
#exec startkde
# exec startxfce4
# ...or the Window Manager of your choice

#run systemd as user intsance
systemd --user &

#exec xcompmgr -cC -t-5 -l-6 -r5 &
#exec xcompmgr -cCfF -t-5 -l-5 -r4.2 -o.55 -D5 &
#exec xcompmgr -cC -t-5 -l-5 -r4.2 -o.55 &
#exec xcompmgr -cC -o.55 &

#feh --bg-center ~/pictures/wallpapers/bruce.jpg
#feh --bg-fill ~/pictures/norway-beauty.jpg
#feh --bg-fill ~/pictures/Wallpapers/alley-stairs-dark.jpg
#feh --bg-center ~/pictures/wallpapers/hd/wallpaper-1411581.png pictures/wallpapers/wallpaper-663431.jpg
#feh --bg-center pictures/wallpapers/asoiaf/stark.jpg pictures/wallpapers/asoiaf/targaryen.jpg 
feh --bg-scale pictures/wallpapers/winter/o1027033.jpg pictures/wallpapers/rain/o1027636.jpg


xsetroot -cursor_name left_ptr


if [ -f $HOME/.config/xmodmap ]; then
    /usr/bin/xmodmap $HOME/.config/xmodmap
fi

# sane keyboard settings for norwegian keyboard
setxkbmap -layout no
setxkbmap -option terminate:ctrl_alt_bksp

#xmodmap -e 'keycode 35 = 0x7e'
# swap Caps Lock and CTRL!
#xmodmap /home/rolf/.config/swap-ctrl-caps.kmap

xset +fp /usr/share/fonts/local
xset fp rehash

#android-receiver &
#wicd-gtk &

#(sleep 2s && trayer --edge top --align right --SetDockType true --expand true --widthtype pixel --width 80 --heighttype pixel --height 12 --expand true --tint 0x202020 --transparent true --alpha 0) &
transmission-remote-gtk &

#source /etc/X11/xinit/xinitrc.d/30-dbus
#exec ck-launch-session xmonad

xmonad
#exec openbox-session
#herbstluftwm

#ck-launch-session xmonad
#dbus-launch xmonad
#exec awesome
#exec subtle
#urxvt -e screen
#exec wmfs
#exec dwm
