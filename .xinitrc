#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# exec gnome-session
#exec startkde
# exec startxfce4
# ...or the Window Manager of your choice

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

#run systemd as user intsance
#systemd --user &


#exec xcompmgr -cC -t-5 -l-6 -r5 &
#exec xcompmgr -cCfF -t-5 -l-5 -r4.2 -o.55 -D5 &
#exec xcompmgr -cC -t-5 -l-5 -r4.2 -o.55 &
#exec xcompmgr -cC -o.55 &
#xcompmgr -c &

#feh --bg-center ~/pictures/wallpapers/bruce.jpg
#feh --bg-fill ~/pictures/norway-beauty.jpg
#feh --bg-fill ~/pictures/Wallpapers/alley-stairs-dark.jpg
#feh --bg-center ~/pictures/wallpapers/hd/wallpaper-1411581.png pictures/wallpapers/wallpaper-663431.jpg
#feh --bg-center pictures/wallpapers/asoiaf/stark.jpg pictures/wallpapers/asoiaf/targaryen.jpg 
#feh --bg-scale pictures/wallpapers/winter/o1027033.jpg pictures/wallpapers/rain/o1027636.jpg
#feh --bg-center pictures/Porn/husøya.natt.jpg 
#feh --bg-scale pictures/wallpapers/wallpaper-lighthouse-445739.jpg
#feh --bg-scale pictures/wallpapers/blade_runner_tokyo.jpg


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
xset s off
xset -dpms

launch-mpd

#android-receiver &
#wicd-gtk &

#(sleep 2s && trayer --edge top --align right --SetDockType true --expand true --widthtype pixel --width 80 --heighttype pixel --height 12 --expand true --tint 0x202020 --transparent true --alpha 0) &
transmission-remote-gtk &

#compton -b
#urxvtc 

#unclutter &

#source /etc/X11/xinit/xinitrc.d/30-dbus
#exec ck-launch-session xmonad

#exec xmonad
#exec bspwm

#exec dbus-launch --sh-syntax --exit-with-session /usr/bin/xmonad
#exec openbox-session
#herbstluftwm
#wmfs #-C /etc/xdg/wmfs/wmfsrc 

#ck-launch-session xmonad
#exec awesome
#exec subtle
#urxvt -e screen
#exec wmfs
#exec dwm

#DEFAULT_SESSION=xmonad
#case $1 in
#        xmonad)
#                exec dbus-launch xmonad
#                ;;
#
#        openbox)
#                exec openbox-session
#                ;;
#
#        dwm)
#                #conky -qc /home/sunn/.xmonad/.conky_dzen | dzen2 -x '666' -w '700' -h '12' -ta 'r' -bg '#222222' -fg '#bbbbbb' -y '0' -fn '-*-lime-*-*-*-*-*-*-*-*-*-*-*-*' &
#                #conky -qc /home/sunn/.xmonad/.conky_dzen | while read -r; do xsetroot -name "$REPLY"; done &
#                exec dbus-launch dwm-launch
#                ;;
#
#        bspwm|*)


#export PANEL_FIFO=/tmp/panel-fifo
#sxhkd &
#[ -e "$PANEL_FIFO" ] && rm "$PANEL_FIFO"
#mkfifo "$PANEL_FIFO"
#exec dbus-launch bspwm -c /home/rolf/.config/bspwm/autostart -s "$PANEL_FIFO" -p W
#                ;;
#esac

# Everything for XMonad
#feh --bg-center ~/pictures/Porn/cityporn/coruscant.jpg

#feh --bg-fill ~/pictures/wallpapers/o1907542.jpg ~/pictures/wallpapers/wallpaper-1403394.jpg
#feh --bg-scale ~/pictures/wallpapers/sanfrancisco-painting-cozy-YWzw1ij.jpg 

# wallpaper across both screens!
feh --no-xinerama --bg-scale ~/pictures/wallpapers/SerialExperimentsLain/1347198861461.jpg

xsetroot -cursor_name left_ptr
(sleep 2s && trayer --edge top --align right --SetDockType true --expand true --widthtype pixel --width 100 --heighttype pixel --height 16 --expand true --tint 0x000000 --transparent true --alpha 0) &
thunar --daemon &
urxvtd -q -f -o
g15-desktop-service start
g15-systemtray &
twitchnotifier -c rkaidgaming &
compton -c -r4 -l-6 -t-4 -z -b -G -C -e0.85 -f -D30 -I0.45 -O0.45 --paint-on-overlay --unredir-if-possible --backend glx --glx-no-stencil --glx-no-rebind-pixmap --vsync opengl

#exec dbus-launch xmonad
exec xmonad
echo "ferdig"
#android-receiver &

# KDE
#startkde
