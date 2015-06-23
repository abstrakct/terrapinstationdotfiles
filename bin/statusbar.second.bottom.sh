#!/bin/bash
# Author: nnoell <nnoell3@gmail.com>
# Depends: dzen2-xft-xpm-xinerama-svn
# Desc: dzen2 bar for XMonad, ran within xmonad.hs via spawnPipe

#Layout
BAR_H=9
BIGBAR_W=65
WIDTH=1680
HEIGHT=16
X_POS=1920
Y_POS=1034

#Colors and font
FONT="-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"
DZEN_BG="#020202"
DZEN_FG="#9d9d9d"
DZEN_FG2="#444444"
CRIT="#99cc66"
BAR_FG="#3955c4"
BAR_BG="#363636"
COLOR_SEP=$DZEN_FG2

CAL_CMD="sh ${HOME}/bin/dzencal.sh"
GCAL_CMD="sh ${HOME}/bin/dzengcal.sh"

#Options
INTERVAL=30
WIFISIGNAL=0
ICONPATH="${HOME}/.config/icons/subtle/"

printDateInfo() {
        echo -n "^ca(1,$CAL_CMD)^ca(3,$GCAL_CMD)^fg()$(date '+%A %B^fg(#444) ^fg(#3955c4)%d^fg(#444), ^fg()%Y^fg(#3955c4) ^fg(#363636)- ^fg()%H^fg(#444):^fg()%M^fg(#444)^fg()')^ca()^ca() "
	return
}

printWeather() {
        WEATHER=$(python2 /home/rolf/bin/yaws-1.5/yaws.py 63.41 10.37 --msl 64 --locale no --template /home/rolf/bin/yaws-1.5/statusbar.template | head -1)
        echo -n " " $WEATHER
        return
}

printSpace() {
	echo -n " ^fg($COLOR_SEP)|^fg() "
	return
}

printTopBar() {
	while true; do
	        printWeather
	        #printDateInfo
		#printDiskInfo
		#printSpace
		#printBattery
		#printSpace
		#printBrightnessInfo
		#printSpace
		#printWifiInfo
		echo
		sleep $INTERVAL
	done
	return
}

#Print all and pipe into dzen2
printTopBar | dzen2 -x $X_POS -y $Y_POS -w $WIDTH -h $HEIGHT -fn $FONT -ta 'l' -bg $DZEN_BG -fg $DZEN_FG -p -e ''
#tray
