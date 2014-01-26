#!/bin/bash
# Author: nnoell <nnoell3@gmail.com>
# Depends: dzen2-xft-xpm-xinerama-svn
# Desc: dzen2 bar for XMonad, ran within xmonad.hs via spawnPipe

#Layout
BAR_H=9
BIGBAR_W=65
WIDTH=1280
HEIGHT=16
X_POS=0
Y_POS=0

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

#printBattery() {
#	BatPresent=$(acpi -b | wc -l)
#	ACPresent=$(acpi -a | grep -c on-line)
#	if [[ $BatPresent == "0" ]]; then
#		echo -n "^fg($DZEN_FG2)AC ^fg($BAR_FG)on ^fg($DZEN_FG2)BAT ^fg($BAR_FG)off"
#		return
#	else
#		RPERC=$(acpi -b | awk '{print $4}' | tr -d "%,")
#		echo -n "^fg($DZEN_FG2)BAT "
#		if [[ $ACPresent == "1" ]]; then
#			echo -n "$(echo $RPERC | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl)"
#		else
#			echo -n "$(echo $RPERC | gdbar -fg $CRIT -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl)"
#		fi
#		echo -n " ^fg($DZEN_FG2)$RPERC%"
#	fi
#	return
#}
#
#printBrightnessInfo() {
#	BRIFILE=$(cat /sys/class/backlight/acpi_video0/actual_brightness)
#	Bri=$(expr $BRIFILE \* 10)
#	echo -n "^fg($DZEN_FG2)BRI ^fg($BAR_FG)${Bri}%"
#	return
#}
#
#printWifiInfo() {
#	WIFIDOWN=$(wicd-cli --wireless -d | wc -l)
#	WIFISIGNAL=0
##	[[ $WIFIDOWN -ne "1" ]] && WIFISIGNAL=$(wicd-cli --wireless -d | grep Quality | awk '{print $2}')
#	echo -n "^fg($DZEN_FG2)WIFI "
#	if [[ $WIFIDOWN -ne "1" ]]; then
#		WIFISIGNAL=$(wicd-cli --wireless -d | grep Quality | awk '{print $2}')
#		echo -n "$(echo $WIFISIGNAL | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl) "
#		echo -n "^fg()$WIFISIGNAL% "
#	else
#		echo -n "^fg($CRIT)N/A "
#	fi
#	return
#}

printDateInfo() {
        #echo -n "^ca(1,$CAL_CMD)^ca(3,$GCAL_CMD)^fg()$(date '+%A %B^fg(#444) ^fg(#3955c4)%d^fg(#444), ^fg()%Y^fg(#3955c4) ^fg(#363636)- ^fg()%H^fg(#444):^fg()%M^fg(#444)^fg()')^ca()^ca() "
        echo -n " ^fg()$(date '+%A %B^fg(#444) ^fg(#3955c4)%d^fg(#444), ^fg()%Y^fg(#3955c4) ^fg(#363636)- ^fg()%H^fg(#444):^fg()%M^fg(#444)^fg()')^ca()^ca() "
	return
}

printSpace() {
	echo -n " ^fg($COLOR_SEP)|^fg() "
	return
}

printTopBar() {
	while true; do
	        printDateInfo
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
