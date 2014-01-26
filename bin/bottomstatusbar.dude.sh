#!/bin/bash
# Author: nnoell <nnoell3@gmail.com>
# Depends: dzen2-xft-xpm-xinerama-svn && conky
# Desc: dzen2 bar for XMonad, ran within xmonad.hs via spawnPipe

#Layout
BAR_H=9
BIGBAR_W=65
WIDTH_L=1138
WIDTH_R=784
HEIGHT=16
X_POS_L=1280
X_POS_R=2416
Y_POS=1064

#Colors and font
CRITORIG="#99cc66"
CRIT="#ff0000"
OKCOLOR="#44cc33"
#BAR_FG="#3955c4"
BAR_FG="#225599"
BAR_BG="#363636"
DZEN_FG="#9d9d9d"
DZEN_FG2="#444444"
#DZEN_FG2=$BAR_FG
DZEN_BG="#020202"
COLOR_SEP=$DZEN_FG2
OK_FG="#55ff55"
FONT="-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"

#Conky
CONKYFILE="${HOME}/bin/conkyrc.dude"
IFS='|'
INTERVAL=1
CPUTemp=0
GPUTemp=0
CPULoad0=0
CPULoad1=0
CPULoad2=0
CPULoad3=0
MpdInfo=0
MpdRandom="Off"
MpdRepeat="Off"

#clickable areas
VOL_TOGGLE_CMD="sh ${HOME}/bin/dzen/dzen_vol.sh t"
VOL_UP_CMD="sh ${HOME}/bin/dzen/dzen_vol.sh +"
VOL_DOWN_CMD="sh ${HOME}/bin/dzen/dzen_vol.sh -"
VOL_BAR_CMD="sh ${HOME}/bin/dzen/dzen_vol_bar.sh"
DROP_START_CMD="dropbox start"
DROP_STOP_CMD="dropbox stop"
MPD_REP_CMD="mpc -h 127.0.0.1 repeat"
MPD_RAND_CMD="mpc -h 127.0.0.1 random"
MPD_TOGGLE_CMD="ncmpcpp toggle"
MPD_STOP_CMD="ncmpcpp stop"
MPD_NEXT_CMD="ncmpcpp next"
MPD_PREV_CMD="ncmpcpp prev"

FM_THEVAULT_CMD="thunar /TheVault"
FM_THEVAULT_ALTCMD="urxvt -e ranger /TheVault"
FM_ROOT_CMD="thunar /"
FM_ROOT_ALTCMD="urxvt -e ranger /"
FM_HOME_CMD="thunar /home/rolf"
FM_HOME_ALTCMD="urxvt -e ranger /home/rolf"
PACMANXG_CMD="pacmanxg"
PACMAN_LIST_CMD="/home/rolf/bin/dzen/dzen_pacman.sh"

LOGSCRIPT="${HOME}/bin/dzen/dzen_log.sh"

ICONPATH="${HOME}/.config/icons/subtle/"
ICONPATH2="${HOME}/.config/icons/dzen/"

printVolInfo() {
	Perc=$(amixer get Master | grep "Mono:" | awk '{print $4}' | tr -d '[]%')
	Mute=$(amixer get Master | grep "Mono:" | awk '{print $6}')
	echo -n "^fg($DZEN_FG2) ^ca(1,$VOL_TOGGLE_CMD)VOL^ca() "
	if [[ $Mute == "[off]" ]]; then
		echo -n "$(echo $Perc | gdbar -fg $CRIT -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl) "
		echo -n "^fg()off"
	else
		echo -n "$(echo $Perc | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl) "
		echo -n "^fg()${Perc}%"
	fi
	return
}

printCPUInfo() {
	[[ $CPULoad0 -gt 70 ]] && CPULoad0="^fg($CRIT)$CPULoad0"
	#[[ $CPULoad1 -gt 70 ]] && CPULoad1="^fg($CRIT)$CPULoad1^fg()"
	#[[ $CPULoad2 -gt 70 ]] && CPULoad2="^fg($CRIT)$CPULoad2^fg()"
	#[[ $CPULoad3 -gt 70 ]] && CPULoad3="^fg($CRIT)$CPULoad3^fg()"
        echo -n "^fg($DZEN_FG2)^i("$ICONPATH"cpu.xbm) ^fg($BAR_FG)${CPULoad0}% ^fg()"
	#echo -n " ^fg($DZEN_FG2)CPU ^fg($BAR_FG)${CPULoad0}%^fg($DZEN_FG2)/^fg($BAR_FG)${CPULoad1}%^fg($DZEN_FG2)/^fg($BAR_FG)${CPULoad2}%^fg($DZEN_FG2)/^fg($BAR_FG)${CPULoad3}% "
	return
}

printNetInfo() {
        echo -n "^fg($DZEN_FG2)^i("$ICONPATH2"net_down_03.xbm) ^fg($BAR_FG)${Downspeed} ^fg($DZEN_FG2)^i("$ICONPATH2"net_up_03.xbm) ^fg($BAR_FG)${Upspeed} "
	return
}

printUptime() {
        echo -n "^ca(1,$LOGSCRIPT)^fg($DZEN_FG2)^i("$ICONPATH"pc.xbm) ^fg($BAR_FG)${Uptime}^ca() "
        return
}

printPacmanPackages() {
        PACKAGES=$(cat /home/rolf/tmp/availableupgrades)
        if [[ $PACKAGES == "0" ]]; then
                echo -n "^ca(1,$PACMAN_LIST_CMD)^ca(3,$PACMANXG_CMD)^fg($DZEN_FG2)^i("$ICONPATH"pacman.xbm) ^fg($OK_FG)^i("$ICONPATH"check2.xbm)^ca()^ca() "
        else
                echo -n "^ca(1,$PACMAN_LIST_CMD)^ca(3,$PACMANXG_CMD)^fg($DZEN_FG2)^i("$ICONPATH"pacman.xbm) ^fg($BAR_FG)${PACKAGES}^ca()^ca() "
        fi
        return
}

printTempInfo() {
        CPUTemp0=$(sensors | grep "CPU Temperature" | awk '{print $3}')
        GPUTemp0=$(aticonfig --od-gettemperature | grep C | cut -c 43-46)
	#CPUTemp0=$(acpi --thermal  | grep "0:" | awk '{print substr($4,0,2)}')
	#CPUTemp1=$(acpi --thermal  | grep "1:" | awk '{print substr($4,0,2)}')
	#[[ $CPUTemp0 -gt 70 ]] && CPUTemp0="^fg($CRIT)$CPUTemp0^fg()"
	#[[ $CPUTemp1 -gt 70 ]] && CPUTemp1="^fg($CRIT)$CPUTemp1^fg()"
        echo -n "^fg($DZEN_FG2)^i("$ICONPATH"temp.xbm) ^fg($BAR_FG)${CPUTemp0}/${GPUTemp0} "
	return
}

printDiskInfo() {
	RFSP=$(df -h / | tail -1 | awk '{ print $4 }' | sed 's/G/ GB/')
	HFSP=$(df -h /mnt/oldroot | tail -1 | awk '{ print $4 }' | sed 's/G/ GB/')
	VFSP=$(df -h /TheVault | tail -1 | awk '{ print $4 }' | sed 's/G/ GB/')

        echo -n "^fg($DZEN_FG2)^ca(1,$FM_THEVAULT_CMD)^ca(3,$FM_THEVAULT_ALTCMD)^i("$ICONPATH"diskette.xbm) ^fg($BAR_FG)${VFSP}^ca()^ca() "
        echo -n "^fg($DZEN_FG2)^ca(1,$FM_ROOT_CMD)^ca(3,$FM_ROOT_ALTCMD)^i("$ICONPATH2"root.xbm) ^fg($BAR_FG)${RFSP}^ca()^ca() "
        echo -n "^fg($DZEN_FG2)^ca(1,$FM_HOME_CMD)^ca(3,$FM_HOME_ALTCMD)^i("$ICONPATH"house.xbm) ^fg($BAR_FG)${HFSP}^ca()^ca() "
	return
}

printMemInfo() {
	[[ $MemPerc -gt 70 ]] && MemPerc="^fg($CRIT)$MemPerc^fg()"
        echo -n "^fg($DZEN_FG2)^i("$ICONPATH"memory.xbm) ^fg($BAR_FG)${MemUsed} "
	return
}

printDropBoxInfo() {
	DropboxON=$(ps -A | grep -c dropbox)
	if [[ $DropboxON == "0" ]]; then
                echo -n "^fg($DZEN_FG2)^ca(1,$DROP_START_CMD)^i("$ICONPATH"box_in.xbm)^ca() ^fg()Off"
	else
                echo -n "^fg($DZEN_FG2)^ca(1,$DROP_STOP_CMD)^i("$ICONPATH"box_in.xbm)^ca() ^fg($OKCOLOR)On"
	fi
	return
}

printMpdInfo() {
	MPDON=$(ps -A | grep -c mpd)
	if [[ $MPDON == "0" ]]; then
		echo -n "^fg($DZEN_FG2)^ca(1,mpd)MPD^ca() ^fg()Off"
	else
		#[[ $MpdRepeat == "On" ]] && MpdRepeat="^fg($CRIT)$MpdRepeat^fg()"
		#[[ $MpdRandom == "On" ]] && MpdRandom="^fg($CRIT)$MpdRandom^fg()"
		#echo -n "^fg($DZEN_FG2)^ca(1,$MPD_REP_CMD)REPEAT^ca() ^fg()$MpdRepeat "
		#echo -n "^fg($DZEN_FG2)| ^ca(1,$MPD_RAND_CMD)RANDOM^ca() ^fg()$MpdRandom "
		
	       #echo -n "^fg($DZEN_FG2)^ca(1,$MPD_TOGGLE_CMD)^ca(3,$MPD_STOP_CMD)^ca(4,$MPD_NEXT_CMD)^ca(5,$MPD_PREV_CMD)MPD^ca()^ca()^ca()^ca() $MpdInfo"
               echo -n "^fg($DZEN_FG2)^ca(1,"${HOME}/bin/dzen/dzen_music.sh")^ca(3,$MPD_NEXT_CMD)^i("$ICONPATH2"note.xbm)^ca()^ca() ^ca(1,$MPD_TOGGLE_CMD)$MpdInfo^ca()"
	fi
	return
}

printSpace() {
	echo -n " ^fg($COLOR_SEP)|^fg() "
	return
}

printLeft() {
	while true; do
		read CPULoad0 CPUFreq MemUsed MemPerc MpdInfo Downspeed Upspeed #MpdRandom MpdRepeat
		printVolInfo
		printSpace
		printDropBoxInfo
		printSpace
		printMpdInfo
		#echo -n " ^fg()>^fg($BAR_FG)>^fg($DZEN_FG2)>"
		echo
	done
	return
}

printRight() {
	while true; do
		read CPULoad0 CPUFreq MemUsed MemPerc MpdInfo Downspeed Upspeed Uptime #MpdRandom MpdRepeat
		printUptime
		printPacmanPackages
		#printSpace
		printNetInfo
		#printSpace
                printDiskInfo
		#printSpace
		printMemInfo
		#printSpace
		printCPUInfo
		printTempInfo
		#printSpace
		#printDateInfo
		echo
	done
	return
}

#Print all and pipe into dzen
conky -c $CONKYFILE -u $INTERVAL | printLeft  | dzen2 -x $X_POS_L -y $Y_POS -w $WIDTH_L -h $HEIGHT -fn $FONT -ta 'l' -bg $DZEN_BG -fg $DZEN_FG -p -e '' &
conky -c $CONKYFILE -u $INTERVAL | printRight | dzen2 -x $X_POS_R -y $Y_POS -w $WIDTH_R -h $HEIGHT -fn $FONT -ta 'r' -bg $DZEN_BG -fg $DZEN_FG -p -e ''
