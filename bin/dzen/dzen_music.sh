#!/bin/bash
#
# (c) Ippytraxx 2012 v.szolnoky@tele2.se
#
#Colours

source $(dirname $0)/config.sh
background="#000000"
foreground=$blue0

#FONT="-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"
XPOS=$(xdotool getmouselocation | awk -F " " '{print $1}' | cut -d ":" -f 2)
export XPOS
YPOS="966"
HEIGHT="20"
WIDTH="500"
LINES="20"

PREV_CMD="mpc prev"
NEXT_CMD="mpc next"
TOGGLE_CMD="mpc toggle"
PAUSE_CMD="mpc pause"
PLAY_CMD="mpc play"

ARTIST_CMD="/home/rolf/bin/dzen/dzen_artistinfo.sh"
ALBUM_CMD="/home/rolf/bin/dzen/dzen_albuminfo.sh"
LYRICS_CMD="/home/rolf/bin/dzen/dzen_lyrics.sh"

FORMAT="%artist% %title% %album%"

ICONPATH="${HOME}/.config/icons/subtle/"

playing=$(mpc -f "$FORMAT" current)
playingshort=$(mpc current)
playingalbum=$(mpc -f "%album%" current)
artist=$(mpc current -f %artist%)
album=$(mpc current -f %album%)
track=$(mpc current -f %title%)
year=$(mpc current -f %date%)
stats=$(mpc stats)
#playlist=$(mpc playlist | sed "s/$playing/> $playing/")
playlistcurrent=$(mpc -f "$FORMAT" playlist | grep -n "$playing" | cut -d ":" -f 1 | head -n1)
nextnum=$(( $playlistcurrent + 1 ))
prevnum=$(( $playlistcurrent - 1 ))
next=$(mpc playlist | sed -n ""$nextnum"p")
prev=$(mpc playlist | sed -n ""$prevnum"p")

# (echo "^fg($foreground)Music"; echo -e "Now playing:\n^fg($foreground)$playingshort\n^fg($cyan)$playingalbum"; echo " "; echo " ^ca(1, $PREV_CMD) ^fg(#556c85)^i("$ICONPATH"player_prev1.xbm) ^ca() ^ca(1, $PAUSE_CMD) ^i("$ICONPATH"player_pause1.xbm) ^ca() ^ca(1, $PLAY_CMD) ^i("$ICONPATH"player_play1.xbm) ^ca() ^ca(1, $NEXT_CMD) ^i("$ICONPATH"player_next1.xbm) ^ca()"; echo " "; echo "Next: ^fg($foreground)$next"; echo "Prev: ^fg($foreground)$prev"; echo " "; echo "$stats"; sleep 15) | dzen2 -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -sa c -e 'onstart=uncollapse,hide;button3=exit'
#(echo "^fg($foreground)Music"; echo -e "^fg($cyan)Now playing\n\n^fg($foreground)^ca(1,$TOGGLE_CMD)$playingshort^ca()\n^fg(#44cc33)$playingalbum"; echo " "; echo "^ca(1,$NEXT_CMD)Next: ^fg($foreground)$next^ca()"; echo "^ca(1,$PREV_CMD)Prev: ^fg($foreground)$prev^ca()"; echo " "; echo "$stats"; sleep 15) | dzen2 -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -sa c -e 'onstart=uncollapse,hide;button1=exit;button3=exit'
(echo "^fg($foreground)Music"; echo -e "^fg($cyan0)Now playing\n\n^fg($foreground)^ca(1,$ARTIST_CMD)$artist^ca()\n^fg($blue1)^ca(1,$LYRICS_CMD)$track^ca()\n^fg($cyan1)^ca(1,$ALBUM_CMD)$album ($year)^ca()"; echo " "; echo "^ca(1,$NEXT_CMD)Next: ^fg($foreground)$next^ca()"; echo "^ca(1,$PREV_CMD)Prev: ^fg($foreground)$prev^ca()"; echo " "; echo "^fg($green2)Statistics:"; echo "$stats"; sleep 15) | dzen2 -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -sa c -e 'onstart=uncollapse,hide;button1=exit;button3=exit'
