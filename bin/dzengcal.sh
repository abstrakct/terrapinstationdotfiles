#!/bin/bash

# config
DEBUGTHISSCRIPT=false
AGENDA_FILENAME="${HOME}/.agenda"

# fonts and colors
FONT="-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"
CRIT="#55ff55"
JOBB="#ff2222"
LOCATION="#4682b4"
ATSIGN="#f2b844"
DEBUGTHISSCRIPT=true

MONTHNAMES=(pad Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec) # pad because months are 01..12, arrays start at 00
TODAY=$(date +'%b %d')

function create_agenda_file() {
        ENDDATE=$(date +'%d')
        REALENDDATE=`expr $ENDDATE + 8`
        MONTHNUM=$(date +'%m')
        MONTHNUM=`echo $MONTHNUM|sed 's/^0*//'`
        
        # Check if end date is > 31, and if so subtract 31 from end date and increase month number.
        if [[ "$REALENDDATE" -gt 28 ]]
        then
                TMP=`expr $REALENDDATE - 28`;
                REALENDDATE=$TMP;
                MONTHNUM=`expr $MONTHNUM + 1`;
        fi
        
        # Check if month number is > 12, if so move on to january
        if [[ $MONTHNUM -gt 12 ]]
        then
                MONTHNUM=1;
        fi
        
        MONTH=${MONTHNAMES[$MONTHNUM]}
        
        gcalcli --tsv agenda "$TODAY" "$MONTH $REALENDDATE" > $AGENDA_FILENAME 
}


function debuggery() {
# set DEBUGTHISSCRIPT=true to debug date parsing stuffz.
        if [[ $DEBUGTHISSCRIPT == true ]]
        then
                # debug
                echo $TODAY
                echo $ENDDATE
                echo $REALENDDATE
                echo $MONTHNUM
                echo $MONTH
        
                echo "from $TODAY to $MONTH $REALENDDATE"
        
                exit 0
        fi
}

if [[ $1 == "-c" ]]
then
        if [ -f $AGENDA_FILENAME ]
        then
                rm $AGENDA_FILENAME
        fi
        create_agenda_file
        #debuggery
        exit 0
fi

# | sed -r -e "1,4 s/.*/^fg(#3955c4)&^fg()/")
# | dzen2 -p 60 -fn $FONT -x 1750 -y 17 -w 170 -l 18 -sa l -e 'onstart=uncollapse;button1=exit'
# gcalcli --nc agenda | grep $MONTH | sed -r -e "s/(^| )($TODAY)(^| )/\1^bg()^fg($CRIT)\2^fg()^bg()/") \

# Get agenda for current month from gcalcli,
# highlight appointments occuring today

HEADER=$(
echo '^bg(#020202)^fg(#111111)'
echo '                ^fg(#3955c4)AGENDA')

#AGENDA=$(gcalcli --nc agenda "$TODAY" "$MONTH $REALENDDATE" | grep '  [0-9][0-9]' | \
#                sed -r -e "/vakt/s|^|^fg($JOBB)|" -e "/vakt/s|$|^fg()|" | \
#                sed -r -e "/$TODAY/s|^|^fg($CRIT)|" -e "/$TODAY/s|$|^fg()|" )

#echo ${myarray[0]} startdate
#echo ${myarray[1]} start
#echo ${myarray[2]} enddate
#echo ${myarray[3]} end
#echo ${myarray[4]} url
#echo ${myarray[5]} text
#echo ${myarray[6]} location
AGENDA=" "
while IFS=$'\t' read -r -a ag
do
        DATETEXT=$(date --date="${ag[0]}" +"%a %b %d")
        THISLINE="$DATETEXT   ${ag[1]} - ${ag[3]} ${ag[5]} ^fg($ATSIGN)@^fg() ^fg($LOCATION)${ag[6]}^fg()"
        THISLINE=$(echo $THISLINE | \
                sed -r -e "/vakt/s|^|^fg($JOBB)|" -e "/vakt/s|$|^fg()|" | \
                sed -r -e "/$TODAY/s|^|^fg($CRIT)|" -e "/$TODAY/s|$|^fg()|" )
        AGENDA="$AGENDA\n$THISLINE"
done < $AGENDA_FILENAME

LINES=$(echo -e "$HEADER" "\n\n" "$AGENDA" | wc -l)

#echo -e "$HEADER\n\n$AGENDA"
echo -e "$HEADER\n$AGENDA" | dzen2 -fn $FONT -x 2700 -y 16 -w 500 -l $LINES -p 60 -sa l -e 'onstart=uncollapse;button1=exit'
