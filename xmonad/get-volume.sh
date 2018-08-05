#!/bin/bash
# Get the maximum volume of any pulseaudio sink channel
# amixer get Master | egrep -o "[0-9]+%"
vol=$(amixer get Master | awk -F'[]%[]' '/%/ {if ($5 == "off") { print "off" } else { print $2"%" }}' | head -n 1)

echo $vol

exit 0
