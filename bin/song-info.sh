#!/bin/bash

function restrict {
    str="$1"
    if [ ${#str} -gt 25 ]; then
        echo "${str:0:24}…"
    else
        echo "$str"
    fi
}

status=$(playerctl status)
if [ "$status" = "Playing" ]; then
    artist=$(playerctl metadata artist)
    song=$(playerctl metadata title)
    echo -n " $(restrict "$artist") - $(restrict "$song")"
elif [ "$status" = "Paused" ]; then
    echo -n " ❚❚"
elif [ "$status" = "Stopped" ]; then
    echo -n " ■"
fi
