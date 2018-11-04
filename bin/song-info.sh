#!/bin/bash

status=$(playerctl status)
if [ "$status" = "Playing" ]; then
    artist=$(playerctl metadata artist)
    song=$(playerctl metadata title)
    echo -n " $artist - $song"
elif [ "$status" = "Paused" ]; then
    echo -n " ❚❚"
elif [ "$status" = "Stopped" ]; then
    echo -n " ■"
fi
