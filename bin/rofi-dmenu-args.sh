#!/bin/bash
prompt="$1"
shift
old="$IFS"
IFS=$'\n'
echo -e "$*" | rofi -dmenu -theme lb -lines 3 -i -p "$prompt"
IFS=$old

