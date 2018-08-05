#!/bin/bash

set -e

backlight_dir=/sys/class/backlight/intel_backlight
cur_brightness=$(cat $backlight_dir/brightness)
min_brightness=40
max_brightness=$(cat $backlight_dir/max_brightness)

action=$1
step=$2

if [ -z $step ]; then
    step=60
fi

if [[ ! $action =~ ^(up|down)$ ]]; then
    echo "up or down"
    exit 1
fi

function bup {
    new_brightness=`expr $cur_brightness + $step`
    if [ $new_brightness -gt $max_brightness ]; then
        new_brightness=$max_brightness
    fi
}

function bdown {
    new_brightness=`expr $cur_brightness - $step`
    if [ $new_brightness -lt $min_brightness ]; then
        new_brightness=$min_brightness
    fi
}

if [ $action = "up" ]; then
    bup
else
    bdown
fi

echo $new_brightness > $backlight_dir/brightness
